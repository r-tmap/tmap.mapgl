# ============================================================
#  Shared helpers
# ============================================================

# Build a MapLibre 'match' expression from a categorical scale mapping.
# Returns a properly nested list that serialises to
#   ["match", ["get", var], level1, value1, ..., fallback]
build_match_expr = function(var, mapping) {
	pairs = as.list(rbind(mapping$levels_orig, mapping$values_orig))  # interleaved pairs
	c(list("match", list("get", var)), pairs, list(mapping$value_na))
}

# Factory: returns a get_pmt_aes() closure over dt and .TMAP.
# For scale-driven aesthetics it resolves the MapLibre expression;
# for fixed values it returns the raw scalar from dt[[a]][1].
make_get_pmt_aes = function(dt) {
	function(a) {
		v      = dt[[a]][1]
		is_var = substr(v, 1, 5) == "scale"
		if (!is_var) return(v)

		snr     = as.numeric(substr(v, 6, 8))
		leg     = .TMAP$legs[[snr]]
		var     = substr(v, 10, nchar(v))
		mapping = leg$layer_args$mapping

		if (!is.null(mapping$levels_orig)) {
			build_match_expr(var, mapping)
		} else {
			mapgl::get_column(var)
		}
	}
}

# ------------------------------------------------------------
#  Interactive-legend categorical columns
# ------------------------------------------------------------
# A data layer can carry up to two data-driven colour aesthetics: the body
# (fill) and the outline (col). Each gets its own legend, and mapgl filters a
# layer by matching a *data column* against the clicked category. We therefore
# materialise a dedicated category column per aesthetic on the source:
#   __tmap_cat_fill__  – for the fill legend
#   __tmap_cat_col__   – for the border/line (col) legend
# Using one shared column (the old "__tmap_cat__") made the two legends collide:
# both keyed off the same column and both targeted the fill layer, so the col
# legend filtered on the fill aesthetic (and clobbered the fill legend's filter,
# since mapgl lets only one interactive legend own a layer).
#
# Which legend backs an aesthetic is found by glid + colour matching: among the
# legends carrying this layer's glid, the backing one is whichever's swatch
# colours (vvalues) actually appear in the resolved per-feature colours. This is
# the original approach (it does NOT parse dt — dt's "scaleNNN_var" encoding only
# exists in the PMTiles path, not the sf path, where colours are already
# resolved). NULL is returned for a constant or continuous aesthetic.

mapgl_cat_info = function(glid, vals) {
	non_na = !is.na(vals)
	if (!any(non_na)) return(NULL)

	# Normalise colours to upper-case #RRGGBB before matching, so an alpha suffix
	# (#RRGGBBAA) or case difference between the resolved fills and the swatch
	# colours doesn't break the match. (Categorical/interval palettes have
	# distinct RGB per class, so dropping alpha never collapses two classes.)
	norm = function(x) toupper(substr(as.character(x), 1L, 7L))
	vn_all = norm(vals)
	vn = vn_all[non_na]

	best = NULL
	best_frac = 0
	best_cc = NULL
	for (leg in .TMAP$legs) {
		if (!("glid" %in% names(leg)) || !isTRUE(leg$glid == glid)) next
		cc = leg$vvalues
		cv = leg$labels
		if (is.null(cc) || is.null(cv)) next
		ccn = norm(cc)
		frac = mean(vn %in% ccn)
		if (frac > best_frac) { best_frac = frac; best = leg; best_cc = ccn }
	}

	# A discrete scale (categorical OR binned numeric) paints (almost) every
	# feature with a swatch colour; a continuous scale interpolates off-swatch
	# colours, so its match fraction is tiny. Require a majority to treat the
	# aesthetic as categorical/interactive (and leave continuous to the
	# non-interactive gradient legend).
	if (is.null(best) || best_frac < 0.5) return(NULL)

	list(
		labels = best$labels[match(vn_all, best_cc)],
		values = best$labels,
		colors = best$vvalues
	)
}

# Attach the categorical columns for the fill and/or col aesthetics to the
# source `shp2`, and record in .TMAP$mapgl_cat[[glid]] which aesthetics are
# categorical plus the concrete fill/border layer names. The legend renderer
# (mapgl_legend_target()) reads this registry to pick the right target layer(s)
# and filter column. `border_layer = NA` marks geometries with no separate
# outline layer (tm_symbols' single circle layer); `fill_layer = NA` marks
# outline-only geometries (lines).
mapgl_attach_cat = function(shp2, glid, fill_layer = NA_character_, border_layer = NA_character_,
							fill_col = "fill", col_col = "col") {
	if (is.null(.TMAP$mapgl_cat)) .TMAP$mapgl_cat = list()
	reg = list(fill = NULL, col = NULL, fill_layer = fill_layer, border_layer = border_layer)

	if (!is.null(fill_col) && fill_col %in% names(shp2)) {
		fi = mapgl_cat_info(glid, shp2[[fill_col]])
		if (!is.null(fi)) {
			shp2[["__tmap_cat_fill__"]] = fi$labels
			reg$fill = list(column = "__tmap_cat_fill__", values = fi$values, colors = fi$colors)
		}
	}
	if (!is.null(col_col) && col_col %in% names(shp2)) {
		ci = mapgl_cat_info(glid, shp2[[col_col]])
		if (!is.null(ci)) {
			shp2[["__tmap_cat_col__"]] = ci$labels
			reg$col = list(column = "__tmap_cat_col__", values = ci$values, colors = ci$colors)
		}
	}
	.TMAP$mapgl_cat[[glid]] = reg
	shp2
}

# Shared PMTiles guard: emits cli messages and returns TRUE when the
# pointer cannot be rendered, so callers can do:
#   if (pmtiles_unsupported(shpTM, mode)) return(NULL)
pmtiles_unsupported = function(shpTM, mode) {
	smeta = shpTM$smeta
	if (mode == "mapbox") {
		cli::cli_inform("Source shapes are not supported in {.str mapbox} mode yet, only in {.str maplibre}")
		return(TRUE)
	}
	if (smeta$type != "pmtiles") {
		cli::cli_inform("Source shapes other than PMTiles not supported yet")
		return(TRUE)
	}
	if (smeta$tile_type != "mvt") {
		cli::cli_inform("Source shape is not a vector format")
		return(TRUE)
	}
	FALSE
}

# ============================================================
#  Popup helper
# ============================================================

# Popup styling (width, max.height, the per-region align/color, and css) lives
# on the tm_popup() `layout` list built in tmap core and is threaded here as
# `popup.layout`. Core is the single source of truth: it owns the defaults
# (popup_layout_default()) and the resolution of bare numbers to CSS lengths
# (complete_popup_layout(): width -> px, max.height -> em, NA/Inf -> "none").
# We reuse those directly via getFromNamespace() — the same pattern the package
# uses elsewhere (e.g. pane_name) — so there is no duplicated default table or
# number-resolution logic to keep in sync with view mode.

view_format_popups_mapgl = function(id = NULL, titles, format, values, layout = NULL) {
	complete_popup_layout = utils::getFromNamespace("complete_popup_layout", "tmap")
	style_attr            = utils::getFromNamespace(".popup_style_attr", "tmap")
	layout = complete_popup_layout(layout)

	h = lapply(format, function(f) {
		if (f$html.escape) {
			htmltools::htmlEscape
		} else {
			function(x) x
		}
	})

	if (!is.null(id)) {
		labels = paste0("<b>", h[[1]](id), "</b>")
	} else {
		labels = ""
	}

	titles_format = mapply(function(ti, hi) {
		hi(ti)
	}, titles, h, SIMPLIFY = FALSE)

	values_format = mapply(function(v, f, hi) {
		if (inherits(v, "units")) {
			popup_append = paste0(" ", as.character(attr(v, "units")))
		} else {
			popup_append = ""
		}
		numbers = hi(if (is.numeric(v)) do.call("fancy_breaks", c(list(vec = as.numeric(v), intervals = FALSE), f)) else v)
		paste0(numbers, popup_append)
	}, values, format, h, SIMPLIFY = FALSE)

	# Per-region styling (alignment + color), shared with view mode.
	td_label = paste0("<td class=\"tmap-popup-label\"", style_attr(layout$label.align, layout$label.color), "><nobr>")
	td_value = paste0("<td class=\"tmap-popup-value\"", style_attr(layout$value.align, layout$value.color), "><nobr>")
	th_attr  = style_attr(layout$title.align, layout$title.color)

	labels2 = mapply(function(l, v) {
		paste0("<tr>", td_label, l, "</nobr></td>", td_value, v, "</nobr></td>")
	}, titles_format, values_format, SIMPLIFY = FALSE)

	labels3 = paste0(do.call("paste", c(labels2, list(sep = "</tr>"))), "</tr>")

	# max.height = "none" removes the cap (popup grows to fit, never scrolls).
	# When capped, reserve scrollbar space with padding-right so the scroll bar
	# doesn't overlap the values (notably Safari's overlay scrollbar). Mirrors
	# view mode: pad once the table is long enough to scroll (~>13 lines).
	padding_right = if (length(titles_format) > 13) 15 else 0
	mh = layout$max.height
	no_cap = is.null(mh) || identical(mh, "none") || (length(mh) == 1L && is.na(mh))
	if (no_cap) {
		div_style = paste0("width:", layout$width, "; overflow-x:hidden;")
	} else {
		div_style = paste0("width:", layout$width, "; max-height:", mh, "; overflow-y:auto; overflow-x:hidden; padding-right:", padding_right, "px;")
	}

	# Mirror view mode: with a fixed width let the table fill it (extra space
	# falls between the label and value columns); with "auto" the table shrinks
	# to fit. This plain table replaced the width:100%/table-layout:fixed version
	# that made popups ~150% too wide and split the columns evenly.
	table_style = if (identical(layout$width, "auto")) "" else " style=\"width:100%;\""

	# Free-form user CSS (tm_popup(css=)) injected verbatim. Target classes:
	# .tmap-popup, .tmap-popup-table, .tmap-popup-title, .tmap-popup-label,
	# .tmap-popup-value. To resize the popup box itself, target MapLibre's own
	# .maplibregl-popup-content.
	css_block = if (!is.null(layout$css) && length(layout$css) == 1L && !is.na(layout$css) && nzchar(layout$css)) {
		paste0("<style>", layout$css, "</style>")
	} else {
		""
	}

	paste0(
		css_block,
		"<div class=\"tmap-popup\" style='", div_style, "'>",
		"<table class=\"tmap-popup-table\"", table_style, ">",
		"<thead><tr><th class=\"tmap-popup-title\" colspan=\"2\"", th_attr, ">", labels, "</th></tr></thead>",
		labels3,
		"</table></div>"
	)
}

# ============================================================
#  Shared hover / popup attachment for sf-backed layers
# ============================================================

# Attaches hover text and popup HTML columns to shp2 and returns
# the resolved tooltip/popup arguments ready for add_*_layer().
#
# Returns a list:
#   $shp2    – the sf data frame, possibly with $hover and/or $popup columns added
#   $hdt_arg – value to pass as tooltip= (NULL or mapgl::get_column("hover"))
#   $pdt_arg – value to pass as popup=   (NULL or mapgl::get_column("popup"))
attach_hover_popup = function(shp2, dt, hdt, pdt, idt, popup.format, popup.layout = NULL, ptdt = NULL) {

	hdt_arg = NULL
	pdt_arg = NULL

	if (!is.null(hdt)) {
		shp2$hover = hdt$hover[match(dt$tmapID__, hdt$tmapID__)]
		shp2$hover = vapply(shp2$hover, htmltools::HTML, FUN.VALUE = character(1))
		hdt_arg    = mapgl::get_column("hover")
	}

	if (!is.null(pdt)) {
		mtch    = match(dt$tmapID__, pdt$tmapID__)
		pdt_sub = pdt[mtch][, tmapID__ := NULL]

		# Popup header: an explicit popup title (tm_popup(title=), carried in
		# ptdt) takes precedence; otherwise fall back to the hover text, then to
		# no header. Mirrors the Leaflet backend's title resolution.
		id_arg = if (!is.null(ptdt)) {
			ptdt$title[match(dt$tmapID__, ptdt$tmapID__)]
		} else if (!is.null(hdt)) {
			shp2$hover
		} else {
			NULL
		}

		shp2$popup = view_format_popups_mapgl(
			id     = id_arg,
			titles = names(pdt_sub),
			values = pdt_sub,
			format = popup.format,
			layout = popup.layout
		)
		pdt_arg = mapgl::get_column("popup")
	}

	list(shp2 = shp2, hdt_arg = hdt_arg, pdt_arg = pdt_arg)
}

# ============================================================
#  split_alpha_channel
# ============================================================

split_alpha_channel = function(x, alpha) {
	if (is.null(x)) {
		list(col = NULL, opacity = 0)
	} else {
		RGBA    = col2rgb(x, alpha = TRUE)
		col     = rgb(RGBA[1, ], RGBA[2, ], RGBA[3, ], maxColorValue = 255)
		opacity = unname(RGBA[4, ] / 255 * alpha)
		list(col = col, opacity = opacity)
	}
}

# ============================================================
#  lty2dash
# ============================================================

lty2dash = function(lty) {
	tab = c(solid = "", dashed = "4 4", dotted = "1 3", dotdash = "1 3 4 3",
			 longdash = "7 3", twodash = "2 2 6 2")
	are_words   = (lty %in% names(tab))
	if (all(are_words)) {
		unname(tab[lty])
	} else {
		are_letters = (suppressWarnings(!is.na(as.numeric(lty))))
		if (!all(are_letters | are_words)) {
			stop("Incorrect lty specification: ", lty[which(!are_letters & !are_words)[1]])
		} else {
			lty[are_words]   = unname(tab[lty[are_words]])
			lty[are_letters] = vapply(
				strsplit(lty[are_letters], ""),
				FUN = function(x) paste(x, collapse = " "),
				FUN.VALUE = character(1)
			)
		}
		lty
	}
}

# ============================================================
#  Source id helper
# ============================================================

# Source ids must be unique per render. clear_layer() (used by tm_remove_layer
# in proxy mode) removes *layers* but not their *sources*, so reusing a source
# id on a redraw collides with the still-present stale source: MapLibre keeps
# the old data and the re-added layers recolour from it. .TMAP$stamp is bumped
# by print.tmap() on every render, giving a fresh source id each time. Layer
# ids stay stable (glid-based) so the tm_remove_layer registry keeps matching.
mapgl_srcid = function(base) sprintf("%s_%.0f", base, .TMAP$stamp * 1e6)

# ============================================================
#  Layer blending guard
# ============================================================

# MapLibre / Mapbox GL composite all layers in a single WebGL context with
# standard (source-over) alpha blending; the GL style spec exposes no
# per-layer compositing operator (unlike grid::groupGrob() in plot mode or CSS
# mix-blend-mode in Leaflet view mode). So a non-trivial `blend` value cannot
# be honoured here. We inform once per layer and render without blending,
# mirroring how the package already reports other unsupported features.
mapgl_blend_unsupported = function(a, mode) {
	b = a$blend
	if (!is.null(b) && length(b) == 1L && !is.na(b) && !identical(b, "over")) {
		cli::cli_inform(c(
			"!" = "Layer blending ({.code blend = {.str {b}}}) is not supported in {.str {mode}} mode.",
			"i" = "The layer is rendered without blending. Use {.str plot} or {.str view} mode to enable layer blending."
		))
	}
	invisible(NULL)
}

# ============================================================
#  mapgl_polygons
# ============================================================

mapgl_polygons = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp,
						   bbx, facet_row, facet_col, facet_page,
						   id, pane, group, glid, o, ..., mode, popup.layout = NULL, ptdt = NULL) {

	m = get_mapgl(facet_row, facet_col, facet_page, mode)
	mapgl_blend_unsupported(a, mode)
	rc_text    = frc(facet_row, facet_col)
	shp_is_pointer = inherits(shpTM$shp, "character")

	# ----------------------------------------------------------
	#  PMTiles branch
	# ----------------------------------------------------------
	if (shp_is_pointer) {
		if (pmtiles_unsupported(shpTM, mode)) return(NULL)

		smeta      = shpTM$smeta
		srcname    = mapgl_srcid(paste0("layer", pane))
		layername1 = paste0(glid, "polygons_fill")
		layername2 = paste0(glid, "polygons_border")
		url        = smeta$url

		get_pmt_aes = make_get_pmt_aes(dt)

		aes_f   = get_pmt_aes("fill")
		aes_fo  = get_pmt_aes("fill_alpha")
		aes_c   = get_pmt_aes("col")
		aes_co  = get_pmt_aes("col_alpha")
		aes_lwd = get_pmt_aes("lwd")

		# Note: hover / popup over PMTiles are intentionally not supported here —
		# the popup data lives in dt (R memory) while tiles are fetched remotely,
		# so there is no row-level join available at render time.

		m |>
			mapgl::add_pmtiles_source(id = srcname, url = url) |>
			mapgl::add_fill_layer(layername1, source = srcname,
								  source_layer  = smeta$layer,
								  fill_color    = aes_f,
								  fill_opacity  = aes_fo) |>
			mapgl::add_line_layer(layername2, source = srcname,
								  source_layer  = smeta$layer,
								  line_color    = aes_c,
								  line_opacity  = aes_co,
								  line_width    = aes_lwd) |>
			assign_mapgl(facet_row, facet_col, facet_page, mode = mode)

		mapgl_submit_group(group, c(layername1, layername2), mode, pane)
		return(NULL)
	}

	# ----------------------------------------------------------
	#  sf branch
	# ----------------------------------------------------------
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt  = res$dt

	x = sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1, 3)]), bbx[2]))), crs = sf::st_crs(bbx))
	y = sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1, 3)]), bbx[4]))), crs = sf::st_crs(bbx))

	gp = impute_gp(gp, dt)
	gp = rescale_gp(gp, o$scale_down)

	if (any(nchar(gp$fill) == 9)) {
		fa        = split_alpha_channel(gp$fill, alpha = gp$fill_alpha)
		gp$fill   = fa$col
		gp$fill_alpha = gp$fill_alpha * fa$opacity
	}
	if (any(nchar(gp$col) == 9)) {
		fa        = split_alpha_channel(gp$col, alpha = gp$col_alpha)
		gp$col    = fa$col
		gp$col_alpha = gp$fill_alpha * fa$opacity
	}

	shp2 = sf::st_sf(
		unclass(gp[c("fill", "col", "lwd", "fill_alpha", "col_alpha")]),
		id       = 1:length(shp),
		geometry = shp
	)

	srcname    = mapgl_srcid(paste0("layer", pane))
	layername1 = paste0(glid, "polygons_fill")
	layername2 = paste0(glid, "polygons_border")

	# Per-aesthetic categorical columns for the interactive legend(s).
	shp2 = mapgl_attach_cat(shp2, glid, fill_layer = layername1, border_layer = layername2)

	ahp  = attach_hover_popup(shp2, dt, hdt, pdt, idt, popup.format, popup.layout, ptdt)
	shp2 = ahp$shp2

	m |>
		mapgl::add_source(srcname, data = shp2) |>
		mapgl::add_fill_layer(layername1, source = srcname,
							  fill_color   = mapgl::get_column("fill"),
							  fill_opacity = mapgl::get_column("fill_alpha"),
							  tooltip      = ahp$hdt_arg,
							  popup        = ahp$pdt_arg) |>
		mapgl::add_line_layer(layername2, source = srcname,
							  line_color   = mapgl::get_column("col"),
							  line_opacity = mapgl::get_column("col_alpha"),
							  line_width   = mapgl::get_column("lwd")) |>
		assign_mapgl(facet_row, facet_col, facet_page, mode = mode)

	mapgl_submit_group(group, c(layername1, layername2), mode, pane)
	NULL
}

# ============================================================
#  mapgl_polygons_3d
# ============================================================
# fill-extrusion paint props added in mapgl >= 0.4.6. Forward ONLY the props
# the user set AND that the active engine supports: MapLibre's style spec only
# implements fill-extrusion-vertical-gradient, whereas ambient occlusion, cast
# shadows and emissive strength are Mapbox GL JS 3D-lighting features. Sending
# an unknown paint property makes MapLibre reject the entire layer (the polygons
# then render with no fill), so Mapbox-only props are dropped in maplibre mode
# with an informative message. (If a future MapLibre adds e.g. emissive strength
# to its spec, flip that entry's `mapbox_only` to FALSE.)
mapgl_fe_props = function(a, mode) {
	specs = list(
		list(arg = "cast.shadows",                param = "fill_extrusion_cast_shadows",                mapbox_only = TRUE),
		list(arg = "ambient.occlusion.intensity", param = "fill_extrusion_ambient_occlusion_intensity", mapbox_only = TRUE),
		list(arg = "ambient.occlusion.radius",    param = "fill_extrusion_ambient_occlusion_radius",    mapbox_only = TRUE),
		list(arg = "vertical.gradient",           param = "fill_extrusion_vertical_gradient",           mapbox_only = FALSE),
		list(arg = "emissive.strength",           param = "fill_extrusion_emissive_strength",           mapbox_only = TRUE)
	)
	is_mapbox = identical(mode, "mapbox")

	out     = list()
	dropped = character(0)
	for (s in specs) {
		val = a[[s$arg]]
		if (is.null(val)) next
		if (s$mapbox_only && !is_mapbox) {
			dropped = c(dropped, s$arg)
		} else {
			out[[s$param]] = val
		}
	}
	if (length(dropped)) {
		cli::cli_inform(c(
			"!" = "These {.fn tm_polygons_3d} options are Mapbox-only and were ignored in {.str {mode}} mode: {.field {dropped}}.",
			"i" = "Use {.code tmap_mode(\"mapbox\")} for ambient occlusion, cast shadows, and emissive strength."
		))
	}
	out
}

mapgl_polygons_3d = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp,
							  bbx, facet_row, facet_col, facet_page,
							  id, pane, group, glid, o, ..., mode, popup.layout = NULL, ptdt = NULL) {

	m       = get_mapgl(facet_row, facet_col, facet_page, mode)
	mapgl_blend_unsupported(a, mode)
	rc_text = frc(facet_row, facet_col)
	shp_is_pointer = inherits(shpTM$shp, "character")

	# ----------------------------------------------------------
	#  PMTiles branch
	# ----------------------------------------------------------
	if (shp_is_pointer) {
		if (pmtiles_unsupported(shpTM, mode)) return(NULL)

		smeta      = shpTM$smeta
		srcname    = mapgl_srcid(paste0("layer", pane))
		layername1 = paste0(glid, "polygons_fill")
		layername2 = paste0(glid, "polygons_border")
		url        = smeta$url

		get_pmt_aes = make_get_pmt_aes(dt)

		aes_f   = get_pmt_aes("fill")
		aes_fo  = get_pmt_aes("fill_alpha")
		aes_c   = get_pmt_aes("col")
		aes_co  = get_pmt_aes("col_alpha")
		aes_lwd = get_pmt_aes("lwd")
		aes_h_raw = get_pmt_aes("height")

		# --- read limits from tm_scale_continuous -------------------------------
		h_var = dt[["height"]][1]
		if (substr(h_var, 1, 5) == "scale") {
			snr = as.numeric(substr(h_var, 6, 8))
			leg = .TMAP$legs[[snr]]
			limits = leg$layer_args$limits
			if (is.null(limits)) {
				cli::cli_abort(c(
					"!" = "Cannot determine height range for PMTiles source.",
					"i" = "Specify {.code height.scale = tm_scale_continuous(limits = c(min, max))}.",
					"i" = "Example: {.code tm_scale_continuous(limits = c(0, 28672))}"
				))
			}
			h_min_raw = limits[1]
			h_max_raw = limits[2]
		} else {
			cli::cli_abort(c(
				"!" = "Height aesthetic for PMTiles must use {.fn tm_scale_continuous}.",
				"i" = "Specify {.code height.scale = tm_scale_continuous(limits = c(min, max))}."
			))
		}

		# --- resolve height.max / height.min to metres -------------------------
		if (is.character(a$height.max)) {
			is_perc_max = grepl("%$", a$height.max)
			height.max  = as.numeric(sub("%$", "", a$height.max))
			if (is_perc_max) height.max = height.max / 100
		} else {
			is_perc_max = FALSE
			height.max  = as.numeric(a$height.max)
		}

		if (is.character(a$height.min)) {
			is_perc_min = grepl("%$", a$height.min)
			height.min  = as.numeric(sub("%$", "", a$height.min))
			if (is_perc_min) height.min = height.min / 100
		} else {
			is_perc_min = FALSE
			height.min  = as.numeric(a$height.min)
		}

		if (is_perc_max || is_perc_min) {
			sqrt_area_m = if (consider_global(bbx)) {
				sqrt(5.1e+14)
			} else {
				bbx |>
					tmaptools::bb_poly() |>
					sf::st_area() |>
					sqrt() |>
					units::set_units("m") |>
					units::drop_units()
			}
			if (is_perc_max) height.max = sqrt_area_m * height.max
			if (is_perc_min) height.min = sqrt_area_m * height.min
		}

		# --- build MapLibre scaling expression ---------------------------------
		# Replicates: height.min + (raw - h_min_raw) / (h_max_raw - h_min_raw)
		#                        * (height.max - height.min)
		h_range   = h_max_raw - h_min_raw
		out_range = height.max - height.min

		aes_h_scaled = list(
			"+",
			height.min,
			list("*",
				 list("/",
				 	 list("-", aes_h_raw, h_min_raw),
				 	 h_range),
				 out_range)
		)

		m = m |>
			mapgl::add_pmtiles_source(id = srcname, url = url) |>
			mapgl::add_line_layer(layername2, source = srcname,
								  source_layer          = smeta$layer,
								  line_color            = aes_c,
								  line_opacity          = aes_co,
								  line_width            = aes_lwd)

		m = do.call(mapgl::add_fill_extrusion_layer, c(
			list(m, layername1,
				 source                 = srcname,
				 source_layer           = smeta$layer,
				 fill_extrusion_color   = aes_f,
				 fill_extrusion_base    = 0,
				 fill_extrusion_height  = aes_h_scaled),
			mapgl_fe_props(a, mode)))

		m |> assign_mapgl(facet_row, facet_col, facet_page, mode = mode)

		mapgl_submit_group(group, c(layername1, layername2), mode, pane)
		return(NULL)
	}

	# ----------------------------------------------------------
	#  sf branch (unchanged)
	# ----------------------------------------------------------
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt  = res$dt

	x = sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1, 3)]), bbx[2]))), crs = sf::st_crs(bbx))
	y = sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1, 3)]), bbx[4]))), crs = sf::st_crs(bbx))

	gp = impute_gp(gp, dt)
	gp = rescale_gp(gp, o$scale_down)

	if (any(nchar(gp$fill) == 9)) {
		fa            = split_alpha_channel(gp$fill, alpha = gp$fill_alpha)
		gp$fill       = fa$col
		gp$fill_alpha = gp$fill_alpha * fa$opacity
	}
	if (any(nchar(gp$col) == 9)) {
		fa           = split_alpha_channel(gp$col, alpha = gp$col_alpha)
		gp$col       = fa$col
		gp$col_alpha = gp$fill_alpha * fa$opacity
	}

	shp2 = sf::st_sf(
		unclass(gp[c("height", "fill", "col", "lwd", "fill_alpha", "col_alpha")]),
		id       = 1:length(shp),
		geometry = shp
	)

	# NOTE on transparency: fill-extrusion layers do NOT support per-feature alpha.
	# The GL spec ignores the alpha channel of fill-extrusion-color (and a long-
	# standing engine bug renders alpha=0 as solid black), and fill-extrusion-opacity
	# is per-LAYER only, not data-driven. So we honour transparency as a single
	# layer-wide opacity, taken as the mean of the per-feature fill_alpha values.
	fill_ext_opacity = {
		fa = suppressWarnings(as.numeric(shp2$fill_alpha))
		fa = fa[is.finite(fa)]
		if (!length(fa)) 1 else min(max(mean(fa), 0), 1)
	}

	ahp  = attach_hover_popup(shp2, dt, hdt, pdt, idt, popup.format, popup.layout, ptdt)
	shp2 = ahp$shp2

	srcname    = mapgl_srcid(paste0("layer", pane))
	layername1 = paste0(glid, "polygons_fill")
	layername2 = paste0(glid, "polygons_border")

	if (is.character(a$height.max)) {
		is_perc_max = grepl("%$", a$height.max)
		height.max  = as.numeric(sub("%$", "", a$height.max))
		if (is_perc_max) height.max = height.max / 100
	} else {
		is_perc_max = FALSE
		height.max  = as.numeric(a$height.max)
	}

	if (is.character(a$height.min)) {
		is_perc_min = grepl("%$", a$height.min)
		height.min  = as.numeric(sub("%$", "", a$height.min))
		if (is_perc_min) height.min = height.min / 100
	} else {
		is_perc_min = FALSE
		height.min  = as.numeric(a$height.min)
	}

	if (is_perc_max || is_perc_min) {
		sqrt_area_m = if (consider_global(shp)) {
			sqrt(5.1e+14)
		} else {
			bbx |>
				tmaptools::bb_poly() |>
				sf::st_area() |>
				sqrt() |>
				units::set_units("m") |>
				units::drop_units()
		}
		if (is_perc_max) height.max = sqrt_area_m * height.max
		if (is_perc_min) height.min = sqrt_area_m * height.min
	}

	shp2$height = height.min + shp2$height * (height.max - height.min)
	shp2_naomit = shp2[!is.na(shp2$height), ]

	m = m |>
		mapgl::add_source(srcname, data = shp2_naomit) |>
		mapgl::add_line_layer(layername2, source = srcname,
							  line_color   = mapgl::get_column("col"),
							  line_opacity = mapgl::get_column("col_alpha"),
							  line_width   = mapgl::get_column("lwd"))

	m = do.call(mapgl::add_fill_extrusion_layer, c(
		list(m, layername1,
			 source                 = srcname,
			 fill_extrusion_color   = mapgl::get_column("fill"),
			 fill_extrusion_opacity = fill_ext_opacity,
			 fill_extrusion_base    = 0,
			 fill_extrusion_height  = mapgl::get_column("height"),
			 tooltip                = ahp$hdt_arg,
			 popup                  = ahp$pdt_arg),
		mapgl_fe_props(a, mode)))

	m |> assign_mapgl(facet_row, facet_col, facet_page, mode = mode)

	mapgl_submit_group(group, c(layername1, layername2), mode, pane)
	NULL
}

# ============================================================
#  mapgl_lines
# ============================================================
mapgl_lines = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp,
						bbx, facet_row, facet_col, facet_page,
						id, pane, group, glid, o, ..., mode, popup.layout = NULL, ptdt = NULL) {
	m          = get_mapgl(facet_row, facet_col, facet_page, mode = mode)
	mapgl_blend_unsupported(a, mode)
	rc_text    = frc(facet_row, facet_col)
	shp_is_pointer = inherits(shpTM$shp, "character")
	# ----------------------------------------------------------
	#  PMTiles branch
	# ----------------------------------------------------------
	if (shp_is_pointer) {
		if (pmtiles_unsupported(shpTM, mode)) return(NULL)
		smeta      = shpTM$smeta
		srcname    = mapgl_srcid(paste0("layer", pane))
		layername1 = paste0(glid, "lines")
		url        = smeta$url
		get_pmt_aes = make_get_pmt_aes(dt)
		aes_c   = get_pmt_aes("col")
		aes_co  = get_pmt_aes("col_alpha")
		aes_lwd = get_pmt_aes("lwd")
		m |>
			mapgl::add_pmtiles_source(id = srcname, url = url) |>
			mapgl::add_line_layer(layername1, source = srcname,
								  source_layer  = smeta$layer,
								  line_color    = aes_c,
								  line_opacity  = aes_co,
								  line_width    = aes_lwd) |>
			assign_mapgl(facet_row, facet_col, facet_page, mode = mode)
		mapgl_submit_group(group, layername1, mode, pane)
		return(NULL)
	}
	# ----------------------------------------------------------
	#  sf branch
	# ----------------------------------------------------------
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt  = res$dt
	x = sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1, 3)]), bbx[2]))), crs = sf::st_crs(bbx))
	y = sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1, 3)]), bbx[4]))), crs = sf::st_crs(bbx))
	gp = impute_gp(gp, dt)
	gp = rescale_gp(gp, o$scale_down)
	shp2 = sf::st_sf(
		unclass(gp[c("col", "lwd", "col_alpha")]),
		id       = 1:length(shp),
		geometry = shp
	)

	srcname    = mapgl_srcid(paste0("layer", pane))
	layername1 = paste0(glid, "lines")  # was paste0(srcname, "lines") — fixed to match legend

	# Lines have only a col (line-colour) aesthetic on a single line layer.
	shp2 = mapgl_attach_cat(shp2, glid, fill_layer = NA_character_, border_layer = layername1,
							fill_col = NULL, col_col = "col")

	ahp  = attach_hover_popup(shp2, dt, hdt, pdt, idt, popup.format, popup.layout, ptdt)
	shp2 = ahp$shp2

	m |>
		mapgl::add_source(srcname, data = shp2) |>
		mapgl::add_line_layer(layername1, source = srcname,
							  line_color   = mapgl::get_column("col"),
							  line_opacity = mapgl::get_column("col_alpha"),
							  line_width   = mapgl::get_column("lwd"),
							  tooltip      = ahp$hdt_arg,
							  popup        = ahp$pdt_arg) |>
		assign_mapgl(facet_row, facet_col, facet_page, mode = mode)
	mapgl_submit_group(group, layername1, mode, pane)
	NULL
}

# ============================================================
#  mapgl_symbols
# ============================================================

mapgl_symbols = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp,
						  bbx, facet_row, facet_col, facet_page,
						  id, pane, group, glid, o, ..., mode, popup.layout = NULL, ptdt = NULL) {

	m          = get_mapgl(facet_row, facet_col, facet_page, mode)
	mapgl_blend_unsupported(a, mode)
	rc_text    = frc(facet_row, facet_col)
	shp_is_pointer = inherits(shpTM$shp, "character")

	# ----------------------------------------------------------
	#  PMTiles branch
	# ----------------------------------------------------------
	if (shp_is_pointer) {
		if (pmtiles_unsupported(shpTM, mode)) return(NULL)

		smeta      = shpTM$smeta
		srcname    = mapgl_srcid(paste0("layer", pane))
		layername1 = paste0(glid, "symbols_fill")
		url        = smeta$url

		get_pmt_aes = make_get_pmt_aes(dt)

		aes_f    = get_pmt_aes("fill")
		aes_fo   = get_pmt_aes("fill_alpha")
		aes_c    = get_pmt_aes("col")
		aes_co   = get_pmt_aes("col_alpha")
		aes_lwd  = get_pmt_aes("lwd")
		aes_size = get_pmt_aes("size")

		# Mirror the *10 scaling applied in the sf branch for numeric constants
		if (is.numeric(aes_size)) aes_size = aes_size * 10

		m |>
			mapgl::add_pmtiles_source(id = srcname, url = url) |>
			mapgl::add_circle_layer(layername1, source = srcname,
									source_layer          = smeta$layer,
									circle_color          = aes_f,
									circle_opacity        = aes_fo,
									circle_stroke_color   = aes_c,
									circle_stroke_opacity = aes_co,
									circle_stroke_width   = aes_lwd,
									circle_radius         = aes_size) |>
			assign_mapgl(facet_row, facet_col, facet_page, mode = mode)

		mapgl_submit_group(group, layername1, mode, pane)
		return(NULL)
	}

	# ----------------------------------------------------------
	#  sf branch
	# ----------------------------------------------------------
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt  = res$dt

	x = sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1, 3)]), bbx[2]))), crs = sf::st_crs(bbx))
	y = sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1, 3)]), bbx[4]))), crs = sf::st_crs(bbx))

	gp = impute_gp(gp, dt)
	gp = rescale_gp(gp, o$scale_down)

	# ----------------------------------------------------------
	#  Grob-shape branch (custom glyphs)
	# ----------------------------------------------------------
	shape_codes = suppressWarnings(as.integer(gp$shape))
	is_grob     = !is.na(shape_codes) & shape_codes > 999L
	if (any(is_grob)) {
		# In non-Grid modes tmapValuesSubmit_shape() already rasterised each grob
		# via grob2icon(); shapeLib holds leaflet icon objects (iconUrl -> a PNG),
		# not grobs. Reuse those PNGs directly, exactly like the Leaflet backend.
		merge_icons = utils::getFromNamespace("merge_icons", "tmap")
		iconset     = merge_icons(get("shapeLib", envir = .TMAP)[shape_codes[is_grob] - 999L])

		gd = a$grob.dim
		rw = if (!is.null(gd)) gd[["render.width"]] else 256   # PNG native px; icon-size is relative to this

		# glyph_scale: per-backend calibration knob (mapgl analogue of the
		# plot/view donut factors) — tune against the legend / view mode.
		glyph_scale = a$icon.scale
		iconsize    = (gp$size * 10 * glyph_scale) / rw

		ids          = rep("", length(shape_codes))
		ids[is_grob] = paste0(glid, "_glyph_", which(is_grob))
		m            = mapgl_add_glyph_images(m, iconset$iconUrl, ids[is_grob])

		shp2 = sf::st_sf(icon = ids, iconsize = iconsize,
						 id = seq_along(shape_codes), geometry = shp)

		srcname    = mapgl_srcid(paste0("layer", pane))
		layername1 = paste0(glid, "symbols_fill")

		shp2 = mapgl_attach_cat(shp2, glid, fill_layer = layername1,
								border_layer = NA_character_, fill_col = NULL, col_col = NULL)
		ahp  = attach_hover_popup(shp2, dt, hdt, pdt, idt, popup.format, popup.layout, ptdt)
		shp2 = ahp$shp2

		m |>
			mapgl::add_source(srcname, data = shp2) |>
			mapgl::add_symbol_layer(layername1, source = srcname,
									icon_image            = mapgl::get_column("icon"),
									icon_size             = mapgl::get_column("iconsize"),
									icon_allow_overlap    = TRUE,
									icon_ignore_placement = TRUE,
									icon_anchor           = "center",
									tooltip               = ahp$hdt_arg,
									popup                 = ahp$pdt_arg) |>
			assign_mapgl(facet_row, facet_col, facet_page, mode = mode)

		mapgl_submit_group(group, layername1, mode, pane)
		return(NULL)
	}

	if (any(nchar(gp$fill) == 9)) {
		fa        = split_alpha_channel(gp$fill, alpha = gp$fill_alpha)
		gp$fill   = fa$col
		gp$fill_alpha = gp$fill_alpha * fa$opacity
	}
	if (any(nchar(gp$col) == 9)) {
		fa        = split_alpha_channel(gp$col, alpha = gp$col_alpha)
		gp$col    = fa$col
		gp$col_alpha = gp$fill_alpha * fa$opacity
	}

	shp2 = sf::st_sf(
		unclass(gp[c("fill", "col", "lwd", "fill_alpha", "col_alpha", "size")]),
		id       = 1:length(shp),
		geometry = shp
	)

	shp2$size = shp2$size * 10

	srcname    = mapgl_srcid(paste0("layer", pane))
	layername1 = paste0(glid, "symbols_fill")  # was paste0(srcname, ...) — fixed

	# tm_symbols renders as a single circle layer carrying both circle-color
	# (fill) and circle-stroke-color (col); there is no separate outline layer,
	# so border_layer = NA. Both legends therefore key off this one layer (and,
	# per mapgl, only one can own its interactive filter at a time).
	shp2 = mapgl_attach_cat(shp2, glid, fill_layer = layername1, border_layer = NA_character_)

	ahp  = attach_hover_popup(shp2, dt, hdt, pdt, idt, popup.format, popup.layout, ptdt)
	shp2 = ahp$shp2

	m |>
		mapgl::add_source(srcname, data = shp2) |>
		mapgl::add_circle_layer(layername1, source = srcname,
								circle_color          = mapgl::get_column("fill"),
								circle_opacity        = mapgl::get_column("fill_alpha"),
								circle_stroke_color   = mapgl::get_column("col"),
								circle_stroke_opacity = mapgl::get_column("col_alpha"),
								circle_stroke_width   = mapgl::get_column("lwd"),
								circle_radius         = mapgl::get_column("size"),
								tooltip               = ahp$hdt_arg,
								popup                 = ahp$pdt_arg) |>
		assign_mapgl(facet_row, facet_col, facet_page, mode = mode)

	mapgl_submit_group(group, layername1, mode, pane)
	NULL
}

mapgl_add_glyph_images = function(m, urls, ids) {
	for (i in seq_along(urls)) m = mapgl::add_image(m, id = ids[i], url = urls[i])
	m
}

# Build smooth great-circle polygons (one per point) in EPSG:4326. Each circle
# is an `npoints`-vertex polygon generated from the spherical geodesic
# destination formula, so it is ground-accurate (radius in metres) and renders
# smoothly at any zoom — unlike an s2 cell-buffer, whose boundary is faceted.
# Longitudes are left continuous around the centre (not wrapped to [-180, 180])
# so circles near the antimeridian stay contiguous rather than splitting.
mapgl_circle_polys = function(pts, radius_m, npoints = 90L) {
	R   = 6378137                       # WGS84 mean Earth radius (m)
	cc  = sf::st_coordinates(pts)       # columns X (lon), Y (lat) in degrees
	n   = nrow(cc)
	rad = rep_len(radius_m / R, n)      # angular radius (radians)
	ang = seq(0, 2 * pi, length.out = npoints + 1L)

	polys = lapply(seq_len(n), function(i) {
		lon0 = cc[i, 1] * pi / 180
		lat0 = cc[i, 2] * pi / 180
		d    = rad[i]
		lat2 = asin(sin(lat0) * cos(d) + cos(lat0) * sin(d) * cos(ang))
		lon2 = lon0 + atan2(sin(ang) * sin(d) * cos(lat0),
							 cos(d) - sin(lat0) * sin(lat2))
		ring = cbind(lon2 * 180 / pi, lat2 * 180 / pi)
		ring[npoints + 1L, ] = ring[1L, ]   # close the ring exactly
		sf::st_polygon(list(ring))
	})
	sf::st_sfc(polys, crs = 4326)
}

# ============================================================
#  mapgl_circles
# ============================================================
#
# Geographically-fixed circles: the radius is expressed in metres (per the
# tm_circles() contract, gp$size is already a metre radius — the scale output
# is used directly and rescale_gp is NOT applied to size), so the circles scale
# with zoom and stay anchored to the map, unlike tm_symbols()/tm_bubbles()
# whose circle-radius is a fixed number of screen pixels.
#
# MapLibre's circle-radius paint property is in PIXELS, so a circle layer would
# be screen-fixed. To get true metre-based, zoom-scaling circles we materialise
# each point as a geodesic buffer polygon (in EPSG:4326, using s2 so the radius
# is in ground metres regardless of the data CRS — this also sidesteps the Web
# Mercator cos(lat) distortion that bit the grid backend) and render it exactly
# like a polygon layer: a fill layer for the body and a line layer for the
# border. The fill layer is named "<glid>symbols_fill" so the interactive
# categorical legend (which keys off cmp$layer containing "symbols") keeps
# matching it.

mapgl_circles = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp,
						  bbx, facet_row, facet_col, facet_page,
						  id, pane, group, glid, o, ..., mode, popup.layout = NULL, ptdt = NULL) {

	m          = get_mapgl(facet_row, facet_col, facet_page, mode)
	mapgl_blend_unsupported(a, mode)
	rc_text    = frc(facet_row, facet_col)
	shp_is_pointer = inherits(shpTM$shp, "character")

	# ----------------------------------------------------------
	#  PMTiles branch — unsupported
	# ----------------------------------------------------------
	# Metre-radius circles require buffering each point geometry at render time,
	# but for a remote tile source the per-feature geometry/data join is not
	# available in R memory (same limitation as hover/popup over PMTiles). Fall
	# back gracefully.
	if (shp_is_pointer) {
		cli::cli_inform(c(
			"!" = "{.fn tm_circles} (metre-based radii) is not supported for source shapes (e.g. PMTiles) in {.str {mode}} mode.",
			"i" = "Use {.fn tm_symbols} / {.fn tm_bubbles} for pixel-based circles over tiled sources."
		))
		return(NULL)
	}

	# ----------------------------------------------------------
	#  sf branch
	# ----------------------------------------------------------
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt  = res$dt

	gp = impute_gp(gp, dt)

	# Preserve the metre radius before rescale_gp (which would convert size to
	# "lines" / apply scale_down — meaningless for a geographic radius). lwd
	# (border width, in px) is left to rescale like a normal polygon border.
	size_raw = gp$size
	gp = rescale_gp(gp, o$scale_down)
	gp$size = size_raw

	if (any(nchar(gp$fill) == 9)) {
		fa            = split_alpha_channel(gp$fill, alpha = gp$fill_alpha)
		gp$fill       = fa$col
		gp$fill_alpha = gp$fill_alpha * fa$opacity
	}
	if (any(nchar(gp$col) == 9)) {
		fa           = split_alpha_channel(gp$col, alpha = gp$col_alpha)
		gp$col       = fa$col
		gp$col_alpha = gp$fill_alpha * fa$opacity
	}

	# --- materialise circles as smooth geodesic polygons in EPSG:4326 --------
	# st_buffer() with s2 approximates the disc with S2 cells, whose boundary
	# follows cell edges and looks faceted ("pixelated"). Instead we generate a
	# proper great-circle polygon with `npoints` vertices per circle: smooth,
	# tunable, ground-accurate, and with no global s2-flag side effects.
	pts4326  = sf::st_transform(sf::st_geometry(shp), 4326)
	radius_m = as.numeric(gp$size)   # metres

	circ = mapgl_circle_polys(pts4326, radius_m, npoints = 90L)

	shp2 = sf::st_sf(
		unclass(gp[c("fill", "col", "lwd", "fill_alpha", "col_alpha", "size")]),
		id       = seq_along(shp),
		geometry = circ
	)

	# drop features without a finite radius (NA-sized) — they have no geometry
	keep = is.finite(radius_m) & radius_m > 0
	shp2 = shp2[keep, ]

	# --- Add categorical columns for interactive legend(s) -------------------
	# tm_circles is materialised as fill + line polygon layers, so the fill and
	# col legends can each own their own layer (like tm_polygons).
	srcname    = mapgl_srcid(paste0("layer", pane))
	layername1 = paste0(glid, "symbols_fill")    # matches the categorical legend
	layername2 = paste0(glid, "symbols_border")

	shp2 = mapgl_attach_cat(shp2, glid, fill_layer = layername1, border_layer = layername2)
	# -------------------------------------------------------------------------

	ahp  = attach_hover_popup(shp2, dt[keep, ], hdt, pdt, idt, popup.format, popup.layout, ptdt)
	shp2 = ahp$shp2

	m |>
		mapgl::add_source(srcname, data = shp2) |>
		mapgl::add_fill_layer(layername1, source = srcname,
							  fill_color   = mapgl::get_column("fill"),
							  fill_opacity = mapgl::get_column("fill_alpha"),
							  tooltip      = ahp$hdt_arg,
							  popup        = ahp$pdt_arg) |>
		mapgl::add_line_layer(layername2, source = srcname,
							  line_color   = mapgl::get_column("col"),
							  line_opacity = mapgl::get_column("col_alpha"),
							  line_width   = mapgl::get_column("lwd")) |>
		assign_mapgl(facet_row, facet_col, facet_page, mode = mode)

	mapgl_submit_group(group, c(layername1, layername2), mode, pane)
	NULL
}

# ============================================================
#  mapgl_raster  (unchanged — reproduced for completeness)
# ============================================================

mapgl_raster = function(a, shpTM, dt, gp, pdt, popup.format, hdt, idt,
						 bbx, facet_row, facet_col, facet_page,
						 id, pane, group, glid, o, ..., mode) {

	rc_text  = frc(facet_row, facet_col)
	shp      = shpTM$shp
	tmapID   = shpTM$tmapID
	shp_is_pointer = inherits(shp, "character")

	if (shp_is_pointer) {
		if (pmtiles_unsupported(shpTM, mode)) return(NULL)

		smeta      = shpTM$smeta
		srcname    = mapgl_srcid(paste0("layer", pane))
		layername1 = paste0(glid, "raster")
		url        = smeta$url

		m = get_mapgl(facet_row, facet_col, facet_page, mode = mode)

		m |>
			mapgl::add_pmtiles_source(id = srcname, url = url, source_type = "raster") |>
			mapgl::add_raster_layer(layername1, source = srcname) |>
			assign_mapgl(facet_row, facet_col, facet_page, mode = mode)

		return(NULL)

	} else if (is_regular_grid(shp)) {

		tid   = intersect(tmapID, dt$tmapID__)
		color = rep(NA, length(tmapID))
		sel   = which(tmapID %in% tid)
		tid2  = tmapID[sel]
		color[sel] = dt$col[match(tid2, dt$tmapID__)]

		pal = na.omit(unique(color))
		pal = pal[substr(pal, 8, 10) != "00"]

		if (!length(pal)) return(NULL)

		res         = split_alpha_channel(pal, alpha = 1)
		pal_col     = res$col
		pal_opacity = if (length(res$opacity) == 0L) 0 else max(res$opacity)

		if ("col_alpha" %in% names(dt)) pal_opacity = max(dt$col_alpha)

		col_ids = match(color, pal)
		m_mat   = matrix(col_ids, ncol = ncol(shp))
		shp2    = stars::st_as_stars(m_mat, dimensions = shp)
		rst     = terra::rast(shp2)

		if (!terra::is.lonlat(rst)) rst = terra::project(rst, "epsg:4326")

		ext = terra::ext(rst)
		if (ext$ymin < -89.9) ext$ymin = -89
		if (ext$ymax >  89.9) ext$ymax =  89
		rst2 = terra::crop(rst, ext)

		srcname    = mapgl_srcid(paste0("layer", pane))
		layername1 = paste0(glid, "raster")

		m = get_mapgl(facet_row, facet_col, facet_page, mode = mode)

		m |>
			mapgl::add_image_source(srcname, data = rst2, colors = pal) |>
			mapgl::add_raster_layer(layername1, source = srcname,
									raster_opacity    = pal_opacity,
									raster_resampling = "nearest") |>
			assign_mapgl(facet_row, facet_col, facet_page, mode = mode)

		mapgl_submit_group(group, layername1, mode, pane)

	} else {

		m_mat = matrix(tmapID, nrow = nrow(shp), ncol = ncol(shp))
		shp2  = structure(list(tmapID = m_mat), class = "stars", dimensions = shp)
		shp3  = sf::st_geometry(sf::st_as_sf(shp2))
		crs   = get_option_class(o$crs_step4, "sf")
		shpTM = tmap::shapeTM(sf::st_transform(shp3, crs), tmapID)

		gp$lty = "solid"

		mapgl_polygons(a, shpTM, dt, pdt,
					   popup.format = NULL, hdt = NULL, idt = NULL,
					   gp, bbx, facet_row, facet_col, facet_page,
					   id, pane, group, glid, o, mode = mode)
	}
	NULL
}
