mapgl_pos = function(pos) {
	if (is.character(pos)) pos = tmap::tm_pos_in(pos[1], pos[2])
	if (pos$type %in% c("out", "autoout")) {
		sel = c("cell.v", "cell.h")
	} else {
		sel = c("pos.v", "pos.h")
	}
	x = tolower(unlist(pos[sel]))

	if (x[1] %in% c("center", "centre")) x[1] = "top"
	if (x[2] %in% c("center", "centre")) x[2] = "left"

  paste(x, collapse = "-")
}

cont_split = function(x) strsplit(x, split = "_", fixed=TRUE)

gp_to_lpar = function(gp, mfun, shape = 20, pick_middle = TRUE) {
	# create a list of gp elements

	lst = c(list(fillColor = {if (!all(is.na(gp$fill))) gp$fill else "#000000"},
				 color = {if (!all(is.na(gp$col))) gp$col else "#000000"},
				 fillOpacity = {if (!all(is.na(gp$fill_alpha))) gp$fill_alpha else 0},
				 opacity = {if (!all(is.na(gp$col_alpha))) gp$col_alpha else 0},
				 'stroke-width' = {if (!all(is.na(gp$lwd))) gp$lwd else 0},
				 'stroke-dasharray' = {if (!all(is.na(gp$lty))) lty2dash(gp$lty) else "none"},
				 size = {if (!all(is.na(gp$size))) gp$size else 1},
				 shape = {if (!all(is.na(gp$shape))) gp$shape else shape}))

	lst_isnum = c(fillColor = FALSE,
				  color = FALSE,
				  fillOpacity = TRUE,
				  opacity = TRUE,
				  'stroke-width' = TRUE,
				  'stroke-dash' = FALSE,
				  size = TRUE,
				  shape = TRUE)

	lst = mapply(function(lsti, isnum) {
		if (!is.character(lsti)) return(lsti)

		if (nchar(lsti[1]) > 50) {
			x = cont_split(lsti)
			x = lapply(x, function(i) {
				i[i=="NA"] = NA
				i
			})
			if (isnum) x = lapply(x, as.numeric)
			if (pick_middle) {
				x = sapply(x, function(i) {
					if (all(is.na(i))) NA else {
						sq = c(5,6,4,7,3,8,2,9,1,10) # priority for middle values
						i[sq[which(!is.na(i)[sq])[1]]]
					}
				})
			}
			return(x)

		} else {
			return(lsti)
		}
	}, lst, lst_isnum[names(lst)], SIMPLIFY = FALSE)

	pch2shp = c("rect", "circle", "triangle", "plus", "cross", "diamond", "triangle",
				"cross", "star", "diamond", "circle", "polygon", "plus", "cross",
				"triangle", "rect", "circle", "triangle", "diamond", "circle",
				"circle", "circle", "rect", "diamond", "triangle", "polygon", "stadium") # shapes for pch 0:25 + 26 for stadium (NOTE: last one is a triangle upside-down. Since 21:25 are the defaults, and a polygon is chosen to differentiate from the other triangle)
	lst$shape = get_pch_names(lst$shape)

	if ("tm_data_lines" %in% mfun) lst$shape = "line"

	lst$width = lst$size * 20
	lst$height = lst$size * 20
	#lst$width[]
	lst$size = NULL
	lst
}



make_equal_list = function(x) {
	cls = class(x)
	n = max(vapply(x, length, integer(1)))
	structure(lapply(x, rep, length.out = n), class = cls)
}




# Map tmap's legend styling onto a mapgl::legend_style() object, to be passed as
# `style=` to the add_*_legend functions.
#
# Background and frame/border are always taken from `grp` (tmap core supplies the
# resolved defaults), so the legend honours tmap's styling regardless of whether
# the user named those arguments explicitly. The two exceptions, kept from the
# explicit-call handling, are bg = FALSE -> fully transparent background, and
# frame = FALSE -> no border. Title/text typography (colour, font family, weight)
# is mapped whenever a concrete value is present. Text/title SIZE is deliberately
# NOT mapped: tmap sizes are relative line-heights whereas legend_style() expects
# pixels, so there is no faithful 1:1 conversion; mapgl's own sizing is kept.
# points -> CSS pixels bridge for the legend frame corner radius. In plot mode
# frame.r is interpreted in points and multiplied by the component scale
# (rndrectGrob -> grid::unit(frame.r * sc, "pt"), sc = min(1/clipT) * o$scale);
# in mapgl border_radius is CSS pixels. The points->pixels part is 96/72; the
# remaining ~2.25 is an empirical density match between the static device and the
# on-screen CSS legend (mapgl can't reproduce tmap's legend auto-scaling). Folded
# together this is ~3. frame.lwd needs no unit factor: grid's lwd unit (1/96")
# already equals one CSS pixel, which is why borders match at x1.
mapgl_frame_r_px = (96 / 72) * 2.25   # ~= 3

# CSS-pixel height of one tmap "line-height" in mapgl mode, used to convert the
# line-height-based stack_margin into the pixel gap between stacked legends. It's
# a different unit bridge from mapgl_frame_r_px (which converts *points*), so the
# two constants are intentionally separate. o$scale is applied at the call site.
mapgl_line_px = 28

mapgl_legend_style = function(grp, grp_called, cmp, o) {
	# NA / NULL / "" -> NULL, so the arg is omitted and mapgl's default applies.
	nn = function(x) if (is.null(x) || (length(x) == 1L && (is.na(x) || identical(x, "")))) NULL else x

	# mapgl formats border width/radius with sprintf("%d"), so they must be whole
	# integers (a fractional frame.lwd like 1.5 would otherwise error).
	as_int = function(x) { x = nn(x); if (is.null(x)) NULL else as.integer(round(x)) }

	# Convert an R colour to #RRGGBB for CSS. CSS understands hex and standard
	# named colours (e.g. "pink"), but NOT R's numbered greys ("gray50"/"grey50")
	# or other R-only names, which would silently void the whole CSS declaration
	# (border/background) and make the frame disappear. Unknown strings are left
	# as-is.
	to_hex = function(col) {
		col = nn(col)
		if (is.null(col)) return(NULL)
		tryCatch({
			m = grDevices::col2rgb(col)
			grDevices::rgb(m[1, ], m[2, ], m[3, ], maxColorValue = 255)
		}, error = function(e) col)
	}

	face_to_weight = function(ff) {
		ff = nn(ff)
		if (is.null(ff)) NULL else if (grepl("bold", ff, fixed = TRUE)) "bold" else "normal"
	}

	# Carry tmap's global scale through, mirroring plot mode's `* sc`. The
	# legend-autoscale part (min(1/clipT)) has no mapgl equivalent, so only o$scale
	# is portable; at the default scale = 1 this is a no-op.
	sc = if (!is.null(o$scale)) o$scale else 1

	# --- background ---
	background_color = NULL
	background_opacity = NULL
	if (("bg" %in% grp_called) && isFALSE(grp$bg)) {
		background_opacity = 0
	} else {
		background_color = to_hex(grp$bg.color)
		background_opacity = nn(grp$bg.alpha)
	}

	# --- frame / border ---
	border_color = NULL
	border_width = NULL
	border_radius = NULL
	if (("frame" %in% grp_called) && isFALSE(grp$frame)) {
		border_width = 0
	} else {
		border_color = to_hex(if (is.character(grp$frame)) grp$frame else grp$frame.color)
		border_width = as_int({ w = nn(grp$frame.lwd); if (is.null(w)) NULL else w * sc })
		border_radius = as_int({ r = nn(grp$frame.r); if (is.null(r)) NULL else r * sc * mapgl_frame_r_px })
	}

	# --- patch (element) border: the symbol / polygon outline ---
	element_border_color = NULL
	element_border_width = NULL
	has_outline = !is.null(cmp$gp$col) && !all(is.na(cmp$gp$col)) &&
		!is.null(cmp$gp$lwd) && any(cmp$gp$lwd > 0, na.rm = TRUE)
	if (has_outline) {
		element_border_color = to_hex(cmp$gp$col[which(!is.na(cmp$gp$col))[1]])
		element_border_width = as_int(max(cmp$gp$lwd, na.rm = TRUE) * sc)
		if (!is.null(element_border_width) && element_border_width < 1L) element_border_width = 1L
	}

	mapgl::legend_style(
		background_color   = background_color,
		background_opacity = background_opacity,
		border_color       = border_color,
		border_width       = border_width,
		border_radius      = border_radius,
		text_color         = nn(cmp$text.color),
		title_color        = nn(cmp$title.color),
		font_family        = nn(cmp$text.fontfamily),
		title_font_family  = nn(cmp$title.fontfamily),
		font_weight        = face_to_weight(cmp$text.fontface),
		title_font_weight  = face_to_weight(cmp$title.fontface),
		element_border_color = element_border_color,
		element_border_width = element_border_width
	)
}

# Resolve the interactive target layer(s) + filter column for a categorical
# legend from the registry written by the layer renderers (.TMAP$mapgl_cat).
# `is_col` marks a border/line-colour legend (vs a fill legend); the caller
# detects it via colVary.
#
# Routing rules (mapgl allows only one interactive legend to own a layer):
#  - both aesthetics categorical -> fill legend owns the fill layer, col legend
#    owns the border layer (each filters its own visual channel; no clash).
#  - only one aesthetic categorical -> that legend owns BOTH layers, so toggling
#    a class hides the whole feature (fill + outline).
#  - no separate outline layer (tm_symbols' single circle layer, or lines) ->
#    fall back to the one layer that exists.
# Returns NULL when there is nothing categorical to filter (caller then keeps a
# legacy single-layer fallback).
mapgl_legend_target = function(glid, is_col) {
	reg = .TMAP$mapgl_cat[[glid]]
	if (is.null(reg)) return(NULL)
	both_cat = !is.null(reg$fill) && !is.null(reg$col)
	fl = reg$fill_layer
	bl = reg$border_layer

	if (is_col) {
		info   = reg$col
		layers = if (is.na(bl)) fl else if (both_cat) bl else c(fl, bl)
	} else {
		info   = reg$fill
		layers = if (both_cat || is.na(bl)) fl else c(fl, bl)
	}
	layers = layers[!is.na(layers)]
	if (!length(layers) || is.null(info)) return(NULL)
	list(layer_id = layers, filter_column = info$column)
}

mapgl_legend = function(cmp, m, o, orientation, mode) {

	legpos = mapgl_pos(cmp$position)

	m2 = if (cmp$type == "none") {
		#message("Text based legends not supported in view mode")
		m
	} else if (cmp$type == "gradient") {

		colVary = length(cmp$gp2$color) > 1L
		if (colVary) cmp$gp2$fillColor = cmp$gp2$color

		# remove na
		if (cmp$na.show) {
			labs = head(cmp$labels, -1)
			cols = head(cmp$gp2$fillColor, -1)
		} else {
			labs = cmp$labels
			cols = cmp$gp2$fillColor
		}
		colsNA = is.na(cols)
		if (any(colsNA)) {
			labs = labs[!colsNA]
			cols = cols[!colsNA]
		}

		m |> mapgl::add_continuous_legend(legend_title = cmp$title, values = labs, colors = cols, add = TRUE, position = legpos,
										  margin_top = cmp$margin_top,
										  margin_bottom = cmp$margin_bottom,
										  margin_left = cmp$margin_left,
										  margin_right = cmp$margin_right, draggable = TRUE,
										  style = cmp$mapgl_style)
	} else if (cmp$type == "lines") {
		cat_col    = "__tmap_cat__"
		colVary    = length(cmp$gp2$color) > 1L
		gp2        = make_equal_list(cmp$gp2)
		if (colVary) gp2$fillColor = gp2$color
		circular_patches = !any(is.na(cmp$gp$shape)) && all(cmp$gp$shape %in% c(1, 10, 16, 19:21))

		# A lines legend always represents the line-colour (col) aesthetic.
		tgt = mapgl_legend_target(cmp$glid, is_col = TRUE)
		if (is.null(tgt)) tgt = list(layer_id = paste0(cmp$glid, "lines"), filter_column = NULL)

		m |> mapgl::add_categorical_legend(
			colors           = gp2$fillColor,
			values           = cmp$labels,
			position         = legpos,
			legend_title     = cmp$title,
			circular_patches = circular_patches,
			add              = TRUE,
			patch_shape      = "line",
			sizes            = cmp$gp$lwd,
			interactive      = TRUE,
			margin_top       = cmp$margin_top,
			margin_bottom    = cmp$margin_bottom,
			margin_left      = cmp$margin_left,
			margin_right     = cmp$margin_right,
			layer_id         = tgt$layer_id,
			draggable        = TRUE,
			filter_column    = tgt$filter_column,
			style            = cmp$mapgl_style
		)
	} else { # "symbols"
		colVary = length(cmp$gp2$color) > 1L   # TRUE => this legend is the border/col aesthetic
		gp2     = make_equal_list(cmp$gp2)
		if (colVary) gp2$fillColor = gp2$color

		patches = if (!any(is.na(cmp$gp$shape)) && all(cmp$gp$shape %in% c(1, 10, 16, 19:21))) {
			"circle"
		} else {
			"square"
		}
		sizes = if (!is.na(cmp$gp$size[1])) cmp$gp$size * 20 else NULL

		# Pick the target layer(s) + per-aesthetic filter column from the registry
		# the layer renderer wrote. Fall back to the legacy single fill-layer
		# target (auto-detected column) if no registry entry exists.
		tgt = mapgl_legend_target(cmp$glid, is_col = colVary)
		if (is.null(tgt)) {
			geom = if ("polygons" %in% cmp$layer) "polygons" else if ("symbols" %in% cmp$layer) "symbols" else cmp$layer[1]
			tgt = list(layer_id = paste0(cmp$glid, geom, "_fill"), filter_column = NULL)
		}

		m |> mapgl::add_categorical_legend(
			colors         = gp2$fillColor,
			values         = cmp$labels,
			position       = legpos,
			legend_title   = cmp$title,
			patch_shape    = patches,
			add            = TRUE,
			interactive    = TRUE,
			sizes          = sizes,
			margin_top     = cmp$margin_top,
			margin_bottom  = cmp$margin_bottom,
			margin_left    = cmp$margin_left,
			margin_right   = cmp$margin_right,
			layer_id       = tgt$layer_id, draggable = TRUE,
			filter_column  = tgt$filter_column,
			style          = cmp$mapgl_style
		)
	}
	m2

}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompPlot.tm_legend_portrait = function(comp, m, o) {
	mapgl_legend(comp, m, o, orientation = "vertical", mode = "mapbox")
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompPlot.tm_legend_landscape = function(comp, m, o) {
	mapgl_legend(comp, m, o, orientation = "horizontal", mode = "mapbox")
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompPlot.tm_legend_portrait = function(comp, m, o) {
	mapgl_legend(comp, m, o, orientation = "vertical", mode = "mapbox")
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompPlot.tm_legend_landscape = function(comp, m, o) {
	mapgl_legend(comp, m, o, orientation = "horizontal", mode = "mapbox")
}

#' @param facet_row,facet_col,facet_page row column and page id
#' @param class class
#' @param stack stack
#' @param stack_auto stack_auto
#' @param pos.h pos.h
#' @param pos.v pos.v
#' @param bbox bbox
#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxComp = function(comp, o, facet_row = NULL, facet_col = NULL, facet_page, class, stack, stack_auto, pos.h, pos.v, bbox) {
	mapgl_comp(comp = comp,
			   o = o,
			   facet_row = facet_row,
			   facet_col = facet_col,
			   facet_page = facet_page,
			   class = class,
			   stack = stack,
			   stack_auto = stack_auto,
			   pos.h = pos.h,
			   pos.v = pos.v,
			   bbox = bbox,
			   mode = "mapbox")}

#' @export
#' @keywords internal
#' @name tmapMapboxLegend
#' @rdname tmapMapbox
tmapMaplibreComp = function(comp, o, facet_row = NULL, facet_col = NULL, facet_page, class, stack, stack_auto, pos.h, pos.v, bbox) {
	mapgl_comp(comp = comp,
			   o = o,
			   facet_row = facet_row,
			   facet_col = facet_col,
			   facet_page = facet_page,
			   class = class,
			   stack = stack,
			   stack_auto = stack_auto,
			   pos.h = pos.h,
			   pos.v = pos.v,
			   bbox = bbox,
			   mode = "maplibre")
}


mapgl_comp = function(comp, o, facet_row = NULL, facet_col = NULL, facet_page, class, stack, stack_auto, pos.h, pos.v, bbox, mode) {

		### from tmapGridComp, migrate to tmap generic
	    # get component group settings
		grp = comp[[1]][c("position",
						  "stack",
						  "frame_combine",
						  "equalize",
						  "resize_as_group",
						  "stack_margin",
						  "offset",
						  "frame" ,
						  "frame.color",
						  "frame.lwd",
						  "frame.r",
						  "bg",
						  "bg.color",
						  "bg.alpha")]

		any_legend_chart_inset = any(vapply(comp, inherits, FUN.VALUE = logical(1), c("tm_legend", "tm_chart", "tm_inset")))
		grp_called = setdiff(unique(do.call(c, lapply(comp, FUN = "[[", "called_via_comp_group"))), "group_id")

		if (!("frame" %in% grp_called)) grp$frame = any_legend_chart_inset
		if (!("bg" %in%grp_called)) grp$bg = any_legend_chart_inset
	    ###

	m = get_mapgl(facet_row, facet_col, facet_page, mode = mode)
	rc_text = frc(facet_row, facet_col)

	stack = stack[1]

	legpos = mapgl_pos(comp[[1]]$position) # should be identical over components

	# stack_margin defaults to a named vector c(combined, apart); frame_combine is
	# always FALSE in mapgl mode, so use the "apart" value (fall back to the first).
	sc = if (!is.null(o$scale)) o$scale else 1
	sm = grp$stack_margin
	sm = if (!is.null(names(sm)) && all(c("combined", "apart") %in% names(sm))) sm[["apart"]] else sm[1]
	stack_margin_px = if (is.null(sm) || length(sm) != 1L || is.na(sm)) 0 else sm * mapgl_line_px * sc

	# Each legend is positioned by an offset (os) from the anchor. When the anchor
	# is the far side (bottom for a vertical stack, right for a horizontal one) the
	# stack grows away from it, so the first component (lowest z) must sit at the
	# far end of the stack. Iterate in reverse for those anchors so the on-screen
	# z-order matches plot mode regardless of position.
	reverse = if (stack == "vertical") {
		legpos %in% c("bottom-left", "bottom-right")
	} else {
		legpos %in% c("top-right", "bottom-right")
	}

	os = 0
	cred = 35
	for (cmp in (if (reverse) rev(comp) else comp)) {
		if (is.null(cmp$height) || is.na(cmp$height)) cmp$height = 100
		if (is.null(cmp$width) || is.na(cmp$width)) cmp$width = 500

		if (stack == "vertical") {
			if (legpos %in% c("bottom-right", "bottom-left")) {
				cmp$margin_bottom = os + ifelse(legpos == "bottom-right", cred, 0)
				cmp$margin_top = 0
			} else {
				cmp$margin_top = os
				cmp$margin_bottom =  ifelse(legpos == "bottom-right", cred, 0)
			}
			cmp$margin_left = 0
			cmp$margin_right = 0
			os = os + cmp$height + stack_margin_px
		} else {
			if (legpos %in% c("top-left", "bottom-left"))  {
				cmp$margin_left = os
				cmp$margin_right = 0
			} else {
				cmp$margin_right = os
				cmp$margin_left = 0
			}
			cmp$margin_top = 0
			cmp$margin_bottom = ifelse(legpos == "bottom-right", cred, 0)
			os = os + cmp$width + stack_margin_px
		}


		cmp$mapgl_style = mapgl_legend_style(grp, grp_called, cmp, o)

		if (mode == "maplibre") {
			m = tmapMaplibreCompPlot(cmp, m, o)
		} else {
			m = tmapMapboxCompPlot(cmp, m, o)
		}
	}

	assign_mapgl(m, facet_row, facet_col, facet_page, mode = mode)
	NULL
}

