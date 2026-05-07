# ============================================================
#  Shared helpers
# ============================================================

# Build a MapLibre 'match' expression from a categorical scale mapping.
# Returns a properly nested list that serialises to
#   ["match", ["get", var], level1, value1, ..., fallback]
build_match_expr <- function(var, mapping) {
	pairs <- as.list(rbind(mapping$levels_orig, mapping$values_orig))  # interleaved pairs
	c(list("match", list("get", var)), pairs, list(mapping$value_na))
}

# Factory: returns a get_pmt_aes() closure over dt and .TMAP.
# For scale-driven aesthetics it resolves the MapLibre expression;
# for fixed values it returns the raw scalar from dt[[a]][1].
make_get_pmt_aes <- function(dt) {
	function(a) {
		v      <- dt[[a]][1]
		is_var <- substr(v, 1, 5) == "scale"
		if (!is_var) return(v)

		snr     <- as.numeric(substr(v, 6, 8))
		leg     <- .TMAP$legs[[snr]]
		var     <- substr(v, 10, nchar(v))
		mapping <- leg$layer_args$mapping

		if (!is.null(mapping$levels_orig)) {
			build_match_expr(var, mapping)
		} else {
			mapgl::get_column(var)
		}
	}
}

# Shared PMTiles guard: emits cli messages and returns TRUE when the
# pointer cannot be rendered, so callers can do:
#   if (pmtiles_unsupported(shpTM, mode)) return(NULL)
pmtiles_unsupported <- function(shpTM, mode) {
	smeta <- shpTM$smeta
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

view_format_popups_mapgl <- function(id = NULL, titles, format, values) {
	h <- lapply(format, function(f) {
		if (f$html.escape) {
			htmltools::htmlEscape
		} else {
			function(x) x
		}
	})

	if (!is.null(id)) {
		labels <- paste("<b>", h[[1]](id), "</b>", sep = "")
	} else {
		labels <- ""
	}

	titles_format <- mapply(function(ti, hi) {
		hi(ti)
	}, titles, h, SIMPLIFY = FALSE)

	values_format <- mapply(function(v, f, hi) {
		if (inherits(v, "units")) {
			popup_append <- paste0(" ", as.character(attr(v, "units")))
		} else {
			popup_append <- ""
		}
		numbers <- hi(if (is.numeric(v)) do.call("fancy_breaks", c(list(vec = as.numeric(v), intervals = FALSE), f)) else v)
		paste0(numbers, popup_append)
	}, values, format, h, SIMPLIFY = FALSE)

	labels2 <- mapply(function(l, v) {
		paste0("<tr><td style=\"color: #888888;\"><nobr>", l, "</nobr></td><td align=\"right\"><nobr>", v, "</nobr></td>")
	}, titles_format, values_format, SIMPLIFY = FALSE)

	labels3 <- paste0(do.call("paste", c(labels2, list(sep = "</tr>"))), "</tr>")

	padding_right <- ifelse(length(titles_format) > 13, 200, 0)

	paste0(
		"<div style='width:auto; max-height:25em; overflow-y:auto; overflow-x:hidden;'>",
		"<table style='width:100%; table-layout:fixed;'>",
		"<thead><tr><th colspan='2'>", labels, "</th></tr></thead>",
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
attach_hover_popup <- function(shp2, dt, hdt, pdt, idt, popup.format) {

	hdt_arg <- NULL
	pdt_arg <- NULL

	if (!is.null(hdt)) {
		shp2$hover <- hdt$hover[match(dt$tmapID__, hdt$tmapID__)]
		shp2$hover <- vapply(shp2$hover, htmltools::HTML, FUN.VALUE = character(1))
		hdt_arg    <- mapgl::get_column("hover")
	}

	if (!is.null(pdt)) {
		mtch    <- match(dt$tmapID__, pdt$tmapID__)
		pdt_sub <- pdt[mtch][, tmapID__ := NULL]

		id_arg <- if (!is.null(hdt)) shp2$hover else NULL
		shp2$popup <- view_format_popups_mapgl(
			id     = id_arg,
			titles = names(pdt_sub),
			values = pdt_sub,
			format = popup.format
		)
		pdt_arg <- mapgl::get_column("popup")
	}

	list(shp2 = shp2, hdt_arg = hdt_arg, pdt_arg = pdt_arg)
}

# ============================================================
#  split_alpha_channel
# ============================================================

split_alpha_channel <- function(x, alpha) {
	if (is.null(x)) {
		list(col = NULL, opacity = 0)
	} else {
		RGBA    <- col2rgb(x, alpha = TRUE)
		col     <- rgb(RGBA[1, ], RGBA[2, ], RGBA[3, ], maxColorValue = 255)
		opacity <- unname(RGBA[4, ] / 255 * alpha)
		list(col = col, opacity = opacity)
	}
}

# ============================================================
#  lty2dash
# ============================================================

lty2dash <- function(lty) {
	tab <- c(solid = "", dashed = "4 4", dotted = "1 3", dotdash = "1 3 4 3",
			 longdash = "7 3", twodash = "2 2 6 2")
	are_words   <- (lty %in% names(tab))
	if (all(are_words)) {
		unname(tab[lty])
	} else {
		are_letters <- (suppressWarnings(!is.na(as.numeric(lty))))
		if (!all(are_letters | are_words)) {
			stop("Incorrect lty specification: ", lty[which(!are_letters & !are_words)[1]])
		} else {
			lty[are_words]   <- unname(tab[lty[are_words]])
			lty[are_letters] <- vapply(
				strsplit(lty[are_letters], ""),
				FUN = function(x) paste(x, collapse = " "),
				FUN.VALUE = character(1)
			)
		}
		lty
	}
}

# ============================================================
#  mapgl_polygons
# ============================================================

mapgl_polygons <- function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp,
						   bbx, facet_row, facet_col, facet_page,
						   id, pane, group, glid, o, ..., mode) {

	m          <- get_mapgl(facet_row, facet_col, facet_page, mode)
	rc_text    <- frc(facet_row, facet_col)
	shp_is_pointer <- inherits(shpTM$shp, "character")

	# ----------------------------------------------------------
	#  PMTiles branch
	# ----------------------------------------------------------
	if (shp_is_pointer) {
		if (pmtiles_unsupported(shpTM, mode)) return(NULL)

		smeta      <- shpTM$smeta
		srcname    <- paste0("layer", pane)
		layername1 <- paste0(glid, "polygons_fill")
		layername2 <- paste0(glid, "polygons_border")
		url        <- smeta$url

		get_pmt_aes <- make_get_pmt_aes(dt)

		aes_f   <- get_pmt_aes("fill")
		aes_fo  <- get_pmt_aes("fill_alpha")
		aes_c   <- get_pmt_aes("col")
		aes_co  <- get_pmt_aes("col_alpha")
		aes_lwd <- get_pmt_aes("lwd")

		# Note: hover / popup over PMTiles are intentionally not supported here —
		# the popup data lives in dt (R memory) while tiles are fetched remotely,
		# so there is no row-level join available at render time.

		m |>
			mapgl::add_pmtiles_source(id = glid, url = url) |>
			mapgl::add_fill_layer(layername1, source = glid,
								  source_layer  = smeta$layer,
								  fill_color    = aes_f,
								  fill_opacity  = aes_fo) |>
			mapgl::add_line_layer(layername2, source = glid,
								  source_layer  = smeta$layer,
								  line_color    = aes_c,
								  line_opacity  = aes_co,
								  line_width    = aes_lwd) |>
			assign_mapgl(facet_row, facet_col, facet_page, mode = mode)

		mapgl_submit_group(group, c(layername1, layername2), mode)
		return(NULL)
	}

	# ----------------------------------------------------------
	#  sf branch
	# ----------------------------------------------------------
	res <- select_sf(shpTM, dt)
	shp <- res$shp
	dt  <- res$dt

	x <- sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1, 3)]), bbx[2]))), crs = sf::st_crs(bbx))
	y <- sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1, 3)]), bbx[4]))), crs = sf::st_crs(bbx))

	gp <- impute_gp(gp, dt)
	gp <- rescale_gp(gp, o$scale_down)

	if (any(nchar(gp$fill) == 9)) {
		fa        <- split_alpha_channel(gp$fill, alpha = gp$fill_alpha)
		gp$fill   <- fa$col
		gp$fill_alpha <- gp$fill_alpha * fa$opacity
	}
	if (any(nchar(gp$col) == 9)) {
		fa        <- split_alpha_channel(gp$col, alpha = gp$col_alpha)
		gp$col    <- fa$col
		gp$col_alpha <- gp$fill_alpha * fa$opacity
	}

	shp2 <- sf::st_sf(
		unclass(gp[c("fill", "col", "lwd", "fill_alpha", "col_alpha")]),
		id       = 1:length(shp),
		geometry = shp
	)

	# --- Add categorical column for interactive legend ---
	legs = .TMAP$legs
	lid = which(vapply(legs, FUN = function(l) {
		("glid" %in% names(l)) && l$glid == glid
	}, FUN.VALUE = logical(1)))[1]
	leg = legs[[lid]]

	cat_colors <- leg$vvalues   # per-category hex colors
	cat_values <- leg$labels    # category labels

	stopifnot(length(cat_colors) == length(cat_values))  # sanity check

	shp2[["__tmap_cat__"]] <- cat_values[match(shp2$fill, cat_colors)]

	attr(shp2, "tmap_cat_col")    <- "__tmap_cat__"
	attr(shp2, "tmap_cat_values") <- cat_values
	attr(shp2, "tmap_cat_colors") <- cat_colors
	# -----------------------------------------------------

	ahp  <- attach_hover_popup(shp2, dt, hdt, pdt, idt, popup.format)
	shp2 <- ahp$shp2

	srcname    <- paste0("layer", pane)
	layername1 <- paste0(glid, "polygons_fill")
	layername2 <- paste0(glid, "polygons_border")

	ahp  <- attach_hover_popup(shp2, dt, hdt, pdt, idt, popup.format)
	shp2 <- ahp$shp2

	srcname    <- paste0("layer", pane)
	layername1 <- paste0(glid, "polygons_fill")
	layername2 <- paste0(glid, "polygons_border")

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

	mapgl_submit_group(group, c(layername1, layername2), mode)
	NULL
}

# ============================================================
#  mapgl_polygons_3d
# ============================================================
mapgl_polygons_3d <- function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp,
							  bbx, facet_row, facet_col, facet_page,
							  id, pane, group, glid, o, ..., mode) {

	m       <- get_mapgl(facet_row, facet_col, facet_page, mode)
	rc_text <- frc(facet_row, facet_col)
	shp_is_pointer <- inherits(shpTM$shp, "character")

	# ----------------------------------------------------------
	#  PMTiles branch
	# ----------------------------------------------------------
	if (shp_is_pointer) {
		if (pmtiles_unsupported(shpTM, mode)) return(NULL)

		smeta      <- shpTM$smeta
		srcname    <- paste0("layer", pane)
		layername1 <- paste0(glid, "polygons_fill")
		layername2 <- paste0(glid, "polygons_border")
		url        <- smeta$url

		get_pmt_aes <- make_get_pmt_aes(dt)

		aes_f   <- get_pmt_aes("fill")
		aes_fo  <- get_pmt_aes("fill_alpha")
		aes_c   <- get_pmt_aes("col")
		aes_co  <- get_pmt_aes("col_alpha")
		aes_lwd <- get_pmt_aes("lwd")
		aes_h_raw <- get_pmt_aes("height")

		# --- read limits from tm_scale_continuous -------------------------------
		h_var <- dt[["height"]][1]
		if (substr(h_var, 1, 5) == "scale") {
			snr <- as.numeric(substr(h_var, 6, 8))
			leg <- .TMAP$legs[[snr]]
			limits <- leg$layer_args$limits
			if (is.null(limits)) {
				cli::cli_abort(c(
					"!" = "Cannot determine height range for PMTiles source.",
					"i" = "Specify {.code height.scale = tm_scale_continuous(limits = c(min, max))}.",
					"i" = "Example: {.code tm_scale_continuous(limits = c(0, 28672))}"
				))
			}
			h_min_raw <- limits[1]
			h_max_raw <- limits[2]
		} else {
			cli::cli_abort(c(
				"!" = "Height aesthetic for PMTiles must use {.fn tm_scale_continuous}.",
				"i" = "Specify {.code height.scale = tm_scale_continuous(limits = c(min, max))}."
			))
		}

		# --- resolve height.max / height.min to metres -------------------------
		if (is.character(a$height.max)) {
			is_perc_max <- grepl("%$", a$height.max)
			height.max  <- as.numeric(sub("%$", "", a$height.max))
			if (is_perc_max) height.max <- height.max / 100
		} else {
			is_perc_max <- FALSE
			height.max  <- as.numeric(a$height.max)
		}

		if (is.character(a$height.min)) {
			is_perc_min <- grepl("%$", a$height.min)
			height.min  <- as.numeric(sub("%$", "", a$height.min))
			if (is_perc_min) height.min <- height.min / 100
		} else {
			is_perc_min <- FALSE
			height.min  <- as.numeric(a$height.min)
		}

		if (is_perc_max || is_perc_min) {
			sqrt_area_m <- if (consider_global(bbx)) {
				sqrt(5.1e+14)
			} else {
				bbx |>
					tmaptools::bb_poly() |>
					sf::st_area() |>
					sqrt() |>
					units::set_units("m") |>
					units::drop_units()
			}
			if (is_perc_max) height.max <- sqrt_area_m * height.max
			if (is_perc_min) height.min <- sqrt_area_m * height.min
		}

		# --- build MapLibre scaling expression ---------------------------------
		# Replicates: height.min + (raw - h_min_raw) / (h_max_raw - h_min_raw)
		#                        * (height.max - height.min)
		h_range   <- h_max_raw - h_min_raw
		out_range <- height.max - height.min

		aes_h_scaled <- list(
			"+",
			height.min,
			list("*",
				 list("/",
				 	 list("-", aes_h_raw, h_min_raw),
				 	 h_range),
				 out_range)
		)

		m |>
			mapgl::add_pmtiles_source(id = glid, url = url) |>
			mapgl::add_line_layer(layername2, source = glid,
								  source_layer          = smeta$layer,
								  line_color            = aes_c,
								  line_opacity          = aes_co,
								  line_width            = aes_lwd) |>
			mapgl::add_fill_extrusion_layer(layername1, source = glid,
											source_layer           = smeta$layer,
											fill_extrusion_color   = aes_f,
											fill_extrusion_base    = 0,
											fill_extrusion_height  = aes_h_scaled) |>
			assign_mapgl(facet_row, facet_col, facet_page, mode = mode)

		mapgl_submit_group(group, c(layername1, layername2), mode)
		return(NULL)
	}

	# ----------------------------------------------------------
	#  sf branch (unchanged)
	# ----------------------------------------------------------
	res <- select_sf(shpTM, dt)
	shp <- res$shp
	dt  <- res$dt

	x <- sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1, 3)]), bbx[2]))), crs = sf::st_crs(bbx))
	y <- sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1, 3)]), bbx[4]))), crs = sf::st_crs(bbx))

	gp <- impute_gp(gp, dt)
	gp <- rescale_gp(gp, o$scale_down)

	if (any(nchar(gp$fill) == 9)) {
		fa            <- split_alpha_channel(gp$fill, alpha = gp$fill_alpha)
		gp$fill       <- fa$col
		gp$fill_alpha <- gp$fill_alpha * fa$opacity
	}
	if (any(nchar(gp$col) == 9)) {
		fa           <- split_alpha_channel(gp$col, alpha = gp$col_alpha)
		gp$col       <- fa$col
		gp$col_alpha <- gp$fill_alpha * fa$opacity
	}

	shp2 <- sf::st_sf(
		unclass(gp[c("height", "fill", "col", "lwd", "fill_alpha", "col_alpha")]),
		id       = 1:length(shp),
		geometry = shp
	)

	ahp  <- attach_hover_popup(shp2, dt, hdt, pdt, idt, popup.format)
	shp2 <- ahp$shp2

	srcname    <- paste0("layer", pane)
	layername1 <- paste0(srcname, "polygons_fill")
	layername2 <- paste0(srcname, "polygons_border")

	if (is.character(a$height.max)) {
		is_perc_max <- grepl("%$", a$height.max)
		height.max  <- as.numeric(sub("%$", "", a$height.max))
		if (is_perc_max) height.max <- height.max / 100
	} else {
		is_perc_max <- FALSE
		height.max  <- as.numeric(a$height.max)
	}

	if (is.character(a$height.min)) {
		is_perc_min <- grepl("%$", a$height.min)
		height.min  <- as.numeric(sub("%$", "", a$height.min))
		if (is_perc_min) height.min <- height.min / 100
	} else {
		is_perc_min <- FALSE
		height.min  <- as.numeric(a$height.min)
	}

	if (is_perc_max || is_perc_min) {
		sqrt_area_m <- if (consider_global(shp)) {
			sqrt(5.1e+14)
		} else {
			bbx |>
				tmaptools::bb_poly() |>
				sf::st_area() |>
				sqrt() |>
				units::set_units("m") |>
				units::drop_units()
		}
		if (is_perc_max) height.max <- sqrt_area_m * height.max
		if (is_perc_min) height.min <- sqrt_area_m * height.min
	}

	shp2$height <- height.min + shp2$height * (height.max - height.min)
	shp2_naomit <- shp2[!is.na(shp2$height), ]

	m |>
		mapgl::add_source(srcname, data = shp2_naomit) |>
		mapgl::add_line_layer(layername2, source = srcname,
							  line_color   = mapgl::get_column("col"),
							  line_opacity = mapgl::get_column("col_alpha"),
							  line_width   = mapgl::get_column("lwd")) |>
		mapgl::add_fill_extrusion_layer(layername1, source = srcname,
										fill_extrusion_color  = mapgl::get_column("fill"),
										fill_extrusion_base   = 0,
										fill_extrusion_height = mapgl::get_column("height"),
										tooltip               = ahp$hdt_arg,
										popup                 = ahp$pdt_arg) |>
		assign_mapgl(facet_row, facet_col, facet_page, mode = mode)

	mapgl_submit_group(group, c(layername1, layername2), mode)
	NULL
}

# ============================================================
#  mapgl_lines
# ============================================================
mapgl_lines <- function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp,
						bbx, facet_row, facet_col, facet_page,
						id, pane, group, glid, o, ..., mode) {
	m          <- get_mapgl(facet_row, facet_col, facet_page, mode = mode)
	rc_text    <- frc(facet_row, facet_col)
	shp_is_pointer <- inherits(shpTM$shp, "character")
	# ----------------------------------------------------------
	#  PMTiles branch
	# ----------------------------------------------------------
	if (shp_is_pointer) {
		if (pmtiles_unsupported(shpTM, mode)) return(NULL)
		smeta      <- shpTM$smeta
		srcname    <- paste0("layer", pane)
		layername1 <- paste0(glid, "lines")
		url        <- smeta$url
		get_pmt_aes <- make_get_pmt_aes(dt)
		aes_c   <- get_pmt_aes("col")
		aes_co  <- get_pmt_aes("col_alpha")
		aes_lwd <- get_pmt_aes("lwd")
		m |>
			mapgl::add_pmtiles_source(id = glid, url = url) |>
			mapgl::add_line_layer(layername1, source = glid,
								  source_layer  = smeta$layer,
								  line_color    = aes_c,
								  line_opacity  = aes_co,
								  line_width    = aes_lwd) |>
			assign_mapgl(facet_row, facet_col, facet_page, mode = mode)
		mapgl_submit_group(group, layername1, mode)
		return(NULL)
	}
	# ----------------------------------------------------------
	#  sf branch
	# ----------------------------------------------------------
	res <- select_sf(shpTM, dt)
	shp <- res$shp
	dt  <- res$dt
	x <- sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1, 3)]), bbx[2]))), crs = sf::st_crs(bbx))
	y <- sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1, 3)]), bbx[4]))), crs = sf::st_crs(bbx))
	gp <- impute_gp(gp, dt)
	gp <- rescale_gp(gp, o$scale_down)
	shp2 <- sf::st_sf(
		unclass(gp[c("col", "lwd", "col_alpha")]),
		id       = 1:length(shp),
		geometry = shp
	)

	# --- Add categorical column for interactive legend ---
	legs <- .TMAP$legs
	lid  <- which(vapply(legs, FUN = function(l) {
		("glid" %in% names(l)) && l$glid == glid
	}, FUN.VALUE = logical(1)))[1]
	leg <- legs[[lid]]

	cat_colors <- leg$vvalues
	cat_values <- leg$labels

	stopifnot(length(cat_colors) == length(cat_values))

	shp2[["__tmap_cat__"]] <- cat_values[match(shp2$col, cat_colors)]

	attr(shp2, "tmap_cat_col")    <- "__tmap_cat__"
	attr(shp2, "tmap_cat_values") <- cat_values
	attr(shp2, "tmap_cat_colors") <- cat_colors
	# -----------------------------------------------------

	ahp  <- attach_hover_popup(shp2, dt, hdt, pdt, idt, popup.format)
	shp2 <- ahp$shp2
	srcname    <- paste0("layer", pane)
	layername1 <- paste0(glid, "lines")  # was paste0(srcname, "lines") — fixed to match legend

	m |>
		mapgl::add_source(srcname, data = shp2) |>
		mapgl::add_line_layer(layername1, source = srcname,
							  line_color   = mapgl::get_column("col"),
							  line_opacity = mapgl::get_column("col_alpha"),
							  line_width   = mapgl::get_column("lwd"),
							  tooltip      = ahp$hdt_arg,
							  popup        = ahp$pdt_arg) |>
		assign_mapgl(facet_row, facet_col, facet_page, mode = mode)
	mapgl_submit_group(group, layername1, mode)
	NULL
}

# ============================================================
#  mapgl_symbols
# ============================================================

mapgl_symbols <- function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp,
						  bbx, facet_row, facet_col, facet_page,
						  id, pane, group, glid, o, ..., mode) {

	m          <- get_mapgl(facet_row, facet_col, facet_page, mode)
	rc_text    <- frc(facet_row, facet_col)
	shp_is_pointer <- inherits(shpTM$shp, "character")

	# ----------------------------------------------------------
	#  PMTiles branch
	# ----------------------------------------------------------
	if (shp_is_pointer) {
		if (pmtiles_unsupported(shpTM, mode)) return(NULL)

		smeta      <- shpTM$smeta
		srcname    <- paste0("layer", pane)
		layername1 <- paste0(glid, "symbols_fill")
		url        <- smeta$url

		get_pmt_aes <- make_get_pmt_aes(dt)

		aes_f    <- get_pmt_aes("fill")
		aes_fo   <- get_pmt_aes("fill_alpha")
		aes_c    <- get_pmt_aes("col")
		aes_co   <- get_pmt_aes("col_alpha")
		aes_lwd  <- get_pmt_aes("lwd")
		aes_size <- get_pmt_aes("size")

		# Mirror the *10 scaling applied in the sf branch for numeric constants
		if (is.numeric(aes_size)) aes_size <- aes_size * 10

		m |>
			mapgl::add_pmtiles_source(id = glid, url = url) |>
			mapgl::add_circle_layer(layername1, source = glid,
									source_layer          = smeta$layer,
									circle_color          = aes_f,
									circle_opacity        = aes_fo,
									circle_stroke_color   = aes_c,
									circle_stroke_opacity = aes_co,
									circle_stroke_width   = aes_lwd,
									circle_radius         = aes_size) |>
			assign_mapgl(facet_row, facet_col, facet_page, mode = mode)

		mapgl_submit_group(group, layername1, mode)
		return(NULL)
	}

	# ----------------------------------------------------------
	#  sf branch
	# ----------------------------------------------------------
	res <- select_sf(shpTM, dt)
	shp <- res$shp
	dt  <- res$dt

	x <- sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1, 3)]), bbx[2]))), crs = sf::st_crs(bbx))
	y <- sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1, 3)]), bbx[4]))), crs = sf::st_crs(bbx))

	gp <- impute_gp(gp, dt)
	gp <- rescale_gp(gp, o$scale_down)

	if (any(nchar(gp$fill) == 9)) {
		fa        <- split_alpha_channel(gp$fill, alpha = gp$fill_alpha)
		gp$fill   <- fa$col
		gp$fill_alpha <- gp$fill_alpha * fa$opacity
	}
	if (any(nchar(gp$col) == 9)) {
		fa        <- split_alpha_channel(gp$col, alpha = gp$col_alpha)
		gp$col    <- fa$col
		gp$col_alpha <- gp$fill_alpha * fa$opacity
	}

	shp2 <- sf::st_sf(
		unclass(gp[c("fill", "col", "lwd", "fill_alpha", "col_alpha", "size")]),
		id       = 1:length(shp),
		geometry = shp
	)

	shp2$size <- shp2$size * 10

	# --- Add categorical column for interactive legend ---
	legs <- .TMAP$legs
	lid  <- which(vapply(legs, FUN = function(l) {
		("glid" %in% names(l)) && l$glid == glid
	}, FUN.VALUE = logical(1)))[1]
	leg <- legs[[lid]]

	cat_colors <- leg$vvalues
	cat_values <- leg$labels

	stopifnot(length(cat_colors) == length(cat_values))

	shp2[["__tmap_cat__"]] <- cat_values[match(shp2$fill, cat_colors)]

	attr(shp2, "tmap_cat_col")    <- "__tmap_cat__"
	attr(shp2, "tmap_cat_values") <- cat_values
	attr(shp2, "tmap_cat_colors") <- cat_colors
	# -----------------------------------------------------

	ahp  <- attach_hover_popup(shp2, dt, hdt, pdt, idt, popup.format)
	shp2 <- ahp$shp2
	srcname    <- paste0("layer", pane)
	layername1 <- paste0(glid, "symbols_fill")  # was paste0(srcname, ...) — fixed

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

	mapgl_submit_group(group, layername1, mode)
	NULL
}

# ============================================================
#  mapgl_raster  (unchanged — reproduced for completeness)
# ============================================================

mapgl_raster <- function(a, shpTM, dt, gp, pdt, popup.format, hdt, idt,
						 bbx, facet_row, facet_col, facet_page,
						 id, pane, group, glid, o, ..., mode) {

	rc_text  <- frc(facet_row, facet_col)
	shp      <- shpTM$shp
	tmapID   <- shpTM$tmapID
	shp_is_pointer <- inherits(shp, "character")

	if (shp_is_pointer) {
		if (pmtiles_unsupported(shpTM, mode)) return(NULL)

		smeta      <- shpTM$smeta
		srcname    <- paste0("layer", pane)
		layername1 <- paste0(srcname, "raster")
		url        <- smeta$url

		m <- get_mapgl(facet_row, facet_col, facet_page, mode = mode)

		m |>
			mapgl::add_pmtiles_source(id = glid, url = url, source_type = "raster") |>
			mapgl::add_raster_layer(layername1, source = glid) |>
			assign_mapgl(facet_row, facet_col, facet_page, mode = mode)

		return(NULL)

	} else if (is_regular_grid(shp)) {

		tid   <- intersect(tmapID, dt$tmapID__)
		color <- rep(NA, length(tmapID))
		sel   <- which(tmapID %in% tid)
		tid2  <- tmapID[sel]
		color[sel] <- dt$col[match(tid2, dt$tmapID__)]

		pal <- na.omit(unique(color))
		pal <- pal[substr(pal, 8, 10) != "00"]

		if (!length(pal)) return(NULL)

		res         <- split_alpha_channel(pal, alpha = 1)
		pal_col     <- res$col
		pal_opacity <- if (length(res$opacity) == 0L) 0 else max(res$opacity)

		if ("col_alpha" %in% names(dt)) pal_opacity <- max(dt$col_alpha)

		col_ids <- match(color, pal)
		m_mat   <- matrix(col_ids, ncol = ncol(shp))
		shp2    <- stars::st_as_stars(m_mat, dimensions = shp)
		rst     <- terra::rast(shp2)

		if (!terra::is.lonlat(rst)) rst <- terra::project(rst, "epsg:4326")

		ext <- terra::ext(rst)
		if (ext$ymin < -89.9) ext$ymin <- -89
		if (ext$ymax >  89.9) ext$ymax <-  89
		rst2 <- terra::crop(rst, ext)

		srcname    <- paste0("layer", pane)
		layername1 <- paste0(srcname, "raster")

		m <- get_mapgl(facet_row, facet_col, facet_page, mode = mode)

		m |>
			mapgl::add_image_source(srcname, data = rst2, colors = pal) |>
			mapgl::add_raster_layer(layername1, source = srcname,
									raster_opacity    = pal_opacity,
									raster_resampling = "nearest") |>
			assign_mapgl(facet_row, facet_col, facet_page, mode = mode)

		mapgl_submit_group(group, layername1, mode)

	} else {

		m_mat <- matrix(tmapID, nrow = nrow(shp), ncol = ncol(shp))
		shp2  <- structure(list(tmapID = m_mat), class = "stars", dimensions = shp)
		shp3  <- sf::st_geometry(sf::st_as_sf(shp2))
		crs   <- get_option_class(o$crs_step4, "sf")
		shpTM <- tmap::shapeTM(sf::st_transform(shp3, crs), tmapID)

		gp$lty <- "solid"

		mapgl_polygons(a, shpTM, dt, pdt,
					   popup.format = NULL, hdt = NULL, idt = NULL,
					   gp, bbx, facet_row, facet_col, facet_page,
					   id, pane, group, glid, o, mode = mode)
	}
	NULL
}
