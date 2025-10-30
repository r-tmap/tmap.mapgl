view_format_popups_mapgl <- function(id=NULL, titles, format, values) {



	# isnull <- vapply(values, is.null, logical(1))
	#
	# titles <- titles[!isnull]
	# titles[names(titles)!=""] <- names(titles)[names(titles)!=""]
	#
	# values <- values[!isnull]

	# islist <- is.list(format) && length(format)>0 && is.list(format[[1]])
	# if (!islist) {
	# 	format <- lapply(1:length(titles), function(i) format)
	# }
	h = lapply(format, function(f) {
		if (f$html.escape) {
			htmltools::htmlEscape
		} else {
			function(x) x
		}
	})
	if (!is.null(id)) {
		labels <- paste("<b>", h[[1]](id), "</b>", sep="")
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
		numbers <- hi(if (is.numeric(v)) do.call("fancy_breaks", c(list(vec=as.numeric(v), intervals=FALSE), f)) else v)
		paste0(numbers, popup_append)
	}, values, format, h, SIMPLIFY = FALSE)


	labels2 <- mapply(function(l, v) {
		paste0("<tr><td style=\"color: #888888;\"><nobr>", l, "</nobr></td><td align=\"right\"><nobr>", v, "</nobr></td>")
	}, titles_format, values_format, SIMPLIFY=FALSE)

	labels3 <- paste0(do.call("paste", c(labels2, list(sep="</tr>"))), "</tr>")

	padding_right <- ifelse(length(titles_format) > 13, 200, 0) # add padding for horizontal scroll bar. These will appear on most browsers when there are over 13 normal lines (tested: RStudio, FF, Chrome)

	x <- paste0(
		"<div style='width:auto; max-height:25em; overflow-y:auto; overflow-x:hidden;'>",
		"<table style='width:100%; table-layout:fixed;'>",
		"<thead><tr><th colspan='2'>", labels, "</th></tr></thead>",
		labels3,
		"</table></div>"
	)

	x
}

mapgl_polygons = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ..., mode) {
	m = get_mapgl(facet_row, facet_col, facet_page, mode)

	rc_text = frc(facet_row, facet_col)

	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt

	popups = NULL

	x = sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1,3)]), bbx[2]))), crs = sf::st_crs(bbx))
	y = sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1,3)]), bbx[4]))), crs = sf::st_crs(bbx))

	gp = impute_gp(gp, dt)
	gp = rescale_gp(gp, o$scale_down)

	if (any(nchar(gp$fill) == 9)) {
		fill_alpha = split_alpha_channel(gp$fill, alpha = gp$fill_alpha)
		gp$fill = fill_alpha$col
		gp$fill_alpha = gp$fill_alpha * fill_alpha$opacity
	}
	if (any(nchar(gp$col) == 9)) {
		fill_alpha = split_alpha_channel(gp$col, alpha = gp$col_alpha)
		gp$col = fill_alpha$col
		gp$col_alpha = gp$fill_alpha * fill_alpha$opacity
	}


	shp2 = sf::st_sf(unclass(gp[c("fill", "col", "lwd", "fill_alpha", "col_alpha")]), id = 1:length(shp), geometry = shp)

	if (!is.null(hdt)) {
		shp2$hover = hdt$hover[match(dt$tmapID__, hdt$tmapID__)]
		shp2$hover = vapply(shp2$hover, htmltools::HTML, FUN.VALUE = character(1))

		hdt = mapgl::get_column("hover")
	}

	if (!is.null(pdt)) {
		mtch = match(dt$tmapID__, pdt$tmapID__)
		pdt = pdt[mtch][, tmapID__ := NULL]

		if (is.null(idt) && !is.null(hdt)) {
			shp2$popup = view_format_popups_mapgl(id = shp2$hover, titles = names(pdt), values = pdt, format = popup.format)
		} else {
			shp2$popup = view_format_popups_mapgl(id = shp2$hover, titles = names(pdt), values = pdt, format = popup.format)
		}
		popup = mapgl::get_column("popup")

	} else {
		popup = NULL
	}


	srcname = paste0("layer", pane)
	layername1 = paste0(srcname, "polygons_fill")
	layername2 = paste0(srcname, "polygons_border")

	nofill = all(gp$fill == o$value.blank$fill)

	m |> mapgl::add_source(srcname, data = shp2) |>
		mapgl::add_fill_layer(layername1, source = srcname,
							  fill_color = mapgl::get_column("fill"),
							  fill_opacity = mapgl::get_column("fill_alpha"),
							  tooltip = hdt,popup = popup
							  ) |>
		mapgl::add_line_layer(layername2, source = srcname,
							  line_color = mapgl::get_column("col"),
							  line_opacity = mapgl::get_column("col_alpha"),
							  line_width = mapgl::get_column("lwd")) |>
		assign_mapgl(facet_row, facet_col, facet_page, mode = mode)

	mapgl_submit_group(group, c(layername1, layername2), mode)
	NULL
}





mapgl_polygons_3d = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ..., mode) {

	m = get_mapgl(facet_row, facet_col, facet_page, mode)

	rc_text = frc(facet_row, facet_col)

	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt

	popups = NULL

	x = sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1,3)]), bbx[2]))), crs = sf::st_crs(bbx))
	y = sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1,3)]), bbx[4]))), crs = sf::st_crs(bbx))

	gp = impute_gp(gp, dt)
	gp = rescale_gp(gp, o$scale_down)

	if (any(nchar(gp$fill) == 9)) {
		fill_alpha = split_alpha_channel(gp$fill, alpha = gp$fill_alpha)
		gp$fill = fill_alpha$col
		gp$fill_alpha = gp$fill_alpha * fill_alpha$opacity
	}
	if (any(nchar(gp$col) == 9)) {
		fill_alpha = split_alpha_channel(gp$col, alpha = gp$col_alpha)
		gp$col = fill_alpha$col
		gp$col_alpha = gp$fill_alpha * fill_alpha$opacity
	}

	shp2 = sf::st_sf(unclass(gp[c("height", "fill", "col", "lwd", "fill_alpha", "col_alpha")]), id = 1:length(shp), geometry = shp)

	if (!is.null(hdt)) {
		shp2$hover = hdt$hover[match(dt$tmapID__, hdt$tmapID__)]
		shp2$hover = vapply(shp2$hover, htmltools::HTML, FUN.VALUE = character(1))

		hdt = mapgl::get_column("hover")
	}

	if (!is.null(pdt)) {
		mtch = match(dt$tmapID__, pdt$tmapID__)
		pdt = pdt[mtch][, tmapID__ := NULL]

		if (is.null(idt) && !is.null(hdt)) {
			shp2$popup = view_format_popups_mapgl(id = shp2$hover, titles = names(pdt), values = pdt, format = popup.format)
		} else {
			shp2$popup = view_format_popups_mapgl(id = shp2$hover, titles = names(pdt), values = pdt, format = popup.format)
		}
		popup = mapgl::get_column("popup")

	} else {
		popup = NULL
	}


	srcname = paste0("layer", pane)
	layername1 = paste0(srcname, "polygons_fill")
	layername2 = paste0(srcname, "polygons_border")

	if (is.character(a$height.max)) {
		is_perc_max = grepl("%$", a$height.max)
		height.max = as.numeric(sub("%$", "", a$height.max))
		if (is_perc_max) height.max = height.max / 100
	} else {
		is_perc_max = FALSE
		height.max = as.numeric(a$height.max)
	}

	if (is.character(a$height.min)) {
		is_perc_min = grepl("%$", a$height.min)
		height.min = as.numeric(sub("%$", "", a$height.min))
		if (is_perc_min) height.min = height.min / 100
	} else {
		is_perc_min = FALSE
		height.min = as.numeric(a$height.min)
	}


	if (is_perc_max || is_perc_min) {
		if (consider_global(shp)) {
			sqrt_area_m = sqrt(5.1e+14)
		} else {
			sqrt_area_m = bbx |>
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

	nofill = all(gp$fill == o$value.blank$fill)

	shp2_naomit = shp2[!is.na(shp2$height), ]

	m |> mapgl::add_source(srcname, data = shp2_naomit) |>
		mapgl::add_line_layer(layername2, source = srcname,
							  line_color = mapgl::get_column("col"),
							  line_opacity = mapgl::get_column("col_alpha"),
							  line_width = mapgl::get_column("lwd")) |>
		mapgl::add_fill_extrusion_layer(layername1, source = srcname,
										fill_extrusion_color = mapgl::get_column("fill"),
										#fill_extrusion_opacity = mapgl::get_column("fill_alpha"),
										fill_extrusion_base = 0,
										fill_extrusion_height = mapgl::get_column("height"),
										tooltip = hdt,popup = popup
										) |>
		assign_mapgl(facet_row, facet_col, facet_page, mode = mode)
	mapgl_submit_group(group, c(layername1, layername2), mode)
	NULL
}



mapgl_lines = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ..., mode) {

	mapbox = get_mapgl(facet_row, facet_col, facet_page, mode = mode)

	rc_text = frc(facet_row, facet_col)

	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt

	popups = NULL

	x = sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1,3)]), bbx[2]))), crs = sf::st_crs(bbx))
	y = sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1,3)]), bbx[4]))), crs = sf::st_crs(bbx))

	gp = impute_gp(gp, dt)
	gp = rescale_gp(gp, o$scale_down)

	shp2 = sf::st_sf(unclass(gp[c("col", "lwd", "col_alpha")]), id = 1:length(shp), geometry = shp)

	srcname = paste0("layer", pane)
	layername1 = paste0(srcname, "lines")

	mapbox |> mapgl::add_source(srcname, data = shp2) |>
		mapgl::add_line_layer(layername1, source = srcname,
							  line_color = mapgl::get_column("col"),
							  line_opacity = mapgl::get_column("col_alpha"),
							  line_width = mapgl::get_column("lwd")) |>
		assign_mapgl(facet_row, facet_col, facet_page, mode = mode)
	mapgl_submit_group(group, layername1, mode)
	NULL
}




lty2dash = function(lty) {
	tab = c(solid = "", dashed = "4 4", dotted = "1 3", dotdash = "1 3 4 3", longdash = "7 3", twodash = "2 2 6 2")
	are_words = (lty %in% names(tab))
	if (all(are_words)) {
		unname(tab[lty])
	} else {
		are_letters = (suppressWarnings(!is.na(as.numeric(lty))))

		if (!all(are_letters | are_words)) {
			stop("Incorrect lty specification: ", lty[which(!are_letters & !are_words)[1]])
		} else {
			lty[are_words] = unname(tab[lty[are_words]])
			lty[are_letters] = vapply(strsplit(lty[are_letters], ""), FUN = function(x) paste(x, collapse = " "), FUN.VALUE = character(1))
		}
		lty
	}

}





mapgl_symbols = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ..., mode) {
	mapbox = get_mapgl(facet_row, facet_col, facet_page, mode)

	rc_text = frc(facet_row, facet_col)

	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt

	popups = NULL

	x = sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1,3)]), bbx[2]))), crs = sf::st_crs(bbx))
	y = sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1,3)]), bbx[4]))), crs = sf::st_crs(bbx))

	gp = impute_gp(gp, dt)
	gp = rescale_gp(gp, o$scale_down)

	if (any(nchar(gp$fill) == 9)) {
		fill_alpha = split_alpha_channel(gp$fill, alpha = gp$fill_alpha)
		gp$fill = fill_alpha$col
		gp$fill_alpha = gp$fill_alpha * fill_alpha$opacity
	}
	if (any(nchar(gp$col) == 9)) {
		fill_alpha = split_alpha_channel(gp$col, alpha = gp$col_alpha)
		gp$col = fill_alpha$col
		gp$col_alpha = gp$fill_alpha * fill_alpha$opacity
	}

	shp2 = sf::st_sf(unclass(gp[c("fill", "col", "lwd", "fill_alpha", "col_alpha", "size")]), id = 1:length(shp), geometry = shp)
	shp2$size = shp2$size * 10


	srcname = paste0("layer", pane)
	layername1 = paste0(srcname, "symbols_fill")



	nofill = all(gp$fill == o$value.blank$fill)

	mapbox |> mapgl::add_source(srcname, data = shp2) |>
		mapgl::add_circle_layer(layername1, source = srcname,
							  circle_color = mapgl::get_column("fill"),
							  circle_opacity = mapgl::get_column("fill_alpha"),
								circle_stroke_color = mapgl::get_column("col"),
								circle_stroke_opacity = mapgl::get_column("col_alpha"),
								circle_stroke_width = mapgl::get_column("lwd"),
								circle_radius = mapgl::get_column("size")) |>
		assign_mapgl(facet_row, facet_col, facet_page, mode = mode)
	mapgl_submit_group(group, layername1, mode)
	NULL
}

split_alpha_channel <- function(x, alpha) {
	if (is.null(x)) {
		list(col=NULL, opacity=0)
	} else {
		RGBA <- col2rgb(x, alpha = TRUE)
		col <- rgb(RGBA[1,], RGBA[2,], RGBA[3,], maxColorValue = 255)
		opacity <- unname(RGBA[4,]/255 * alpha)
		list(col=col, opacity=opacity)
	}
}



mapgl_raster = function(a, shpTM, dt, gp, pdt, popup.format, hdt, idt, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ..., mode) {

	rc_text = frc(facet_row, facet_col)


	shp = shpTM$shp
	tmapID = shpTM$tmapID


	if (is_regular_grid(shp)) {

		tid = intersect(tmapID, dt$tmapID__)

		color = rep(NA, length(tmapID)) # NA

		sel = which(tmapID %in% tid)
		tid2 = tmapID[sel]

		color[sel] = dt$col[match(tid2, dt$tmapID__)]

		pal <- na.omit(unique(color))
		pal <- pal[substr(pal, 8,10)!="00"] ## remove transparant colors

		if (!length(pal)) return(NULL)

		res <- split_alpha_channel(pal, alpha = 1)
		pal_col <- res$col
		pal_opacity <- if (length(res$opacity) == 0L) 0 else max(res$opacity)

		if ("col_alpha" %in% names(dt)) pal_opacity = max(dt$col_alpha)


		col_ids <- match(color, pal)

		m <- matrix(col_ids, ncol = ncol(shp))

		shp2 = stars::st_as_stars(m, dimensions = shp)

		rst = terra::rast(shp2)

		if (!terra::is.lonlat(rst)) {
			rst = terra::project(rst, "epsg:4326")
		}

		# crop latitutes
		ext = terra::ext(rst)

		ext2 = ext
		if (ext2$ymin < -89.9) ext2$ymin = -89
		if (ext2$ymax > 89.9) ext2$ymax = 89

		rst2 = terra::crop(rst, ext2)


		srcname = paste0("layer", pane)
		layername1 = paste0(srcname, "raster")



		m = get_mapgl(facet_row, facet_col, facet_page, mode = mode)



		m |> mapgl::add_image_source(srcname,
							   data = rst2, colors = pal) |>
			mapgl::add_raster_layer(layername1, source = srcname,
								 raster_opacity = pal_opacity,
									raster_resampling = "nearest") |>
			assign_mapgl(facet_row, facet_col, facet_page, mode = mode)
		mapgl_submit_group(group, layername1, mode)
	} else {
		#shp2 = stars::st_as_stars(list(values = tmapID), dimensions = shp)
		#shpTM = shapeTM(sf::st_geometry(sf::st_as_sf(shp2)), as.vector(tmapID))

		m = matrix(tmapID, nrow = nrow(shp), ncol = ncol(shp))
		shp2 = structure(list(tmapID = m), class = "stars", dimensions = shp)

		shp3 = sf::st_geometry(sf::st_as_sf(shp2))

		crs = get_option_class(o$crs_step4, "sf")

		shpTM = tmap::shapeTM(sf::st_transform(shp3, crs), tmapID)


		gp$lty = "solid"

		mapgl_polygons(shpTM, dt, pdt, popup.format = NULL, hdt = NULL, idt = NULL, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o)
	}
	NULL
}

