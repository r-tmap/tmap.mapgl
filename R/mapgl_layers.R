#' @param shpTM, dt, pdt, popup.format, hdt, idt, gp args
#' @export
#' @keywords internal
#' @name tmapMapboxPolygons
#' @rdname tmapMapbox
tmapMapboxPolygons = function(shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	mapgl_polygons(shpTM,
				   dt,
				   pdt,
				   popup.format,
				   hdt,
				   idt,
				   gp,
				   bbx,
				   facet_row,
				   facet_col,
				   facet_page,
				   id,
				   pane,
				   group,
				   o,
				   ...,
				   mode = "mapbox")
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibrePolygons = function(shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	mapgl_polygons(shpTM,
				   dt,
				   pdt,
				   popup.format,
				   hdt,
				   idt,
				   gp,
				   bbx,
				   facet_row,
				   facet_col,
				   facet_page,
				   id,
				   pane,
				   group,
				   o,
				   ...,
				   mode = "maplibre")
}



mapgl_polygons = function(shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ..., mode) {
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

	srcname = paste0("layer", pane)
	layername1 = paste0(srcname, "polygons_fill")
	layername2 = paste0(srcname, "polygons_border")

	nofill = all(gp$fill == o$value.blank$fill)

	m |> mapgl::add_source(srcname, data = shp2) |>
		mapgl::add_fill_layer(layername1, source = srcname,
							  fill_color = mapgl::get_column("fill"),
							  fill_opacity = mapgl::get_column("fill_alpha")) |>
		mapgl::add_line_layer(layername2, source = srcname,
							  line_color = mapgl::get_column("col"),
							  line_opacity = mapgl::get_column("col_alpha"),
							  line_width = mapgl::get_column("lwd")) |>
		assign_mapgl(facet_row, facet_col, facet_page, mode = mode)

	NULL
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxPolygons3d = function(shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	mapgl_polygons3d(shpTM,
				   dt,
				   pdt,
				   popup.format,
				   hdt,
				   idt,
				   gp,
				   bbx,
				   facet_row,
				   facet_col,
				   facet_page,
				   id,
				   pane,
				   group,
				   o,
				   ...,
				   mode = "mapbox")
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibrePolygons3d = function(shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	mapgl_polygons3d(shpTM,
				   dt,
				   pdt,
				   popup.format,
				   hdt,
				   idt,
				   gp,
				   bbx,
				   facet_row,
				   facet_col,
				   facet_page,
				   id,
				   pane,
				   group,
				   o,
				   ...,
				   mode = "maplibre")
}

mapgl_polygons3d = function(shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ..., mode) {
	args = list(...)

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

	srcname = paste0("layer", pane)
	layername1 = paste0(srcname, "polygons_fill")
	layername2 = paste0(srcname, "polygons_border")

	if (is.na(args$height.max)) {
		if (consider_global(shp)) {
			height.max = sqrt(5.1e+14) * 0.1
		} else {
			sqrt_area_m = bbx |>
				tmaptools::bb_poly() |>
				sf::st_area() |>
				sqrt() |>
				units::set_units("m") |>
				units::drop_units()
			height.max = sqrt_area_m * 0.1
		}

	} else {
		height.max = args$height.max
	}

	shp2$height = shp2$height * height.max

	nofill = all(gp$fill == o$value.blank$fill)

	m |> mapgl::add_source(srcname, data = shp2) |>
		mapgl::add_line_layer(layername2, source = srcname,
							  line_color = mapgl::get_column("col"),
							  line_opacity = mapgl::get_column("col_alpha"),
							  line_width = mapgl::get_column("lwd")) |>
		mapgl::add_fill_extrusion_layer(layername1, source = srcname,
										fill_extrusion_color = mapgl::get_column("fill"),
										#fill_extrusion_opacity = mapgl::get_column("fill_alpha"),
										fill_extrusion_base = 0,
										fill_extrusion_height = mapgl::get_column("height")) |>
		assign_mapgl(facet_row, facet_col, facet_page, mode = mode)


}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxLines = function(shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	mapgl_lines(shpTM,
				   dt,
				   pdt,
				   popup.format,
				   hdt,
				   idt,
				   gp,
				   bbx,
				   facet_row,
				   facet_col,
				   facet_page,
				   id,
				   pane,
				   group,
				   o,
				   ...,
				   mode = "mapbox")

}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreLines = function(shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	mapgl_lines(shpTM,
				dt,
				pdt,
				popup.format,
				hdt,
				idt,
				gp,
				bbx,
				facet_row,
				facet_col,
				facet_page,
				id,
				pane,
				group,
				o,
				...,
				mode = "maplibre")

}


mapgl_lines = function(shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ..., mode) {

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

	mapbox |> mapgl::add_source("sourceLines", data = shp2) |>
		mapgl::add_line_layer("layerLine", source = "sourceLines",
							  line_color = mapgl::get_column("col"),
							  line_opacity = mapgl::get_column("col_alpha"),
							  line_width = mapgl::get_column("lwd")) |>
		assign_mapgl(facet_row, facet_col, facet_page, mode = mode)
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




#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxSymbols = function(shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	mapgl_symbols(shpTM,
				dt,
				pdt,
				popup.format,
				hdt,
				idt,
				gp,
				bbx,
				facet_row,
				facet_col,
				facet_page,
				id,
				pane,
				group,
				o,
				...,
				mode = "mapbox")

}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreSymbols = function(shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	mapgl_symbols(shpTM,
				dt,
				pdt,
				popup.format,
				hdt,
				idt,
				gp,
				bbx,
				facet_row,
				facet_col,
				facet_page,
				id,
				pane,
				group,
				o,
				...,
				mode = "maplibre")

}



mapgl_symbols = function(shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ..., mode) {
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



mapgl_raster = function(shpTM, dt, gp, pdt, popup.format, hdt, idt, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ..., mode) {

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

		m = get_mapgl(facet_row, facet_col, facet_page, mode = mode)

		m |> mapgl::add_image_source("sourceRaster",
							   data = rst2, colors = pal) |>
			mapgl::add_raster_layer("layerRaster", source = "sourceRaster",
								 raster_opacity = pal_opacity,
									raster_resampling = "nearest") |>
			assign_mapgl(facet_row, facet_col, facet_page, mode = mode)
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


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxRaster = function(shpTM, dt, gp, pdt, popup.format, hdt, idt, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	mapgl_raster(shpTM,
				   dt,
				   pdt,
				   popup.format,
				   hdt,
				   idt,
				   gp,
				   bbx,
				   facet_row,
				   facet_col,
				   facet_page,
				   id,
				   pane,
				   group,
				   o,
				   ...,
				   mode = "mapbox")

	NULL
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreRaster = function(shpTM, dt, gp, pdt, popup.format, hdt, idt, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	mapgl_raster(shpTM,
				 dt,
				 pdt,
				 popup.format,
				 hdt,
				 idt,
				 gp,
				 bbx,
				 facet_row,
				 facet_col,
				 facet_page,
				 id,
				 pane,
				 group,
				 o,
				 ...,
				 mode = "maplibre")

	NULL
}

#' @export
#' @keywords internal
#' @name tmapMapboxText
#' @rdname tmapMapbox
tmapMapboxText = function(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	warning("tm_text not yet implemented for this mode")
	NULL
}
