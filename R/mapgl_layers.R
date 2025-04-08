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
#' @name tmapMapboxLines
#' @rdname tmapMapbox
tmapMapboxLines = function(shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	mapbox = get_mapbox(facet_row, facet_col, facet_page)

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
		assign_mapgl(facet_row, facet_col, facet_page)
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
#' @name tmapMapboxSymbols
#' @rdname tmapMapbox
tmapMapboxSymbols = function(shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	mapbox = get_mapbox(facet_row, facet_col, facet_page)

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
		assign_mapgl(facet_row, facet_col, facet_page)
	NULL
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

#' @export
#' @keywords internal
#' @name tmapMapboxRaster
#' @rdname tmapMapbox
tmapMapboxRaster = function(shpTM, dt, gp, pdt, popup.format, hdt, idt, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	warning("tm_raster not yet implemented for this mode")

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
