#' @param bbx bbx
#' @export
#' @keywords internal
#' @name tmapMapboxShape
#' @rdname tmapMapbox
tmapMapboxShape = function(bbx, facet_row, facet_col, facet_page, o) {
	mapgl_shape(bbx = bbx,
				facet_row = facet_row,
				facet_col = facet_col,
				facet_page = facet_page,
				o = o,
				mode = "mapbox")
}

#' @export
#' @rdname tmapMapbox
tmapMaplibreShape = function(bbx, facet_row, facet_col, facet_page, o) {
	mapgl_shape(bbx = bbx,
				facet_row = facet_row,
				facet_col = facet_col,
				facet_page = facet_page,
				o = o,
				mode = "maplibre")
}

mapgl_shape = function(bbx, facet_row, facet_col, facet_page, o, mode) {
	m = get_mapgl(facet_row, facet_col, facet_page, mode)

	bbx = sf::st_bbox(sf::st_transform(tmaptools::bb_poly(bbx), crs = 4326))
	ll = unname(c(mean(bbx[c(1,3)]), mean(bbx[c(2,4)])))
	zoom = findZoom(bbx)


	style = if (mode == "mapbox") {
		mapgl::mapbox_style(.TMAP_MAPBOX$style)
	} else {
		mapgl::carto_style(.TMAP_MAPLIBRE$style)
	}

	if (mode == "mapbox") {
		# quick & dirty
		if (zoom < 3) {
			# ignore center for global view (otherwise it will be (0, -3) due to Antarctica)
			m = mapgl::mapboxgl(center = c(0,0), zoom = zoom, pitch = o$pitch, style = style)
		} else {
			m = mapgl::mapboxgl(center = ll, zoom = zoom, pitch = o$pitch, style = style)
		}
	} else {
		# quick & dirty
		if (zoom < 3) {
			# ignore center for global view (otherwise it will be (0, -3) due to Antarctica)
			m = mapgl::maplibre(center = c(0,0), zoom = zoom, pitch = o$pitch, style = style) |>
				mapgl::add_navigation_control(visualize_pitch = TRUE)
		} else {
			m = mapgl::maplibre(center = ll, zoom = zoom, pitch = o$pitch, style = style) |>
				mapgl::add_navigation_control(visualize_pitch = TRUE)
		}
	}
	assign_mapgl(m, facet_row, facet_col, facet_page, mode = mode)

	NULL
}



#' @export
#' @keywords internal
#' @name tmapMapboxOverlay
#' @rdname tmapMapbox
tmapMapboxOverlay = function(bbx, facet_row, facet_col, facet_page, o) {
	NULL
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreOverlay = function(bbx, facet_row, facet_col, facet_page, o) {
	NULL
}
