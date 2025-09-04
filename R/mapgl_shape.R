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

get_style = function(name) {
	sub("^[A-Za-z]+[._]([A-Za-z]+(?:[._][A-Za-z]+)*)$", "\\1", name) |>
		chartr("._", "--", x = _)
#	sub("^[A-Za-z]+[._]([A-Za-z]+)[._]([A-Za-z]+)$", "\\1-\\2", name)
}

mapgl_shape = function(bbx, facet_row, facet_col, facet_page, o, mode) {
	m = get_mapgl(facet_row, facet_col, facet_page, mode)

	bbx = sf::st_bbox(sf::st_transform(tmaptools::bb_poly(bbx), crs = 4326))
	ll = unname(c(mean(bbx[c(1,3)]), mean(bbx[c(2,4)])))
	zoom = findZoom(bbx)

	style = if (mode == "mapbox") {
		if (substr(.TMAP_MAPBOX$style, 1, 4) == "ofm.") {
			paste0("https://tiles.openfreemap.org/styles/", substr(.TMAP_MAPBOX$style, 5, nchar(.TMAP_MAPBOX$style)))
		} else if (.TMAP_MAPBOX$style %in% tmap_providers()) {
			mapgl::mapbox_style(get_style(.TMAP_MAPBOX$style))
		} else {
			.TMAP_MAPBOX$style
		}

	} else {
		if (substr(.TMAP_MAPLIBRE$style, 1, 4) == "ofm.") {
			paste0("https://tiles.openfreemap.org/styles/", substr(.TMAP_MAPLIBRE$style, 5, nchar(.TMAP_MAPLIBRE$style)))
		} else if (.TMAP_MAPLIBRE$style %in% tmap_providers()) {
			mapgl::carto_style(get_style(.TMAP_MAPLIBRE$style))
		} else {
			.TMAP_MAPLIBRE$style
		}
	}

	if (mode == "mapbox") {
		# quick & dirty
		if (zoom < 3) {
			# ignore center for global view (otherwise it will be (0, -3) due to Antarctica)
			m = mapgl::mapboxgl(center = c(0,0), zoom = zoom, pitch = o$pitch, style = style) |>
				mapgl::add_navigation_control(visualize_pitch = TRUE)
		} else {
			m = mapgl::mapboxgl(center = ll, zoom = zoom, pitch = o$pitch, style = style) |>
				mapgl::add_navigation_control(visualize_pitch = TRUE)
		}
	} else {
		# quick & dirty
		if (zoom < 3) {
			# ignore center for global view (otherwise it will be (0, -3) due to Antarctica)
			m = mapgl::maplibre(center = c(0,0), zoom = zoom, pitch = o$pitch, style = style) |>
				mapgl::add_navigation_control(visualize_pitch = TRUE) |>
				mapgl::add_globe_control()
		} else {
			m = mapgl::maplibre(center = ll, zoom = zoom, pitch = o$pitch, style = style) |>
				mapgl::add_navigation_control(visualize_pitch = TRUE) |>
				mapgl::add_globe_control()
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
