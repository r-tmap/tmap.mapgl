#' @param bbx bbx
#' @export
#' @keywords internal
#' @name tmapMapboxShape
#' @rdname tmapMapbox
tmapMapboxShape = function(bbx, facet_row, facet_col, facet_page, o) {
	m = get_mapbox(facet_row, facet_col, facet_page)

 	bbx = sf::st_bbox(sf::st_transform(tmaptools::bb_poly(bbx), crs = 4326))
 	ll = unname(c(mean(bbx[c(1,3)]), mean(bbx[c(2,4)])))
 	zoom = findZoom(bbx)

 	# quick & dirty
 	if (zoom < 3) {
 		# ignore center for global view (otherwise it will be (0, -3) due to Antarctica)
 		m = mapgl::mapboxgl(center = c(0,0), zoom = zoom, pitch = o$pitch, style = mapgl::mapbox_style(.TMAP_MAPBOX$style))
 	} else {
 		m = mapgl::mapboxgl(center = ll, zoom = zoom, pitch = o$pitch, style = mapgl::mapbox_style(.TMAP_MAPBOX$style))
 	}

 	assign_mapbox(m, facet_row, facet_col, facet_page)

	NULL
}

#' @export
#' @keywords internal
#' @name tmapMapboxOverlay
#' @rdname tmapMapbox
tmapMapboxOverlay = function(bbx, facet_row, facet_col, facet_page, o) {
	NULL
}
