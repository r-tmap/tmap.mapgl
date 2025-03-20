#' @param bbx bbx
#' @export
#' @keywords internal
#' @name tmapMapboxShape
#' @rdname tmapMapbox
tmapMapboxShape = function(bbx, facet_row, facet_col, facet_page, o) {
	dummy = get_mapbox(facet_row, facet_col, facet_page)

#
#
# 	bbx = sf::st_bbox(sf::st_transform(tmaptools::bb_poly(bbx), crs = 4326))
#
# 	ll = c(mean(bbx[c(1,3)]), mean(bbx[c(2,4)]))
# 	zoom = findZoom(bbx)
#
# 	if (length(dummy) == 0) {
# 		mapbox = mapbox::mapbox(zoom = zoom, latitude = ll[2], longitude = ll[1], pitch = o$pitch)
# 	} else {
# 		mapbox = dummy
# 	}
#
# 	assign_mapbox(mapbox, facet_row, facet_col, facet_page)

	# TODO

	# via add_source, but that is layer dependent

	NULL
}

#' @export
#' @keywords internal
#' @name tmapMapboxOverlay
#' @rdname tmapMapbox
tmapMapboxOverlay = function(bbx, facet_row, facet_col, facet_page, o) {
	NULL
}
