#' @param bs bs
#' @export
#' @keywords internal
#' @name tmapMapboxTilesPrep
#' @rdname tmapMapbox
tmapMapboxTilesPrep = function(a, bs, id, o) {
	tiles = lapply(1L:length(bs), function(i) a)
	.TMAP_MAPBOX$tiles[[id]] = tiles
	paste0(a$server, collapse = "__")
}

#' @export
#' @keywords internal
#' @name tmapMapboxTiles
#' @rdname tmapMapbox
tmapMapboxTiles = function(bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {

	# TODO
	NULL
}

#' @export
#' @keywords internal
#' @name tmapMapboxGridPrep
#' @rdname tmapMapbox
tmapMapboxGridPrep = function(a, bs, id, o) {
	return("grid")
}

#' @param id id
#' @param pane pane
#' @param group group
#' @export
#' @keywords internal
#' @name tmapMapboxGrid
#' @rdname tmapMapbox
tmapMapboxGrid = function(bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	NULL
}

#' @param bi bi
#' @export
#' @keywords internal
#' @name tmapMapboxGridXLab
#' @rdname tmapMapbox
tmapMapboxGridXLab = function(bi, bbx, facet_row, facet_col, facet_page, o) {
	NULL
}

#' @export
#' @keywords internal
#' @name tmapMapboxGridYLab
#' @rdname tmapMapbox
tmapMapboxGridYLab = function(bi, bbx, facet_row, facet_col, facet_page, o) {
	NULL
}
