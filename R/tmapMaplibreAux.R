#' @export
#' @name tmapMaplibreTilesPrep
#' @rdname tmapMapbox
tmapMaplibreTilesPrep = function(a, bs, id, o) {
	.TMAP_MAPLIBRE$style = a$server
	a$server
}

#' @export
#' @name tmapMaplibreTiles
#' @rdname tmapMapbox
tmapMaplibreTiles = function(bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	NULL

}

#' @export
#' @name tmapMaplibreGridPrep
#' @rdname tmapMapbox
tmapMaplibreGridPrep = function(a, bs, id, o) {
	return("grid")
}

#' @export
#' @name tmapMaplibreGrid
#' @rdname tmapMapbox
tmapMaplibreGrid = function(bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	NULL
}

#' @export
#' @name tmapMaplibreGridXLab
#' @rdname tmapMapbox
tmapMaplibreGridXLab = function(bi, bbx, facet_row, facet_col, facet_page, o) {
	NULL
}

#' @export
#' @name tmapMaplibreGridYLab
#' @rdname tmapMapbox
tmapMaplibreGridYLab = function(bi, bbx, facet_row, facet_col, facet_page, o) {
	NULL
}
