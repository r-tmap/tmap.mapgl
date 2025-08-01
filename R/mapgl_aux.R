#' @param id id
#' @param pane pane
#' @param group group
#' @param bs bs
#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxAuxPrepare = function(a, bs, id, o) {
	UseMethod("tmapMapboxAuxPrepare")
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxAuxPrepare.default = function(a, bs, id, o) {
	""
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxAuxPlot = function(a, bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	UseMethod("tmapMapboxAuxPlot")
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxAuxPlot.default = function(a, bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	NULL
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreAuxPrepare = function(a, bs, id, o) {
	UseMethod("tmapMaplibreAuxPrepare")
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreAuxPrepare.default = function(a, bs, id, o) {
	""
}



#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreAuxPlot = function(a, bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	UseMethod("tmapMaplibreAuxPlot")
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreAuxPlot.default = function(a, bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	NULL
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxAuxPrepare.tm_aux_basemap = function(a, bs, id, o) {
	mapgl_tiles_prep(a, bs, id, o, e = .TMAP_MAPBOX)
}

#' @export
#' @rdname tmapMapbox
tmapMaplibreAuxPrepare.tm_aux_basemap = function(a, bs, id, o) {
	mapgl_tiles_prep(a, bs, id, o, e = .TMAP_MAPLIBRE)
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxAuxPrepare.tm_aux_tiles = function(a, bs, id, o) {
	mapgl_tiles_prep(a, bs, id, o, e = .TMAP_MAPBOX)
}

#' @export
#' @rdname tmapMapbox
tmapMaplibreAuxPrepare.tm_aux_tiles = function(a, bs, id, o) {
	mapgl_tiles_prep(a, bs, id, o, e = .TMAP_MAPLIBRE)
}




mapgl_tiles_prep = function(a, bs, id, o, e) {
	e$style = a$server
	a$server
}


#' @export
#' @rdname tmapMapbox
tmapMapboxAuxPlot.tm_aux_basemap = function(a, bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	NULL
}

#' @export
#' @rdname tmapMapbox
tmapMaplibreAuxPlot.tm_aux_basemap = function(a, bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	NULL
}


#' @export
#' @rdname tmapMapbox
tmapMapboxAuxPlot.tm_aux_tiles = function(a, bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	NULL
}

#' @export
#' @rdname tmapMapbox
tmapMaplibreAuxPlot.tm_aux_tiles = function(a, bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
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

