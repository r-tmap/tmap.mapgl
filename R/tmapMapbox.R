

get_facet_id = function(row, col, nrow, ncol) {
	col + (row - 1L) * ncol
}


get_mapgl = function(facet_row, facet_col, facet_page, mode) {
	e = if (mode == "mapbox") {
		.TMAP_MAPBOX
	} else {
		.TMAP_MAPLIBRE
	}

	ms = get("ms", envir = e)
	nrow = get("nrow", envir = e)
	ncol = get("ncol", envir = e)

	m = ms[[facet_page]]

	fr = max(1, facet_row) # facet_row can be -1 or -2
	fc = max(1, facet_col) # facet_row can be -1 or -2

	mbid = get_facet_id(fr, fc, nrow, ncol)

	m[[mbid]]
}

assign_mapgl = function(m, facet_row, facet_col, facet_page, mode) {
	e = if (mode == "mapbox") {
		.TMAP_MAPBOX
	} else {
		.TMAP_MAPLIBRE
	}

	ms = get("ms", envir = .TMAP_MAPBOX)
	nrow = get("nrow", envir = .TMAP_MAPBOX)
	ncol = get("ncol", envir = .TMAP_MAPBOX)

	fr = max(1, facet_row) # facet_row can be -1 or -2
	fc = max(1, facet_col) # facet_row can be -1 or -2

	mid = get_facet_id(fr, fc, nrow, ncol)

	ms[[facet_page]][[mid]] = mapbox
	assign("ms", ms, envir = e)
	NULL
}

#' @param label label
#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxWrap = function(label, facet_row, facet_col, facet_page, o) {
	NULL
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxXtab = function(label, facet_row, facet_col, facet_page, o) {
	NULL
}


#' @param label label
#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreWrap = function(label, facet_row, facet_col, facet_page, o) {
	NULL
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreXtab = function(label, facet_row, facet_col, facet_page, o) {
	NULL
}
