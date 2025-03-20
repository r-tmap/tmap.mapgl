

get_facet_id = function(row, col, nrow, ncol) {
	col + (row - 1L) * ncol
}

get_mapbox = function(facet_row, facet_col, facet_page) {
	mapboxs = get("mapboxs", envir = .TMAP_MAPBOX)
	nrow = get("nrow", envir = .TMAP_MAPBOX)
	ncol = get("ncol", envir = .TMAP_MAPBOX)

	mapboxsi = mapboxs[[facet_page]]

	fr = max(1, facet_row) # facet_row can be -1 or -2
	fc = max(1, facet_col) # facet_row can be -1 or -2

	mbid = get_facet_id(fr, fc, nrow, ncol)

	mapboxsi[[mbid]]
}

assign_mapbox = function(mapbox, facet_row, facet_col, facet_page) {
	mapboxs = get("mapboxs", envir = .TMAP_MAPBOX)
	nrow = get("nrow", envir = .TMAP_MAPBOX)
	ncol = get("ncol", envir = .TMAP_MAPBOX)


	fr = max(1, facet_row) # facet_row can be -1 or -2
	fc = max(1, facet_col) # facet_row can be -1 or -2



	mapboxid = get_facet_id(fr, fc, nrow, ncol)

	mapboxs[[facet_page]][[mapboxid]] = mapbox
	assign("mapboxs", mapboxs, envir = .TMAP_MAPBOX)
	NULL
}

#' @param label label
#' @export
#' @keywords internal
#' @name tmapMapboxWrap
#' @rdname tmapMapbox
tmapMapboxWrap = function(label, facet_row, facet_col, facet_page, o) {
	NULL
}


#' @export
#' @keywords internal
#' @name tmapMapboxXtab
#' @rdname tmapMapbox
tmapMapboxXtab = function(label, facet_row, facet_col, facet_page, o) {
	NULL
}
