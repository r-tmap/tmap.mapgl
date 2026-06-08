#' @param id id
#' @param pane pane
#' @param group group
#' @param bs bs
#' @export
#' @keywords internal
#' @return internal tmap lists
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
	mapgl_tiles_prep(a, bs, id, o, e = .TMAP_MAPBOX, mode = "mapbox")
}

#' @export
#' @rdname tmapMapbox
tmapMaplibreAuxPrepare.tm_aux_basemap = function(a, bs, id, o) {
	mapgl_tiles_prep(a, bs, id, o, e = .TMAP_MAPLIBRE, mode = "maplibre")
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxAuxPrepare.tm_aux_tiles = function(a, bs, id, o) {
	mapgl_tiles_prep(a, bs, id, o, e = .TMAP_MAPBOX, mode = "mapbox")
}

#' @export
#' @rdname tmapMapbox
tmapMaplibreAuxPrepare.tm_aux_tiles = function(a, bs, id, o) {
	mapgl_tiles_prep(a, bs, id, o, e = .TMAP_MAPLIBRE, mode = "maplibre")
}




mapgl_tiles_prep = function(a, bs, id, o, e, mode) {
	serv = a$server
	# Allow raw style URLs to pass through; only validate named providers.
	is_url = is.character(serv) && grepl("^(https?|mapbox|maptiler)://", serv)
	if (!is_url && !(serv %in% tmap::tmap_providers(mode))) {
		eq = utils::getFromNamespace("basemap_equivalent", "tmap")(serv, mode)
		if (!is.na(eq)) {
			utils::getFromNamespace("message_basemaps_equivalent", "tmap")(serv, mode, eq)
			serv = eq
		} else {
			fallback = o$basemap.server[1]
			utils::getFromNamespace("message_basemaps_invalid_provider", "tmap")(serv, mode, fallback)
			serv = fallback
		}
	}
	e$style = serv
	# Optional per-basemap API key from tm_basemap(api = ...) (the same `api`
	# argument used for Stadia/Thunderforest in plot mode). NULL unless supplied,
	# in which case it is used for esri/maptiler instead of the env var.
	e$api_key = a$api
	serv
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

