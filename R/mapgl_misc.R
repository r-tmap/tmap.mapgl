

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

	ms = get("ms", envir = e)
	nrow = get("nrow", envir = e)
	ncol = get("ncol", envir = e)

	fr = max(1, facet_row) # facet_row can be -1 or -2
	fc = max(1, facet_col) # facet_row can be -1 or -2

	mid = get_facet_id(fr, fc, nrow, ncol)

	ms[[facet_page]][[mid]] = m
	assign("ms", ms, envir = e)
	NULL
}


mapgl_submit_group = function(group, layers, mode) {
	e = if (mode == "mapbox") {
		.TMAP_MAPBOX
	} else {
		.TMAP_MAPLIBRE
	}

	grps = get("grps", envir = e)

	#id = if (nrow(grps) == 0) 1L else max(grps$id) + 1L

	#grps = rbind(grps, data.frame(id = id, group = group, layer = layers))

	if (group %in% names(grps)) {
		grps[[group]] = unique(c(grps[[group]], layers))
	} else {
		grps[[group]] = layers
	}
	assign("grps", grps, envir = e)
	NULL
}
