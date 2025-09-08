#' @param return.asp return.asp
#' @param vp vp
#' @export
#' @keywords internal
#' @name tmapMapboxInit
#' @rdname tmapMapbox
tmapMapboxInit = function(o, return.asp = FALSE, vp, prx, dg = NULL, ...) {
	mapgl_init(o = o,
			   return.asp = return.asp,
			   vp = vp,
			   prx = prx,
			   dg = dg,
			   e = .TMAP_MAPBOX,
			   ...)
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreInit = function(o, return.asp = FALSE, vp, prx, dg = NULL, ...) {
	mapgl_init(o = o,
			   return.asp = return.asp,
			   vp = vp,
			   prx = prx,
			   dg = dg,
			   e = .TMAP_MAPLIBRE,
			   ...)
}


mapgl_init = function(o, return.asp, vp, prx, dg, e,...) {
	if (return.asp) return(1)

	# set projection
	crs = if (!is.na(o$crs)) o$crs else "auto"

	crs_str = if (inherits(crs, "crs")) {
		sf::st_crs(crs)$input
	} else {
		crs
	}

	crs_new = "globe"
	for (i in 1L:length(e$crs_options)) {
		if (length(grep(names(e$crs_options[i]), crs_str, fixed = TRUE)) > 0) {
			crs_new = unname(e$crs_options[i])
			break
		}
	}

	per_page = rep(o$ncols * o$nrows, o$npages)
	k = o$ncols * o$nrows * o$npages
	if (o$n < k) {
		per_page[o$npages] = per_page[o$npages] - (k - o$n)
	}

	ms = lapply(per_page, function(p) {
		lapply(seq_len(p), function(i) {
			if (!is.null(dg)) dg else NULL
		})
	})

	e$crs_new = crs_new
	e$ms = ms
	e$nrow = o$nrows
	e$ncol = o$ncols
	e$leg_id = 1
	NULL
}


#' @param q q
#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxAux = function(o, q) {
	NULL
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreAux = function(o, q) {
	NULL
}

