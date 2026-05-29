#' @param return.asp return.asp
#' @param vp vp
#' @param m the proxy object from `maplibre_proxy()`/`mapboxgl_proxy()`, supplied in proxy mode
#' @export
#' @keywords internal
#' @name tmapMapboxInit
#' @rdname tmapMapbox
tmapMapboxInit = function(o, return.asp = FALSE, vp, prx, dg = NULL, m = NULL, ...) {
	mapgl_init(o = o, return.asp = return.asp, vp = vp, prx = prx,
			   dg = dg, m = m, e = .TMAP_MAPBOX, ...)
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreInit = function(o, return.asp = FALSE, vp, prx, dg = NULL, m = NULL, ...) {
	mapgl_init(o = o, return.asp = return.asp, vp = vp, prx = prx,
			   dg = dg, m = m, e = .TMAP_MAPLIBRE, ...)
}


mapgl_init = function(o, return.asp, vp, prx, dg = NULL, m = NULL, e, ...) {
	if (return.asp) return(1)

	per_page = rep(o$ncols * o$nrows, o$npages)
	k = o$ncols * o$nrows * o$npages
	if (o$n < k) {
		per_page[o$npages] = per_page[o$npages] - (k - o$n)
	}

	proxy = isTRUE(.TMAP$proxy)

	# proxy mode: draw onto the live map via `m`; otherwise use base widget `dg`
	base = if (proxy) m else dg

	if (proxy && length(prx)) {
		pane_name = getFromNamespace("pane_name", "tmap")
		zres   = vapply(prx, function(p) p$zindex, FUN.VALUE = numeric(1))
		rm_ids = mapgl_layer_ids(e, pane_name(zres))   # 401 -> "tmap401", matches the keys
		if (length(rm_ids)) {
			mapgl::clear_layer(m, rm_ids)
			e$layer_zindex[pane_name(zres)] = NULL
		}
	}

	ms = lapply(per_page, function(p) lapply(seq_len(p), function(i) base))

	e$ms     = ms
	e$nrow   = o$nrows
	e$ncol   = o$ncols
	e$leg_id = 1

	# don't wipe the registries on a proxy update
	if (!proxy) {
		e$grps         = list()
		e$layer_zindex = list()
	}
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

