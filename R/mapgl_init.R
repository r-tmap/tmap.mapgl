#' @param return.asp return.asp
#' @param vp vp
#' @param dg base map widget (internal; \code{NULL} in proxy mode, where the
#'   proxy object \code{m} is used as the drawing canvas instead)
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
	if (proxy) {
		mode = if (identical(e, .TMAP_MAPBOX)) "mapbox" else "maplibre"

		# Legends + controls are rebuilt wholesale on every render, so clear the
		# previous ones first (otherwise they stack). clear-all = no id argument.
		m = m |>
			mapgl::clear_legend() |>
			mapgl::clear_controls()

		# clear_controls() also removes the navigation / globe controls. On a full
		# render those are created in mapgl_shape() (navigation then globe, leaving
		# the globe icon on top), but mapgl_shape() early-returns in proxy mode, so
		# re-add them here or they vanish after the first update.
		#
		# In the proxy re-add path the controls stack in the OPPOSITE visual order
		# to the initial widget render, so to keep the globe on top (as at start)
		# we add globe first, then navigation.
		if (mode == "maplibre") m = mapgl::add_globe_control(m)
		m = mapgl::add_navigation_control(m, visualize_pitch = TRUE)
	}

	# layer removal is targeted: only the zindexes named by tm_remove_layer()
	if (proxy && length(prx)) {
		pane_name = utils::getFromNamespace("pane_name", "tmap")
		zres   = vapply(prx, function(p) p$zindex, FUN.VALUE = numeric(1))
		rm_ids = mapgl_layer_ids(e, pane_name(zres))
		if (length(rm_ids)) {
			mapgl::clear_layer(m, rm_ids)
			e$layer_zindex[pane_name(zres)] = NULL
		}
	}

	# proxy mode: draw onto the live map via `m`; otherwise use base widget `dg`
	base = if (proxy) m else dg
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
