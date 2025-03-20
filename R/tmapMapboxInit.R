#' @param return.asp return.asp
#' @param vp vp
#' @export
#' @keywords internal
#' @name tmapMapboxInit
#' @rdname tmapMapbox
tmapMapboxInit = function(o, return.asp = FALSE, vp, prx, dg = NULL, ...) {
	if (return.asp) return(1)

	per_page = rep(o$ncols * o$nrows, o$npages)
	k = o$ncols * o$nrows * o$npages
	if (o$n < k) {
		per_page[o$npages] = per_page[o$npages] - (k - o$n)
	}


	mapboxs = lapply(per_page, function(p) {
		lapply(seq_len(p), function(i) {
			if (!is.null(dg)) dg else mapgl::mapboxgl() #dummy
			#mapbox::mapbox(zoom = 2, latitude = 0, longitude = 0, pitch = o$pitch)

		})
	})

	.TMAP_MAPBOX$mapboxs = mapboxs
	.TMAP_MAPBOX$nrow = o$nrows
	.TMAP_MAPBOX$ncol = o$ncols
	.TMAP_MAPBOX$leg_id = 1
	NULL
}

#' @param q q
#' @export
#' @keywords internal
#' @name tmapMapboxAux
#' @rdname tmapMapbox
tmapMapboxAux = function(o, q) {
	NULL
}
