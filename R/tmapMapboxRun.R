#' @param show show
#' @param knit knit
#' @param args args
#' @export
#' @keywords internal
#' @name tmapMapboxRun
#' @importFrom grDevices col2rgb rgb
#' @importFrom colorspace deutan protan tritan
#' @importFrom htmltools tags
#' @importFrom htmlwidgets prependContent
#' @importFrom leafsync latticeview
#' @import sf
#' @importFrom tmaptools bb_poly
#' @importFrom units drop_units set_units
#' @rdname tmapMapbox
tmapMapboxRun = function(o, q, show, knit, args) {
	mapgl_run(o, q, show, knit, args, mode = "mapbox")
}


#' @export
#' @rdname tmapMapbox
tmapMaplibreRun = function(o, q, show, knit, args) {
	mapgl_run(o, q, show, knit, args, mode = "maplibre")

}

mapgl_run = function(o, q, show, knit, args, mode) {
	e = if (mode == "mapbox") {
		.TMAP_MAPBOX
	} else {
		.TMAP_MAPLIBRE
	}

	ms = get("ms", envir = e)

	ms2 = lapply(ms, function(msi) {
		x = if (o$nrows == 1 && o$ncols == 1) {
			msi[[1]]
		} else {
			msi[[1]]
			cli::cli_warn("facets not yet supported for mapbox")
		}
		x
	})

	if (length(ms2) == 1) ms2 = ms2[[1]]
	if (show && !knit) {
		print(ms2)
	}
	ms2
}
