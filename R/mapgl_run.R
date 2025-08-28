#' @param show show
#' @param knit knit
#' @param knit_opts knit options
#' @param args args
#' @export
#' @keywords internal
#' @name tmapMapboxRun
#' @importFrom grDevices col2rgb rgb
#' @importFrom colorspace deutan protan tritan
#' @importFrom htmltools tags
#' @importFrom htmlwidgets prependContent
#' @import sf
#' @importFrom tmaptools bb_poly
#' @importFrom terra rast is.lonlat project ext crop
#' @importFrom stats na.omit
#' @importFrom utils head tail
#' @import stars
#' @import cli
#' @importFrom units drop_units set_units
#' @rdname tmapMapbox
tmapMapboxRun = function(o, q, show, knit, knit_opts, args) {
	mapgl_run(o, q, show, knit, args, mode = "mapbox")
}


#' @export
#' @rdname tmapMapbox
tmapMaplibreRun = function(o, q, show, knit, knit_opts, args) {
	mapgl_run(o, q, show, knit, args, mode = "maplibre")

}

mapgl_run = function(o, q, show, knit, args, mode) {
	e = if (mode == "mapbox") {
		.TMAP_MAPBOX
	} else {
		.TMAP_MAPLIBRE
	}

	if (show && o$show_gif_ani) {
		cli::cli_alert("{.field {mode} mode} Animations are not implemented in {mode} mode, so they are shown as facets")
	}


	ms = get("ms", envir = e)

	ms2 = lapply(ms, function(msi) {
		x = if (o$nrows == 1 && o$ncols == 1) {
			msi[[1]]
				#mapgl::add_layers_control() # in development #3
		} else {
			if (length(msi) > 2) cli::cli_warn("more than 2 facets not supported for the mode {.str mapbox}")
			orientation = ifelse(o$ncols >= o$nrows, "vertical", "horizontal")

			mapgl::compare(msi[[1]], msi[[2]], mode = "sync", orientation = orientation)
		}
		x
	})

	if (length(ms2) == 1) ms2 = ms2[[1]]
	if (show && !knit) {
		print(ms2)
	}
	ms2
}
