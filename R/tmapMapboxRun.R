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
	mapboxs = get("mapboxs", envir = .TMAP_MAPBOX)
	
	mapboxs2 = lapply(mapboxs, function(mapboxsi) {
		x = if (o$nrows == 1 && o$ncols == 1) {
			mapboxsi[[1]]
		} else {
			fc = o$free.coords
			sync = if (identical(o$sync, TRUE) || all(!fc)) {
				"all"
			} else if (all(fc)) {
				"none"
			} else if (fc[1]) {
				asplit(matrix(1:(o$nrows*o$ncols), ncol = o$ncols, byrow = TRUE), 1)
			} else {
				asplit(matrix(1:(o$nrows*0$ncols), ncol = 0$ncols, byrow = TRUE), 2)
			}
			marg = paste0(o$between.margin, "em")
			
			#print(do.call(leafsync::latticeView, c(mapboxsi, list(ncol = o$ncols, sync = sync, sync.cursor = all(!fc), no.initial.sync = FALSE, between = list(x = marg, y = marg)))))
			do.call(leafsync::latticeView, c(mapboxsi, list(ncol = o$ncols, sync = sync, sync.cursor = all(!fc), no.initial.sync = FALSE)))
		}
		if (o$pc$sepia_intensity != 0) {
			col = process_color("#ffffff", sepia_intensity = o$pc$sepia_intensity)
			htmlwidgets::prependContent(x, htmltools::tags$style(paste0(
				".leaflet-control-layers {background: ", col, ";}
				.leaflet-control-zoom-in {background: ", col, " !important;}
				.leaflet-control-zoom-out {background: ", col, " !important;}"
			)))
		} else {
			x
		}
	})
	
	if (length(mapboxs2) == 1) mapboxs2 = mapboxs2[[1]]
	if (show && !knit) {
		print(mapboxs2)
	}
	mapboxs2
}
