#' @param tms tmap objects
#' @param nx number of facets
#' @param ncol, nrow number of rows and columns
#' @param opts options
#' @param knit knit
#' @param show show
#' @param args args
#' @param options options
#' @export
#' @keywords internal
#' @name tmapMapboxArrange
#' @rdname tmapMapbox
tmapMapboxArrange = function(tms, nx, ncol, nrow, opts, knit, show, args, options) {
	res = lapply(tms, function(tm) {
		print(tm, show = FALSE)
	})
	res2 = do.call(leafsync::latticeView, c(res, list(ncol=ncol, sync=ifelse(identical(opts$sync, TRUE), "all", "none"), no.initial.sync = FALSE)))
	
	if (show) {
		if (knit) {
			kp <- get("knit_print", asNamespace("knitr"))
			return(do.call(kp, c(list(x=res2), args, list(options=options))))
		} else {
			return(print(res2))
		}
	} else res2
}
