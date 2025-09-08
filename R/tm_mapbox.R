#' Mapbox mode options
#'
#' Mapbox mode options. These options are specific to the mapbox mode.
#'
#' @param pitch The pitch angle
#' @example examples/mapbox.R
#' @export
tm_mapbox = function(pitch) {
	args = lapply(as.list(rlang::call_match(dots_expand = TRUE)[-1]), eval, envir = parent.frame())
	args$called_from = "tm_mapbox"
	do.call(tmap::tm_options, args)
}

#' Maplibre mode options
#'
#' Maplibre mode options. These options are specific to the maplibre mode.
#'
#' @param pitch The pitch angle
#' @example examples/maplibre.R
#' @export
tm_maplibre = function(pitch) {
	args = lapply(as.list(rlang::call_match(dots_expand = TRUE)[-1]), eval, envir = parent.frame())
	args$called_from = "tm_maplibre"
	do.call(tmap::tm_options, args)
}
