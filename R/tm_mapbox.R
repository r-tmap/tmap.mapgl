#' Mapbox mode options
#'
#' Mapbox mode options. These options are specific to the mapbox mode.
#'
#' @param style To do: migrate with tmap styles and basemaps?
#' @param pitch The pitch angle
#' @example examples/mapbox.R
#' @export
tm_mapbox = function(style, pitch) {
	args = lapply(as.list(rlang::call_match(dots_expand = TRUE)[-1]), eval, envir = parent.frame())
	args$called_from = "tm_mapbox"
	do.call(tmap::tm_options, args)
}

#' Maplibre mode options
#'
#' Maplibre mode options. These options are specific to the maplibre mode.
#'
#' @param style To do: migrate with tmap styles and basemaps?
#' @param pitch The pitch angle
#' @example examples/maplibre.R
#' @export
tm_maplibre = function(style, pitch) {
	args = lapply(as.list(rlang::call_match(dots_expand = TRUE)[-1]), eval, envir = parent.frame())
	args$called_from = "tm_maplibre"
	do.call(tmap::tm_options, args)
}
