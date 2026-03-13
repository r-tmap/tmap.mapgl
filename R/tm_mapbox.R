#' Mapbox mode options
#'
#' Mapbox mode options. These options are specific to the mapbox mode.
#'
#' @param pitch The pitch angle
#' @param control.position The position of the layer control box
#' @param control.collapse Should the layer control box be collapsed?
#' @param zoom The zoom level of the map
#' @example examples/mapbox.R
#' @return a [tmap::tmap-element]
#' @export
tm_mapbox = function(pitch, control.position, control.collapse, zoom) {
	args = lapply(as.list(rlang::call_match(dots_expand = TRUE)[-1]), eval, envir = parent.frame())
	args$called_from = "tm_mapbox"
	do.call(tmap::tm_options, args)
}

#' Maplibre mode options
#'
#' Maplibre mode options. These options are specific to the maplibre mode.
#'
#' @param pitch The pitch angle
#' @param control.position The position of the layer control box
#' @param control.collapse Should the layer control box be collapsed?
#' @param zoom The zoom level of the map
#' @example examples/maplibre.R
#' @return a [tmap::tmap-element]
#' @export
tm_maplibre = function(pitch, control.position, control.collapse, zoom) {
	args = lapply(as.list(rlang::call_match(dots_expand = TRUE)[-1]), eval, envir = parent.frame())
	args$called_from = "tm_maplibre"
	do.call(tmap::tm_options, args)
}
