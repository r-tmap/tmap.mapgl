#' Mapbox mode options
#'
#' Mapbox mode options. These options are specific to the mapbox mode.
#'
#' @param pitch The pitch angle
#' @param bearing The bearing (rotation angle) of the map, in degrees
#' @param control.position The position of the layer control box
#' @param control.collapse Should the layer control box be collapsed?
#' @param zoom The zoom level of the map
#' @example examples/mapbox.R
#' @return a [tmap::tmap-element]
#' @export
tm_mapbox = function(pitch, bearing, control.position, control.collapse, zoom) {
	args = lapply(as.list(rlang::call_match(dots_expand = TRUE)[-1]), eval, envir = parent.frame())
	args$called_from = "tm_mapbox"
	do.call(tmap::tm_options, args)
}

#' Maplibre mode options
#'
#' Maplibre mode options. These options are specific to the maplibre mode.
#'
#' @param pitch The pitch angle
#' @param bearing The bearing (rotation angle) of the map, in degrees
#' @param control.position The position of the layer control box
#' @param control.collapse Should the layer control box be collapsed?
#' @param zoom The zoom level of the map
#' @param pattern.zoom How fill patterns behave when zooming: `"screen"` (default) keeps them at a fixed size on
#'   the screen, like the other patterns of tmap; `"map"` attaches them to the map, so that they are enlarged when
#'   zooming in (as if they were painted on the map). When zoomed in far, the
#'   patterns become blurry, since the pattern images are magnified.
#' @example examples/maplibre.R
#' @return a [tmap::tmap-element]
#' @export
tm_maplibre = function(pitch, bearing, control.position, control.collapse, zoom, pattern.zoom) {
	args = lapply(as.list(rlang::call_match(dots_expand = TRUE)[-1]), eval, envir = parent.frame())
	args$called_from = "tm_maplibre"
	do.call(tmap::tm_options, args)
}
