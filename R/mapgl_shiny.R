#' @export
#' @keywords internal
#' @rdname tmapMapbox
renderTmapMapbox = function(expr, env, quoted, execOnResize) {
	mapgl::renderMapboxgl(expr, env, quoted)
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapOutputMapbox = function(outputId, width, height) {
	mapgl::mapboxglOutput(outputId, width = width, height = height)
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapProxyMapbox = function(mapId, session, x) {
	print(x, dg = mapgl::mapboxgl_proxy(mapId, session), show = FALSE, in.shiny = TRUE, proxy = TRUE)
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
renderTmapMaplibre = function(expr, env, quoted, execOnResize) {
	mapgl::renderMaplibre(expr, env, quoted)
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapOutputMaplibre = function(outputId, width, height) {
	mapgl::maplibreOutput(outputId, width = width, height = height)
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapProxyMaplibre = function(mapId, session, x) {
	print(x, dg = mapgl::maplibre_proxy(mapId, session), show = FALSE, in.shiny = TRUE, proxy = TRUE)
}
