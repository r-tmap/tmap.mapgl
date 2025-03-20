#' @export
renderTmapMapbox = function(expr, env, quoted, execOnResize) {
	mapgl::renderMapboxgl(expr, env, quoted)
}

#' @export
tmapOutputMapbox = function(outputId, width, height) {
	mapgl::mapboxglOutput(outputId, width = width, height = height)
}

#' @export
tmapProxyMapbox = function(mapId, session, x) {
	print.tmap(x, dg = mapbox::mapbox_proxy(mapId, session), show = FALSE, in.shiny = TRUE, proxy = TRUE)
}
