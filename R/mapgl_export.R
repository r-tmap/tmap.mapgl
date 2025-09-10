#' Export tmap to mapbox and maplibre
#'
#' * `tmap_mapbox()` returns a [`mapgl`][mapgl::mapboxgl()] object (`"mapbox" mode`)
#' * `tmap_maplibre()` a [`mapgl`][mapgl::maplibregl()] object (`"maplibre"` mode).
#'
#' @param x a tmap object.
#' @param show show the map?
#' @param ... passed on to [`tmap`][tmap::print.tmap()]
#' @return  a [`mapgl`][mapgl::mapboxgl()] object (`"mapbox" mode`) or a [`mapgl`][mapgl::maplibregl()] object (`"maplibre"` mode). In case small multiples are shown, a list is returned.
#' @export
#' @examples
#' library(tmap)
#' library(tmap.mapgl)
#' map = tm_shape(World) + tm_polygons()
#' tmap_maplibre(map, show = TRUE)
tmap_mapbox = function(x,
						show = FALSE,
						...) {
	current_mode = getOption("tmap.mode")
	on.exit({
		options(tmap.mode = current_mode)
	})
	options(tmap.mode = "mapbox")
	print.tmap(x, show = show, ...)
}

#' @rdname tmap_mapbox
#' @export
tmap_maplibre = function(x,
						 show = FALSE,
						 ...) {
	current_mode = getOption("tmap.mode")
	on.exit({
		options(tmap.mode = current_mode)
	})
	options(tmap.mode = "maplibre")
	print.tmap(x, show = show, ...)
}
