#' Export tmap to mapbox and maplibre
#'
#' * `tmap_mapbox()` returns a [`mapgl`][mapgl::mapboxgl()] object (`"mapbox" mode`)
#' * `tmap_maplibre()` a [`mapgl`][mapgl::maplibregl()] object (`"maplibre"` mode).
#'
#' @param x a tmap object.
#' @param asp,scale the desired aspect ratio and scale of the map. Only applicable for `"plot"` mode.
#' @param show show the map?
#' @inheritDotParams print.tmap
#' @return
#' * `tmap_grob()` returns a [`grob`][grid::grob()] object (`"plot"` mode)
#' * `tmap_leaflet()` a [`leaflet`][leaflet::leaflet()] object (`"view"` mode).
#'   In case small multiples are shown, a list is returned.
#' @export
#' @examples
#' map = tm_shape(World) + tm_polygons()
#' tmap_mapbox(map, show = TRUE)
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
