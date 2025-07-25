#' Map layer: let it snow!
#'
#' Map layer that generates snow.
#'
#' Arguments and their default values have been taken from [mapbox::set_snow()] (package version 0.2.2)
#'
#' @param density A number between 0 and 1 controlling the snow particles density. Default is 0.85.
#' @param intensity A number between 0 and 1 controlling the snow particles movement speed. Default is 1.0.
#' @param color A string specifying the color of the snow particles. Default is "#ffffff".
#' @param opacity A number between 0 and 1 controlling the snow particles opacity. Default is 1.0.
#' @param center_thinning A number between 0 and 1 controlling the thinning factor of snow particles from center. Default is 0.4.
#' @param direction A numeric vector of length 2 defining the azimuth and polar angles of the snow direction. Default is c(0, 50).
#' @param flake_size A number between 0 and 5 controlling the snow flake particle size. Default is 0.71.
#' @param vignette A number between 0 and 1 controlling the snow vignette screen-space effect. Default is 0.3.
#' @param vignette_color A string specifying the snow vignette screen-space corners tint color. Default is "#ffffff".
tm_snow = function(density = 0.85, intensity = 1.0, color = "#ffffff",
				   opacity = 1.0, center_thinning = 0.4, direction = c(0, 50),
				   flake_size = 0.71, vignette = 0.3, vignette_color = "#ffffff") {
	tmap::tm_element_list(tmap::tm_element(
		args = list(density = density, intensity = intensity, color = color,
					opacity = opacity, center_thinning = center_thinning, direction = direction,
					flake_size = flake_size, vignette = vignette, vignette_color = vignette_color),
		mapping.fun = "tm_aux_snow",
		zindex = 0,
		group = "",
		group.control = "none",
		subclass = c("tm_snow", "tm_aux_layer")))
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxAuxPrepare.tm_aux_snow = function(a, bs, id, o) {
	""
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxAuxPlot.tm_aux_snow = function(a, bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	get_mapgl(facet_row, facet_col, facet_page, mode = "mapbox") |>
		(\(map) do.call(mapgl::set_snow, c(list(map = map), unclass(a))))() |>
		assign_mapgl(facet_row, facet_col, facet_page, mode = "mapbox")
	NULL
}

#' Map layer: let it rain!
#'
#' Map layer that generates rain. Only available in "mapbox" mode.
#'
#' Arguments and their default values have been taken from [mapbox::set_rain()] (package version 0.2.2)
#'
#' @param density A number between 0 and 1 controlling the rain particles density. Default is 0.5.
#' @param intensity A number between 0 and 1 controlling the rain particles movement speed. Default is 1.
#' @param color A string specifying the color of the rain droplets. Default is "#a8adbc".
#' @param opacity A number between 0 and 1 controlling the rain particles opacity. Default is 0.7.
#' @param center_thinning A number between 0 and 1 controlling the thinning factor of rain particles from center. Default is 0.57.
#' @param direction A numeric vector of length 2 defining the azimuth and polar angles of the rain direction. Default is c(0, 80).
#' @param droplet_size A numeric vector of length 2 controlling the rain droplet size (x - normal to direction, y - along direction). Default is c(2.6, 18.2).
#' @param distortion_strength A number between 0 and 1 controlling the rain particles screen-space distortion strength. Default is 0.7.
#' @param vignette A number between 0 and 1 controlling the screen-space vignette rain tinting effect intensity. Default is 1.0.
#' @param vignette_color A string specifying the rain vignette screen-space corners tint color. Default is "#464646".
tm_rain = function(density = 0.5, intensity = 1.0, color = "#a8adbc",
				   opacity = 0.7, center_thinning = 0.57, direction = c(0, 80),
				   droplet_size = c(2.6, 18.2), distortion_strength = 0.7,
				   vignette = 1.0, vignette_color = "#464646") {
	tmap::tm_element_list(tmap::tm_element(
		args = list(density = density, intensity = intensity, color = color,
					opacity = opacity, center_thinning = center_thinning, direction = direction,
					droplet_size = droplet_size, distortion_strength = distortion_strength,
					vignette = vignette, vignette_color = vignette_color),
		mapping.fun = "tm_aux_rain",
		zindex = 0,
		group = "",
		group.control = "none",
		subclass = c("tm_rain", "tm_aux_layer")))
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxAuxPrepare.tm_aux_rain = function(a, bs, id, o) {
	""
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxAuxPlot.tm_aux_rain = function(a, bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	get_mapgl(facet_row, facet_col, facet_page, mode = "mapbox") |>
		(\(map) do.call(mapgl::set_rain, c(list(map = map), unclass(a))))() |>
		assign_mapgl(facet_row, facet_col, facet_page, mode = "mapbox")
	NULL
}
