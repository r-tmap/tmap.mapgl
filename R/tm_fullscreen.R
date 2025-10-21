#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompPrepare.tm_fullscreen = function(comp, o) {
	mapgl_comp_prepare(comp, o)
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompHeight.tm_fullscreen = function(comp, o) {
	comp
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompWidth.tm_fullscreen = function(comp, o) {
	comp
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompPlot.tm_fullscreen = function(comp, m, o) {
	m |> mapgl::add_fullscreen_control(position = comp$legpos)
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompPrepare.tm_fullscreen = function(comp, o) {
	mapgl_comp_prepare(comp, o)
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompHeight.tm_fullscreen = function(comp, o) {
	comp
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompWidth.tm_fullscreen = function(comp, o) {
	comp
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompPlot.tm_fullscreen = function(comp, m, o) {
	m |> mapgl::add_fullscreen_control(position = comp$legpos)
}

#' Map component: fullscreen button
#'
#' Map component that adds a fullscreen button
#'
#' @param stack stack with other map components, either `"vertical"` or `"horizontal"`.
#' @param position The position specification of the component: an object created with `tm_pos_in()` or `tm_pos_out()`. Or, as a shortcut, a vector of two values, specifying the x and y coordinates. The first is `"left"`, `"center"` or `"right"` (or upper case, meaning tighter to the map frame), the second `"top"`, `"center"` or `"bottom"`. Numeric values are also supported, where 0, 0 means left bottom and 1, 1 right top. See also \href{https://r-tmap.github.io/tmap/articles/adv_positions}{vignette about positioning}. In case multiple components should be combined (stacked), use `group_id` and specify `component` in [tm_comp_group()].
#' @param group_id Component group id name. All components (e.g. legends, titles, etc) with the same `group_id` will be grouped. The specifications of how they are placed (e.g. stacking, margins etc.) are determined in [tm_comp_group()] where its argument `id` should correspond to `group_id`.
#' @param z z index, e.g. the place of the component relative to the other componets
#' @seealso \href{https://r-tmap.github.io/tmap/articles/basics_components}{Vignette about components}
#' @return a [tmap::tmap-element]
#' @export
tm_fullscreen = function (stack, position, group_id, z) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args$called = names(args)
	args$group_id = args$group_id %||% NA_character_
	args$z = args$z %||% NA_integer_
	tmap::tm_element_list(do.call(tmap::tm_element, c(args, list(subclass = c("tm_fullscreen",
																			  "tm_component")))))
}
