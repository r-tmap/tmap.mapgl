
#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompPrepare.tm_title = function(comp, o) {
	comp$show = TRUE
	comp
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompHeight.tm_title = function(comp, o) {
	comp
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompWidth.tm_title = function(comp, o) {
	comp
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompPlot.tm_title = function(comp, m, o) {
	m
}


############ scalebar


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompPrepare.tm_scalebar = function(comp, o) {
	mapgl_comp_prepare(comp, o)
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompHeight.tm_scalebar = function(comp, o) {
	comp
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompWidth.tm_scalebar = function(comp, o) {
	comp
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompPlot.tm_scalebar = function(comp, m, o) {
	mapgl_scalebar_plot(comp, m, o)

}

############ credits



#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompPrepare.tm_snow = function(comp, o) {
	comp$show = TRUE
	comp
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompHeight.tm_snow = function(comp, o) {
	comp
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompWidth.tm_snow = function(comp, o) {
	comp
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompPlot.tm_snow = function(comp, m, o) {
	m |> mapgl::set_snow()
}




#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompPrepare.tm_title = function(comp, o) {
	comp$show = TRUE
	comp
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompHeight.tm_title = function(comp, o) {
	comp
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompWidth.tm_title = function(comp, o) {
	comp
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompPlot.tm_title = function(comp, m, o) {
	m
}




#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompPrepare.tm_scalebar = function(comp, o) {
	mapgl_comp_prepare(comp, o)
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompHeight.tm_scalebar = function(comp, o) {
	comp
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompWidth.tm_scalebar = function(comp, o) {
	comp
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompPlot.tm_scalebar = function(comp, m, o) {
	mapgl_scalebar_plot(comp, m, o)
}


mapgl_scalebar_plot = function(comp, m, o) {
	m |> mapgl::add_scale_control(position = comp$legpos)
}





#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompPrepare.tm_credits = function(comp, o) {
	comp$show = TRUE
	comp
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompHeight.tm_credits = function(comp, o) {
	comp
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompWidth.tm_credits = function(comp, o) {
	comp
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompPlot.tm_credits = function(comp, m, o) {
	m
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompPrepare.tm_mouse_coordinates = function(comp, o) {
	comp$show = TRUE
	comp
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompHeight.tm_mouse_coordinates = function(comp, o) {
	comp
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompWidth.tm_mouse_coordinates = function(comp, o) {
	comp
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompPlot.tm_mouse_coordinates = function(comp, m, o) {
	m
}








########## minimap

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompPrepare.tm_minimap = function(comp, o) {
	mapgl_comp_prepare(comp, o)
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompPrepare.tm_minimap = function(comp, o) {
	mapgl_comp_prepare(comp, o)
}




#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompHeight.tm_minimap = function(comp, o) {
	comp
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompWidth.tm_minimap = function(comp, o) {
	comp
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompHeight.tm_minimap = function(comp, o) {
	comp
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompWidth.tm_minimap = function(comp, o) {
	comp
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompPlot.tm_minimap = function(comp, m, o) {
	mapgl_minimap_plot(comp, m, o)
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompPlot.tm_minimap = function(comp, m, o) {
	mapgl_minimap_plot(comp, m, o)
}


mapgl_comp_prepare = function(comp, o) {
	comp$show = TRUE
	comp$legpos = mapgl_pos(comp$position)
	comp
}

mapgl_minimap_plot = function(comp, m, o) {
	m |> mapgl::add_globe_minimap(position = comp$legpos)
}
