#' Internal tmap methods
#'
#' Internal tmap methods
#'
#' @param comp the shape object
#' @param o the list of options
#' @param m mapbox oer maplibre object
#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompPrepare = function(comp, o) {
	UseMethod("tmapMapboxCompPrepare")
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompHeight = function(comp, o) {
	UseMethod("tmapMapboxCompHeight")
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompWidth = function(comp, o) {
	UseMethod("tmapMapboxCompWidth")
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompPlot = function(comp, m, o) {
	UseMethod("tmapMapboxCompPlot")
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompPrepare.default = function(comp, o) {
	cls = class(comp)[1]
	id = paste("mapbox_mode", cls, sep = "_")
	cli::cli_inform("{.field [mapbox mode]} Map component {.fun {cls}} not supported in {.str mapbox} mode.",
					.frequency_id = id,
					.frequency = "once")
	comp$show = FALSE
	comp
}






#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompPrepare = function(comp, o) {
	UseMethod("tmapMaplibreCompPrepare")
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompHeight = function(comp, o) {
	UseMethod("tmapMaplibreCompHeight")
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompWidth = function(comp, o) {
	UseMethod("tmapMaplibreCompWidth")
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompPlot = function(comp, m, o) {
	UseMethod("tmapMaplibreCompPlot")
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompPrepare.default = function(comp, o) {
	cls = class(comp)[1]
	id = paste("maplibre_mode", cls, sep = "_")
	cli::cli_inform("{.field [maplibre mode]} Map component {.fun {cls}} not supported in {.str maplibre} mode.",
					.frequency_id = id,
					.frequency = "once")
	comp$show = FALSE
	comp
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompPrepare.tm_chart_none = function(comp, o) {
	comp$show = FALSE
	comp
}

