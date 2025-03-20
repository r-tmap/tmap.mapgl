#' Internal tmap methods
#' 
#' Internal tmap methods 
#'
#' @param comp the shape object
#' @param o the list of options
#' @param mapbox mapbox object
#' @export
#' @keywords internal
#' @name tmapMapboxCompPrepare
#' @rdname tmapMapbox
tmapMapboxCompPrepare = function(comp, o) {
	UseMethod("tmapMapboxCompPrepare")
}

#' @export
#' @keywords internal
#' @name tmapMapboxCompHeight
#' @rdname tmapMapbox
tmapMapboxCompHeight = function(comp, o) {
	UseMethod("tmapMapboxCompHeight")
}

#' @export
#' @keywords internal
#' @name tmapMapboxCompWidth
#' @rdname tmapMapbox
tmapMapboxCompWidth = function(comp, o) {
	UseMethod("tmapMapboxCompWidth")
}

#' @export
#' @keywords internal
#' @name tmapMapboxLegPlot
#' @rdname tmapMapbox
tmapMapboxLegPlot = function(comp, mapbox, o) {
	UseMethod("tmapMapboxLegPlot")
}


#' @method tmapMapboxCompPrepare tm_chart
#' @export
tmapMapboxCompPrepare.tm_chart = function(comp, o) {
	message("charts not implemented in view mode")
	comp
}

#' @method tmapMapboxCompPrepare tm_chart_none
#' @export
tmapMapboxCompPrepare.tm_chart_none = function(comp, o) {
	comp
}


#' @method tmapMapboxCompWidth tm_chart
#' @export
tmapMapboxCompWidth.tm_chart = function(comp, o) {
	comp
}

#' @method tmapMapboxCompHeight tm_chart
#' @export
tmapMapboxCompHeight.tm_chart = function(comp, o) {
	comp
}

#' @method tmapMapboxLegPlot tm_chart_histogram
#' @export
tmapMapboxLegPlot.tm_chart_histogram = function(comp, mapbox, o) {
	mapbox
}
#' @method tmapMapboxLegPlot tm_chart
#' @export
tmapMapboxLegPlot.tm_chart = function(comp, mapbox, o) {
	mapbox
}
