#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxDataPlot = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	UseMethod("tmapMapboxDataPlot")
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxDataPlot.default = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	NULL
}


#' @param shpTM, dt, pdt, popup.format, hdt, idt, gp args
#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxDataPlot.tm_data_polygons = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	mapgl_polygons(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ..., mode = "mapbox")
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxDataPlot.tm_data_polygons_3d = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	mapgl_polygons_3d(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ..., mode = "mapbox")
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxDataPlot.tm_data_lines = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	mapgl_lines(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ..., mode = "mapbox")
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxDataPlot.tm_data_symbols = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	mapgl_symbols(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ..., mode = "mapbox")
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxDataPlot.tm_data_raster = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	mapgl_raster(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ..., mode = "mapbox")
}



#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxDataPlot.tm_data_fill = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	NextMethod()
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxDataPlot.tm_data_borders = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	NextMethod()
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxDataPlot.tm_data_dots = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	NextMethod()
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxDataPlot.tm_data_bubbles = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	NextMethod()
}

