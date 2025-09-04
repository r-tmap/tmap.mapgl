#' @param credits credits
#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxProviders = function(credits) {
	p = c("mapbox.standard", "mapbox.streets", "mapbox.outdoors", "mapbox.light", "mapbox.dark", "mapbox.satellite", "mapbox.satellite_streets", "mapbox.navigation_day", "navigation_night", "mapbox.standard_satellite", "ofm.liberty", "ofm.bright", "ofm.positron", "ofm.dark", "ofm.fiord")
	structure(as.list(p), names = p)
}

#' @param credits credits
#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreProviders = function(credits) {
	p = c("ofm.liberty", "ofm.bright", "ofm.positron", "ofm.dark", "ofm.fiord", "carto.voyager", "carto.positron", "carto.dark_matter")
	structure(as.list(p), names = p)
}
