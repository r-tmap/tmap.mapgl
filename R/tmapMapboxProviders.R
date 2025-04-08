#' @param credits credits
#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxProviders = function(credits) {
	p = c("standard", "streets", "outdoors", "light", "dark", "satellite", "satellite-streets", "navigation-day", "navigation-night", "standard-satellite")
	structure(as.list(p), names = p)
}

#' @param credits credits
#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreProviders = function(credits) {
	p = c("voyager", "positron", "dark-matter")
	structure(as.list(p), names = p)
}
