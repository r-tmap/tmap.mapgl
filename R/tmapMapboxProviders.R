#' @param credits credits
#' @export
#' @keywords internal
#' @name tmapMapboxProviders
#' @rdname tmapMapbox
tmapMapboxProviders = function(credits) {
	p = c("standard", "streets", "outdoors", "light", "dark", "satellite", "satellite-streets", "navigation-day", "navigation-night", "standard-satellite")
	structure(as.list(p), names = p)
}
