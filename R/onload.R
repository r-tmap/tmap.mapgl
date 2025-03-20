.onLoad = function(...) {
	requireNamespace("tmap", quietly = TRUE)
	requireNamespace("mapgl", quietly = TRUE)
	requireNamespace("data.table", quietly = TRUE)
	tmap::tmapMode("mapbox", "Mapbox", pitch = 60, server = NULL)
}

.TMAP_MAPBOX = new.env(FALSE, parent = globalenv())
