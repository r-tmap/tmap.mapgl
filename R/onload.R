.onLoad = function(...) {
	requireNamespace("tmap", quietly = TRUE)
	requireNamespace("mapgl", quietly = TRUE)
	requireNamespace("data.table", quietly = TRUE)
	tmap::tmapMode("mapbox", "Mapbox", pitch = 0, basemap.show = TRUE, basemap.server = "standard", crs_basemap = 4326)
}

.TMAP_MAPBOX = new.env(FALSE, parent = globalenv())
