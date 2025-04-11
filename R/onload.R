.onLoad = function(...) {
	requireNamespace("tmap", quietly = TRUE)
	requireNamespace("mapgl", quietly = TRUE)
	requireNamespace("data.table", quietly = TRUE)
	tmap::tmapMode("mapbox", "Mapbox",
				   pitch = 0,
				   basemap.show = TRUE,
				   scalebar.position = tm_pos_in("left", "bottom"),
				   minimap.position = tm_pos_in("left", "top"),
				   basemap.server = "standard",
				   rs_basemap = 4326)
	tmap::tmapMode("maplibre", "Maplibre",
				   pitch = 0,
				   basemap.show = TRUE,
				   scalebar.position = tm_pos_in("left", "bottom"),
				   minimap.position = tm_pos_in("left", "top"),
				   basemap.server = "voyager",
				   crs_basemap = 4326)
}

.TMAP_MAPBOX = new.env(FALSE, parent = globalenv())
.TMAP_MAPLIBRE = new.env(FALSE, parent = globalenv())
