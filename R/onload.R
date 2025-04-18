.onLoad = function(...) {
	requireNamespace("tmap", quietly = TRUE)
	requireNamespace("mapgl", quietly = TRUE)
	requireNamespace("data.table", quietly = TRUE)

	opts = list(value.const = 0,
				value.na = 0,
				value.blank = 0,
				values.var = c(0, 1),
				values.range = c(0, 1),
				scales.var =  list(fact = "categorical", num = "continuous", datetime = "continuous", date = "continuous"))
	mapply(tmap::tmapAddLayerOptions, names(opts), rep("height", length(opts)), opts, SIMPLIFY = FALSE)


	tmap::tmapMode("mapbox", "Mapbox",
				   pitch = 0,
				   basemap.show = TRUE,
				   scalebar.position = tmap::tm_pos_in("left", "bottom"),
				   minimap.position = tmap::tm_pos_in("left", "top"),
				   basemap.server = "standard",
				   crs_basemap = 4326)
	tmap::tmapMode("maplibre", "Maplibre",
				   pitch = 0,
				   basemap.show = TRUE,
				   scalebar.position = tmap::tm_pos_in("left", "bottom"),
				   minimap.position = tmap::tm_pos_in("left", "top"),
				   basemap.server = "voyager",
				   crs_basemap = 4326)
}

.TMAP_MAPBOX = new.env(FALSE, parent = globalenv())
.TMAP_MAPLIBRE = new.env(FALSE, parent = globalenv())
