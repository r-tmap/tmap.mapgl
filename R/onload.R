.onLoad = function(...) {
	requireNamespace("tmap", quietly = TRUE)
	requireNamespace("mapgl", quietly = TRUE)
	requireNamespace("data.table", quietly = TRUE)

	tmap::tmapSubmitOptions(
		options = list(
			value.const = list(height = 0),
			value.na = list(height = NA),
			value.blank = list(height = 0),
			values.var = list(height = c(0, 1)),
			values.range = list(height = c(0, 1)),
			scales.var =  list(height = list(fact = "categorical", num = "continuous", datetime = "continuous", date = "continuous")),
			modes =
			list(mapbox =
				list(name = "Mapbox",
					 pitch = 0,
					   basemap.show = TRUE,
					   scalebar.position = tmap::tm_pos_in("left", "bottom"),
					   minimap.position = tmap::tm_pos_in("left", "top"),
					   geocoder.position = tmap::tm_pos_in(pos.h = "left", pos.v = "top"),
					   basemap.server = "mapbox.standard",
					   crs_basemap = 4326),
				maplibre =
				list(name = "Maplibre",
					 pitch = 0,
					 basemap.show = TRUE,
					 scalebar.position = tmap::tm_pos_in("left", "bottom"),
					 minimap.position = tmap::tm_pos_in("left", "top"),
					 geocoder.position = tmap::tm_pos_in(pos.h = "left", pos.v = "top"),
					 basemap.server = "ofm.liberty",
					 crs_basemap = 4326))),
		styleOptions = list(cobalt = list(modes =
										  	list(mapbox = list(basemap.server = "ofm.dark"),
										  		 maplibre = list(basemap.server = "ofm.dark")))))
	.TMAP_MAPBOX$crs_options = c("eqearth" = "equalEarth", "wintri" = "winkelTripel", "aea" = "albers", "3857" = "mercator", "lcc" =  "lambertConformalConic")
	.TMAP_MAPLIBRE$crs_options = c("3857" = "mercator")
}

.TMAP_MAPBOX = new.env(FALSE, parent = globalenv())
.TMAP_MAPLIBRE = new.env(FALSE, parent = globalenv())
