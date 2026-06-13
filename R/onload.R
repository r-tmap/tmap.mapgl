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
					 bearing = 0,
					   basemap.show = TRUE,
					 control.position = c("left", "top"),
					 control.collapse = TRUE,
					 zoom = NA,
					   scalebar.position = tmap::tm_pos_in("left", "bottom"),
					   minimap.position = tmap::tm_pos_in("left", "bottom"),
					   geocoder.position = tmap::tm_pos_in(pos.h = "right", pos.v = "top"),
					   geocoder.placeholder = "Search",
					   draw.download_button = TRUE,
					   draw.show_measurements = TRUE,
					   fullscreen.position =  tmap::tm_pos_in(pos.h = "right", pos.v = "top"),
					   basemap.server = "ofm.positron",
					   component.frame.color = "gray40",
					   crs_basemap = 4326),
				maplibre =
				list(name = "Maplibre",
					 pitch = 0,
					 bearing = 0,
					 basemap.show = TRUE,
					 control.position = c("left", "top"),
					 control.collapse = TRUE,
					 zoom = NA,
					 scalebar.position = tmap::tm_pos_in("left", "bottom"),
					 minimap.position = tmap::tm_pos_in("left", "bottom"),
					 geocoder.position = tmap::tm_pos_in(pos.h = "right", pos.v = "top"),
					 geocoder.placeholder = "Search",
					 draw.download_button = TRUE,
					 draw.show_measurements = TRUE,
					 fullscreen.position =  tmap::tm_pos_in(pos.h = "right", pos.v = "top"),
					 basemap.server = "ofm.positron",
					 component.frame.color = "gray40",
					 crs_basemap = 4326))),
		styleOptions = list(cobalt = list(modes =
										  	list(mapbox = list(basemap.server = "ofm.dark"),
										  		 maplibre = list(basemap.server = "ofm.dark")))))

	# Cross-mode basemap equivalences: when a basemap valid in one mode is
	# reproduced in another mode that lacks it, the closest provider below is
	# substituted instead of dropping to the mode default. Mappings are
	# approximate; the first provider listed per mode is the representative used
	# when substituting into that mode.
	tmap::tmapSubmitBasemapEquivalents(list(
		gray = list(
			plot     = "Esri.WorldGrayCanvas",
			view     = "Esri.WorldGrayCanvas",
			maplibre = "ofm.positron",
			mapbox   = "mapbox.light"),
		positron = list(
			plot     = "CartoDB.Positron",
			view     = "CartoDB.Positron",
			maplibre = "carto.positron",
			mapbox   = "carto.positron"),
		dark = list(
			plot     = "CartoDB.DarkMatter",
			view     = "CartoDB.DarkMatter",
			maplibre = c("carto.dark_matter", "ofm.dark"),
			mapbox   = c("mapbox.dark", "carto.dark_matter")),
		voyager = list(
			plot     = "CartoDB.Voyager",
			view     = "CartoDB.Voyager",
			maplibre = "carto.voyager",
			mapbox   = "carto.voyager"),
		streets = list(
			plot     = c("OpenStreetMap", "Esri.WorldStreetMap"),
			view     = c("OpenStreetMap", "Esri.WorldStreetMap"),
			maplibre = c("ofm.liberty", "esri.streets"),
			mapbox   = "mapbox.streets"),
		bright = list(
			plot     = "OpenStreetMap",
			view     = "OpenStreetMap",
			maplibre = "ofm.bright",
			mapbox   = "mapbox.standard"),
		topo = list(
			plot     = "Esri.WorldTopoMap",
			view     = "Esri.WorldTopoMap",
			maplibre = c("esri.topographic", "maptiler.topo"),
			mapbox   = "mapbox.outdoors"),
		satellite = list(
			plot     = "Esri.WorldImagery",
			view     = "Esri.WorldImagery",
			maplibre = c("esri.imagery", "maptiler.satellite"),
			mapbox   = c("mapbox.satellite", "mapbox.satellite_streets"))
	))

	.TMAP_MAPBOX$crs_options = c("eqearth" = "equalEarth", "wintri" = "winkelTripel", "aea" = "albers", "3857" = "mercator", "lcc" =  "lambertConformalConic")
	.TMAP_MAPLIBRE$crs_options = c("3857" = "mercator")
}

.TMAP_MAPBOX = new.env(FALSE, parent = globalenv())
.TMAP_MAPLIBRE = new.env(FALSE, parent = globalenv())
