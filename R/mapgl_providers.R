# ============================================================
#  Basemap style providers
# ============================================================
# Provider names follow "<family>.<style>[.<variant>]" and are resolved to a
# mapgl style URL/spec by resolve_style() in mapgl_shape.R. Families:
#   ofm      -> OpenFreeMap        (no key)
#   carto    -> mapgl::carto_style()      (no key; works on both engines)
#   mapbox   -> mapgl::mapbox_style()     (Mapbox token; mapbox mode only)
#   maptiler -> mapgl::maptiler_style()   (MAPTILER_API_KEY; both engines)
#   esri     -> mapgl::esri_style()       (ARCGIS_API_KEY; both engines)

.mapgl_styles_ofm = c("ofm.positron", "ofm.liberty", "ofm.bright", "ofm.dark", "ofm.fiord")

.mapgl_styles_carto = c("carto.voyager", "carto.positron", "carto.dark_matter",
						"carto.voyager_nolabels", "carto.positron_nolabels",
						"carto.dark_matter_nolabels")

# MapTiler styles are not a fixed enum (any MapTiler style works); this is a
# curated set. Colour variants (.dark/.light/.pastel) are appended as a final
# segment and parsed back into maptiler_style(variant=). Not all styles support
# all variants.
.mapgl_styles_maptiler = c("maptiler.streets", "maptiler.streets.dark", "maptiler.streets.light",
						   "maptiler.basic", "maptiler.basic.dark", "maptiler.basic.light",
						   "maptiler.bright", "maptiler.outdoor", "maptiler.topo",
						   "maptiler.winter", "maptiler.satellite", "maptiler.hybrid",
						   "maptiler.ocean", "maptiler.dataviz", "maptiler.dataviz.dark",
						   "maptiler.dataviz.light", "maptiler.backdrop", "maptiler.backdrop.dark",
						   "maptiler.backdrop.light", "maptiler.landscape", "maptiler.openstreetmap")

# Esri ArcGIS basemap styles (esri_style()).
.mapgl_styles_esri = paste0("esri.",
						c("navigation", "navigation_night", "streets", "streets_night",
						  "streets_relief", "community", "outdoor", "topographic", "terrain",
						  "imagery", "light_gray", "dark_gray", "oceans", "hillshade",
						  "human_geography", "human_geography_dark", "charted_territory",
						  "colored_pencil", "nova", "modern_antique", "midcentury", "newspaper"))

.mapgl_styles_mapbox = c("mapbox.standard", "mapbox.streets", "mapbox.outdoors",
						 "mapbox.light", "mapbox.dark", "mapbox.satellite",
						 "mapbox.satellite_streets", "mapbox.navigation_day",
						 "mapbox.navigation_night", "mapbox.standard_satellite")

#' @param credits credits
#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxProviders = function(credits) {
	p = c(.mapgl_styles_mapbox, .mapgl_styles_ofm, .mapgl_styles_carto,
		  .mapgl_styles_maptiler, .mapgl_styles_esri)
	structure(as.list(p), names = p)
}

#' @param credits credits
#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreProviders = function(credits) {
	p = c(.mapgl_styles_ofm, .mapgl_styles_carto, .mapgl_styles_maptiler,
		  .mapgl_styles_esri)
	structure(as.list(p), names = p)
}
