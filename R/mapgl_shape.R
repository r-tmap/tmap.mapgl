#' @param bbx bbx
#' @export
#' @keywords internal
#' @name tmapMapboxShape
#' @rdname tmapMapbox
tmapMapboxShape = function(bbx, facet_row, facet_col, facet_page, o) {
	mapgl_shape(bbx = bbx,
				facet_row = facet_row,
				facet_col = facet_col,
				facet_page = facet_page,
				o = o,
				mode = "mapbox")
}

#' @export
#' @rdname tmapMapbox
tmapMaplibreShape = function(bbx, facet_row, facet_col, facet_page, o) {
	mapgl_shape(bbx = bbx,
				facet_row = facet_row,
				facet_col = facet_col,
				facet_page = facet_page,
				o = o,
				mode = "maplibre")
}

get_style = function(name) {
	x = sub("^[A-Za-z]+[._]([A-Za-z]+(?:[._][A-Za-z]+)*)$", "\\1", name)
	chartr("._", "--", x = x)
}

# Resolve a tmap basemap provider name to a mapgl style URL/spec. Provider names
# are "<family>.<style>[.<variant>]" (e.g. "carto.positron",
# "mapbox.satellite_streets", "maptiler.dataviz.dark", "esri.human_geography",
# "ofm.liberty"). Unknown or prefixless names are returned verbatim, so a raw
# style URL passed by the user still works. esri/maptiler require an API key
# (ARCGIS_API_KEY / MAPTILER_API_KEY); mapbox styles require a Mapbox token and
# are only offered in mapbox mode.
# Resolve a tmap basemap provider name to a mapgl style URL/spec. Provider names
# are "<family>.<style>[.<variant>]" (e.g. "carto.positron",
# "mapbox.satellite_streets", "maptiler.dataviz.dark", "esri.human_geography",
# "ofm.liberty"). Unknown or prefixless names are returned verbatim, so a raw
# style URL passed by the user still works.
#
# esri/maptiler need an API key (ARCGIS_API_KEY / MAPTILER_API_KEY env var, or
# `api_key` passed through from tm_basemap). If the key is missing, we warn and
# fall back to `default` - the caller passes the mode's basemap option
# (o$basemap.server[1]); the literal here is only a last-resort safety net.
#
# `bg` is the layout background colour (o$bg.color). When no basemap is applied
# (e.g. tm_basemap(NULL)) it is used to paint a background layer, so the blank
# canvas matches tm_layout(bg.color = ...), the way view mode colours the
# leaflet container.
resolve_style = function(name, api_key = NULL, default = "ofm.positron", bg = NULL) {
	fallback = function(msg) {
		cli::cli_warn(c("!" = msg,
						"i" = "Falling back to the default basemap {.str {default}}."))
		resolve_style(default, default = default, bg = bg)
	}

	# An empty / NULL style means no basemap was applied (e.g. tm_basemap(NULL)):
	# return a minimal blank GL style so the engine renders no basemap (like view
	# mode), instead of erroring in regexpr() below. If a background colour is
	# available, add a background layer so the canvas honours tm_layout(bg.color).
	if (is.null(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
		layers = list()
		if (!is.null(bg) && length(bg) == 1L && !is.na(bg) && nzchar(bg)) {
			# Normalise to #RRGGBB (handles R colour names like "grey90" and hex
			# with alpha); fall back to the raw value if it can't be parsed.
			bg_css = tryCatch(
				grDevices::rgb(t(grDevices::col2rgb(bg)), maxColorValue = 255),
				error = function(e) bg)
			layers = list(list(id = "background", type = "background",
							   paint = list(`background-color` = bg_css)))
		}
		return(list(version = 8L,
					sources = structure(list(), names = character(0)),
					layers  = layers))
	}

	pos = regexpr("[._]", name)
	if (pos < 1L) return(name)
	family = substr(name, 1L, pos - 1L)
	rest   = substr(name, pos + 1L, nchar(name))
	dash   = function(x) chartr("._", "--", x)

	have_key = function(env) (!is.null(api_key) && nzchar(api_key)) || nzchar(Sys.getenv(env))

	switch(family,
		   ofm      = paste0("https://tiles.openfreemap.org/styles/", dash(rest)),
		   carto    = mapgl::carto_style(dash(rest)),
		   mapbox   = mapgl::mapbox_style(dash(rest)),
		   esri     = if (have_key("ARCGIS_API_KEY")) {
		   	mapgl::esri_style(dash(rest), token = api_key)
		   } else {
		   	fallback("Esri basemap {.str {name}} needs an ArcGIS API key (set env var {.envvar ARCGIS_API_KEY}, or pass {.arg api_key} to {.fn tm_basemap}).")
		   },
		   maptiler = if (!have_key("MAPTILER_API_KEY")) {
		   	fallback("MapTiler basemap {.str {name}} needs a MapTiler API key (set env var {.envvar MAPTILER_API_KEY}, or pass {.arg api_key} to {.fn tm_basemap}).")
		   } else {
		   	parts   = strsplit(rest, "[._]")[[1]]
		   	variant = NULL
		   	if (length(parts) > 1L && parts[length(parts)] %in% c("dark", "light", "pastel")) {
		   		variant = parts[length(parts)]
		   		parts   = parts[-length(parts)]
		   	}
		   	mapgl::maptiler_style(paste(parts, collapse = "-"), variant = variant, api_key = api_key)
		   },
		   name)
}

mapgl_shape = function(bbx, facet_row, facet_col, facet_page, o, mode) {
	# In proxy mode the canvas is the proxy object that mapgl_init() placed in
	# e$ms. Building a fresh base map here would discard the proxy and render a
	# throwaway widget (the one leaking into the RStudio viewer). Skip it.
	if (isTRUE(.TMAP$proxy)) return(NULL)

	m = get_mapgl(facet_row, facet_col, facet_page, mode)
	bbx = sf::st_bbox(sf::st_transform(tmaptools::bb_poly(bbx), crs = 4326))

	ll = unname(c(mean(bbx[c(1,3)]), mean(bbx[c(2,4)])))
	zoom = if (is.na(o$zoom)) findZoom(bbx) else o$zoom


	e = if (mode == "mapbox") {
		.TMAP_MAPBOX
	} else {
		.TMAP_MAPLIBRE
	}

	# set projection
	crs_o = if (!is.na(o$crs)) o$crs else "auto"

	crs_str = if (inherits(crs_o, "crs")) {
		sf::st_crs(crs_o)$input
	} else {
		crs_o
	}

	crs = "globe"
	for (i in 1L:length(e$crs_options)) {
		if (length(grep(names(e$crs_options[i]), crs_str, fixed = TRUE)) > 0) {
			crs = unname(e$crs_options[i])
			break
		}
	}

	style = resolve_style(e$style, api_key = e$api_key, default = o$basemap.server[1], bg = o$bg.color)

	if (mode == "mapbox") {
		# quick & dirty
		if (zoom < 3) {
			# ignore center for global view (otherwise it will be (0, -3) due to Antarctica)
			m = mapgl::mapboxgl(center = c(0,0), zoom = zoom, pitch = o$pitch, bearing = o$bearing, style = style) |>
				mapgl::add_navigation_control(visualize_pitch = TRUE) |>
				mapgl::set_projection(crs)
		} else {
			m = mapgl::mapboxgl(center = ll, zoom = zoom, pitch = o$pitch, bearing = o$bearing, style = style) |>
				mapgl::add_navigation_control(visualize_pitch = TRUE) |>
				mapgl::set_projection(crs)
		}
	} else {
		# quick & dirty
		if (zoom < 3) {
			# ignore center for global view (otherwise it will be (0, -3) due to Antarctica)
			m = mapgl::maplibre(center = c(0,0), zoom = zoom, pitch = o$pitch, bearing = o$bearing, style = style) |>
				mapgl::add_navigation_control(visualize_pitch = TRUE) |>
				mapgl::set_projection(crs) |>
				mapgl::add_globe_control()
		} else {
			m = mapgl::maplibre(center = ll, zoom = zoom, pitch = o$pitch, bearing = o$bearing, style = style) |>
				mapgl::add_navigation_control(visualize_pitch = TRUE) |>
				mapgl::set_projection(crs) |>
				mapgl::add_globe_control()
		}
	}
	assign_mapgl(m, facet_row, facet_col, facet_page, mode = mode)

	NULL
}



#' @export
#' @keywords internal
#' @name tmapMapboxOverlay
#' @rdname tmapMapbox
tmapMapboxOverlay = function(bbx, facet_row, facet_col, facet_page, o) {
	NULL
}


#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreOverlay = function(bbx, facet_row, facet_col, facet_page, o) {
	NULL
}
