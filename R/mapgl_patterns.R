# ============================================================
#  Fill patterns (map variables pattern, pattern_col, pattern_size, pattern_density)
# ============================================================
#
# MapLibre / Mapbox GL draw fill patterns from images (fill-pattern, fill-extrusion-pattern), which are
# registered once per map (mapgl::add_image()) and referenced per feature by id. The images are pattern tiles
# created by the pattern engine of tmap (tmap::pattern_png()), rendered at twice the resolution (pixel ratio 2)
# for sharp patterns on high-density screens. Like the patterns in view mode (leaflet), the patterns have a
# fixed size on the screen.
#
# - 2D polygons: a pattern layer with transparent tiles on top of the fill layer (same source).
# - 3D polygons: fill-extrusion-pattern replaces the fill color, so the tiles contain the fill color as
#   background, and features without a pattern get a tile of their fill color.

# Resolution of the pattern images: twice the screen resolution, for sharp patterns on high-density screens. In
# Mapbox GL (v3), large pattern images cause black tiles (the pattern images of a tile do not fit), so there the
# images have the screen resolution.
mapgl_pattern_dpi = function(mode) if (identical(mode, "mapbox")) 96 else 192

mapgl_unresolved = function(x) is.null(x) || (is.character(x) && any(startsWith(x, "__"), na.rm = TRUE))

# Pattern image per feature. Returns NULL when there is no pattern, otherwise list(ids, files), where ids are
# the image ids per feature (NA: no pattern) and files the PNG files per unique id. With bg (3D), the tiles have
# the fill color as background, and features without a pattern get a tile of their fill color (all = TRUE).
mapgl_pattern_images = function(gp, n, o, bg = NULL, mode = "maplibre") {
	dpi = mapgl_pattern_dpi(mode)
	if (mapgl_unresolved(gp$pattern) || all(is.na(gp$pattern) | gp$pattern == "none")) return(NULL)
	val = function(x, default) if (mapgl_unresolved(x)) default else x
	pdt = data.frame(pattern = rep_len(gp$pattern, n),
					 col = rep_len(val(gp$pattern_col, "grey25"), n),
					 size = rep_len(val(gp$pattern_size, 1), n),
					 density = tmap::pattern_density_round(rep_len(val(gp$pattern_density, 0.8), n)),
					 bg = rep_len(if (is.null(bg)) NA_character_ else as.character(bg), n),
					 stringsAsFactors = FALSE)
	pdt$pattern[is.na(pdt$pattern) | is.na(pdt$density)] = "none"

	# 3D: limit the number of combinations of fill color and pattern (e.g. continuous fill scales) by
	# rounding the fill colors
	if (!is.null(bg)) {
		key0 = paste(pdt$pattern, pdt$col, pdt$size, pdt$density, pdt$bg)
		if (length(unique(key0)) > 256) {
			ok = !is.na(pdt$bg)
			rgbm = grDevices::col2rgb(pdt$bg[ok])
			pdt$bg[ok] = grDevices::rgb(t(round(rgbm / 17) * 17), maxColorValue = 255)
			cli::cli_inform(c("!" = "{.field [mapgl]} Many combinations of fill color and pattern: the fill colors of the 3D polygons with patterns are rounded."),
							.frequency = "regularly", .frequency_id = "mapgl_pattern_colors")
		}
	}

	key = do.call(paste, c(unname(as.list(pdt)), sep = "|"))
	ukey = unique(key)
	updt = pdt[match(ukey, key), , drop = FALSE]
	uids = paste0("tmap-pattern-", vapply(ukey, function(k) substr(rlang::hash(k), 1, 12), character(1),
										  USE.NAMES = FALSE))
	files = mapply(function(p, col, size, density, bg) {
		if (p == "none") {
			if (is.na(bg)) NA_character_ else mapgl_solid_png(bg)
		} else {
			f = tmap::pattern_png(p, density = density, size = size, col = col, scale = o$scale,
								  dpi = dpi, bg = bg)
			if (is.null(f)) NA_character_ else f
		}
	}, updt$pattern, updt$col, updt$size, updt$density, updt$bg, USE.NAMES = FALSE)
	ids = uids[match(key, ukey)]
	ids[is.na(files[match(key, ukey)])] = NA_character_
	names(files) = uids
	files = files[!is.na(files)]
	if (!length(files)) return(NULL)
	list(ids = ids, files = files, dpi = dpi)
}

# PNG of a single color (3D polygons without a pattern)
mapgl_solid_png = function(col) {
	file = file.path(tempdir(), paste0("tmap_solid_", substr(rlang::hash(col), 1, 12), ".png"))
	if (!file.exists(file)) {
		grDevices::png(file, width = 8, height = 8, bg = col)
		grid::grid.newpage()
		grDevices::dev.off()
	}
	file
}

# Register the pattern images (once per map)
mapgl_add_pattern_images = function(m, files, dpi) {
	existing = if (inherits(m, c("mapboxgl", "maplibregl"))) {
		vapply(m$x$images, function(im) im$id, character(1))
	} else character()
	for (id in setdiff(names(files), existing)) {
		m = mapgl::add_image(m, id = id, url = files[[id]], pixel_ratio = dpi / 96)
	}
	mapgl_pattern_zoom_js(m, ratio = dpi / 96)
}

# Keep the patterns at a fixed size on the screen. MapLibre draws fill patterns in the coordinates of the tiles of
# the integer zoom level (the floor of the zoom level), so at zoom level z, they are enlarged by a factor
# 2^(z - floor(z)), i.e. up to twice as large. After each zoom, the pattern images are therefore added again with
# a pixel ratio that compensates for this factor (once per map). Mapbox GL (v3) already draws patterns at a fixed
# size on the screen.
mapgl_pattern_zoom_js = function(m, ratio) {
	if (!inherits(m, "maplibregl")) return(m)
	if (any(grepl("_tmapPatternZoom", unlist(m$jsHooks), fixed = TRUE))) return(m)
	htmlwidgets::onRender(m, "function(el, x, data) {
		var init = function(tries) {
			var map = el.map;
			if (!map) {
				if (tries < 100) setTimeout(function() { init(tries + 1); }, 100);
				return;
			}
			if (map._tmapPatternZoom) return;
			map._tmapPatternZoom = true;
			var ids = (x.images || []).map(function(im) { return im.id; })
				.filter(function(id) { return id.indexOf('tmap-pattern-') === 0; });
			var cur = null;
			var getImage = function(id) {
				return map.getImage ? map.getImage(id) : (map.style && map.style.getImage ? map.style.getImage(id) : null);
			};
			var update = function() {
				if (!ids.length) return;
				var z = map.getZoom();
				var r = Math.round(data.ratio * Math.pow(2, z - Math.floor(z)) * 1000) / 1000;
				if (r === cur) return;
				// wait until all images are loaded
				if (!ids.every(function(id) { return map.hasImage(id); })) return;
				ids.forEach(function(id) {
					var im = getImage(id);
					if (!im || !im.data) return;
					var d = im.data;
					map.removeImage(id);
					map.addImage(id, {width: d.width, height: d.height, data: d.data}, {pixelRatio: r});
				});
				cur = r;
			};
			map.on('zoomend', update);
			map.on('idle', update);
			update();
		};
		init(0);
	}", data = list(ratio = ratio))
}

# Legend swatches of pattern legends (data uris): fill, pattern, and border
mapgl_pattern_swatches = function(gp, n, o, px = 24) {
	gpvars = c("fill", "fill_alpha", "col", "col_alpha", "lwd", "pattern", "pattern_col", "pattern_size",
			   "pattern_density")
	g = lapply(gp[gpvars], function(x) rep_len(if (is.null(x) || all(is.na(x))) NA else x, n))
	vapply(seq_len(n), function(i) {
		file = tmap::pattern_swatch_png(g$pattern[i], density = g$pattern_density[i], size = g$pattern_size[i],
										col = g$pattern_col[i], fill = g$fill[i], fill_alpha = g$fill_alpha[i],
										border = g$col[i], border_alpha = g$col_alpha[i], lwd = g$lwd[i],
										scale = o$scale, px = px)
		paste0("data:image/png;base64,", base64enc::base64encode(file))
	}, character(1))
}

# ============================================================
#  Values of the map variables (interactive cross and pattern legends)
# ============================================================
#
# The features get the values of the varying map variables as properties ("__tmap_v_<var>__"), normalized with
# mapgl_cross_value(), so that the items of cross and pattern legends can filter them (see
# mapgl_legend_filter_js()).

mapgl_cross_vars = c("fill", "col", "pattern", "pattern_col", "pattern_size", "pattern_density")

mapgl_cross_value = function(x, var) {
	if (var %in% c("fill", "col", "pattern_col")) {
		out = rep(NA_character_, length(x))
		ok = !is.na(x)
		if (any(ok)) {
			m = grDevices::col2rgb(x[ok])
			out[ok] = grDevices::rgb(m[1, ], m[2, ], m[3, ], maxColorValue = 255)
		}
		out
	} else if (is.numeric(x)) {
		format(round(x, 4), trim = TRUE, scientific = FALSE)
	} else {
		as.character(x)
	}
}

# Add the values of the varying map variables to the features
mapgl_add_cross_props = function(shp2, gp) {
	for (v in mapgl_cross_vars) {
		x = gp[[v]]
		if (length(x) > 1 && !mapgl_unresolved(x)) shp2[[paste0("__tmap_v_", v, "__")]] = mapgl_cross_value(x, v)
	}
	shp2
}
