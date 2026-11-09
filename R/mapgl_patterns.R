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
	# patterns attached to the map (pattern.zoom = "map") are enlarged when zooming in, so their images have twice
	# the resolution
	res = if (identical(o$pattern.zoom, "map") && !identical(mode, "mapbox")) 2 else 1
	dpi = mapgl_pattern_dpi(mode) * res
	if (mapgl_unresolved(gp$pattern) || all(is.na(gp$pattern) | gp$pattern == "none")) return(NULL)
	val = function(x, default) if (mapgl_unresolved(x)) default else x
	pdt = data.frame(pattern = rep_len(gp$pattern, n),
					 col = rep_len(val(gp$pattern_col, "grey25"), n),
					 size = rep_len(val(gp$pattern_size, 1), n),
					 density = tmap::pattern_density_round(rep_len(val(gp$pattern_density, 0.8), n)),
					 bg = rep_len(if (is.null(bg)) NA_character_ else as.character(bg), n),
					 stringsAsFactors = FALSE)
	pdt$pattern[is.na(pdt$pattern) | is.na(pdt$density)] = "none"

	# raster textures ("terrain", "mottle", "sketch"): one image per density, and these images are large, so the
	# densities (the strength of the texture) are rounded to steps of 0.05, which is hardly visible
	upat = unique(pdt$pattern)
	tex = pdt$pattern %in% upat[mapgl_pattern_texture_size(upat, rep(1, length(upat)), o$scale) > 0]
	pdt$density[tex] = round(pdt$density[tex] * 20) / 20

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
	files = lapply(seq_len(nrow(updt)), function(i) {
		p = updt$pattern[i]
		col = updt$col[i]
		size = updt$size[i]
		density = updt$density[i]
		bg = updt$bg[i]
		if (p == "none") {
			if (is.na(bg)) NA_character_ else mapgl_solid_png(bg)
		} else {
			# the images of raster textures (tiles of 1 to 6 inches) are limited to mapgl_texture_max_px pixels:
			# MapLibre and Mapbox GL pack the pattern images of a vector tile in one texture (atlas)
			w = mapgl_pattern_texture_size(p, density, o$scale)
			dpi_p = if (w > 0) min(dpi, res * mapgl_texture_max_px(mode) / w) else dpi
			f = tmap::pattern_png(p, density = density, size = size, col = col, scale = o$scale,
								  dpi = dpi_p, bg = bg)
			if (is.null(f)) NA_character_ else structure(f, dpi = dpi_p)
		}
	})
	# the resolution per image (lower for large raster textures)
	dpis = vapply(files, function(f) if (is.null(attr(f, "dpi"))) dpi else attr(f, "dpi"), numeric(1))
	files = vapply(files, as.character, character(1))
	ids = uids[match(key, ukey)]
	ids[is.na(files[match(key, ukey)])] = NA_character_
	names(files) = uids
	names(dpis) = uids
	keep = !is.na(files)
	if (!any(keep)) return(NULL)
	list(ids = ids, files = files[keep], dpi = dpis[keep])
}

# Tile size (inches) of raster texture patterns ("terrain", "mottle", "sketch"; see tmap::tm_patterns()), and 0
# for other patterns (vectorized)
mapgl_pattern_texture_size = function(pattern, density, scale = 1) {
	pattern_geom = utils::getFromNamespace("pattern_geom", "tmap")
	pattern_spacing = utils::getFromNamespace("pattern_spacing", "tmap")
	vapply(seq_along(pattern), function(i) {
		p = pattern[i]
		if (is.na(p) || p == "none" || !grepl("^(terrain|mottle|sketch)", p)) return(0)
		g = tryCatch(pattern_geom(p, pattern_spacing(density[i], scale = scale, line_in = 0.2), density = density[i]),
					 error = function(e) NULL)
		if (is.null(g) || is.null(g$raster)) 0 else max(g$w, g$h)
	}, numeric(1))
}

# Maximum size (pixels) of the image of a raster texture: images of up to 6 inches would be 1152 pixels at 192 dpi,
# and several of them (one per density) do not fit in the texture atlas of a vector tile
mapgl_texture_max_px = function(mode) if (identical(mode, "mapbox")) 384 else 576

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

# Register the pattern images (once per map). dpi: the resolution per image (named by id), or one for all
mapgl_add_pattern_images = function(m, files, dpi, zoom = "screen") {
	existing = if (inherits(m, c("mapboxgl", "maplibregl"))) {
		vapply(m$x$images, function(im) im$id, character(1))
	} else character()
	dpi = stats::setNames(rep_len(dpi, length(files)), names(files))
	for (id in setdiff(names(files), existing)) {
		m = mapgl::add_image(m, id = id, url = files[[id]], pixel_ratio = dpi[[id]] / 96)
	}
	mapgl_pattern_zoom_js(m, zoom = zoom)
}

# Keep the patterns at a fixed size on the screen. MapLibre draws fill patterns in the coordinates of the tiles of
# the integer zoom level (the floor of the zoom level), so at zoom level z, they are enlarged by a factor
# 2^(z - floor(z)), i.e. up to twice as large. After each zoom, the pattern images are therefore added again with
# a pixel ratio that compensates for this factor (once per map). Mapbox GL (v3) already draws patterns at a fixed
# size on the screen. The pixel ratio of each image (as registered, see mapgl_add_pattern_images()) is the base
# ratio, since the images may have different resolutions.
#
# zoom = "map" (option pattern.zoom): the patterns are attached to the map instead, i.e. enlarged by 2^(z - z0)
# relative to the initial zoom level z0. This cannot be done with the pattern images: re-adding them (as above)
# makes the patterns flicker, since the tiles are laid out again, and images per zoom level do not work for tiles
# of another zoom level, which MapLibre shows while the tiles of the new zoom level are loaded. Instead, the
# pattern coordinates are changed: MapLibre computes them in pixels at the integer zoom level (uniforms u_scale
# and u_pixel_coord_upper/lower of the fill and fill-extrusion pattern programs), and for the layers with
# patterns, these are replaced by the pixels at zoom level z0, using the zoom level of the tile that is drawn. This
# relies on internals of MapLibre (Program.draw, Tile.getBucket, map.painter.cache), and if these are not found,
# the patterns keep their size on the screen (between integer zoom levels).
mapgl_pattern_zoom_js = function(m, zoom = "screen") {
	if (!inherits(m, "maplibregl")) return(m)
	if (any(grepl("_tmapPatternZoom", unlist(m$jsHooks), fixed = TRUE))) return(m)
	zoom = if (is.null(zoom)) "screen" else match.arg(zoom, c("screen", "map"))
	htmlwidgets::onRender(m, data = list(mode = zoom), "function(el, x, data) {
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
			if (!ids.length) return;
			var base = {};
			var getImage = function(id) {
				return map.getImage ? map.getImage(id) : (map.style && map.style.getImage ? map.style.getImage(id) : null);
			};
			var loaded = function() { return ids.every(function(id) { return map.hasImage(id); }); };

			if (data && data.mode === 'map') {
				// layers with patterns, and the zoom level to which the patterns are attached
				var z0 = map.getZoom();
				var reg = window._tmapPatternMapLayers = window._tmapPatternMapLayers || {};
				var started = false;
				var start = function() {
					if (started || !loaded()) return;
					var progs = map.painter && map.painter.cache ? Object.values(map.painter.cache).filter(Boolean) : [];
					var tile = null;
					var tms = map.style && (map.style.tileManagers || map.style.sourceCaches);
					for (var k in (tms || {})) {
						var tm = tms[k], tids = tm && tm.getRenderableIds ? tm.getRenderableIds() : [];
						if (tids.length) tile = tm.getTileByID ? tm.getTileByID(tids[0]) : (tm._tiles ? tm._tiles[tids[0]] : null);
						if (tile) break;
					}
					if (!progs.length || !tile) return;
					started = true;
					map.getStyle().layers.forEach(function(l) {
						var prop = l.type === 'fill' ? 'fill-pattern' : (l.type === 'fill-extrusion' ? 'fill-extrusion-pattern' : null);
						var v = prop && l.paint ? l.paint[prop] : null;
						if (v && JSON.stringify(v).indexOf('pattern_id') >= 0) reg[l.id] = {z0: z0, map: map};
					});
					// the tile that is drawn: MapLibre asks the bucket of the layer just before drawing it
					var TP = Object.getPrototypeOf(tile);
					if (!TP._tmapPatternMap) {
						TP._tmapPatternMap = true;
						var getBucket = TP.getBucket;
						TP.getBucket = function(layer) {
							window._tmapPatternTile = {tile: this, layer: layer && layer.id};
							return getBucket.apply(this, arguments);
						};
					}
					var PP = Object.getPrototypeOf(progs[0]);
					if (!PP._tmapPatternMap) {
						PP._tmapPatternMap = true;
						var draw = PP.draw;
						PP.draw = function() {
							var uv = arguments[6], lid = arguments[9], r = reg[lid], cur = window._tmapPatternTile;
							if (r && uv && uv.u_scale && cur && cur.layer === lid) {
								// pattern coordinates in pixels at zoom level z0 instead of the integer zoom level
								var t = cur.tile, c = Math.pow(2, t.tileID.overscaledZ);
								var f = Math.pow(2, r.z0 - r.map.transform.tileZoom);
								var s = t.tileSize * Math.pow(2, r.z0) / c;
								var px = s * (t.tileID.canonical.x + t.tileID.wrap * c), py = s * t.tileID.canonical.y;
								var ux = Math.floor(px / 65536), uy = Math.floor(py / 65536);
								// no cross-fade between zoom levels: MapLibre draws the 'from' image twice (or half) as large, as if it were
								// the image of the previous zoom level
								uv.u_scale = [uv.u_scale[0] * f, 1, 1];
								uv.u_pixel_coord_upper = [ux, uy];
								uv.u_pixel_coord_lower = [px - ux * 65536, py - uy * 65536];
							}
							return draw.apply(this, arguments);
						};
					}
					map.triggerRepaint();
				};
				map.on('idle', start);
				start();
				return;
			}

			var cur = null;
			var update = function() {
				var z = map.getZoom();
				var f = Math.round(Math.pow(2, z - Math.floor(z)) * 1000) / 1000;
				if (f === cur) return;
				// wait until all images are loaded
				if (!loaded()) return;
				ids.forEach(function(id) {
					var im = getImage(id);
					if (!im || !im.data) return;
					if (!(id in base)) base[id] = im.pixelRatio || 1;
					var d = im.data;
					map.removeImage(id);
					map.addImage(id, {width: d.width, height: d.height, data: d.data}, {pixelRatio: base[id] * f});
				});
				cur = f;
			};
			map.on('zoomend', update);
			map.on('idle', update);
			update();
		};
		init(0);
	}")
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
