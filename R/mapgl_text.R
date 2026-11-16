# Text layers: tm_text() and tm_labels()
#
# tm_text() draws all texts (overlap allowed). tm_labels() (with label placement, see tmap::tm_placement()) leaves
# the collision of labels to MapLibre/Mapbox (option D): labels that overlap are hidden, in the order of their
# priority (symbol-sort-key: lower keys are placed first, so the key is -priority), and reappear when zooming in.
# Point labels use the positions around the point in the order of preference of Imhof (text-variable-anchor);
# polygon labels are centered at the center of the largest inscribed circle (computed by tmap).
mapgl_text = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp,
					  bbx, facet_row, facet_col, facet_page,
					  id, pane, group, glid, o, ..., mode) {

	m = get_mapgl(facet_row, facet_col, facet_page, mode)
	mapgl_blend_unsupported(a, mode)
	if (inherits(shpTM$shp, "character")) {
		if (pmtiles_unsupported(shpTM, mode)) return(NULL)
		return(NULL)
	}

	res = select_sf(shpTM, dt[!is.na(dt$size), ])
	shp = res$shp
	dt = res$dt
	if (!length(shp)) return(NULL)

	gp = impute_gp(gp, dt)
	gp = rescale_gp(gp, o$scale_down)

	n = length(shp)
	text = as.character(rep_len(gp$text, n))
	text[is.na(text)] = ""
	if (all(text == "")) return(NULL)

	col = rep_len(gp$col, n)
	col_alpha = rep_len(gp$col_alpha, n)
	if (any(nchar(col) == 9)) {
		fa = split_alpha_channel(col, alpha = col_alpha)
		col = fa$col
		col_alpha = col_alpha * fa$opacity
	}

	# label placement (tm_labels)
	pl = a$placement
	is_labels = !is.null(pl)
	is_poly = if ("poly" %in% names(dt)) !vapply(dt$poly, is.null, logical(1)) else rep(FALSE, n)
	collide = is_labels && utils::tail(pl$fallback, 1) == "drop"
	priority = if ("priority" %in% names(dt) && length(unique(dt$priority)) > 1L) {
		as.numeric(dt$priority)
	} else if (any(is_poly)) {
		vapply(dt$poly, function(p) if (is.null(p)) 0 else as.numeric(sf::st_area(p)), numeric(1))
	} else {
		rep_len(gp$cex, n)
	}
	priority[is.na(priority)] = -Inf

	fontface = rep_len(if (is.null(gp$fontface)) "plain" else as.character(gp$fontface), n)
	# font variant; the font stacks are taken from the basemap style (see mapgl_text_fonts)
	font = ifelse(fontface %in% c("bold", "2"), "bold",
				  ifelse(fontface %in% c("italic", "3"), "italic",
				  	   ifelse(fontface %in% c("bold.italic", "4"), "bolditalic", "regular")))

	shp2 = sf::st_sf(text = text,
					 col = col,
					 col_alpha = col_alpha,
					 size = rep_len(gp$cex, n) * 12,
					 angle = rep_len(if (is.null(gp$angle)) 0 else gp$angle, n),
					 sortkey = -priority,
					 font = font,
					 id = seq_len(n),
					 geometry = sf::st_geometry(shp))
	if (any(sf::st_geometry_type(shp2) == "MULTIPOINT")) shp2 = suppressWarnings(sf::st_cast(shp2, "POINT"))

	ahp = attach_hover_popup(shp2, dt, hdt, pdt, idt, popup.format)
	shp2 = ahp$shp2

	halo = isTRUE(a$halo) || isTRUE(a$shadow)
	halo_col = if (isTRUE(a$halo) && !is.na(a$halo.col)) a$halo.col else if (isTRUE(a$shadow) && !is.na(a$shadow.col)) a$shadow.col else {
		if (mean(grDevices::col2rgb(col[1])) > 127) "#000000" else "#FFFFFF"
	}

	srcname = mapgl_srcid(paste0("layer", pane))
	layername = paste0(glid, "text")

	# point labels: positions around the point (Imhof); polygon labels: centered
	points_mode = is_labels && !any(is_poly)
	gap = if (is_labels) pl$gap else 0

	args = list(m, id = layername, source = srcname,
				text_field = mapgl::get_column("text"),
				text_size = mapgl::get_column("size"),
				text_color = mapgl::get_column("col"),
				text_opacity = mapgl::get_column("col_alpha"),
				text_rotate = mapgl::get_column("angle"),
				text_allow_overlap = !collide,
				text_ignore_placement = !collide,
				tooltip = ahp$hdt_arg,
				popup = ahp$pdt_arg)
	if (halo) {
		args$text_halo_color = halo_col
		args$text_halo_width = if (isTRUE(a$halo)) max(1, a$halo.width * 12) else 1
		args$text_halo_blur = if (isTRUE(a$halo)) a$halo.blur * 12 else 0
	}
	if (collide) args$symbol_sort_key = mapgl::get_column("sortkey")
	if (points_mode) {
		args$text_variable_anchor = c("bottom-left", "bottom-right", "top-left", "top-right", "left", "right", "bottom", "top")
		args$text_radial_offset = gap
		args$text_justify = "auto"
	} else {
		args$text_anchor = "center"
	}

	if (mode == "maplibre") args$text_font = list("Noto Sans Regular")

	m = mapgl_add_source(m, srcname, data = shp2)
	args[[1]] = m
	m = do.call(mapgl::add_symbol_layer, args)
	m = mapgl_text_fonts(m, layername)
	assign_mapgl(m, facet_row, facet_col, facet_page, mode = mode)

	mapgl_submit_group(group, layername, mode, pane)
	NULL
}

# Font stacks of text layers: the fonts of the basemap style are used (the glyphs of a style only support its own
# fonts). The regular, bold, and italic font stacks are taken from the symbol layers of the style. If the style has
# no glyphs (e.g. a raster basemap), the glyphs of OpenFreeMap (Noto Sans) are used.
mapgl_text_fonts = function(m, layer) {
	htmlwidgets::onRender(m, "function(el, x, data) {
		var map = el.map;
		if (!map) return;
		var done = false;
		var apply = function() {
			if (done || !map.getLayer(data.layer) || !map.isStyleLoaded()) return;
			var style = map.getStyle();
			var stacks = [];
			(style.layers || []).forEach(function(l) {
				var f = l.layout && l.layout['text-font'];
				if (Array.isArray(f) && f.every(function(s) { return typeof s === 'string'; })) stacks.push(f);
			});
			var find = function(yes, no, fallback) {
				for (var i = 0; i < stacks.length; i++) {
					var f = stacks[i][0];
					if (yes.test(f) && !(no && no.test(f))) return stacks[i];
				}
				return fallback;
			};
			var regular = find(/regular|medium|book|normal/i, /italic|bold/i, find(/./, /italic|bold/i, stacks.length ? stacks[0] : null));
			if (!regular) {
				if (!style.glyphs && map.setGlyphs) map.setGlyphs('https://tiles.openfreemap.org/fonts/{fontstack}/{range}.pbf');
				regular = ['Noto Sans Regular'];
			}
			var bold = find(/bold/i, /italic/i, regular);
			var italic = find(/italic/i, /bold/i, regular);
			map.setLayoutProperty(data.layer, 'text-font', ['match', ['get', 'font'],
				'bold', ['literal', bold], 'italic', ['literal', italic], 'bolditalic', ['literal', bold],
				['literal', regular]]);
			done = true;
		};
		map.on('styledata', apply);
		map.on('idle', apply);
	}", data = list(layer = layer))
}
