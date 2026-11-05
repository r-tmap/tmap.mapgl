# ============================================================
#  Interactive legends
# ============================================================
#
# The items of the categorical legends (fill, col), the pattern legends, and the cross legends
# (tmap::tm_legend_cross()) can be clicked to hide (or show again) the features of that item. Each clickable
# element has a condition (a MapLibre / Mapbox GL expression, as JSON) that matches its features. The hidden
# items of all legends of a layer are combined (["all", ["!", cond1], ["!", cond2], ...]) and set in the "legend"
# slot of mapgl's filter stack of that layer. In this way, clicking in one legend does not undo what was clicked
# in another legend of the same layer (which is the case for mapgl's own interactive legends, since they each set
# the whole "legend" slot).
#
# - Legends made by tmap.mapgl (pattern, bivariate, cross legends): the clickable elements have the condition as
#   data attribute (data-tmap-cond), and optionally the conditions of their "parents" (data-tmap-parents), e.g. the
#   row and column of a cell of a cross legend, so that a cell is shown as hidden when its row is hidden.
# - Legends made by mapgl (mapgl::add_categorical_legend(interactive = FALSE)): the conditions of the items
#   (".legend-item" elements) are passed as items, and set as data attributes when the map is rendered.

mapgl_json = function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA))

# Condition: property col of the features equals value
mapgl_cond_eq = function(col, value) {
	vapply(value, function(v) mapgl_json(list("==", list("get", col), v)), character(1), USE.NAMES = FALSE)
}

mapgl_cond_all = function(...) {
	conds = list(...)
	vapply(seq_along(conds[[1]]), function(i) {
		paste0("[\"all\",", paste(vapply(conds, `[`, character(1), i), collapse = ","), "]")
	}, character(1))
}

mapgl_attr_esc = function(x) gsub("\"", "&quot;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE)

# Data attributes of clickable legend elements
mapgl_cond_attr = function(cond, parents = NULL) {
	if (is.null(cond)) return("")
	out = ifelse(is.na(cond), "", paste0(" data-tmap-cond=\"", mapgl_attr_esc(cond), "\""))
	if (!is.null(parents)) {
		p = vapply(seq_along(cond), function(i) {
			paste0("[", paste(vapply(parents, function(pp) mapgl_json(pp[i]), character(1)), collapse = ","), "]")
		}, character(1))
		out = ifelse(is.na(cond), out, paste0(out, " data-tmap-parents=\"", mapgl_attr_esc(p), "\""))
	}
	out
}

# Register the legend uid (filtering layers) for the click handling
mapgl_legend_filter_js = function(m, uid, layers, items = NULL) {
	if (!inherits(m, c("mapboxgl", "maplibregl")) || !length(layers)) return(m)
	htmlwidgets::onRender(m, "function(el, x, data) {
		var S = el._tmapLegend;
		if (!S) {
			S = el._tmapLegend = {legends: {}};
			// set the conditions of the items of legends made by mapgl
			S.prep = function() {
				Object.keys(S.legends).forEach(function(u) {
					var L = S.legends[u];
					if (!L.items || L.prepped) return;
					var leg = document.getElementById(u);
					if (!leg) return;
					leg.querySelectorAll('.legend-item').forEach(function(e, i) {
						if (i < L.items.length && L.items[i]) {
							e.setAttribute('data-tmap-cond', L.items[i]);
							e.style.cursor = 'pointer';
						}
					});
					L.prepped = true;
				});
			};
			var find = function(t) {
				if (!t || !t.closest) return null;
				var e = t.closest('[data-tmap-cond]');
				if (!e) return null;
				var us = Object.keys(S.legends);
				for (var i = 0; i < us.length; i++) {
					var leg = document.getElementById(us[i]);
					if (leg && leg.contains(e)) return {e: e, u: us[i]};
				}
				return null;
			};
			S.apply = function() {
				var map = el.map;
				if (!map) return;
				var per = {};
				Object.keys(S.legends).forEach(function(u) {
					var L = S.legends[u];
					L.layers.forEach(function(l) {
						per[l] = per[l] || [];
						L.hidden.forEach(function(c) { per[l].push(['!', JSON.parse(c)]); });
					});
				});
				var state = window._mapglEnsureLayerState(map);
				Object.keys(per).forEach(function(l) {
					if (!state.filterStack[l]) state.filterStack[l] = {};
					state.filterStack[l].legend = per[l].length ? ['all'].concat(per[l]) : null;
					window._mapglComposeFilter(map, l);
				});
				Object.keys(S.legends).forEach(function(u) {
					var L = S.legends[u];
					var leg = document.getElementById(u);
					if (!leg) return;
					leg.querySelectorAll('[data-tmap-cond]').forEach(function(e) {
						var c = e.getAttribute('data-tmap-cond');
						var par = JSON.parse(e.getAttribute('data-tmap-parents') || '[]');
						var off = L.hidden.indexOf(c) >= 0 || par.some(function(p) { return L.hidden.indexOf(p) >= 0; });
						e.style.opacity = off ? 0.25 : 1;
					});
				});
			};
			// a mouse down on a clickable element should not start dragging the legend
			var stop = function(e) {
				S.prep();
				if (find(e.target)) e.stopPropagation();
			};
			el.addEventListener('mousedown', stop, true);
			el.addEventListener('touchstart', stop, true);
			el.addEventListener('click', function(e) {
				S.prep();
				var h = find(e.target);
				if (!h) return;
				e.preventDefault();
				e.stopPropagation();
				var L = S.legends[h.u];
				var c = h.e.getAttribute('data-tmap-cond');
				var i = L.hidden.indexOf(c);
				if (i < 0) L.hidden.push(c); else L.hidden.splice(i, 1);
				S.apply();
			});
		}
		S.legends[data.uid] = {layers: [].concat(data.layers), hidden: [], items: data.items || null};
		S.prep();
	}", data = list(uid = uid, layers = as.list(layers), items = if (!is.null(items)) as.list(items)))
}

# The layers that are filtered by the legends of the layer with group id glid: the fill (and pattern) layer(s)
# and the border layer, so that the whole feature is hidden
mapgl_legend_layers = function(glid) {
	reg = .TMAP$mapgl_cat[[glid]]
	if (is.null(reg)) return(character())
	as.character(stats::na.omit(c(reg$fill_layer, reg$border_layer)))
}


# ============================================================
#  Dot density legends: the number of units per dot
# ============================================================
#
# As in view mode (tmap:::leaflet_dot_note()): the number of units per dot (legend argument dot.value) is
# K * s1^2 * A, where s1 is the spacing of the dots at density 1 (in screen pixels) and A the map area (km2) per
# pixel at the center of the map. It is updated after each move. The area scale may vary across the map, for
# a tilted map (pitch) or with the Web Mercator projection at small scales. When the area per pixel in a corner of
# the map differs more than 25% from the center, the note mentions that the number applies to the map center.

mapgl_dot_ratio_max = 1.25

# The variation of the area scale across the initial map (bbox bb, pitch in degrees): the ratio between the
# largest and smallest area per pixel, approximately. Web Mercator: the area scale is proportional to
# 1 / cos(lat)^2. Pitch: the area per pixel at the angle t from the vertical is proportional to 1 / cos(t)^3,
# where the edges of the map are at the pitch plus or minus half the field of view (36.87 degrees).
mapgl_dot_ratio = function(bb, pitch) {
	r = 1
	bbll = tryCatch(sf::st_bbox(sf::st_transform(sf::st_as_sfc(bb), 4326)), error = function(e) NULL)
	if (!is.null(bbll)) {
		lat = c(bbll[2], bbll[4], (bbll[2] + bbll[4]) / 2) * pi / 180
		lat = pmin(abs(lat), 85 * pi / 180)
		s = 1 / cos(lat)^2
		r = max(s) / min(s)
	}
	if (!is.null(pitch) && !is.na(pitch) && pitch > 0) {
		h = 36.87 / 2 * 0.8
		t = pmin(c(pitch - h, pitch, pitch + h), 80) * pi / 180
		s = 1 / cos(t)^3
		r = r * max(s) / min(s)
	}
	unname(r)
}

mapgl_dot_note_js = function(m, uid, dot_info, scale) {
	if (!inherits(m, c("mapboxgl", "maplibregl"))) return(m)
	htmlwidgets::onRender(m, "function(el, x, data) {
		var init = function(tries) {
			var map = el.map;
			var leg = document.getElementById(data.uid);
			if (!map || !leg) {
				if (tries < 100) setTimeout(function() { init(tries + 1); }, 100);
				return;
			}
			var note = leg.querySelector('.tmap-dot-note');
			// map area (km2) per pixel at container point (x, y)
			var area = function(x, y) {
				var d = 10;
				var p0 = map.unproject([x, y]), px = map.unproject([x + d, y]), py = map.unproject([x, y + d]);
				var k = 111.32 * Math.cos(p0.lat * Math.PI / 180);
				var ax = (px.lng - p0.lng) * k, ay = (px.lat - p0.lat) * 110.574;
				var bx = (py.lng - p0.lng) * k, by = (py.lat - p0.lat) * 110.574;
				return Math.abs(ax * by - ay * bx) / (d * d);
			};
			var update = function() {
				var c = map.getContainer();
				var w = c.clientWidth, h = c.clientHeight;
				var a0 = area(w / 2, h / 2);
				if (!isFinite(a0) || a0 <= 0) {
					note.style.display = 'none';
					return;
				}
				note.style.display = '';
				var a = [[0.1, 0.1], [0.9, 0.1], [0.1, 0.9], [0.9, 0.9]].map(function(p) { return area(w * p[0], h * p[1]); });
				var varies = a.some(function(ai) { return !isFinite(ai) || ai <= 0 || Math.max(ai / a0, a0 / ai) > data.ratio_max; });
				var n = data.K * data.s1 * data.s1 * a0;
				var p = Math.pow(10, Math.floor(Math.log10(n)) - 1);
				n = Math.round(n / p) * p;
				var txt = n >= 1 ? Math.round(n).toLocaleString('en-US') : String(Number(n.toPrecision(2)));
				note.textContent = data.text.split('{n}').join(txt) + (varies ? ' (at the map center)' : '');
			};
			map.on('moveend', update);
			update();
		};
		init(0);
	}", data = list(uid = uid, K = dot_info$K, text = dot_info$text, s1 = 0.25 * 0.2 * scale * 96,
					ratio_max = mapgl_dot_ratio_max))
}
