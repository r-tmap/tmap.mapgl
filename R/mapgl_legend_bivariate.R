# ============================================================
#  Bivariate legend
# ============================================================

# mapgl::add_bivariate_legend() only supports 3 x 3 grids without class labels,
# so the legend is built here as an HTML table, and added to the widget the same
# way mapgl adds its legends (legend_html / legend_css).
#
# cmp$gp$fill is a vector of m x n colours (column-major): rows (i) are the
# classes of the first variable (scale1, y-axis), columns (j) the classes of the
# second (scale2, x-axis). cmp$labels contains the m row labels followed by the
# n column labels. As in plot mode, the highest row class is drawn on top.
#
# Cross legends (tmap::tm_legend_cross(), e.g. pattern x fill) are drawn the same way, with pattern swatches
# (images) in the cells.
mapgl_legend_bivariate = function(cmp, m, legpos, o = NULL) {
	m_rows = attr(cmp$gp$fill, "m")
	n_cols = attr(cmp$gp$fill, "n")
	cols = matrix(cmp$gp$fill, nrow = m_rows, ncol = n_cols)
	is_cross = isTRUE(cmp$is_cross)
	cell_style = if (is_cross) {
		img = mapgl_pattern_swatches(cmp$gp, m_rows * n_cols, o, px = 22)
		matrix(paste0("background-image:url(", img, ");background-size:100% 100%;"), nrow = m_rows, ncol = n_cols)
	} else {
		matrix(paste0("background-color:", cols, ";"), nrow = m_rows, ncol = n_cols)
	}

	# interactive cross legends: the conditions of the cells, rows, and columns (see mapgl_legend_filter_js())
	cross_vars = cmp$cross_vars
	layers = mapgl_legend_layers(cmp$glid)
	interactive = is_cross && !is.null(cross_vars) && all(cross_vars %in% mapgl_cross_vars) && length(layers) > 0
	if (interactive) {
		vr = mapgl_cross_value(cmp$gp[[cross_vars[1]]], cross_vars[1])
		vc = mapgl_cross_value(cmp$gp[[cross_vars[2]]], cross_vars[2])
		cr = mapgl_cond_eq(paste0("__tmap_v_", cross_vars[1], "__"), vr)
		cc = mapgl_cond_eq(paste0("__tmap_v_", cross_vars[2], "__"), vc)
		cr[is.na(vr)] = NA
		cc[is.na(vc)] = NA
		cell = mapgl_cond_all(cr, cc)
		cell[is.na(cr) | is.na(cc)] = NA
		cell_attr = matrix(mapgl_cond_attr(cell, parents = list(cr, cc)), nrow = m_rows, ncol = n_cols)
		row_attr = mapgl_cond_attr(matrix(cr, nrow = m_rows)[, 1])
		col_attr = mapgl_cond_attr(matrix(cc, nrow = m_rows)[1, ])
	} else {
		cell_attr = matrix("", nrow = m_rows, ncol = n_cols)
		row_attr = rep("", m_rows)
		col_attr = rep("", n_cols)
	}
	labs_y = cmp$labels[seq_len(m_rows)]
	labs_x = cmp$labels[m_rows + seq_len(n_cols)]

	esc = function(x) {
		x = gsub("&", "&amp;", x, fixed = TRUE)
		x = gsub("<", "&lt;", x, fixed = TRUE)
		x = gsub(">", "&gt;", x, fixed = TRUE)
		gsub("\n", "<br>", x, fixed = TRUE)
	}

	uid = paste0("legend-", as.hexmode(sample(1:1e6, 1)))

	ylab = if (is.null(cmp$ylab) || is.na(cmp$ylab)) "" else cmp$ylab
	xlab = if (is.null(cmp$xlab) || is.na(cmp$xlab)) "" else cmp$xlab

	rows = vapply(m_rows:1, function(i) {
		ycell = if (i == m_rows && nzchar(ylab)) {
			paste0("<td class=\"tmap-biv-ylab\" rowspan=\"", m_rows, "\"><div>", esc(ylab), "</div></td>")
		} else ""
		paste0("<tr>", ycell,
			   "<td class=\"tmap-biv-label tmap-biv-label-y\"", row_attr[i], ">", esc(labs_y[i]), "</td>",
			   paste0("<td class=\"tmap-biv-cell\" style=\"", cell_style[i, ], "\"", cell_attr[i, ], "></td>", collapse = ""),
			   "</tr>")
	}, character(1))

	empty_y = if (nzchar(ylab)) "<td></td>" else ""
	row_x = paste0("<tr>", empty_y, "<td></td>",
				   paste0("<td class=\"tmap-biv-label tmap-biv-label-x\"", col_attr, "><div>", esc(labs_x), "</div></td>", collapse = ""),
				   "</tr>")
	row_xlab = if (nzchar(xlab)) {
		paste0("<tr>", empty_y, "<td></td><td class=\"tmap-biv-xlab\" colspan=\"", n_cols, "\">", esc(xlab), "</td></tr>")
	} else ""

	title = if (is.null(cmp$title) || is.na(cmp$title) || !nzchar(cmp$title)) "" else {
		paste0("<h2 class=\"mapgl-legend-title\">", esc(cmp$title), "</h2>")
	}

	html = paste0("<div id=\"", uid, "\" class=\"mapboxgl-legend ", legpos, "\" data-draggable=\"true\">",
				  title,
				  "<table class=\"tmap-biv-table\">", paste(rows, collapse = ""), row_x, row_xlab, "</table>",
				  "</div>")

	px = function(x, default = 10) paste0(if (is.null(x) || is.na(x)) default else x, "px")
	font = "'Open Sans', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif"

	css = paste0(
		"#", uid, " { position: absolute; border-radius: 10px; margin: 10px; background-color: #ffffffd9;",
		" padding: 10px; z-index: 1002; font-family: ", font, "; font-size: 12px; color: #333333; }\n",
		"#", uid, ".top-left { top: ", px(cmp$margin_top), "; left: ", px(cmp$margin_left), "; }\n",
		"#", uid, ".bottom-left { bottom: ", px(cmp$margin_bottom), "; left: ", px(cmp$margin_left), "; }\n",
		"#", uid, ".top-right { top: ", px(cmp$margin_top), "; right: ", px(cmp$margin_right), "; }\n",
		"#", uid, ".bottom-right { bottom: ", px(cmp$margin_bottom), "; right: ", px(cmp$margin_right), "; }\n",
		"#", uid, " h2 { font-size: 14px; line-height: 20px; margin: 0 0 8px 0; }\n",
		"#", uid, " .tmap-biv-table { border-collapse: collapse; }\n",
		"#", uid, " .tmap-biv-table td { padding: 0; }\n",
		"#", uid, " .tmap-biv-cell { width: 22px; height: 22px; border: 1px solid #ffffff; }\n",
		"#", uid, " .tmap-biv-label-y { text-align: right; padding-right: 5px; white-space: nowrap; }\n",
		"#", uid, " .tmap-biv-label-x { vertical-align: top; text-align: center; padding-top: 4px; }\n",
		"#", uid, " .tmap-biv-label-x div { display: inline-block; white-space: nowrap; writing-mode: vertical-rl; transform: rotate(180deg); }\n",
		"#", uid, " .tmap-biv-ylab { vertical-align: middle; padding-right: 4px; }\n",
		"#", uid, " .tmap-biv-ylab div { writing-mode: vertical-rl; transform: rotate(180deg); text-align: center; }\n",
		"#", uid, " .tmap-biv-xlab { text-align: center; padding-top: 2px; white-space: nowrap; }\n",
		"#", uid, " [data-tmap-cond] { cursor: pointer; }\n")

	style_css = tryCatch(utils::getFromNamespace(".translate_style_to_css", "mapgl")(cmp$mapgl_style, uid),
						 error = function(e) "")
	css = paste0(css, style_css)

	if (inherits(m, "mapboxgl_proxy") || inherits(m, "maplibre_proxy")) {
		proxy_class = if (inherits(m, "mapboxgl_proxy")) "mapboxgl-proxy" else "maplibre-proxy"
		m$session$sendCustomMessage(proxy_class, list(id = m$id,
			message = list(type = "add_legend", html = html, legend_css = css, add = TRUE)))
	} else {
		m$x$legend_html = paste(m$x$legend_html, html)
		m$x$legend_css = paste(m$x$legend_css, css)
		if (interactive) m = mapgl_legend_filter_js(m, uid, layers)
	}
	m
}


# ============================================================
#  Pattern legend
# ============================================================

# Legend of the pattern map variables (pattern, pattern_col, pattern_size, pattern_density): an HTML legend with
# pattern swatches (PNG images, as data uris), added in the same way as the bivariate legend. The items of
# legends with discrete scales can be clicked to hide the features (see mapgl_legend_filter_js()). Dot density
# legends (legend argument dot.value) get a note with the number of units per dot.
mapgl_legend_pattern = function(cmp, m, legpos, o) {
	n = length(cmp$labels)
	img = mapgl_pattern_swatches(cmp$gp, n, o, px = 24)

	esc = function(x) {
		x = gsub("&", "&amp;", x, fixed = TRUE)
		x = gsub("<", "&lt;", x, fixed = TRUE)
		x = gsub(">", "&gt;", x, fixed = TRUE)
		gsub("\n", "<br>", x, fixed = TRUE)
	}
	uid = paste0("legend-", as.hexmode(sample(1:1e6, 1)))

	# interactive: the map variable of the legend (the first varying one), for discrete scales
	layers = mapgl_legend_layers(cmp$glid)
	nuq = vapply(cmp$gp[intersect(mapgl_cross_vars, names(cmp$gp))], function(x) length(unique(x)), integer(1))
	var = names(nuq)[nuq > 1][1]
	interactive = !is.na(var) && !identical(cmp$scale, "continuous") && length(layers) > 0
	item_attr = if (interactive) {
		v = mapgl_cross_value(rep_len(cmp$gp[[var]], n), var)
		cond = mapgl_cond_eq(paste0("__tmap_v_", var, "__"), v)
		cond[is.na(v)] = NA
		mapgl_cond_attr(cond)
	} else rep("", n)

	# dot density legend
	dot_info = cmp[["dot_info"]]
	if (!is.null(dot_info) && !utils::getFromNamespace("legend_dot_pattern", "tmap")(cmp$gp)) {
		utils::getFromNamespace("message_legend_dot_value", "tmap")("the pattern is not a (constant) dot pattern")
		dot_info = NULL
	} else if (is.null(dot_info) && !is.null(cmp[["dot.value"]]) && !isFALSE(cmp[["dot.value"]])) {
		utils::getFromNamespace("message_legend_dot_value", "tmap")()
	}
	if (!is.null(dot_info) && !is.null(cmp$bbox)) {
		r = mapgl_dot_ratio(cmp$bbox, o$pitch)
		if (r > mapgl_dot_ratio_max) {
			cli::cli_inform(c("{.field [legend]} the number of units per dot ({.arg dot.value}) applies to the center of the map: the map area per dot varies by more than {round(100 * (mapgl_dot_ratio_max - 1))}% across the map.",
							  i = "This is caused by tilting the map ({.arg pitch}) or by the Web Mercator projection for large areas."),
							.frequency = "regularly", .frequency_id = "mapgl_dot_value_varies")
		}
	}

	title = if (is.null(cmp$title) || is.na(cmp$title) || !nzchar(cmp$title)) "" else {
		paste0("<h2 class=\"mapgl-legend-title\">", esc(cmp$title), "</h2>")
	}
	items = paste0("<div class=\"tmap-pat-item\"", item_attr, "><img src=\"", img, "\" width=\"24\" height=\"24\">",
				   "<span>", esc(cmp$labels), "</span></div>", collapse = "")
	note = if (is.null(dot_info)) "" else "<div class=\"tmap-dot-note\"></div>"
	html = paste0("<div id=\"", uid, "\" class=\"mapboxgl-legend ", legpos, "\" data-draggable=\"true\">",
				  title, items, note, "</div>")

	px = function(x, default = 10) paste0(if (is.null(x) || is.na(x)) default else x, "px")
	font = "'Open Sans', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif"
	css = paste0(
		"#", uid, " { position: absolute; border-radius: 10px; margin: 10px; background-color: #ffffffd9;",
		" padding: 10px; z-index: 1002; font-family: ", font, "; font-size: 12px; color: #333333; }\n",
		"#", uid, ".top-left { top: ", px(cmp$margin_top), "; left: ", px(cmp$margin_left), "; }\n",
		"#", uid, ".bottom-left { bottom: ", px(cmp$margin_bottom), "; left: ", px(cmp$margin_left), "; }\n",
		"#", uid, ".top-right { top: ", px(cmp$margin_top), "; right: ", px(cmp$margin_right), "; }\n",
		"#", uid, ".bottom-right { bottom: ", px(cmp$margin_bottom), "; right: ", px(cmp$margin_right), "; }\n",
		"#", uid, " h2 { font-size: 14px; line-height: 20px; margin: 0 0 8px 0; }\n",
		"#", uid, " .tmap-pat-item { display: flex; align-items: center; margin: 4px 0; }\n",
		"#", uid, " .tmap-pat-item img { margin-right: 6px; }\n",
		"#", uid, " [data-tmap-cond] { cursor: pointer; }\n",
		"#", uid, " .tmap-dot-note { margin-top: 6px; white-space: nowrap; }\n")

	style_css = tryCatch(utils::getFromNamespace(".translate_style_to_css", "mapgl")(cmp$mapgl_style, uid),
						 error = function(e) "")
	css = paste0(css, style_css)

	if (inherits(m, "mapboxgl_proxy") || inherits(m, "maplibre_proxy")) {
		proxy_class = if (inherits(m, "mapboxgl_proxy")) "mapboxgl-proxy" else "maplibre-proxy"
		m$session$sendCustomMessage(proxy_class, list(id = m$id,
			message = list(type = "add_legend", html = html, legend_css = css, add = TRUE)))
	} else {
		m$x$legend_html = paste(m$x$legend_html, html)
		m$x$legend_css = paste(m$x$legend_css, css)
		if (interactive) m = mapgl_legend_filter_js(m, uid, layers)
		if (!is.null(dot_info)) m = mapgl_dot_note_js(m, uid, dot_info, scale = o$scale)
	}
	m
}
