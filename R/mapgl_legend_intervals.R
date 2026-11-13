# ============================================================
#  Intervals legend with labeled breaks
# ============================================================

# Legend of tm_scale_intervals(label.style = "continuous"): one solid block per class (equal size, as in
# plot mode) with the breaks labeled at the block boundaries. mapgl::add_continuous_legend() draws a
# (landscape) colour gradient, so the legend is drawn here as an SVG (made by tmap), and added to the
# widget in the same way as the bivariate legend. The orientation (portrait/landscape) is respected.
mapgl_legend_intervals = function(cmp, m, legpos, orientation = "vertical") {
	font = "'Open Sans', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif"
	# legend sizes (px): the width and height computed for mapgl legends are those of the whole legend
	k = length(cmp$labels) - 1L - isTRUE(cmp$na.show)
	if (orientation == "vertical") {
		cmp$width = 16
		cmp$height = 22 * k
	} else {
		cmp$width = 40 * k
		cmp$height = 14
	}
	x = utils::getFromNamespace("legend_intervals_svg", "tmap")(cmp, orientation = orientation, fs = 12, font = font)

	esc = function(x) {
		x = gsub("&", "&amp;", x, fixed = TRUE)
		x = gsub("<", "&lt;", x, fixed = TRUE)
		x = gsub(">", "&gt;", x, fixed = TRUE)
		gsub("\n", "<br>", x, fixed = TRUE)
	}
	uid = paste0("legend-", as.hexmode(sample(1:1e6, 1)))

	title = if (is.null(cmp$title) || is.na(cmp$title) || !nzchar(cmp$title)) "" else {
		paste0("<h2 class=\"mapgl-legend-title\">", esc(cmp$title), "</h2>")
	}
	html = paste0("<div id=\"", uid, "\" class=\"mapboxgl-legend ", legpos, "\" data-draggable=\"true\">",
				  title, "<div class=\"tmap-int-bar\">", x$svg, "</div></div>")

	px = function(x, default = 10) paste0(if (is.null(x) || is.na(x)) default else x, "px")
	css = paste0(
		"#", uid, " { position: absolute; border-radius: 10px; margin: 10px; background-color: #ffffffd9;",
		" padding: 10px; z-index: 1002; font-family: ", font, "; font-size: 12px; color: #333333; }\n",
		"#", uid, ".top-left { top: ", px(cmp$margin_top), "; left: ", px(cmp$margin_left), "; }\n",
		"#", uid, ".bottom-left { bottom: ", px(cmp$margin_bottom), "; left: ", px(cmp$margin_left), "; }\n",
		"#", uid, ".top-right { top: ", px(cmp$margin_top), "; right: ", px(cmp$margin_right), "; }\n",
		"#", uid, ".bottom-right { bottom: ", px(cmp$margin_bottom), "; right: ", px(cmp$margin_right), "; }\n",
		"#", uid, " h2 { font-size: 14px; line-height: 20px; margin: 0 0 8px 0; }\n",
		"#", uid, " .tmap-int-bar svg { display: block; }\n",
		"#", uid, " .tmap-int-bar text { fill: #333333; }\n")

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
	}
	m
}
