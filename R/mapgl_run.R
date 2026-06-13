#' @param show show
#' @param knit knit
#' @param knit_opts knit options
#' @param args args
#' @export
#' @keywords internal
#' @name tmapMapboxRun
#' @importFrom grDevices col2rgb rgb
#' @importFrom colorspace deutan protan tritan
#' @importFrom htmltools tags
#' @importFrom htmlwidgets prependContent
#' @import sf
#' @importFrom tmaptools bb_poly
#' @importFrom terra rast is.lonlat project ext crop
#' @importFrom stats na.omit
#' @importFrom utils head tail getFromNamespace
#' @import stars
#' @import cli
#' @importFrom units drop_units set_units
#' @rdname tmapMapbox
tmapMapboxRun = function(o, q, show, knit, knit_opts, args) {
	mapgl_run(o, q, show, knit, args, mode = "mapbox")
}


#' @export
#' @rdname tmapMapbox
tmapMaplibreRun = function(o, q, show, knit, knit_opts, args) {
	mapgl_run(o, q, show, knit, args, mode = "maplibre")

}

# Per-widget CSS injected into mapgl maps. Always hides the interactive-legend
# "Reset Filter" button (mapgl injects it via JS and toggles display:none/block,
# which otherwise shifts/grows the bottom-anchored legend). When a frame colour
# is given, it also draws a matching border on the control/globe button groups
# and on popups, so they read consistently with the legend frame. `frame_hex`
# must be a CSS-valid colour (hex); pass NULL to skip the borders.
mapgl_extra_css = function(frame_hex = NULL) {
	css = ".mapboxgl-legend .legend-reset-btn { display: none !important; }"
	if (!is.null(frame_hex)) {
		css = paste0(
			css, "\n",
			# Give control/globe buttons the same flat frame as the legend: add the
			# gray border and drop maplibre's default 2px "halo" (box-shadow), so
			# the two read consistently. (To instead keep the halo and add it to the
			# legend, drop the box-shadow:none here and shadow .mapboxgl-legend.)
			".maplibregl-ctrl-group, .mapboxgl-ctrl-group { border: 1px solid ", frame_hex, " !important; box-shadow: none !important; }\n",
			".maplibregl-popup-content, .mapboxgl-popup-content { border: 1px solid ", frame_hex, " !important; }"
		)
	}
	htmltools::HTML(css)
}

mapgl_prepend_css = function(w, css) {
	if (inherits(w, "htmlwidget")) {
		htmlwidgets::prependContent(w, htmltools::tags$style(css))
	} else {
		w
	}
}

# Convert an R colour to CSS-safe #RRGGBB (R's "gray40" etc. are not valid CSS);
# NULL/NA -> NULL so callers can skip.
mapgl_col_to_hex = function(col) {
	if (is.null(col) || (length(col) == 1L && is.na(col))) return(NULL)
	tryCatch({
		m = grDevices::col2rgb(col)
		grDevices::rgb(m[1, ], m[2, ], m[3, ], maxColorValue = 255)
	}, error = function(e) NULL)
}

mapgl_run = function(o, q, show, knit, args, mode) {
	e = if (mode == "mapbox") {
		.TMAP_MAPBOX
	} else {
		.TMAP_MAPLIBRE
	}

	if (show && o$show_gif_ani) {
		cli::cli_alert("{.field {mode} mode} Animations are not implemented in {mode} mode, so they are shown as facets")
	}


	ms = get("ms", envir = e)
	grps = get("grps", envir = e)

	# Prepend per-widget CSS: hide the reset button, and (from the legend frame
	# colour) border the control/globe buttons and popups to match.
	extra_css = mapgl_extra_css(mapgl_col_to_hex(o[["component.frame.color"]]))
	ms = lapply(ms, function(msi) lapply(msi, mapgl_prepend_css, css = extra_css))

	ctrl = split(q$group.control, f = q$group)
	ctrl = sapply(ctrl, tail, 1)

	no_grp = names(ctrl)[ctrl == "none"]

	if (length(no_grp)) {
		grps[no_grp] = NULL
	}


	ms2 = lapply(ms, function(msi) {
		x = if (o$nrows == 1 && o$ncols == 1) {
			if (length(grps)) {
				msi[[1]] |>
					mapgl::add_layers_control(layers = grps, collapsible = o$control.collapse, position = mapgl_pos(o$control.position))
			} else {
				msi[[1]]
			}
		} else {
			if (length(msi) > 2) cli::cli_warn("more than 2 facets not supported for the mode {.str mapbox}")
			orientation = ifelse(o$ncols >= o$nrows, "vertical", "horizontal")

			fc = o$free.coords
			sync = if (identical(o$sync, FALSE)) {
				"none"
			} else if (identical(o$sync, TRUE) || all(!fc)) {
				"sync"
			} else if (all(fc)) {
				"none"
			}
			if (o$swipe) {
				mode = "swipe"
			} else if (all(!fc)) {
				mode = sync
			} else {
				mode = "none"
			}

			if (mode == "none") {
				map_layout(msi[[1]], msi[[2]], orientation = orientation)
			} else {
				mapgl::compare(msi[[1]], msi[[2]], mode = mode, orientation = orientation)
			}
		}
		x
	})

	if (length(ms2) == 1) ms2 = ms2[[1]]
	if (show && !knit && !isTRUE(.TMAP$in.shiny)) {
		print(ms2)
	}
	ms2
}

map_layout = function(..., orientation = c("vertical", "horizontal")) {
	orientation = match.arg(orientation)

	flex_direction = if (orientation == "horizontal") "column" else "row"

	flex_style = sprintf(
		"display: flex; flex-direction: %s; gap: 10px; width: 100vw; height: 100vh; overflow: hidden; box-sizing: border-box;",
		flex_direction
	)

	maps = list(...)
	item_style = "flex: 1; min-width: 0; min-height: 0; overflow: hidden;"

	htmltools::browsable(
		htmltools::tagList(
			htmltools::tags$style("body { margin: 0; padding: 0; overflow: hidden; }"),
			htmltools::div(
				style = flex_style,
				lapply(maps, function(m) htmltools::div(style = item_style, m))
			)
		)
	)
}


# ============================================================
#  tmap_arrange support
# ============================================================
# Called from tmap's print_tmap_arrange() via do.call(paste0("tmap", gs,
# "Arrange"), ...) where gs is "Maplibre" / "Mapbox". Each tmap object is
# rendered to its own mapgl widget and the widgets are placed in an
# nrow x ncol CSS grid.

#' @param tms list of tmap objects
#' @param nx number of facets
#' @param nrow number of grid rows
#' @param opts arrange options (widths, heights, height, sync, ...)
#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxArrange = function(tms, nx, ncol, nrow, opts, knit, show, args, options) {
	mapgl_arrange(tms, nx, ncol, nrow, opts, knit, show, args, options, mode = "mapbox")
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreArrange = function(tms, nx, ncol, nrow, opts, knit, show, args, options) {
	mapgl_arrange(tms, nx, ncol, nrow, opts, knit, show, args, options, mode = "maplibre")
}

mapgl_arrange = function(tms, nx, ncol, nrow, opts, knit, show, args, options, mode) {
	if (isTRUE(opts$sync)) {
		cli::cli_inform(c(
			"!" = "{.arg sync} is not supported for arranged maps in {.str {mode}} mode; the maps pan/zoom independently.",
			"i" = "Synchronised navigation is available for two facets within a single map (via {.fn tm_facets})."
		))
	}

	# Render each tmap object to its own mapgl widget. show = FALSE so nothing is
	# printed here; print.tmap() returns the widget invisibly. Force each widget
	# to fill its grid cell - otherwise it renders at the htmlwidget default
	# height and the grid's height has no visible effect.
	widgets = lapply(tms, function(tm) {
		w = print(tm, show = FALSE, knit = FALSE)
		if (inherits(w, "htmlwidget")) {
			w$width  = "100%"
			w$height = "100%"
		}
		w
	})

	out = mapgl_grid_layout(widgets, ncol = ncol, nrow = nrow,
							widths = opts$widths, heights = opts$heights,
							height = opts$height)

	if (show && !knit && !isTRUE(.TMAP$in.shiny)) {
		print(out)
	}
	out
}

# Place a list of htmlwidgets in an nrow x ncol CSS grid (row-major, matching
# tmap's facet order). `widths`/`heights` are optional proportions (each summing
# to 1, length ncol / nrow); when absent the tracks are equal-sized.
mapgl_grid_layout = function(widgets, ncol, nrow, widths = NA, heights = NA, height = NULL) {
	tracks = function(n, sizes) {
		if (length(sizes) == n && !anyNA(sizes)) {
			paste(sprintf("%ffr", sizes), collapse = " ")
		} else {
			paste(rep("1fr", n), collapse = " ")
		}
	}

	# Overall height of the arranged grid. NULL -> fill the viewport (100vh); a
	# bare number is interpreted as pixels; a string is used as-is (e.g. "80vh").
	h_css = if (is.null(height) || (length(height) == 1L && is.na(height))) {
		"100vh"
	} else if (is.numeric(height)) {
		paste0(height, "px")
	} else {
		as.character(height)
	}

	grid_style = sprintf(
		paste0("display: grid; grid-template-columns: %s; grid-template-rows: %s; ",
			   "gap: 10px; width: 100%%; height: %s; overflow: hidden; box-sizing: border-box;"),
		tracks(ncol, widths), tracks(nrow, heights), h_css
	)
	item_style = "width: 100%; height: 100%; min-width: 0; min-height: 0; overflow: hidden;"

	htmltools::browsable(
		htmltools::tagList(
			htmltools::tags$style("body { margin: 0; padding: 0; }"),
			htmltools::div(
				style = grid_style,
				lapply(widgets, function(w) htmltools::div(style = item_style, w))
			)
		)
	)
}
