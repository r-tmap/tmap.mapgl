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
#' @importFrom utils head tail
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
	if (show && !knit) {
		print(ms2)
	}
	ms2
}

map_layout <- function(..., orientation = c("vertical", "horizontal")) {
	orientation <- match.arg(orientation)

	flex_direction <- if (orientation == "horizontal") "column" else "row"

	flex_style <- sprintf(
		"display: flex; flex-direction: %s; gap: 10px; width: 100vw; height: 100vh; overflow: hidden; box-sizing: border-box;",
		flex_direction
	)

	maps <- list(...)
	item_style <- "flex: 1; min-width: 0; min-height: 0; overflow: hidden;"

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
