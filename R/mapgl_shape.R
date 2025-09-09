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

mapgl_shape = function(bbx, facet_row, facet_col, facet_page, o, mode) {
	m = get_mapgl(facet_row, facet_col, facet_page, mode)

	bbx = sf::st_bbox(sf::st_transform(tmaptools::bb_poly(bbx), crs = 4326))
	ll = unname(c(mean(bbx[c(1,3)]), mean(bbx[c(2,4)])))
	zoom = findZoom(bbx)


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



	style = if (mode == "mapbox") {
		if (substr(e$style, 1, 4) == "ofm.") {
			paste0("https://tiles.openfreemap.org/styles/", substr(e$style, 5, nchar(e$style)))
		} else if (e$style %in% tmap_providers()) {
			mapgl::mapbox_style(get_style(e$style))
		} else {
			e$style
		}

	} else {
		if (substr(e$style, 1, 4) == "ofm.") {
			paste0("https://tiles.openfreemap.org/styles/", substr(e$style, 5, nchar(e$style)))
		} else if (e$style %in% tmap_providers()) {
			mapgl::carto_style(get_style(e$style))
		} else {
			e$style
		}
	}

	if (mode == "mapbox") {
		# quick & dirty
		if (zoom < 3) {
			# ignore center for global view (otherwise it will be (0, -3) due to Antarctica)
			m = mapgl::mapboxgl(center = c(0,0), zoom = zoom, pitch = o$pitch, style = style) |>
				mapgl::add_navigation_control(visualize_pitch = TRUE) |>
				mapgl::set_projection(crs)
		} else {
			m = mapgl::mapboxgl(center = ll, zoom = zoom, pitch = o$pitch, style = style) |>
				mapgl::add_navigation_control(visualize_pitch = TRUE) |>
				mapgl::set_projection(crs)
		}
	} else {
		# quick & dirty
		if (zoom < 3) {
			# ignore center for global view (otherwise it will be (0, -3) due to Antarctica)
			m = mapgl::maplibre(center = c(0,0), zoom = zoom, pitch = o$pitch, style = style) |>
				mapgl::add_navigation_control(visualize_pitch = TRUE) |>
				mapgl::set_projection(crs) |>
				mapgl::add_globe_control()
		} else {
			m = mapgl::maplibre(center = ll, zoom = zoom, pitch = o$pitch, style = style) |>
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
