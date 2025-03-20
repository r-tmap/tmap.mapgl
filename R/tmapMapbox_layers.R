#' @param shpTM, dt, pdt, popup.format, hdt, idt, gp args
#' @export
#' @keywords internal
#' @name tmapMapboxPolygons
#' @rdname tmapMapbox
tmapMapboxPolygons = function(shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	mapbox = get_mapbox(facet_row, facet_col, facet_page)

	rc_text = frc(facet_row, facet_col)

	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt

	popups = NULL

	x = sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1,3)]), bbx[2]))), crs = sf::st_crs(bbx))
	y = sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1,3)]), bbx[4]))), crs = sf::st_crs(bbx))
	dist_m = suppressWarnings(units::drop_units(units::set_units(sf::st_distance(x, y), "m")[1,1]))

	lineScale = dist_m * o$lineH / 24

	gp = impute_gp(gp, dt)
	gp = rescale_gp(gp, o$scale_down)

	shp2 = sf::st_sf(id = 1:length(shp), geometry = shp)
	shp2$fill = gp$fill
	shp2$col = gp$col
	shp2$lwd = gp$lwd

	mapbox |> mapbox::add_geojson_layer(data = shp2,
									  getFillColor = mapbox::JS("object => object.fill"),
									  getLineColor = mapbox::JS("object => object.col"),
									  getLineWidth = mapbox::JS("object => object.lwd"),
									  lineWidthScale = lineScale) |>
		assign_mapbox(facet_row, facet_col, facet_page)
	NULL
}

#' @export
#' @keywords internal
#' @name tmapMapboxLines
#' @rdname tmapMapbox
tmapMapboxLines = function(shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	mapbox = get_mapbox(facet_row, facet_col, facet_page)

	rc_text = frc(facet_row, facet_col)

	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt

	popups = NULL

	x = sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1,3)]), bbx[2]))), crs = sf::st_crs(bbx))
	y = sf::st_sfc(list(sf::st_point(c(mean(bbx[c(1,3)]), bbx[4]))), crs = sf::st_crs(bbx))
	dist_m = suppressWarnings(units::drop_units(units::set_units(sf::st_distance(x, y), "m")[1,1]))

	lineScale = dist_m * o$lineH / 24

	gp = impute_gp(gp, dt)
	gp = rescale_gp(gp, o$scale_down)

	shp2 = sf::st_sf(id = 1:length(shp), geometry = shp)
	shp2$fill = gp$fill
	shp2$col = gp$col
	shp2$lwd = gp$lwd

	mapbox |> mapbox::add_geojson_layer(data = shp2,
									  getFillColor = mapbox::JS("object => object.fill"),
									  getLineColor = mapbox::JS("object => object.col"),
									  getLineWidth = mapbox::JS("object => object.lwd"),
									  lineWidthScale = lineScale) |>
		assign_mapbox(facet_row, facet_col, facet_page)
	NULL
}




lty2dash = function(lty) {
	tab = c(solid = "", dashed = "4 4", dotted = "1 3", dotdash = "1 3 4 3", longdash = "7 3", twodash = "2 2 6 2")
	are_words = (lty %in% names(tab))
	if (all(are_words)) {
		unname(tab[lty])
	} else {
		are_letters = (suppressWarnings(!is.na(as.numeric(lty))))

		if (!all(are_letters | are_words)) {
			stop("Incorrect lty specification: ", lty[which(!are_letters & !are_words)[1]])
		} else {
			lty[are_words] = unname(tab[lty[are_words]])
			lty[are_letters] = vapply(strsplit(lty[are_letters], ""), FUN = function(x) paste(x, collapse = " "), FUN.VALUE = character(1))
		}
		lty
	}

}

#' @export
#' @keywords internal
#' @name tmapMapboxLines
#' @rdname tmapMapbox
tmapMapboxLines = function(shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	# lf = get_lf(facet_row, facet_col, facet_page)
	#
	# rc_text = frc(facet_row, facet_col)
	#
	# res = select_sf(shpTM, dt)
	# shp = res$shp
	# dt = res$dt
	#
	# if (is.null(pdt)) {
	# 	popups = NULL
	# } else {
	# 	pdt = pdt[match(dt$tmapID__, pdt$tmapID__)][, tmapID__ := NULL]
	# 	popups = view_format_popups(id = hdt, titles = names(pdt), values = pdt, format = popup.format)
	# }
	#
	# gp = impute_gp(gp, dt)
	# gp = rescale_gp(gp, o$scale_down)
	#
	# opt = Mapbox::pathOptions(interactive = TRUE, pane = pane)
	#
	# if (o$use.WebGL) {
	# 	shp2 = sf::st_sf(id = 1:length(shp), geom = shp)
	# 	shp3 = suppressWarnings(sf::st_cast(shp2, "LINESTRING"))
	# 	gp3 = lapply(gp, function(gpi) {if (length(gpi) == 1) gpi else gpi[shp3$id]})
	# 	lf %>%
	# 		leafgl::addGlPolylines(data = shp3, color = gp3$col, opacity = gp3$col_alpha, weight = gp3$lwd, pane = pane, group = group) %>%
	# 		assign_lf(facet_row, facet_col, facet_page)
	# } else {
	# 	lf %>%
	# 		Mapbox::addPolylines(data = shp, layerId = idt, label = hdt, color = gp$col, opacity = gp$col_alpha, weight = gp$lwd, group = group, options = opt, dashArray = lty2dash(gp$lty), popup = popups) %>%
	# 		assign_lf(facet_row, facet_col, facet_page)
	#
	# }
	NULL
}

#' @export
#' @keywords internal
#' @name tmapMapboxSymbols
#' @rdname tmapMapbox
tmapMapboxSymbols = function(shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	# lf = get_lf(facet_row, facet_col, facet_page)
	#
	# rc_text = frc(facet_row, facet_col)
	#
	# res = select_sf(shpTM, dt[!is.na(dt$size), ])
	# shp = res$shp
	# dt = res$dt
	#
	# if (is.null(pdt)) {
	# 	popups = NULL
	# } else {
	# 	pdt = pdt[match(dt$tmapID__, pdt$tmapID__)][, tmapID__ := NULL]
	#
	# 	popups = view_format_popups(id = hdt, titles = names(pdt), values = pdt, format = popup.format)
	# }
	#
	# gp = impute_gp(gp, dt)
	# gp = rescale_gp(gp, o$scale_down)
	#
	# coords = sf::st_coordinates(shp)
	#
	# opt = Mapbox::pathOptions(interactive = TRUE, pane = pane)
	#
	# gp2 = gp_to_lpar(gp, mfun = "Symbols")
	# #gp = gp2leafgp(gp)
	# names(gp2)[names(gp2) == 'stroke-width'] = "strokeWidth"
	# gp2$baseSize = 20
	#
	#
	#
	# if (o$use.WebGL) {
	# 	vary = vapply(dt, function(x)any(x!=x[1]), FUN.VALUE = logical(1))[c("col", "shape", "lwd", "lty", "fill_alpha", "col_alpha")]
	# 	if (any(vary)) warning("WegGL enabled: the only supported visual variables are: fill and size. The visual variable(s) ", paste(names(vary)[vary], collapse = ", "), " are not supported. Disable WebGL to show them.", call. = FALSE)
	# 	lf %>% leafgl::addGlPoints(sf::st_sf(shp), fillColor = gp2$fillColor, radius = gp2$width, fillOpacity = gp2$fillOpacity[1], pane = pane, group = group) %>%
	# 		assign_lf(facet_row, facet_col, facet_page)
	# } else {
	#
	# 	sn = suppressWarnings(as.numeric(gp2$shape))
	#
	# 	sid = which(!is.na(sn))
	#
	# 	gp2$shape[sid] = "circle" # as dummy
	# 	symbols = do.call(leaflegend::makeSymbolIcons, gp2)
	#
	# 	symbols$iconWidth = rep(NA, length(symbols$iconUrl))
	# 	symbols$iconHeight = rep(NA, length(symbols$iconUrl))
	#
	# 	if (length(sid)) {
	# 		iconLib <- get("shapeLib", envir = .TMAP)[sn[sid]-999]
	# 		symbols_icons <- merge_icons(iconLib)
	# 		size = gp2$width[sid] / gp2$baseSize
	#
	# 		for (i in seq_along(sid)) {
	# 			symbols$iconUrl[sid[i]] = symbols_icons$iconUrl[i]
	# 			symbols$iconWidth[sid[i]] <- symbols_icons$iconWidth[i] * size[i]
	# 			symbols$iconHeight[sid[i]] <- symbols_icons$iconHeight[i] * size[i]
	# 			if (all(c("iconAnchorX", "iconAnchorY") %in% names(symbols_icons))) {
	# 				symbols$iconAnchorX[sid[i]] <- symbols_icons$iconAnchorX[i] * size[i]
	# 				symbols$iconAnchorY[sid[i]] <- symbols_icons$iconAnchorY[i] * size[i]
	#
	# 			}
	# 		}
	# 	}
	#
	# 	lf %>% Mapbox::addMarkers(lng = coords[, 1], lat = coords[, 2],
	# 							  icon = symbols, group = group, layerId = idt, label = hdt, popup = popups) %>%
	# 		assign_lf(facet_row, facet_col, facet_page)
	#
	# }

	NULL
}

split_alpha_channel <- function(x, alpha) {
	if (is.null(x)) {
		list(col=NULL, opacity=0)
	} else {
		RGBA <- col2rgb(x, alpha = TRUE)
		col <- rgb(RGBA[1,], RGBA[2,], RGBA[3,], maxColorValue = 255)
		opacity <- unname(RGBA[4,]/255 * alpha)
		list(col=col, opacity=opacity)
	}
}

#' @export
#' @keywords internal
#' @name tmapMapboxRaster
#' @rdname tmapMapbox
tmapMapboxRaster = function(shpTM, dt, gp, pdt, popup.format, hdt, idt, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	#
	# rc_text = frc(facet_row, facet_col)
	#
	#
	# #bb_target <- bbx #attr(shp, "bbox")
	# #bb_real <- bbx #sf::st_bbox(shp)
	#
	# shp = shpTM$shp
	# tmapID = shpTM$tmapID
	#
	# if (is_regular_grid(shp)) {
	#
	# 	tid = intersect(tmapID, dt$tmapID__)
	#
	# 	color = rep(NA, length(tmapID)) # NA
	#
	# 	sel = which(tmapID %in% tid)
	# 	tid2 = tmapID[sel]
	#
	# 	color[sel] = dt$col[match(tid2, dt$tmapID__)]
	#
	#
	# 	#color = rep("#FFFFFF", length(tmapID))
	# 	#color[match(dt$tmapID__, tmapID)] = dt$color
	#
	# 	pal <- na.omit(unique(color))
	# 	pal <- pal[substr(pal, 8,10)!="00"] ## remove transparant colors
	#
	# 	if (!length(pal)) return(NULL)
	#
	# 	res <- split_alpha_channel(pal, alpha = 1)
	# 	pal_col <- res$col
	# 	pal_opacity <- if (length(res$opacity) == 0L) 0 else max(res$opacity)
	#
	# 	if ("col_alpha" %in% names(dt)) pal_opacity = max(dt$col_alpha)
	#
	#
	# 	col_ids <- match(color, pal)
	#
	# 	m <- matrix(col_ids, ncol = ncol(shp))
	#
	# 	#matrix(color, ncol = ncol(shp))
	#
	# 	#m <- matrix(tmapID, ncol = ncol(shp))
	#
	#
	# 	#m = tmapID
	#
	# 	#m[1,5] = 4
	#
	# 	shp2 = st_as_stars(m, dimensions = shp)
	#
	# 	lf = get_lf(facet_row, facet_col, facet_page)
	#
	# 	lf %>%
	# 		leafem::addStarsImage(shp2, band = 1, colors = pal_col, opacity = pal_opacity, group = group) %>%
	# 		assign_lf(facet_row, facet_col, facet_page)
	# } else {
	# 	#shp2 = st_as_stars(list(values = tmapID), dimensions = shp)
	# 	#shpTM <- shapeTM(sf::st_geometry(sf::st_as_sf(shp2)), as.vector(tmapID))
	#
	# 	m = matrix(tmapID, nrow = nrow(shp), ncol = ncol(shp))
	# 	shp2 = structure(list(tmapID = m), class = "stars", dimensions = shp)
	#
	# 	shp3 = sf::st_geometry(sf::st_as_sf(shp2))
	#
	# 	crs = get_option_class(tmap_options_mode("view")$crs, "sf")
	#
	# 	shpTM = shapeTM(sf::st_transform(shp3, crs), tmapID)
	#
	#
	# 	gp$lty = "solid"
	# 	tmapMapboxPolygons(shpTM, dt, pdt, popup.format = NULL, hdt = NULL, idt = NULL, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o)
	# 	#grid.shape(s, gp=gpar(fill=color, col=NA), bg.col=NA, i, k)
	# }
	NULL
}

#' @export
#' @keywords internal
#' @name tmapMapboxText
#' @rdname tmapMapbox
tmapMapboxText = function(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	# args = list(...)
	#
	# lf = get_lf(facet_row, facet_col, facet_page)
	#
	# rc_text = frc(facet_row, facet_col)
	#
	# res = select_sf(shpTM, dt)
	# shp = res$shp
	# dt = res$dt
	#
	# #shadow = gp$shadow
	#
	# gp = impute_gp(gp, dt)
	# gp = rescale_gp(gp, o$scale_down)
	# text = as.character(gp$text)
	#
	# coords = sf::st_coordinates(shp)
	#
	# opt = Mapbox::pathOptions(interactive = TRUE, pane = pane)
	#
	# cex_set = unique(gp$cex)
	# alpha_set = unique(gp$col_alpha)
	# face_set = unique(gp$fontface)
	# col_set = unique(gp$col)
	#
	# bgcol_set = unique(gp$bgcol)
	#
	# if (any(bgcol_set != "#00000000")) {
	# 	message("Variable bgcol and bgcol_alpha not supported by view mode")
	# }
	#
	#
	# if (length(face_set) != 1) message("Variable fontfaces not supported by view mode")
	#
	# vary = (length(cex_set) != 1) || (length(alpha_set) != 1) || (length(face_set) != 1) || (length(col_set) != 1)
	#
	# sizeChar <- paste(round(gp$cex * 12), "px", sep="")
	#
	# # direction <- ifelse(gpl$text.just == "left", "right",
	# # 			 ifelse(gpl$text.just == "right", "left",
	# # 			 ifelse(gpl$text.just == "top", "bottom",
	# # 			 ifelse(gpl$text.just == "bottom", "top", "center"))))
	# direction = "right"
	#
	# clustering = args$clustering
	#
	# if (identical(clustering, FALSE)) {
	# 	clustering = NULL
	# } else if (identical(clustering, TRUE)) {
	# 	clustering = Mapbox::markerClusterOptions()
	# }
	#
	#
	#
	#
	#
	# if (!vary) {
	# 	lf = lf %>% addLabelOnlyMarkers(lng = coords[, 1], lat = coords[,2],
	# 									 label=text,
	# 									 group=group,
	# 									 #layerId = ids,
	# 									 labelOptions = labelOptions(noHide = TRUE,
	# 									 							textOnly = TRUE,
	# 									 							direction = direction,
	# 									 							opacity=gp$col_alpha[1],
	# 									 							textsize=sizeChar[1],
	# 									 							style=list(color=gp$col[1])),
	# 									 clusterOptions = clustering,
	# 									 options = markerOptions(pane = pane))
	# } else {
	# 	for (i in 1:length(text)) {
	# 		lf = lf %>% addLabelOnlyMarkers(lng = coords[i,1], lat = coords[i,2],
	# 										 label=text[i],
	# 										 group=group,
	# 										 #layerId = ids[i],
	# 										 labelOptions = labelOptions(noHide = TRUE,
	# 										 							textOnly = TRUE,
	# 										 							direction = direction,
	# 										 							opacity=gp$col_alpha[i],
	# 										 							textsize=sizeChar[i],
	# 										 							style=list(color=gp$col[i])),
	# 										 clusterOptions = clustering,
	# 										 options = markerOptions(pane = pane))
	# 	}
	# }
	# assign_lf(lf, facet_row, facet_col, facet_page)
	#


	NULL
}
