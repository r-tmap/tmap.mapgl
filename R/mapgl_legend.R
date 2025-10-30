mapgl_pos = function(pos) {
	if (is.character(pos)) pos = tm_pos_in(pos[1], pos[2])
	if (pos$type %in% c("out", "autoout")) {
		sel = c("cell.v", "cell.h")
	} else {
		sel = c("pos.v", "pos.h")
	}
	x = tolower(unlist(pos[sel]))

	if (x[1] %in% c("center", "centre")) x[1] = "top"
	if (x[2] %in% c("center", "centre")) x[2] = "left"

  paste(x, collapse = "-")
}

cont_split = function(x) strsplit(x, split = "_", fixed=TRUE)

gp_to_lpar = function(gp, mfun, shape = 20, pick_middle = TRUE) {
	# create a list of gp elements

	lst = c(list(fillColor = {if (!all(is.na(gp$fill))) gp$fill else "#000000"},
				 color = {if (!all(is.na(gp$col))) gp$col else "#000000"},
				 fillOpacity = {if (!all(is.na(gp$fill_alpha))) gp$fill_alpha else 0},
				 opacity = {if (!all(is.na(gp$col_alpha))) gp$col_alpha else 0},
				 'stroke-width' = {if (!all(is.na(gp$lwd))) gp$lwd else 0},
				 'stroke-dasharray' = {if (!all(is.na(gp$lty))) lty2dash(gp$lty) else "none"},
				 size = {if (!all(is.na(gp$size))) gp$size else 1},
				 shape = {if (!all(is.na(gp$shape))) gp$shape else shape}))

	lst_isnum = c(fillColor = FALSE,
				  color = FALSE,
				  fillOpacity = TRUE,
				  opacity = TRUE,
				  'stroke-width' = TRUE,
				  'stroke-dash' = FALSE,
				  size = TRUE,
				  shape = TRUE)

	lst = mapply(function(lsti, isnum) {
		if (!is.character(lsti)) return(lsti)

		if (nchar(lsti[1]) > 50) {
			x = cont_split(lsti)
			x = lapply(x, function(i) {
				i[i=="NA"] <- NA
				i
			})
			if (isnum) x = lapply(x, as.numeric)
			if (pick_middle) {
				x = sapply(x, function(i) {
					if (all(is.na(i))) NA else {
						sq = c(5,6,4,7,3,8,2,9,1,10) # priority for middle values
						i[sq[which(!is.na(i)[sq])[1]]]
					}
				})
			}
			return(x)

		} else {
			return(lsti)
		}
	}, lst, lst_isnum[names(lst)], SIMPLIFY = FALSE)

	pch2shp = c("rect", "circle", "triangle", "plus", "cross", "diamond", "triangle",
				"cross", "star", "diamond", "circle", "polygon", "plus", "cross",
				"triangle", "rect", "circle", "triangle", "diamond", "circle",
				"circle", "circle", "rect", "diamond", "triangle", "polygon", "stadium") # shapes for pch 0:25 + 26 for stadium (NOTE: last one is a triangle upside-down. Since 21:25 are the defaults, and a polygon is chosen to differentiate from the other triangle)
	lst$shape = get_pch_names(lst$shape)

	if ("tm_data_lines" %in% mfun) lst$shape = "line"

	lst$width = lst$size * 20
	lst$height = lst$size * 20
	#lst$width[]
	lst$size = NULL
	lst
}



make_equal_list = function(x) {
	cls = class(x)
	n = max(vapply(x, length, integer(1)))
	structure(lapply(x, rep, length.out = n), class = cls)
}




mapgl_legend = function(cmp, m, o, orientation, mode) {

	legpos = mapgl_pos(cmp$position)

	m2 = if (cmp$type == "none") {
		#message("Text based legends not supported in view mode")
		m
	} else if (cmp$type == "gradient") {

		colVary = length(cmp$gp2$color) > 1L
		if (colVary) cmp$gp2$fillColor = cmp$gp2$color

		# remove na
		if (cmp$na.show) {
			labs = head(cmp$labels, -1)
			cols = head(cmp$gp2$fillColor, -1)
		} else {
			labs = cmp$labels
			cols = cmp$gp2$fillColor
		}
		colsNA = is.na(cols)
		if (any(colsNA)) {
			labs = labs[!colsNA]
			cols = cols[!colsNA]
		}

		m |> mapgl::add_continuous_legend(legend_title = cmp$title, values = labs, colors = cols, add = TRUE, position = legpos,
										  margin_top = cmp$margin_top,
										  margin_bottom = cmp$margin_bottom,
										  margin_left = cmp$margin_left,
										  margin_right = cmp$margin_right)
	} else if (cmp$type == "lines") {
		colVary = length(cmp$gp2$color) > 1L
		gp2 = make_equal_list(cmp$gp2)
		if (colVary) gp2$fillColor = gp2$color

		circular_patches = !any(is.na(cmp$gp$shape)) && all(cmp$gp$shape %in% c(1, 10, 16, 19:21))

		m |> mapgl::add_categorical_legend(colors = gp2$fillColor, values = cmp$labels, position = legpos, legend_title = cmp$title,
										   circular_patches = circular_patches, add = TRUE,patch_shape = "line", sizes = cmp$gp$lwd,
										   margin_top = cmp$margin_top,
										   margin_bottom = cmp$margin_bottom,
										   margin_left = cmp$margin_left,
										   margin_right = cmp$margin_right)
	} else { # "symbols"
		colVary = length(cmp$gp2$color) > 1L
		gp2 = make_equal_list(cmp$gp2)
		if (colVary) gp2$fillColor = gp2$color

		patches = if (!any(is.na(cmp$gp$shape)) && all(cmp$gp$shape %in% c(1, 10, 16, 19:21))) "circle" else "square"

		sizes = if (!is.na(cmp$gp$size[1])) cmp$gp$size * 20 else NULL
		m |> mapgl::add_legend(colors = gp2$fillColor, values = cmp$labels, position = legpos, legend_title = cmp$title, type = "categorical", patch_shape = patches, add = TRUE,
							   sizes = sizes,
							   margin_top = cmp$margin_top,
							   margin_bottom = cmp$margin_bottom,
							   margin_left = cmp$margin_left,
							   margin_right = cmp$margin_right)
	}
	m2

}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompPlot.tm_legend_portrait = function(comp, m, o) {
	mapgl_legend(comp, m, o, orientation = "vertical", mode = "mapbox")
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxCompPlot.tm_legend_landscape = function(comp, m, o) {
	mapgl_legend(comp, m, o, orientation = "horizontal", mode = "mapbox")
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompPlot.tm_legend_portrait = function(comp, m, o) {
	mapgl_legend(comp, m, o, orientation = "vertical", mode = "mapbox")
}

#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMaplibreCompPlot.tm_legend_landscape = function(comp, m, o) {
	mapgl_legend(comp, m, o, orientation = "horizontal", mode = "mapbox")
}

#' @param facet_row,facet_col,facet_page row column and page id
#' @param class class
#' @param stack stack
#' @param stack_auto stack_auto
#' @param pos.h pos.h
#' @param pos.v pos.v
#' @param bbox bbox
#' @export
#' @keywords internal
#' @rdname tmapMapbox
tmapMapboxComp = function(comp, o, facet_row = NULL, facet_col = NULL, facet_page, class, stack, stack_auto, pos.h, pos.v, bbox) {
	mapgl_comp(comp = comp,
			   o = o,
			   facet_row = facet_row,
			   facet_col = facet_col,
			   facet_page = facet_page,
			   class = class,
			   stack = stack,
			   stack_auto = stack_auto,
			   pos.h = pos.h,
			   pos.v = pos.v,
			   bbox = bbox,
			   mode = "mapbox")}

#' @export
#' @keywords internal
#' @name tmapMapboxLegend
#' @rdname tmapMapbox
tmapMaplibreComp = function(comp, o, facet_row = NULL, facet_col = NULL, facet_page, class, stack, stack_auto, pos.h, pos.v, bbox) {
	mapgl_comp(comp = comp,
			   o = o,
			   facet_row = facet_row,
			   facet_col = facet_col,
			   facet_page = facet_page,
			   class = class,
			   stack = stack,
			   stack_auto = stack_auto,
			   pos.h = pos.h,
			   pos.v = pos.v,
			   bbox = bbox,
			   mode = "maplibre")
}


mapgl_comp = function(comp, o, facet_row = NULL, facet_col = NULL, facet_page, class, stack, stack_auto, pos.h, pos.v, bbox, mode) {

		### from tmapGridComp, migrate to tmap generic
	    # get component group settings
		grp = comp[[1]][c("position",
						  "stack",
						  "frame_combine",
						  "equalize",
						  "resize_as_group",
						  "stack_margin",
						  "offset",
						  "frame" ,
						  "frame.color",
						  "frame.lwd",
						  "frame.r",
						  "bg",
						  "bg.color",
						  "bg.alpha")]

		any_legend_chart_inset = any(vapply(comp, inherits, FUN.VALUE = logical(1), c("tm_legend", "tm_chart", "tm_inset")))
		grp_called = setdiff(unique(do.call(c, lapply(comp, FUN = "[[", "called_via_comp_group"))), "group_id")

		if (!("frame" %in% grp_called)) grp$frame = any_legend_chart_inset
		if (!("bg" %in%grp_called)) grp$bg = any_legend_chart_inset
	    ###

	m = get_mapgl(facet_row, facet_col, facet_page, mode = mode)
	rc_text = frc(facet_row, facet_col)

	stack = stack[1]

	legpos = mapgl_pos(comp[[1]]$position) # should be identical over components

	os = 0
	cred = 35
	for (cmp in comp) {
		if (is.null(cmp$height) || is.na(cmp$height)) cmp$height = 100
		if (is.null(cmp$width) || is.na(cmp$width)) cmp$width = 500

		if (stack == "vertical") {
			if (legpos %in% c("bottom-right", "bottom-left")) {
				cmp$margin_bottom = os + ifelse(legpos == "bottom-right", cred, 0)
				cmp$margin_top = 0
			} else {
				cmp$margin_top = os
				cmp$margin_bottom =  ifelse(legpos == "bottom-right", cred, 0)
			}
			cmp$margin_left = 0
			cmp$margin_right = 0
			os = os + cmp$height
		} else {
			if (legpos %in% c("top-left", "bottom-left"))  {
				cmp$margin_left = os
				cmp$margin_right = 0
			} else {
				cmp$margin_right = os
				cmp$margin_left = 0
			}
			cmp$margin_top = 0
			cmp$margin_bottom = ifelse(legpos == "bottom-right", cred, 0)
			os = os + cmp$width
		}


		if (mode == "maplibre") {
			m = tmapMaplibreCompPlot(cmp, m, o)
		} else {
			m = tmapMapboxCompPlot(cmp, m, o)
		}
	}

	assign_mapgl(m, facet_row, facet_col, facet_page, mode = mode)
	NULL
}

