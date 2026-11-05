tmapMapboxCompPrepare = function(comp, o) {
	UseMethod("tmapMapboxCompPrepare")
}

tmapMapboxCompHeight = function(comp, o) {
	UseMethod("tmapMapboxCompHeight")
}

tmapMapboxCompWidth = function(comp, o) {
	UseMethod("tmapMapboxCompWidth")
}

tmapMapboxCompPlot = function(comp, m, o) {
	UseMethod("tmapMapboxCompPlot")
}


#' @export
tmapMapboxCompPrepare.tm_legend_portrait = function(comp, o) {
	mapgl_legend_comp(comp, o, mode = "mapbox")
}


#' @export
tmapMapboxCompHeight.tm_legend_portrait = function(comp, o) {
	mapgl_leg_height(comp, o)
}



#' @export
tmapMapboxCompWidth.tm_legend_portrait = function(comp, o) {
	mapgl_leg_width(comp, o)

}


#' @export
tmapMapboxCompPrepare.tm_legend_landscape = function(comp, o) {
	mapgl_legend_comp(comp, o, mode = "mapbox")
}

#' @export
tmapMapboxCompHeight.tm_legend_landscape = function(comp, o) {
	mapgl_leg_height(comp, o)
}



#' @export
tmapMapboxCompWidth.tm_legend_landscape = function(comp, o) {
	mapgl_leg_width(comp, o)

}










tmapMaplibreCompPrepare = function(comp, o) {
	UseMethod("tmapMaplibreCompPrepare")
}

tmapMaplibreCompHeight = function(comp, o) {
	UseMethod("tmapMaplibreCompHeight")
}

tmapMaplibreCompWidth = function(comp, o) {
	UseMethod("tmapMaplibreCompWidth")
}

tmapMaplibreCompPlot = function(comp, m, o) {
	UseMethod("tmapMaplibreCompPlot")
}


#' @export
tmapMaplibreCompPrepare.tm_legend_portrait = function(comp, o) {
	mapgl_legend_comp(comp, o, mode = "maplibre")
}


#' @export
tmapMaplibreCompHeight.tm_legend_portrait = function(comp, o) {
	mapgl_leg_height(comp, o)
}



#' @export
tmapMaplibreCompWidth.tm_legend_portrait = function(comp, o) {
	mapgl_leg_width(comp, o)
}


#' @export
tmapMaplibreCompPrepare.tm_legend_landscape = function(comp, o) {
	mapgl_legend_comp(comp, o, mode = "maplibre")
}

#' @export
tmapMaplibreCompHeight.tm_legend_landscape = function(comp, o) {
	mapgl_leg_height(comp, o)
}



#' @export
tmapMaplibreCompWidth.tm_legend_landscape = function(comp, o) {
	mapgl_leg_width(comp, o)

}

mapgl_leg_height = function (comp, o) {
	if (is.na(comp$height)) {
		if (comp$type == "bivariate") {
			comp$height = 20 + 22 * attr(comp$gp$fill, "m") + 60 + 25 * (number_text_lines(comp$title) > 0)
		} else if (comp$type == "gradient") {
			comp$height = 5.5 * 20
		} else if (comp$type == "pattern") {
			comp$height = 20 + 28 * comp$nitems + 30 * (number_text_lines(comp$title) > 0) + 20 * !is.null(comp[["dot_info"]])
		} else {
			comp$height = (number_text_lines(comp$title) + 2 + comp$nitems * 1.25) * 20
		}
	}
	comp
}


mapgl_leg_width = function (comp, o) {
	if (is.na(comp$width)) {
		if (comp$type == "bivariate") {
			m = attr(comp$gp$fill, "m")
			comp$width = 40 + 22 * attr(comp$gp$fill, "n") + 7 * max(nchar(comp$labels[seq_len(m)]))
		} else if (comp$type == "gradient") {
			comp$width = 250
		} else {
			comp$width = 75 + max(nchar(c(comp$title, comp$labels))) * 6
		}
	}
	comp
}


mapgl_legend_comp = function(comp, o, mode) {
	within(comp, {
		is_biv = "biv" %in% names(attributes(gp$fill))

		nuq = vapply(comp$gp, length, FUN.VALUE = integer(1))
		varying = names(nuq)[which(nuq>1)]

		if (all(c("col", "fill") %in% varying)) {
			cli::cli_inform("Legend in mode {.val {mode}} doesn't support both fill and col varying. Setting col to the first value")
			nuq["col"] = 1
			varying = setdiff(varying, "col")
			gp$col = gp$col[1]
		}


		# pattern legends: legends of the pattern map variables (other legends with a constant pattern keep
		# their (interactive) color patches)
		is_pat = any(c("pattern", "pattern_col", "pattern_size", "pattern_density") %in% varying) &&
			is.character(gp$pattern) && any(!is.na(gp$pattern) & gp$pattern != "none")

		type = if (is_biv) {
			"bivariate"
		} else if (is_pat) {
			"pattern"
		} else if (!any(c("col", "fill", "lwd", "size") %in% varying)) {
			cli::cli_inform("No legends available in mode {.val {mode}} for map variables {.val {varying}}")
			"none"
		} else if ((!is.na(gp$fill[1]) && any(nchar(gp$fill) > 50)) || (!is.na(gp$fill_alpha[1]) && any(nchar(gp$fill_alpha) > 50)) ||
				   (!is.na(gp$col[1]) && any(nchar(gp$col) > 50)) || (!is.na(gp$col_alpha[1]) && any(nchar(gp$col_alpha) > 50))) {
			#message("Continuous legend not implemented in mapbox mode")
			"gradient"
		} else if (any(c("bgcol", "bgcol_alpha") %in% varying)) {
			"none"
		} else if ("lwd" %in% varying || ("tm_data_lines" %in% mfun)) {
			"lines"
		} else {
			"symbols"
		}

		if (type == "none") show = FALSE

		bg.color = do.call("process_color", c(list(col=bg.color), o$pc))
		title.color = do.call("process_color", c(list(col=title.color), o$pc))
		text.color = do.call("process_color", c(list(col=text.color), o$pc))

		gp2 = if (is_biv || is_pat) list() else gp_to_lpar(gp, mfun = comp$mfun, shape = comp$item.shape)
		rm(is_biv, is_pat)
	})
}
