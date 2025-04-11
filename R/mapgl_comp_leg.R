tmapMapboxCompPrepare = function(comp, o) {
	UseMethod("tmapMapboxCompPrepare")
}

tmapMapboxCompHeight = function(comp, o) {
	UseMethod("tmapMapboxCompHeight")
}

tmapMapboxCompWidth = function(comp, o) {
	UseMethod("tmapMapboxCompWidth")
}

tmapMapboxLegPlot = function(comp, m, o) {
	UseMethod("tmapMapboxLegPlot")
}


#' @export
tmapMapboxCompPrepare.tm_legend_standard_portrait = function(comp, o) {
	mapgl_legend_comp(comp, o)
}


#' @export
tmapMapboxCompHeight.tm_legend_standard_portrait = function(comp, o) {
	comp
}



#' @export
tmapMapboxCompWidth.tm_legend_standard_portrait = function(comp, o) {
	comp
}


#' @export
tmapMapboxCompPrepare.tm_legend_standard_landscape = function(comp, o) {
	mapgl_legend_comp(comp, o)
}

#' @export
tmapMapboxCompHeight.tm_legend_standard_landscape = function(comp, o) {
	comp
}



#' @export
tmapMapboxCompWidth.tm_legend_standard_landscape = function(comp, o) {
	comp
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

tmapMaplibreLegPlot = function(comp, m, o) {
	UseMethod("tmapMaplibreLegPlot")
}


#' @export
tmapMaplibreCompPrepare.tm_legend_standard_portrait = function(comp, o) {
	mapgl_legend_comp(comp, o)
}


#' @export
tmapMaplibreCompHeight.tm_legend_standard_portrait = function(comp, o) {
	comp
}



#' @export
tmapMaplibreCompWidth.tm_legend_standard_portrait = function(comp, o) {
	comp
}


#' @export
tmapMaplibreCompPrepare.tm_legend_standard_landscape = function(comp, o) {
	mapgl_legend_comp(comp, o)
}

#' @export
tmapMaplibreCompHeight.tm_legend_standard_landscape = function(comp, o) {
	comp
}



#' @export
tmapMaplibreCompWidth.tm_legend_standard_landscape = function(comp, o) {
	comp
}




mapgl_legend_comp = function(comp, o) {
	within(comp, {
		if ("biv" %in% names(attributes(gp$fill))) {
			warning("Bivariate legend not implemented for mapbox mode", call. = FALSE)
			show = FALSE
		}

		nuq = vapply(comp$gp, length, FUN.VALUE = integer(1))
		varying = names(nuq)[which(nuq>1)]

		if (all(c("col", "fill") %in% varying)) {
			message("Legend in mapbox mode doesn't support both fill and col varying. Setting col to the first value")
			nuq["col"] = 1
			varying = setdiff(varying, "col")
			gp$col = gp$col[1]
		}

		type = if (!any(c("col", "fill") %in% varying)) {
			message("Only color legends supported in mapbox mode")
			"none"
		} else if ((!is.na(gp$fill[1]) && any(nchar(gp$fill) > 50)) || (!is.na(gp$fill_alpha[1]) && any(nchar(gp$fill_alpha) > 50)) ||
				   (!is.na(gp$col[1]) && any(nchar(gp$col) > 50)) || (!is.na(gp$col_alpha[1]) && any(nchar(gp$col_alpha) > 50))) {
			#message("Continuous legend not implemented in mapbox mode")
			"gradient"
		} else if (any(c("bgcol", "bgcol_alpha") %in% varying)) {
			"none"
		} else {
			"symbols"
		}

		bg.color = do.call("process_color", c(list(col=bg.color), o$pc))
		title.color = do.call("process_color", c(list(col=title.color), o$pc))
		text.color = do.call("process_color", c(list(col=text.color), o$pc))

		gp2 = gp_to_lpar(gp, mfun = comp$mfun, shape = comp$item.shape)
	})
}
