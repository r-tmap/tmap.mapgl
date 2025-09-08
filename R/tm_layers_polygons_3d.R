#' Map layer: polygons in 3d (experimental)
#'
#' Map layer that draws 3d (extruded) polygons. Supported visual variables are: `height` (the ), `fill` (the fill color),
#' `col` (the border color), `lwd` (line width), `lty` (line type),
#' `fill_alpha` (fill color alpha transparency) and `col_alpha` (border color alpha transparency).
#'
#' The visual variable arguments (e.g. `col`) can be specified with either a data
#' variable name (e.g., a spatial vector attribute or a raster layer of the object
#' specified in [tm_shape()]), or with a visual value (for `col`, a color is expected).
#' See \href{https://r-tmap.github.io/tmap/articles/basics_vv}{vignette about visual variables}.
#'
#' Multiple values can be specified: in that case facets are created.
#' These facets can be combined with other faceting data variables, specified with [tm_facets()].
#' See \href{https://r-tmap.github.io/tmap/articles/basics_facets}{vignette about facets}.
#'
#' - The `*.scale` arguments determine the used scale to map the data values to
#' visual variable values. These can be specified with one of the available
#' `tm_scale_*()` functions. The default is specified by the tmap option ([tm_options()]) `scales.var`.
#' See \href{https://r-tmap.github.io/tmap/articles/basics_scales}{vignette about scales}.
#'
#' - The `*.legend` arguments determine the used legend, specified with [tm_legend()].
#' The default legend and its settings are determined by the tmap options ([tm_options()]) `legend.` .
#' See \href{https://r-tmap.github.io/tmap/articles/basics_legends}{vignette about legends}.
#'
#' - The `*.chart` arguments specify additional charts, specified with `tm_chart_`, e.g. [tm_chart_histogram()].
#' See \href{https://r-tmap.github.io/tmap/articles/basics_charts}{vignette about charts}.
#'
#' - The `*.free` arguments determine whether scales are applied freely across facets, or shared.
#' A logical value is required. They can also be specified with a vector of three
#' logical values; these determine whether scales are applied freely per facet dimension.
#' This is only useful when facets are applied (see [tm_facets()]).
#' There are maximally three facet dimensions: rows, columns, and pages. This only
#' applies for a facet grid ([tm_facets_grid()]). For instance, `col.free = c(TRUE, FALSE, FALSE)`
#' means that for the visual variable `col`, each row of facets will have its own
#' scale, and therefore its own legend. For facet wraps and stacks
#' ([tm_facets_wrap()] and [tm_facets_stack()]) there is only one facet dimension,
#' so the `*.free` argument requires only one logical value.
#'
#' @param height,height.scale,height.legend,height.chart,height.free `r .doc_vv("height")`
#' @param fill,fill.scale,fill.legend,fill.chart,fill.free `r .doc_vv("fill")`
#' @param col,col.scale,col.legend,col.chart,col.free `r .doc_vv("col")`
#' @param lwd,lwd.scale,lwd.legend,lwd.chart,lwd.free `r .doc_vv("lwd")`
#' @param lty,lty.scale,lty.legend,lty.chart,lty.free `r .doc_vv("lty")`
#' @param fill_alpha,fill_alpha.scale,fill_alpha.chart,fill_alpha.legend,fill_alpha.free `r .doc_vv("fill_alpha")`
#' @param col_alpha,col_alpha.scale,col_alpha.legend,col_alpha.chart,col_alpha.free `r .doc_vv("col_alpha")`
#' @param linejoin,lineend Line join and line end. See [gpar()][grid::gpar()] for details.
#' @param plot.order Specification in which order the spatial features are drawn.
#'   See [tm_plot_order()] for details.
#' @param zindex Map layers are drawn on top of each other. The `zindex` numbers
#'   (one for each map layer) determines the stacking order.
#'   By default the map layers are drawn in the order they are called.
#' @param group Name of the group to which this layer belongs. This is only
#'   relevant in view mode, where layer groups can be switched (see `group.control`)
#' @param group.control In view mode, the group control determines how layer groups
#'   can be switched on and off. Options: `"radio"` for radio buttons
#'   (meaning only one group can be shown), `"check"` for check boxes
#'   (so multiple groups can be shown), and `"none"` for no control
#'   (the group cannot be (de)selected).
#' @param options options passed on to the corresponding `opt_<layer_function>` function
#' @param popup.vars names of data variables that are shown in the popups
#'   in `"view"` mode. Set popup.vars to `TRUE` to show all variables in the
#'   shape object. Set popup.vars to `FALSE` to disable popups. Set `popup.vars`
#'   to a character vector of variable names to those those variables in the popups.
#'   The default (`NA`) depends on whether visual variables (e.g.`fill`) are used.
#'   If so, only those are shown. If not all variables in the shape object are shown.
#' @param popup.format list of formatting options for the popup values.
#'   See the argument `legend.format` for options. Only applicable for
#'   numeric data variables. If one list of formatting options is provided,
#'   it is applied to all numeric variables of `popup.vars`. Also, a (named)
#'   list of lists can be provided. In that case, each list of formatting options
#'   is applied to the named variable.
#' @param hover name of the data variable that specifies the hover labels (view mode only). Set to `FALSE` to disable hover labels. By default `FALSE`, unless `id` is specified. In that case, it is set to `id`,
#' @param id name of the data variable that specifies the indices of the spatial
#'   features. Only used for `"view"` mode.
#' @example ./examples/tm_polygons_3d.R
#' @seealso \href{https://r-tmap.github.io/tmap/articles/examples_choro_World}{Choropleth example (1)} and \href{https://r-tmap.github.io/tmap/articles/examples_choro_NLD}{choropleth example (2)}
#' @export
tm_polygons_3d = function(height = tmap::tm_const(),
						 height.scale = tmap::tm_scale(),
						 height.legend = tmap::tm_legend_hide(),
						 height.chart = tmap::tm_chart_none(),
						 height.free = NA,
					   fill = tmap::tm_const(),
					   fill.scale = tmap::tm_scale(),
					   fill.legend = tmap::tm_legend(),
					   fill.chart = tmap::tm_chart_none(),
					   fill.free = NA,
					   col = tmap::tm_const(),
					   col.scale = tmap::tm_scale(),
					   col.legend = tmap::tm_legend(),
					   col.chart = tmap::tm_chart_none(),
					   col.free = NA,
					   lwd = tmap::tm_const(),
					   lwd.scale = tmap::tm_scale(),
					   lwd.legend = tmap::tm_legend(),
					   lwd.chart = tmap::tm_chart_none(),
					   lwd.free = NA,
					   lty = tmap::tm_const(),
					   lty.scale = tmap::tm_scale(),
					   lty.legend = tmap::tm_legend(),
					   lty.chart = tmap::tm_chart_none(),
					   lty.free = NA,
					   fill_alpha = tmap::tm_const(),
					   fill_alpha.scale = tmap::tm_scale(),
					   fill_alpha.legend = tmap::tm_legend(),
					   fill_alpha.chart = tmap::tm_chart_none(),
					   fill_alpha.free = NA,
					   col_alpha = tmap::tm_const(),
					   col_alpha.scale = tmap::tm_scale(),
					   col_alpha.legend = tmap::tm_legend(),
					   col_alpha.chart = tmap::tm_chart_none(),
					   col_alpha.free = NA,
					   linejoin = "round",
					   lineend = "round",
					   plot.order = tmap::tm_plot_order("lwd", reverse = TRUE, na.order = "bottom"),
					   zindex = NA,
					   group = NA,
					   group.control = "check",
					   popup.vars = NA,
					   popup.format = list(),
					   hover = NA,
					   id = "",
					   options = opt_tm_polygons_3d()) {

	args_called = names(rlang::call_match()[-1])


	tmap::tm_element_list(tmap::tm_element(
		layer = c("polygons_3d", "polygons"),
		trans.fun = tmap::tmapTransPolygons,
		trans.args = options$trans.args,
		trans.aes = list(),
		trans.apply_to = "this",
		mapping.aes = list(height = tmap::tmapScale(aes = "height",
										   value = height,
										   scale = height.scale,
										   legend = height.legend,
										   chart = height.chart,
										   free = height.free),
						   fill = tmap::tmapScale(aes = "fill",
											value = fill,
											scale = fill.scale,
											legend = fill.legend,
											chart = fill.chart,
											free = fill.free),
						   col = tmap::tmapScale(aes = "col",
						   				value = col,
						   				scale = col.scale,
						   				legend = col.legend,
						   				chart = col.chart,
						   				free = col.free),
						   lwd = tmap::tmapScale(aes = "lwd",
						   				value = lwd,
						   				scale = lwd.scale,
						   				legend = lwd.legend,
						   				chart = lwd.chart,
						   				free = lwd.free),
						   lty = tmap::tmapScale(aes = "lty",
						   				value = lty,
						   				scale = lty.scale,
						   				legend = lty.legend,
						   				chart = lty.chart,
						   				free = lty.free),
						   fill_alpha = tmap::tmapScale(aes = "fill_alpha",
						   					   value = fill_alpha,
						   					   scale = fill_alpha.scale,
						   					   legend = fill_alpha.legend,
						   					   chart = fill_alpha.chart,
						   					   free = fill_alpha.free),
						   col_alpha = tmap::tmapScale(aes = "col_alpha",
						   					  value = col_alpha,
						   					  scale = col_alpha.scale,
						   					  legend = col_alpha.legend,
						   					  chart = col_alpha.chart,
						   					  free = col_alpha.free)),

		gpar = tmap::tmapGpar(height = "__height",
						fill = "__fill",
						col = "__col",
						shape = NA,
						size = NA,
						fill_alpha = "__fill_alpha",
						col_alpha = "__col_alpha",
						pattern = "fill",
						lty = "__lty",
						lwd = "__lwd",
						linejoin = linejoin,
						lineend = lineend),
		tpar = tmap::tmapTpar(area = "AREA"),
		plot.order = plot.order,
		mapping.fun = "Polygons3d",
		mapping.args = options$mapping.args,
		zindex = zindex,
		group = group,
		group.control = group.control,
		popup.vars = popup.vars,
		popup.format = popup.format,
		hover = hover,
		id = id,
		subclass = c("tm_aes_layer", "tm_layer")))
}

#' @param polygons.only should only polygon geometries of the shape object (defined in [tm_shape()]) be plotted? By default `"ifany"`, which means `TRUE` in case a geometry collection is specified.
#' @param height.max,height.min Maximum and minimum height. The data variable values assigned to `height` are are scaled between these two values. Each can be an absolute number (in meters) or relative to the bounding box area, more specifically, the percentage of the square root of the bounding box area. The default values are `"10\%"` for `height.max` and `0.1\%` for `height.min`. The latter is not set to 0 because of artefacts with maplibre in globe view.
#' @export
#' @rdname tm_polygons_3d
opt_tm_polygons_3d = function(polygons.only = "ifany", height.max = "10%", height.min = "0.1%") {
	list(trans.args = list(polygons.only = polygons.only),
		 mapping.args = list(height.max = height.max, height.min = height.min))
}

