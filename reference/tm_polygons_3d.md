# Map layer: polygons in 3d (experimental)

Map layer that draws 3d (extruded) polygons. Supported visual variables
are: \`height\` (the ), \`fill\` (the fill color), \`col\` (the border
color), \`lwd\` (line width), \`lty\` (line type), \`fill_alpha\` (fill
color alpha transparency) and \`col_alpha\` (border color alpha
transparency).

## Usage

``` r
tm_polygons_3d(
  height = tmap::tm_const(),
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
  options = opt_tm_polygons_3d()
)

opt_tm_polygons_3d(
  polygons.only = "ifany",
  height.max = "10%",
  height.min = "0.1%"
)
```

## Arguments

- height, height.scale, height.legend, height.chart, height.free:

  \`r .doc_vv("height")\`

- fill, fill.scale, fill.legend, fill.chart, fill.free:

  \`r .doc_vv("fill")\`

- col, col.scale, col.legend, col.chart, col.free:

  \`r .doc_vv("col")\`

- lwd, lwd.scale, lwd.legend, lwd.chart, lwd.free:

  \`r .doc_vv("lwd")\`

- lty, lty.scale, lty.legend, lty.chart, lty.free:

  \`r .doc_vv("lty")\`

- fill_alpha, fill_alpha.scale, fill_alpha.chart, fill_alpha.legend,
  fill_alpha.free:

  \`r .doc_vv("fill_alpha")\`

- col_alpha, col_alpha.scale, col_alpha.legend, col_alpha.chart,
  col_alpha.free:

  \`r .doc_vv("col_alpha")\`

- linejoin, lineend:

  Line join and line end. See \[gpar()\]\[grid::gpar()\] for details.

- plot.order:

  Specification in which order the spatial features are drawn. See
  \[tm_plot_order()\] for details.

- zindex:

  Map layers are drawn on top of each other. The \`zindex\` numbers (one
  for each map layer) determines the stacking order. By default the map
  layers are drawn in the order they are called.

- group:

  Name of the group to which this layer belongs. This is only relevant
  in view mode, where layer groups can be switched (see
  \`group.control\`)

- group.control:

  In view mode, the group control determines how layer groups can be
  switched on and off. Options: \`"radio"\` for radio buttons (meaning
  only one group can be shown), \`"check"\` for check boxes (so multiple
  groups can be shown), and \`"none"\` for no control (the group cannot
  be (de)selected).

- popup.vars:

  names of data variables that are shown in the popups in \`"view"\`
  mode. Set popup.vars to \`TRUE\` to show all variables in the shape
  object. Set popup.vars to \`FALSE\` to disable popups. Set
  \`popup.vars\` to a character vector of variable names to those those
  variables in the popups. The default (\`NA\`) depends on whether
  visual variables (e.g.\`fill\`) are used. If so, only those are shown.
  If not all variables in the shape object are shown.

- popup.format:

  list of formatting options for the popup values. See the argument
  \`legend.format\` for options. Only applicable for numeric data
  variables. If one list of formatting options is provided, it is
  applied to all numeric variables of \`popup.vars\`. Also, a (named)
  list of lists can be provided. In that case, each list of formatting
  options is applied to the named variable.

- hover:

  name of the data variable that specifies the hover labels (view mode
  only). Set to \`FALSE\` to disable hover labels. By default \`FALSE\`,
  unless \`id\` is specified. In that case, it is set to \`id\`,

- id:

  name of the data variable that specifies the indices of the spatial
  features. Only used for \`"view"\` mode.

- options:

  options passed on to the corresponding \`opt\_\<layer_function\>\`
  function

- polygons.only:

  should only polygon geometries of the shape object (defined in
  \[tm_shape()\]) be plotted? By default \`"ifany"\`, which means
  \`TRUE\` in case a geometry collection is specified.

- height.max, height.min:

  Maximum and minimum height. The data variable values assigned to
  \`height\` are are scaled between these two values. Each can be an
  absolute number (in meters) or relative to the bounding box area, more
  specifically, the percentage of the square root of the bounding box
  area. The default values are \`"10%"\` for \`height.max\` and \`0.1%\`
  for \`height.min\`. The latter is not set to 0 because of artefacts
  with maplibre in globe view.

## Value

a \[tmap::tmap-element\], supposed to be stacked after
\[tmap::tm_shape()\] using the \`+\` operator. The
\`opt\_\<layer_function\>\` function returns a list that should be
passed on to the \`options\` argument.

## Details

The visual variable arguments (e.g. \`col\`) can be specified with
either a data variable name (e.g., a spatial vector attribute or a
raster layer of the object specified in \[tm_shape()\]), or with a
visual value (for \`col\`, a color is expected). See [vignette about
visual variables](https://r-tmap.github.io/tmap/articles/basics_vv).

Multiple values can be specified: in that case facets are created. These
facets can be combined with other faceting data variables, specified
with \[tm_facets()\]. See [vignette about
facets](https://r-tmap.github.io/tmap/articles/basics_facets).

\- The \`\*.scale\` arguments determine the used scale to map the data
values to visual variable values. These can be specified with one of the
available \`tm_scale\_\*()\` functions. The default is specified by the
tmap option (\[tm_options()\]) \`scales.var\`. See [vignette about
scales](https://r-tmap.github.io/tmap/articles/basics_scales).

\- The \`\*.legend\` arguments determine the used legend, specified with
\[tm_legend()\]. The default legend and its settings are determined by
the tmap options (\[tm_options()\]) \`legend.\` . See [vignette about
legends](https://r-tmap.github.io/tmap/articles/basics_legends).

\- The \`\*.chart\` arguments specify additional charts, specified with
\`tm_chart\_\`, e.g. \[tm_chart_histogram()\]. See [vignette about
charts](https://r-tmap.github.io/tmap/articles/basics_charts).

\- The \`\*.free\` arguments determine whether scales are applied freely
across facets, or shared. A logical value is required. They can also be
specified with a vector of three logical values; these determine whether
scales are applied freely per facet dimension. This is only useful when
facets are applied (see \[tm_facets()\]). There are maximally three
facet dimensions: rows, columns, and pages. This only applies for a
facet grid (\[tm_facets_grid()\]). For instance, \`col.free = c(TRUE,
FALSE, FALSE)\` means that for the visual variable \`col\`, each row of
facets will have its own scale, and therefore its own legend. For facet
wraps and stacks (\[tm_facets_wrap()\] and \[tm_facets_stack()\]) there
is only one facet dimension, so the \`\*.free\` argument requires only
one logical value.

## See also

[Choropleth example
(1)](https://r-tmap.github.io/tmap/articles/examples_choro_World) and
[choropleth example
(2)](https://r-tmap.github.io/tmap/articles/examples_choro_NLD)

## Examples

``` r
# \donttest{
library(tmap)
library(tmap.mapgl)
tmap_mode("maplibre")
#> ℹ tmap modes "plot" -> "view" -> "mapbox" -> "maplibre"
NLD_dist$pop_dens = NLD_dist$population / NLD_dist$area
tm_shape(NLD_dist) +
  tm_polygons_3d(height = "pop_dens",
    fill = "edu_appl_sci",
    fill.scale = tm_scale_intervals(style = "kmeans", values = "-pu_gn"),
    fill.legend = tm_legend("Univeristy degree")) +
tm_maplibre(pitch = 45) +
tm_crs(3857)
#> Warning: Fill-extrusion layers may have rendering artifacts in globe projection. Consider using projection = "mercator" in maplibre() for better performance. See https://github.com/maplibre/maplibre-gl-js/issues/5025
# }
```
