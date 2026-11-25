# Map layer: polygons in 3d (experimental)

Map layer that draws 3d (extruded) polygons. Supported visual variables
are: `height` (the ), `fill` (the fill color), `col` (the border color),
`lwd` (line width), `lty` (line type), `fill_alpha` (fill color alpha
transparency), `col_alpha` (border color alpha transparency), and the
fill pattern map variables `pattern`, `pattern_col`, `pattern_size`, and
`pattern_density` (see
[`tmap::tm_polygons()`](https://r-tmap.github.io/tmap/reference/tm_polygons.html)).

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
  pattern = tmap::tm_const(),
  pattern.scale = tmap::tm_scale(),
  pattern.legend = tmap::tm_legend(),
  pattern.chart = tmap::tm_chart_none(),
  pattern.free = NA,
  pattern_col = tmap::tm_const(),
  pattern_col.scale = tmap::tm_scale(),
  pattern_col.legend = tmap::tm_legend(),
  pattern_col.chart = tmap::tm_chart_none(),
  pattern_col.free = NA,
  pattern_size = tmap::tm_const(),
  pattern_size.scale = tmap::tm_scale(),
  pattern_size.legend = tmap::tm_legend(),
  pattern_size.chart = tmap::tm_chart_none(),
  pattern_size.free = NA,
  pattern_density = tmap::tm_const(),
  pattern_density.scale = tmap::tm_scale(),
  pattern_density.legend = tmap::tm_legend(),
  pattern_density.chart = tmap::tm_chart_none(),
  pattern_density.free = NA,
  linejoin = "round",
  lineend = "round",
  plot.order = tmap::tm_plot_order("lwd", reverse = TRUE, na.order = "bottom"),
  zindex = NA,
  group = NA,
  group.control = "check",
  popup = tmap::tm_popup(),
  popup.vars = NA,
  popup.format = list(),
  hover = NA,
  id = "",
  options = opt_tm_polygons_3d()
)

opt_tm_polygons_3d(
  polygons.only = "ifany",
  height.max = "10%",
  height.min = "0.1%",
  cast.shadows = NULL,
  ambient.occlusion.intensity = NULL,
  ambient.occlusion.radius = NULL,
  vertical.gradient = NULL,
  emissive.strength = NULL,
  pattern.walls = FALSE
)
```

## Arguments

- height, height.scale, height.legend, height.chart, height.free:

  Map variable that determines the height. See details.

- fill, fill.scale, fill.legend, fill.chart, fill.free:

  Map variable that determines the fill color. See details.

- col, col.scale, col.legend, col.chart, col.free:

  Map variable that determines the color. See details.

- lwd, lwd.scale, lwd.legend, lwd.chart, lwd.free:

  Map variable that determines the line width. See details.

- lty, lty.scale, lty.legend, lty.chart, lty.free:

  Map variable that determines the line type. See details.

- fill_alpha, fill_alpha.scale, fill_alpha.chart, fill_alpha.legend,
  fill_alpha.free:

  Map variable that determines the fill color transparency. See details.

- col_alpha, col_alpha.scale, col_alpha.legend, col_alpha.chart,
  col_alpha.free:

  Map variable that determines the color transparency. See details.

- pattern, pattern.scale, pattern.legend, pattern.chart, pattern.free:

  Fill pattern, drawn on the roofs of the extrusions (and also on the
  walls with `opt_tm_polygons_3d(pattern.walls = TRUE)`). See
  [`tmap::tm_polygons()`](https://r-tmap.github.io/tmap/reference/tm_polygons.html)
  and
  [`tmap::tm_patterns()`](https://r-tmap.github.io/tmap/reference/tm_patterns.html).

- pattern_col, pattern_col.scale, pattern_col.legend, pattern_col.chart,
  pattern_col.free:

  Pattern color.

- pattern_size, pattern_size.scale, pattern_size.legend,
  pattern_size.chart, pattern_size.free:

  Line width or dot size of the pattern.

- pattern_density, pattern_density.scale, pattern_density.legend,
  pattern_density.chart, pattern_density.free:

  Density of the pattern (0 to 1).

- linejoin, lineend:

  Line join and line end. See [gpar()](https://rdrr.io/r/grid/gpar.html)
  for details.

- plot.order:

  Specification in which order the spatial features are drawn. See
  [`tm_plot_order()`](https://r-tmap.github.io/tmap/reference/tm_plot_order.html)
  for details.

- zindex:

  Map layers are drawn on top of each other. The `zindex` numbers (one
  for each map layer) determines the stacking order. By default the map
  layers are drawn in the order they are called.

- group:

  Name of the group to which this layer belongs. This is only relevant
  in view mode, where layer groups can be switched (see `group.control`)

- group.control:

  In view mode, the group control determines how layer groups can be
  switched on and off. Options: `"radio"` for radio buttons (meaning
  only one group can be shown), `"check"` for check boxes (so multiple
  groups can be shown), and `"none"` for no control (the group cannot be
  (de)selected).

- popup:

  popup specification, the output of
  [`tmap::tm_popup()`](https://r-tmap.github.io/tmap/reference/tm_popup.html).
  It determines the data variables shown in the popup table, the popup
  title, and the popup layout. This replaces the deprecated arguments
  `popup.vars` and `popup.format`.

- popup.vars:

  (Deprecated.) Use `popup` with
  [`tmap::tm_popup()`](https://r-tmap.github.io/tmap/reference/tm_popup.html)
  instead (via its `vars` argument). Names of data variables that are
  shown in the popups. Set popup.vars to `TRUE` to show all variables in
  the shape object. Set popup.vars to `FALSE` to disable popups. Set
  `popup.vars` to a character vector of variable names to those those
  variables in the popups. The default (`NA`) depends on whether visual
  variables (e.g.`fill`) are used. If so, only those are shown. If not
  all variables in the shape object are shown.

- popup.format:

  (Deprecated.) Use `popup` with
  [`tmap::tm_popup()`](https://r-tmap.github.io/tmap/reference/tm_popup.html)
  instead (via its `format` argument). List of formatting options for
  the popup values. See the argument `legend.format` for options. Only
  applicable for numeric data variables. If one list of formatting
  options is provided, it is applied to all numeric variables of
  `popup.vars`. Also, a (named) list of lists can be provided. In that
  case, each list of formatting options is applied to the named
  variable.

- hover:

  name of the data variable that specifies the hover labels (view mode
  only). Set to `FALSE` to disable hover labels. By default `FALSE`,
  unless `id` is specified. In that case, it is set to `id`,

- id:

  name of the data variable that specifies the indices of the spatial
  features. Only used for `"view"` mode.

- options:

  options passed on to the corresponding `opt_<layer_function>` function

- polygons.only:

  should only polygon geometries of the shape object (defined in
  [`tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.html))
  be plotted? By default `"ifany"`, which means `TRUE` in case a
  geometry collection is specified.

- height.max, height.min:

  Maximum and minimum height. The data variable values assigned to
  `height` are are scaled between these two values. Each can be an
  absolute number (in meters) or relative to the bounding box area, more
  specifically, the percentage of the square root of the bounding box
  area. The default values are `"10\%"` for `height.max` and `0.1\%` for
  `height.min`. The latter is not set to 0 because of artefacts with
  maplibre in globe view.

- cast.shadows:

  Should the extruded polygons cast shadows? **`"mapbox"` mode only**
  (silently ignored with a message in `"maplibre"` mode, whose
  fill-extrusion implementation does not support shadows). Passed to
  `fill_extrusion_cast_shadows`. The default `NULL` keeps the engine
  default.

- ambient.occlusion.intensity, ambient.occlusion.radius:

  Ambient-occlusion intensity (0-1) and radius for the extruded
  polygons, adding soft contact shading in corners and crevices.
  **`"mapbox"` mode only** (ignored with a message in `"maplibre"`
  mode). Passed to `fill_extrusion_ambient_occlusion_intensity` /
  `fill_extrusion_ambient_occlusion_radius`. The default `NULL` keeps
  the engine default.

- vertical.gradient:

  Should a vertical gradient be applied to the sides of the extrusions?
  When `TRUE`, the side walls are shaded from darker at the base to
  lighter toward the top, which gives a sense of depth. Supported in
  **both** `"maplibre"` and `"mapbox"` modes. Passed to
  `fill_extrusion_vertical_gradient`. The default `NULL` keeps the
  engine default, which is `TRUE` - so the visible effect is mainly when
  set to `FALSE` (flat, evenly-coloured side walls).

- emissive.strength:

  Emissive strength of the extruded polygons, controlling how much they
  appear self-lit under the map's 3D lighting. **`"mapbox"` mode only**
  (ignored with a message in `"maplibre"` mode). Passed to
  `fill_extrusion_emissive_strength`. The default `NULL` keeps the
  engine default.

- pattern.walls:

  Should the fill pattern (map variable `pattern`) also be drawn on the
  walls of the extrusions? By default `FALSE`: only on the roofs.

## Value

a
[tmap::tmap-element](https://r-tmap.github.io/tmap/reference/tmap-element.html),
supposed to be stacked after
[`tmap::tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.html)
using the `+` operator. The `opt_<layer_function>` function returns a
list that should be passed on to the `options` argument.

## Details

The visual variable arguments (e.g. `col`) can be specified with either
a data variable name (e.g., a spatial vector attribute or a raster layer
of the object specified in
[`tm_shape()`](https://r-tmap.github.io/tmap/reference/tm_shape.html)),
or with a visual value (for `col`, a color is expected). See [vignette
about visual
variables](https://r-tmap.github.io/tmap/articles/basics_vv).

Multiple values can be specified: in that case facets are created. These
facets can be combined with other faceting data variables, specified
with
[`tm_facets()`](https://r-tmap.github.io/tmap/reference/tm_facets.html).
See [vignette about
facets](https://r-tmap.github.io/tmap/articles/basics_facets).

- The `*.scale` arguments determine the used scale to map the data
  values to visual variable values. These can be specified with one of
  the available `tm_scale_*()` functions. The default is specified by
  the tmap option
  ([`tm_options()`](https://r-tmap.github.io/tmap/reference/tm_options.html))
  `scales.var`. See [vignette about
  scales](https://r-tmap.github.io/tmap/articles/basics_scales).

- The `*.legend` arguments determine the used legend, specified with
  [`tm_legend()`](https://r-tmap.github.io/tmap/reference/tm_legend.html).
  The default legend and its settings are determined by the tmap options
  ([`tm_options()`](https://r-tmap.github.io/tmap/reference/tm_options.html))
  `legend.` . See [vignette about
  legends](https://r-tmap.github.io/tmap/articles/basics_legends).

- The `*.chart` arguments specify additional charts, specified with
  `tm_chart_`, e.g.
  [`tm_chart_histogram()`](https://r-tmap.github.io/tmap/reference/tm_chart.html).
  See [vignette about
  charts](https://r-tmap.github.io/tmap/articles/basics_charts).

- The `*.free` arguments determine whether scales are applied freely
  across facets, or shared. A logical value is required. They can also
  be specified with a vector of three logical values; these determine
  whether scales are applied freely per facet dimension. This is only
  useful when facets are applied (see
  [`tm_facets()`](https://r-tmap.github.io/tmap/reference/tm_facets.html)).
  There are maximally three facet dimensions: rows, columns, and pages.
  This only applies for a facet grid
  ([`tm_facets_grid()`](https://r-tmap.github.io/tmap/reference/tm_facets.html)).
  For instance, `col.free = c(TRUE, FALSE, FALSE)` means that for the
  visual variable `col`, each row of facets will have its own scale, and
  therefore its own legend. For facet wraps and stacks
  ([`tm_facets_wrap()`](https://r-tmap.github.io/tmap/reference/tm_facets.html)
  and
  [`tm_facets_stack()`](https://r-tmap.github.io/tmap/reference/tm_facets.html))
  there is only one facet dimension, so the `*.free` argument requires
  only one logical value.

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
# }
```
