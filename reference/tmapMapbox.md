# Internal tmap methods

Internal tmap methods

## Usage

``` r
tmapValuesCheck_height(x, is_var = TRUE)

tmapValuesIsDiv_height(x)

tmapValuesRange_height(x, n, isdiv)

tmapValuesVV_height(
  x,
  value.na,
  isdiv,
  n,
  dvalues,
  are_breaks,
  midpoint,
  range,
  scale,
  rep,
  o
)

tmapValuesSubmit_height(x, args)

tmapValuesScale_height(x, scale)

tmapValuesColorize_height(x, pc)

tmapValuesCVV_height(x, value.na, n, range, scale, rep, o)

tmapMapboxArrange(tms, nx, ncol, nrow, opts, knit, show, args, options)

tmapMaplibreArrange(tms, nx, ncol, nrow, opts, knit, show, args, options)

tmapMapboxAuxPrepare(a, bs, id, o)

# Default S3 method
tmapMapboxAuxPrepare(a, bs, id, o)

tmapMapboxAuxPlot(
  a,
  bi,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o
)

# Default S3 method
tmapMapboxAuxPlot(
  a,
  bi,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o
)

tmapMaplibreAuxPrepare(a, bs, id, o)

# Default S3 method
tmapMaplibreAuxPrepare(a, bs, id, o)

tmapMaplibreAuxPlot(
  a,
  bi,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o
)

# Default S3 method
tmapMaplibreAuxPlot(
  a,
  bi,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o
)

# S3 method for class 'tm_aux_basemap'
tmapMapboxAuxPrepare(a, bs, id, o)

# S3 method for class 'tm_aux_basemap'
tmapMaplibreAuxPrepare(a, bs, id, o)

# S3 method for class 'tm_aux_tiles'
tmapMapboxAuxPrepare(a, bs, id, o)

# S3 method for class 'tm_aux_tiles'
tmapMaplibreAuxPrepare(a, bs, id, o)

# S3 method for class 'tm_aux_basemap'
tmapMapboxAuxPlot(
  a,
  bi,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o
)

# S3 method for class 'tm_aux_basemap'
tmapMaplibreAuxPlot(
  a,
  bi,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o
)

# S3 method for class 'tm_aux_tiles'
tmapMapboxAuxPlot(
  a,
  bi,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o
)

# S3 method for class 'tm_aux_tiles'
tmapMaplibreAuxPlot(
  a,
  bi,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o
)

tmapMapboxGridXLab(bi, bbx, facet_row, facet_col, facet_page, o)

tmapMapboxGridYLab(bi, bbx, facet_row, facet_col, facet_page, o)

tmapMaplibreGridXLab(bi, bbx, facet_row, facet_col, facet_page, o)

tmapMaplibreGridYLab(bi, bbx, facet_row, facet_col, facet_page, o)

# S3 method for class 'tm_title'
tmapMapboxCompPrepare(comp, o)

# S3 method for class 'tm_title'
tmapMapboxCompHeight(comp, o)

# S3 method for class 'tm_title'
tmapMapboxCompWidth(comp, o)

# S3 method for class 'tm_title'
tmapMapboxCompPlot(comp, m, o)

# S3 method for class 'tm_scalebar'
tmapMapboxCompPrepare(comp, o)

# S3 method for class 'tm_scalebar'
tmapMapboxCompHeight(comp, o)

# S3 method for class 'tm_scalebar'
tmapMapboxCompWidth(comp, o)

# S3 method for class 'tm_scalebar'
tmapMapboxCompPlot(comp, m, o)

# S3 method for class 'tm_snow'
tmapMapboxCompPrepare(comp, o)

# S3 method for class 'tm_snow'
tmapMapboxCompHeight(comp, o)

# S3 method for class 'tm_snow'
tmapMapboxCompWidth(comp, o)

# S3 method for class 'tm_snow'
tmapMapboxCompPlot(comp, m, o)

# S3 method for class 'tm_title'
tmapMaplibreCompPrepare(comp, o)

# S3 method for class 'tm_title'
tmapMaplibreCompHeight(comp, o)

# S3 method for class 'tm_title'
tmapMaplibreCompWidth(comp, o)

# S3 method for class 'tm_title'
tmapMaplibreCompPlot(comp, m, o)

# S3 method for class 'tm_scalebar'
tmapMaplibreCompPrepare(comp, o)

# S3 method for class 'tm_scalebar'
tmapMaplibreCompHeight(comp, o)

# S3 method for class 'tm_scalebar'
tmapMaplibreCompWidth(comp, o)

# S3 method for class 'tm_scalebar'
tmapMaplibreCompPlot(comp, m, o)

# S3 method for class 'tm_credits'
tmapMaplibreCompPrepare(comp, o)

# S3 method for class 'tm_credits'
tmapMaplibreCompHeight(comp, o)

# S3 method for class 'tm_credits'
tmapMaplibreCompWidth(comp, o)

# S3 method for class 'tm_credits'
tmapMaplibreCompPlot(comp, m, o)

# S3 method for class 'tm_mouse_coordinates'
tmapMaplibreCompPrepare(comp, o)

# S3 method for class 'tm_mouse_coordinates'
tmapMaplibreCompHeight(comp, o)

# S3 method for class 'tm_mouse_coordinates'
tmapMaplibreCompWidth(comp, o)

# S3 method for class 'tm_mouse_coordinates'
tmapMaplibreCompPlot(comp, m, o)

# S3 method for class 'tm_minimap'
tmapMapboxCompPrepare(comp, o)

# S3 method for class 'tm_minimap'
tmapMaplibreCompPrepare(comp, o)

# S3 method for class 'tm_minimap'
tmapMapboxCompHeight(comp, o)

# S3 method for class 'tm_minimap'
tmapMapboxCompWidth(comp, o)

# S3 method for class 'tm_minimap'
tmapMaplibreCompHeight(comp, o)

# S3 method for class 'tm_minimap'
tmapMaplibreCompWidth(comp, o)

# S3 method for class 'tm_minimap'
tmapMapboxCompPlot(comp, m, o)

# S3 method for class 'tm_minimap'
tmapMaplibreCompPlot(comp, m, o)

tmapMapboxCompPrepare(comp, o)

tmapMapboxCompHeight(comp, o)

tmapMapboxCompWidth(comp, o)

tmapMapboxCompPlot(comp, m, o)

# Default S3 method
tmapMapboxCompPrepare(comp, o)

# S3 method for class 'tm_chart_none'
tmapMapboxCompPrepare(comp, o)

tmapMaplibreCompPrepare(comp, o)

tmapMaplibreCompHeight(comp, o)

tmapMaplibreCompWidth(comp, o)

tmapMaplibreCompPlot(comp, m, o)

# Default S3 method
tmapMaplibreCompPrepare(comp, o)

# S3 method for class 'tm_chart_none'
tmapMaplibreCompPrepare(comp, o)

# S3 method for class 'tm_draw'
tmapMapboxCompPrepare(comp, o)

# S3 method for class 'tm_draw'
tmapMapboxCompHeight(comp, o)

# S3 method for class 'tm_draw'
tmapMapboxCompWidth(comp, o)

# S3 method for class 'tm_draw'
tmapMapboxCompPlot(comp, m, o)

# S3 method for class 'tm_draw'
tmapMaplibreCompPrepare(comp, o)

# S3 method for class 'tm_draw'
tmapMaplibreCompHeight(comp, o)

# S3 method for class 'tm_draw'
tmapMaplibreCompWidth(comp, o)

# S3 method for class 'tm_draw'
tmapMaplibreCompPlot(comp, m, o)

# S3 method for class 'tm_geocoder'
tmapMapboxCompPrepare(comp, o)

# S3 method for class 'tm_geocoder'
tmapMapboxCompHeight(comp, o)

# S3 method for class 'tm_geocoder'
tmapMapboxCompWidth(comp, o)

# S3 method for class 'tm_geocoder'
tmapMapboxCompPlot(comp, m, o)

# S3 method for class 'tm_geocoder'
tmapMaplibreCompPrepare(comp, o)

# S3 method for class 'tm_geocoder'
tmapMaplibreCompHeight(comp, o)

# S3 method for class 'tm_geocoder'
tmapMaplibreCompWidth(comp, o)

# S3 method for class 'tm_geocoder'
tmapMaplibreCompPlot(comp, m, o)

tmapMapboxInit(o, return.asp = FALSE, vp, prx, dg = NULL, ...)

tmapMaplibreInit(o, return.asp = FALSE, vp, prx, dg = NULL, ...)

tmapMapboxAux(o, q)

tmapMaplibreAux(o, q)

# S3 method for class 'tm_legend_portrait'
tmapMapboxCompPlot(comp, m, o)

# S3 method for class 'tm_legend_landscape'
tmapMapboxCompPlot(comp, m, o)

# S3 method for class 'tm_legend_portrait'
tmapMaplibreCompPlot(comp, m, o)

# S3 method for class 'tm_legend_landscape'
tmapMaplibreCompPlot(comp, m, o)

tmapMapboxComp(
  comp,
  o,
  facet_row = NULL,
  facet_col = NULL,
  facet_page,
  class,
  stack,
  stack_auto,
  pos.h,
  pos.v,
  bbox
)

tmapMaplibreComp(
  comp,
  o,
  facet_row = NULL,
  facet_col = NULL,
  facet_page,
  class,
  stack,
  stack_auto,
  pos.h,
  pos.v,
  bbox
)

tmapMapboxProviders(credits)

tmapMaplibreProviders(credits)

tmapMapboxRun(o, q, show, knit, knit_opts, args)

tmapMaplibreRun(o, q, show, knit, knit_opts, args)

tmapMapboxShape(bbx, facet_row, facet_col, facet_page, o)

tmapMaplibreShape(bbx, facet_row, facet_col, facet_page, o)

tmapMapboxOverlay(bbx, facet_row, facet_col, facet_page, o)

tmapMaplibreOverlay(bbx, facet_row, facet_col, facet_page, o)

renderTmapMapbox(expr, env, quoted, execOnResize)

tmapOutputMapbox(outputId, width, height)

tmapProxyMapbox(mapId, session, x)

renderTmapMaplibre(expr, env, quoted, execOnResize)

tmapOutputMaplibre(outputId, width, height)

tmapProxyMaplibre(mapId, session, x)

tmapMapboxWrap(label, facet_row, facet_col, facet_page, o)

tmapMapboxXtab(label, facet_row, facet_col, facet_page, o)

tmapMaplibreWrap(label, facet_row, facet_col, facet_page, o)

tmapMaplibreXtab(label, facet_row, facet_col, facet_page, o)

# S3 method for class 'tm_aux_snow'
tmapMapboxAuxPrepare(a, bs, id, o)

# S3 method for class 'tm_aux_snow'
tmapMapboxAuxPlot(
  a,
  bi,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o
)

# S3 method for class 'tm_aux_rain'
tmapMapboxAuxPrepare(a, bs, id, o)

# S3 method for class 'tm_aux_rain'
tmapMapboxAuxPlot(
  a,
  bi,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  o
)

# S3 method for class 'tm_fullscreen'
tmapMapboxCompPrepare(comp, o)

# S3 method for class 'tm_fullscreen'
tmapMapboxCompHeight(comp, o)

# S3 method for class 'tm_fullscreen'
tmapMapboxCompWidth(comp, o)

# S3 method for class 'tm_fullscreen'
tmapMapboxCompPlot(comp, m, o)

# S3 method for class 'tm_fullscreen'
tmapMaplibreCompPrepare(comp, o)

# S3 method for class 'tm_fullscreen'
tmapMaplibreCompHeight(comp, o)

# S3 method for class 'tm_fullscreen'
tmapMaplibreCompWidth(comp, o)

# S3 method for class 'tm_fullscreen'
tmapMaplibreCompPlot(comp, m, o)

tmapMapboxDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  glid,
  o,
  ...
)

# Default S3 method
tmapMapboxDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  glid,
  o,
  ...
)

# S3 method for class 'tm_data_polygons'
tmapMapboxDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  glid,
  o,
  ...
)

# S3 method for class 'tm_data_polygons_3d'
tmapMapboxDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  glid,
  o,
  ...
)

# S3 method for class 'tm_data_lines'
tmapMapboxDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  glid,
  o,
  ...
)

# S3 method for class 'tm_data_symbols'
tmapMapboxDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  glid,
  o,
  ...
)

# S3 method for class 'tm_data_raster'
tmapMapboxDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  glid,
  o,
  ...
)

# S3 method for class 'tm_data_fill'
tmapMapboxDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  glid,
  o,
  ...
)

# S3 method for class 'tm_data_borders'
tmapMapboxDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  glid,
  o,
  ...
)

# S3 method for class 'tm_data_dots'
tmapMapboxDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  glid,
  o,
  ...
)

# S3 method for class 'tm_data_bubbles'
tmapMapboxDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  glid,
  o,
  ...
)

tmapMaplibreDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  glid,
  o,
  ...
)

# Default S3 method
tmapMaplibreDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  glid,
  o,
  ...
)

# S3 method for class 'tm_data_polygons'
tmapMaplibreDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  glid,
  o,
  ...
)

# S3 method for class 'tm_data_polygons_3d'
tmapMaplibreDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  glid,
  o,
  ...
)

# S3 method for class 'tm_data_lines'
tmapMaplibreDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  glid,
  o,
  ...
)

# S3 method for class 'tm_data_symbols'
tmapMaplibreDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  glid,
  o,
  ...
)

# S3 method for class 'tm_data_raster'
tmapMaplibreDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  glid,
  o,
  ...
)

# S3 method for class 'tm_data_fill'
tmapMaplibreDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  glid,
  o,
  ...
)

# S3 method for class 'tm_data_borders'
tmapMaplibreDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  glid,
  o,
  ...
)

# S3 method for class 'tm_data_dots'
tmapMaplibreDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  glid,
  o,
  ...
)

# S3 method for class 'tm_data_bubbles'
tmapMaplibreDataPlot(
  a,
  shpTM,
  dt,
  pdt,
  popup.format,
  hdt,
  idt,
  gp,
  bbx,
  facet_row,
  facet_col,
  facet_page,
  id,
  pane,
  group,
  glid,
  o,
  ...
)
```

## Arguments

- o:

  the list of options

- args:

  args

- tms:

  tmap objects

- nx:

  number of facets

- ncol, :

  nrow number of rows and columns

- opts:

  options

- knit:

  knit

- show:

  show

- options:

  options

- bs:

  bs

- id:

  id

- bi:

  bi

- bbx:

  bbx

- facet_row, facet_col, facet_page:

  row column and page id

- pane:

  pane

- group:

  group

- comp:

  the shape object

- m:

  mapbox oer maplibre object

- return.asp:

  return.asp

- vp:

  vp

- q:

  q

- class:

  class

- stack:

  stack

- stack_auto:

  stack_auto

- pos.h:

  pos.h

- pos.v:

  pos.v

- bbox:

  bbox

- credits:

  credits

- knit_opts:

  knit options

- label:

  label

- shpTM, :

  dt, pdt, popup.format, hdt, idt, gp args

## Value

internal tmap lists
