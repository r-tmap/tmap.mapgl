# Changelog

## tmap.mapgl (development version)

- fixed:
  [`tm_polygons_3d()`](https://r-tmap.github.io/tmap.mapgl/reference/tm_polygons_3d.md)
  in plot and view mode (drawn as flat polygons). Requires the
  development version of tmap
- fixed: help pages showed raw markdown, e.g. the argument descriptions
  of
  [`tm_polygons_3d()`](https://r-tmap.github.io/tmap.mapgl/reference/tm_polygons_3d.md)
- the dark basemap `"Esri.WorldDarkGrayCanvas"` of plot and view mode is
  replaced by `"ofm.dark"` in the maplibre mode. Requires the
  development version of tmap
- \[!\] added
  [`tm_text()`](https://r-tmap.github.io/tmap/reference/tm_text.html)
  and
  [`tm_labels()`](https://r-tmap.github.io/tmap/reference/tm_text.html).
  Overlapping labels are hidden in the order of `priority`. Requires the
  development version of tmap
- fixed: text angles of `along_lines` did not match the features when
  `plot.order` was used
- fixed: legend of `tm_scale_intervals(label.style = "continuous")`
  (labels were shifted). Requires the development version of tmap
- \[!\] added fill patterns for
  [`tm_polygons()`](https://r-tmap.github.io/tmap/reference/tm_polygons.html)
  and
  [`tm_polygons_3d()`](https://r-tmap.github.io/tmap.mapgl/reference/tm_polygons_3d.md)
  (motifs and textures). See
  [`tmap::tm_patterns()`](https://r-tmap.github.io/tmap/reference/tm_patterns.html).
  Requires tmap \>= 4.4-1.9000
- [`tm_credits()`](https://r-tmap.github.io/tmap/reference/tm_credits.html)
  is shown in maplibre mode, in the attribution (as in view mode)
- legends of categorical map variables, patterns, and cross legends are
  interactive: clicking an item hides its features, and the hidden items
  of all legends of a layer are combined
- fixed: legends jumped when dragged, and clicks on legend items were
  sometimes missed
- maps with 3D polygons use Web Mercator instead of the globe projection
  (unless the map shows a continent or the world, or the crs is
  specified), since 3D polygons are wobbly on the globe
  ([\#11](https://github.com/r-tmap/tmap.mapgl/issues/11))
