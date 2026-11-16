# tmap.mapgl (development version)

- [!] added `tm_text()` and `tm_labels()`. Overlapping labels are hidden in the order of `priority`. Requires the development version of tmap
- fixed: text angles of `along_lines` did not match the features when `plot.order` was used
- fixed: legend of `tm_scale_intervals(label.style = "continuous")` (labels were shifted). Requires the development version of tmap
- [!] added fill patterns for `tm_polygons()` and `tm_polygons_3d()` (motifs and textures). See `tmap::tm_patterns()`. Requires tmap >= 4.4-1.9000
- `tm_credits()` is shown in maplibre mode, in the attribution (as in view mode)
- legends of categorical map variables, patterns, and cross legends are interactive: clicking an item hides its features, and the hidden items of all legends of a layer are combined
- fixed: legends jumped when dragged, and clicks on legend items were sometimes missed
- maps with 3D polygons use Web Mercator instead of the globe projection (unless the map shows a continent or the world, or the crs is specified), since 3D polygons are wobbly on the globe (#11)
