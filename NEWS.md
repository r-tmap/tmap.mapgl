# tmap.mapgl (development version)

- [!] added fill patterns for `tm_polygons()` and `tm_polygons_3d()` (map variables `pattern`, `pattern_col`, `pattern_size`, and `pattern_density`; requires tmap >= 4.4-1.9000), including dot density maps with `tm_legend(dot.value)`. 3D polygons show the pattern on the roofs only; use `opt_tm_polygons_3d(pattern.walls = TRUE)` to include the walls
- added pattern legends and cross legends (`tm_legend_cross()`)
- legends of categorical map variables, patterns, and cross legends are interactive: clicking an item hides its features, and the hidden items of all legends of a layer are combined
- fixed: legends jumped when dragged, and clicks on legend items were sometimes missed
- maps with 3D polygons use Web Mercator instead of the globe projection (unless the map shows a continent or the world, or the crs is specified), since 3D polygons are wobbly on the globe (#11)
