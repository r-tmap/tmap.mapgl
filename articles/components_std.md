# Standard components

This vignette will illustrate `"maplibre"`:

``` r

tmap_mode("maplibre")
#> ℹ tmap modes "plot" -> "view" -> "mapbox" -> "maplibre"
#> ℹ rotate with `tmap::rtm()`switch to "plot" with `tmap::ttm()`
```

## Scalebar

For this example we’ll create a choropleth of well being per country. We
assign the map to `map` without showing it.

``` r

tm_shape(World) + 
  tm_polygons("well_being") +
    tm_scalebar()
```

## Minimap

``` r

tm_shape(NLD_dist) + 
    tm_polygons(lwd = 0.5, fill = "income_high") +
    tm_shape(NLD_muni) +
    tm_borders(col = "black") +
tm_minimap()
```
