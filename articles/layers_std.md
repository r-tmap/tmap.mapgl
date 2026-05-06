# Standard layers

This vignette will illustrate `"maplibre"`:

``` r

tmap_mode("maplibre")
#> ℹ tmap modes "plot" -> "view" -> "mapbox" -> "maplibre"
#> ℹ rotate with `tmap::rtm()`switch to "plot" with `tmap::ttm()`
```

### Polygons

For this example we’ll create a choropleth of well being per country. We
assign the map to `map` without showing it.

``` r

tm_shape(World) + 
  tm_polygons("well_being")
```

With a continuous diverging color scale:

``` r

tm_shape(World) + 
  tm_polygons("well_being",
    fill.scale = tm_scale_continuous(values = "pu_gn"))
```

### Lines

``` r

tm_shape(World_rivers) +
    tm_lines(lwd = "strokelwd", lwd.scale = tm_scale_continuous(values.scale = 5))
#> [1] "group1_layer1lines"
```

## Bubbles

``` r

tm_shape(metro) +
    tm_bubbles(size = "pop2030", fill = "red")
```
