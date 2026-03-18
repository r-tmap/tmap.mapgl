# Introduction

Both `"mapbox"` and `"maplibre"` offer Web Mercator and ‘Globe’
projections:

## Globe projection (default)

``` r
tmap_mode("maplibre")
#> ℹ tmap modes "plot" -> "view" -> "mapbox" -> "maplibre"
#> ℹ rotate with `tmap::rtm()`switch to "plot" with `tmap::ttm()`

tm_shape(metro) + tm_bubbles(size = "pop2030", fill = "red")
```

## Mercator projection

``` r
tmap_mode("maplibre")
#> ℹ tmap modes "plot" -> "view" ->
#> "mapbox" -> "maplibre"

tm_shape(metro) + tm_bubbles(size = "pop2030", fill = "red") + tm_crs(3857)
```

In addition, `"mapbox"` offers a couple of other projections:

## Other projections

### Pseudo-cylindrical

``` r
tmap_mode("mapbox")

tm_shape(metro) + tm_bubbles(size = "pop2030", fill = "red") + tm_crs("+proj=eqearth")
```

``` r
tm_shape(metro) + tm_bubbles(size = "pop2030", fill = "red") + tm_crs("+proj=wintri")
```

## Conic

``` r
tm_shape(metro) + tm_bubbles(size = "pop2030", fill = "red") + tm_crs("+proj=eqearth")
```

``` r
tm_shape(metro) + tm_bubbles(size = "pop2030", fill = "red") + tm_crs("+proj=eqearth")
```
