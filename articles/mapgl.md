# Introduction

Recall that *tmap* offers two modes: `"plot"` for static mapping and
`"view"` for interactive mapping. See
[introduction](https://r-tmap.github.io/tmap/articles/basics_modes). The
`"view"` mode uses the JavaScript library Leaflet as backend.

The extension package *tmap.mapgl* offers two new modes which are also
interactive: `"mapbox"` and `"maplibre"` which use the JavaScript
libraries Mapbox GL and Maplibre GL respectively. An API key is required
to use `"mapbox"` (free for personal use), but `"maplibre"` is (as the
name suggests) free for any use.

Note that *tmap.mapgl* is a bridge between the R packages *mapgl* and
*tmap*. It makes the functionality of *mapgl* (making the JavaScript
libraries available to R) also available via the *tmap* user interface.

## Installation

See
[intructions](https://r-tmap.github.io/tmap.mapgl/index.html#installation)

## Switching modes

Get the current mode

``` r
tmap_mode()
#> ℹ tmap modes "plot" -> "view" -> "mapbox" -> "maplibre"
#> ℹ rotate with `tmap::rtm()`switch to "plot" with `tmap::ttm()`
```

Switch to mapbox

``` r
tmap_mode("mapbox")
#> ℹ tmap modes "plot" -> "view" ->
#> "mapbox" -> "maplibre"
```

Alternatively,
[`rtm()`](https://r-tmap.github.io/tmap/reference/tmap_mode.html) can be
used to rotate between modes:

``` r
rtm()
#> ℹ tmap modes "plot" -> "view" ->
#> "mapbox" -> "maplibre"
```

Now we are in `maplibre` mode:

## Making the map

The syntax to create the map is exactly the same as for the other tmap
modes:

``` r
tm_shape(World) + 
  tm_polygons("well_being",
    fill.scale = tm_scale_continuous(values = "pu_gn"))
```
