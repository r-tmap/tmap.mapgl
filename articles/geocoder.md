# Geocoder

**tmap.mapgl** will also features a couple of new map component
functions that are only available for the `"mapbox"` and `"maplibre"`
modes.

One is already implemented: \[tm_geocoder()\]

## Example: metro

``` r

tmap_mode("maplibre")
#> ℹ tmap modes "plot" -> "view" -> "mapbox" -> "maplibre"
#> ℹ rotate with `tmap::rtm()`switch to "plot" with `tmap::ttm()`

tm_shape(World) +
  tm_borders(lwd = 2) +
tm_shape(metro) +
  tm_dots(size = "pop2030", fill = "gold") +
tm_geocoder()
```
