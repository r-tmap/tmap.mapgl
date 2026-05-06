# Weather (mapbox only)

These layers are only available in `"mapbox"` mode:

``` r

tmap_mode("mapbox")
#> ℹ tmap modes "plot" -> "view" -> "mapbox" -> "maplibre"
#> ℹ rotate with `tmap::rtm()`switch to "plot" with `tmap::ttm()`
```

Let’s create a choropleth of the Netherlands

``` r

tm = tm_shape(NLD_dist) + 
    tm_polygons(lwd = 0.5, fill = "income_high", fill.scale = tm_scale_continuous(values = "-pu_gn")) +
    tm_shape(NLD_muni) +
    tm_borders(col = "black") +
    tm_mapbox(pitch = 65)
```

``` r

tm + tm_rain()
tm + tm_snow()
```

![tmap maplibre
mode](https://r-tmap.github.io/tmap.mapgl/reference/figures/mapbox_snow.jpg)

tmap maplibre mode
