# Providers "mapbox"

## Basemaps

Mapbox offers several basemap providers, namely:

``` r

tmap_mode("mapbox")
#> ℹ tmap modes "plot" -> "view" -> "mapbox" -> "maplibre"
#> ℹ rotate with `tmap::rtm()`switch to "plot" with `tmap::ttm()`
tmap_providers()
#>  [1] "mapbox.standard"           "mapbox.streets"           
#>  [3] "mapbox.outdoors"           "mapbox.light"             
#>  [5] "mapbox.dark"               "mapbox.satellite"         
#>  [7] "mapbox.satellite_streets"  "mapbox.navigation_day"    
#>  [9] "navigation_night"          "mapbox.standard_satellite"
#> [11] "ofm.liberty"               "ofm.bright"               
#> [13] "ofm.positron"              "ofm.dark"                 
#> [15] "ofm.fiord"
```

``` r

tm_shape(metro) + 
  tm_dots(size = "pop2020", fill = "red")+
tm_basemap("mapbox.streets")
```

![tmap mapbox
mode](https://r-tmap.github.io/tmap.mapgl/reference/figures/mapbox_streets.jpg)

tmap mapbox mode
