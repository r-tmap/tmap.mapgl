# Providers "maplibre"

## Basemaps

Mapbox offers several basemap providers, namely:

``` r

tmap_mode("maplibre")
#> ℹ tmap modes "plot" -> "view" -> "mapbox" -> "maplibre"
#> ℹ rotate with `tmap::rtm()`switch to "plot" with `tmap::ttm()`
tmap_providers()
#> [1] "ofm.liberty"       "ofm.bright"        "ofm.positron"     
#> [4] "ofm.dark"          "ofm.fiord"         "carto.voyager"    
#> [7] "carto.positron"    "carto.dark_matter"
```

``` r

tm_shape(metro) + 
  tm_dots(size = "pop2020", fill = "red")+
tm_basemap("ofm.liberty")
```

Note that this default basemap from Open Free Maps also renders the
buildings in 3d!

``` r

tm_shape(NLD_dist[NLD_dist$code == "WK093500", ]) + 
  tm_borders() +
tm_basemap("ofm.liberty") +
    tm_maplibre(pitch = 60)
```
