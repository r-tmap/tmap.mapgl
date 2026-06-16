# Providers "maplibre"

## Basemaps

Maplibre offers several basemap providers, namely:

``` r

tmap_mode("maplibre")
#> ℹ tmap modes "plot" -> "view" -> "mapbox" -> "maplibre"
#> ℹ rotate with `tmap::rtm()`switch to "plot" with `tmap::ttm()`
tmap_providers()
#>  [1] "ofm.positron"               "ofm.liberty"               
#>  [3] "ofm.bright"                 "ofm.dark"                  
#>  [5] "ofm.fiord"                  "carto.voyager"             
#>  [7] "carto.positron"             "carto.dark_matter"         
#>  [9] "carto.voyager_nolabels"     "carto.positron_nolabels"   
#> [11] "carto.dark_matter_nolabels" "maptiler.streets"          
#> [13] "maptiler.streets.dark"      "maptiler.streets.light"    
#> [15] "maptiler.basic"             "maptiler.basic.dark"       
#> [17] "maptiler.basic.light"       "maptiler.bright"           
#> [19] "maptiler.outdoor"           "maptiler.topo"             
#> [21] "maptiler.winter"            "maptiler.satellite"        
#> [23] "maptiler.hybrid"            "maptiler.ocean"            
#> [25] "maptiler.dataviz"           "maptiler.dataviz.dark"     
#> [27] "maptiler.dataviz.light"     "maptiler.backdrop"         
#> [29] "maptiler.backdrop.dark"     "maptiler.backdrop.light"   
#> [31] "maptiler.landscape"         "maptiler.openstreetmap"    
#> [33] "esri.navigation"            "esri.navigation_night"     
#> [35] "esri.streets"               "esri.streets_night"        
#> [37] "esri.streets_relief"        "esri.community"            
#> [39] "esri.outdoor"               "esri.topographic"          
#> [41] "esri.terrain"               "esri.imagery"              
#> [43] "esri.light_gray"            "esri.dark_gray"            
#> [45] "esri.oceans"                "esri.hillshade"            
#> [47] "esri.human_geography"       "esri.human_geography_dark" 
#> [49] "esri.charted_territory"     "esri.colored_pencil"       
#> [51] "esri.nova"                  "esri.modern_antique"       
#> [53] "esri.midcentury"            "esri.newspaper"
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
