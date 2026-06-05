# Providers "mapbox"

## Basemaps

Mapbox offers several basemap providers, namely:

``` r

tmap_mode("mapbox")
#> ℹ tmap modes "plot" -> "view" -> "mapbox" -> "maplibre"
#> ℹ rotate with `tmap::rtm()`switch to "plot" with `tmap::ttm()`
tmap_providers()
#>  [1] "mapbox.standard"            "mapbox.streets"            
#>  [3] "mapbox.outdoors"            "mapbox.light"              
#>  [5] "mapbox.dark"                "mapbox.satellite"          
#>  [7] "mapbox.satellite_streets"   "mapbox.navigation_day"     
#>  [9] "mapbox.navigation_night"    "mapbox.standard_satellite" 
#> [11] "ofm.positron"               "ofm.liberty"               
#> [13] "ofm.bright"                 "ofm.dark"                  
#> [15] "ofm.fiord"                  "carto.voyager"             
#> [17] "carto.positron"             "carto.dark_matter"         
#> [19] "carto.voyager_nolabels"     "carto.positron_nolabels"   
#> [21] "carto.dark_matter_nolabels" "maptiler.streets"          
#> [23] "maptiler.streets.dark"      "maptiler.streets.light"    
#> [25] "maptiler.basic"             "maptiler.basic.dark"       
#> [27] "maptiler.basic.light"       "maptiler.bright"           
#> [29] "maptiler.outdoor"           "maptiler.topo"             
#> [31] "maptiler.winter"            "maptiler.satellite"        
#> [33] "maptiler.hybrid"            "maptiler.ocean"            
#> [35] "maptiler.dataviz"           "maptiler.dataviz.dark"     
#> [37] "maptiler.dataviz.light"     "maptiler.backdrop"         
#> [39] "maptiler.backdrop.dark"     "maptiler.backdrop.light"   
#> [41] "maptiler.landscape"         "maptiler.openstreetmap"    
#> [43] "esri.navigation"            "esri.navigation_night"     
#> [45] "esri.streets"               "esri.streets_night"        
#> [47] "esri.streets_relief"        "esri.community"            
#> [49] "esri.outdoor"               "esri.topographic"          
#> [51] "esri.terrain"               "esri.imagery"              
#> [53] "esri.light_gray"            "esri.dark_gray"            
#> [55] "esri.oceans"                "esri.hillshade"            
#> [57] "esri.human_geography"       "esri.human_geography_dark" 
#> [59] "esri.charted_territory"     "esri.colored_pencil"       
#> [61] "esri.nova"                  "esri.modern_antique"       
#> [63] "esri.midcentury"            "esri.newspaper"
```

``` r

tm_shape(metro) + 
  tm_dots(size = "pop2020", fill = "red")+
tm_basemap("mapbox.streets")
```

![tmap mapbox
mode](https://r-tmap.github.io/tmap.mapgl/reference/figures/mapbox_streets.jpg)

tmap mapbox mode
