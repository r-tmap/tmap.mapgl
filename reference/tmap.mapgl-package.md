# Extensions to 'tmap' with Two New Modes: 'mapbox' and 'maplibre'

The 'tmap' package provides two plotting modes for static and
interactive thematic maps. This package extends 'tmap' with two
additional modes based on 'mapbox' \<https://mapbox.com\> and 'maplibre'
\<https://maplibre.org\>. These modes feature interactive vector tiles,
globe views, and other modern web-mapping capabilities, while using the
same 'tmap' syntax for creating maps (Tennekes 2018,
\<doi:10.32614/RJ-2018-027\>).

## See also

Useful links:

- <https://github.com/r-tmap/tmap.mapgl>

- <https://r-tmap.github.io/tmap.mapgl/>

- Report bugs at <https://github.com/r-tmap/tmap.mapgl/issues>

## Author

Martijn Tennekes <mtennekes@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{
library(tmap)
library(tmap.mapgl)
# getting API: https://walker-data.com/mapgl/articles/getting-started.html
# check API envir var: Sys.getenv("MAPBOX_PUBLIC_TOKEN")

tmap_mode("mapbox")
tm_shape(World) +
  tm_polygons("HPI", fill.scale = tm_scale_intervals(values = "brewer.rd_yl_gn"))

tm_shape(NLD_dist) +
  tm_polygons("employment_rate",
    fill.scale = tm_scale_intervals(values = "scico.roma"),
    lwd = 0.1) +
tm_shape(NLD_muni) +
  tm_polygons(fill = NULL, lwd = 1) +
tm_mapbox(pitch = 60) +
tm_basemap("mapbox.dark")
} # }
```
