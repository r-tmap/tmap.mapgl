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
# \donttest{
library(tmap)
library(tmap.mapgl)
# getting API: https://walker-data.com/mapgl/articles/getting-started.html
# check API envir var: Sys.getenv("MAPBOX_PUBLIC_TOKEN")

tmap_mode("mapbox")
#> ℹ tmap modes "plot" -> "view" -> "mapbox" -> "maplibre"
tm_shape(World) +
  tm_polygons("HPI", fill.scale = tm_scale_intervals(values = "brewer.rd_yl_gn"))
#> Error in mapgl::mapboxgl(center = c(0, 0), zoom = zoom, pitch = o$pitch,     style = style): A Mapbox access token is required. Get one from your account at https://www.mapbox.com, and do one of the following:
#> ℹ Run `usethis::edit_r_environ()` and add the line MAPBOX_PUBLIC_TOKEN='your_token_goes_here';
#> ℹ Install the mapboxapi R package and run `mb_access_token('your_token_goes_here', install = TRUE)`
#> ℹ Alternatively, supply your token to the `access_token` parameter in this function or run `Sys.setenv(MAPBOX_PUBLIC_TOKEN='your_token_goes_here') for this session.

tm_shape(NLD_dist) +
  tm_polygons("employment_rate",
    fill.scale = tm_scale_intervals(values = "scico.roma"),
    lwd = 0.1) +
tm_shape(NLD_muni) +
  tm_polygons(fill = NULL, lwd = 1) +
tm_mapbox(pitch = 60) +
tm_basemap("mapbox.dark")
#> Error in mapgl::mapboxgl(center = ll, zoom = zoom, pitch = o$pitch, style = style): A Mapbox access token is required. Get one from your account at https://www.mapbox.com, and do one of the following:
#> ℹ Run `usethis::edit_r_environ()` and add the line MAPBOX_PUBLIC_TOKEN='your_token_goes_here';
#> ℹ Install the mapboxapi R package and run `mb_access_token('your_token_goes_here', install = TRUE)`
#> ℹ Alternatively, supply your token to the `access_token` parameter in this function or run `Sys.setenv(MAPBOX_PUBLIC_TOKEN='your_token_goes_here') for this session.
# }
```
