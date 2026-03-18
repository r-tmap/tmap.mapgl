# Mapbox mode options

Mapbox mode options. These options are specific to the mapbox mode.

## Usage

``` r
tm_mapbox(pitch, control.position, control.collapse, zoom)
```

## Arguments

- pitch:

  The pitch angle

- control.position:

  The position of the layer control box

- control.collapse:

  Should the layer control box be collapsed?

- zoom:

  The zoom level of the map

## Value

a \[tmap::tmap-element\]

## Examples

``` r
# \donttest{
library(tmap)
library(tmap.mapgl)
# getting API: https://walker-data.com/mapgl/articles/getting-started.html
# check API envir var: Sys.getenv("MAPBOX_PUBLIC_TOKEN")

tmap_mode("mapbox")
#> ℹ tmap modes "plot" -> "view" -> "mapbox" -> "maplibre"
#> ℹ rotate with `tmap::rtm()`switch to "plot" with `tmap::ttm()`
#> This message is displayed once per session.
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
