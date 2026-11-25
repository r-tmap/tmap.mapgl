# Maplibre mode options

Maplibre mode options. These options are specific to the maplibre mode.

## Usage

``` r
tm_maplibre(
  pitch,
  bearing,
  control.position,
  control.collapse,
  zoom,
  pattern.zoom
)
```

## Arguments

- pitch:

  The pitch angle

- bearing:

  The bearing (rotation angle) of the map, in degrees

- control.position:

  The position of the layer control box

- control.collapse:

  Should the layer control box be collapsed?

- zoom:

  The zoom level of the map

- pattern.zoom:

  How fill patterns behave when zooming: `"screen"` (default) keeps them
  at a fixed size on the screen, like the other patterns of tmap;
  `"map"` attaches them to the map, so that they are enlarged when
  zooming in (as if they were painted on the map). When zoomed in far,
  the patterns become blurry, since the pattern images are magnified.

## Value

a
[tmap::tmap-element](https://r-tmap.github.io/tmap/reference/tmap-element.html)

## Examples

``` r
# \donttest{
library(tmap)
library(tmap.mapgl)
tmap_mode("maplibre")
#> ℹ tmap modes "plot" -> "view" -> "mapbox" -> "maplibre"
#> ℹ rotate with `tmap::rtm()`switch to "plot" with `tmap::ttm()`
#> This message is displayed once per session.
tm_shape(World) +
  tm_polygons("HPI", fill.scale = tm_scale_intervals(values = "brewer.rd_yl_gn"))

tm_shape(NLD_dist) +
  tm_polygons("employment_rate",
    fill.scale = tm_scale_intervals(values = "scico.roma"),
    lwd = 0.1) +
  tm_shape(NLD_muni) +
  tm_polygons(fill = NULL, lwd = 1) +
tm_mapbox(pitch = 60) +
tm_basemap("carto.dark_matter")
# }
```
