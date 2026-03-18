# Maplibre mode options

Maplibre mode options. These options are specific to the maplibre mode.

## Usage

``` r
tm_maplibre(pitch, control.position, control.collapse, zoom)
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
tmap_mode("maplibre")
#> ℹ tmap modes "plot" -> "view" -> "mapbox" -> "maplibre"
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
