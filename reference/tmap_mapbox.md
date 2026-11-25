# Export tmap to mapbox and maplibre

- `tmap_mapbox()` returns a
  [`mapgl`](https://walker-data.com/mapgl/reference/mapboxgl.html)
  object (`"mapbox" mode`)

- `tmap_maplibre()` a
  [`mapgl`](https://walker-data.com/mapgl/reference/maplibre.html)
  object (`"maplibre"` mode).

## Usage

``` r
tmap_mapbox(x, show = FALSE, ...)

tmap_maplibre(x, show = FALSE, ...)
```

## Arguments

- x:

  a tmap object.

- show:

  show the map?

- ...:

  passed on to
  [`tmap`](https://r-tmap.github.io/tmap/reference/print.tmap.html)

## Value

a [`mapgl`](https://walker-data.com/mapgl/reference/mapboxgl.html)
object (`"mapbox" mode`) or a
[`mapgl`](https://walker-data.com/mapgl/reference/maplibre.html) object
(`"maplibre"` mode). In case small multiples are shown, a list is
returned.

## Examples

``` r
library(tmap)
library(tmap.mapgl)
map = tm_shape(World) + tm_polygons()
tmap_maplibre(map, show = TRUE)
```
