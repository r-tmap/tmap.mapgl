# Export tmap to mapbox and maplibre

\* \`tmap_mapbox()\` returns a \[\`mapgl\`\]\[mapgl::mapboxgl()\] object
(\`"mapbox" mode\`) \* \`tmap_maplibre()\` a
\[\`mapgl\`\]\[mapgl::maplibregl()\] object (\`"maplibre"\` mode).

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

  passed on to \[\`tmap\`\]\[tmap::print.tmap()\]

## Value

a \[\`mapgl\`\]\[mapgl::mapboxgl()\] object (\`"mapbox" mode\`) or a
\[\`mapgl\`\]\[mapgl::maplibregl()\] object (\`"maplibre"\` mode). In
case small multiples are shown, a list is returned.

## Examples

``` r
library(tmap)
library(tmap.mapgl)
map = tm_shape(World) + tm_polygons()
tmap_maplibre(map, show = TRUE)
```
