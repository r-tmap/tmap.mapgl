
# tmap.mapgl: two new tmap modes: mapbox and maplibre <img src="man/figures/logo.png" align="right" height="139" alt="" />

[tmap](https://r-tmap.github.io/tmap/) is a R package for visualizing
spatial data. This package is an extension. It features two new modes:
`"mapbox"` and `"maplibre"`.

## Installation

This package is in development so the devopment version of both **tmap**
and **tmap.mapgl** are required.

### tmap

``` r
# install.packages("remotes")
remotes::install_github("r-tmap/tmap")

# install.packages("pak")
pak::pak("r-tmap/tmap")

# Or from r-universe
install.packages("tmap", repos = c("https://r-tmap.r-universe.dev", "https://cloud.r-project.org"))
```

For Linux and macOS users who are new to working with spatial data in R,
this may fail since additional (non-R) libraries are required (which are
automatically installed for Windows users).

### tmap.mapgl

``` r
# install.packages("remotes")
remotes::install_github("r-tmap/tmap.mapgl")

# install.packages("pak")
pak::pak("r-tmap/tmap.mapgl")

# Or from r-universe
install.packages("tmap.mapgl", repos = c("https://r-tmap.r-universe.dev", "https://cloud.r-project.org"))
```

## Example

``` r
library(tmap)
library(tmap.mapgl)

tmap_mode("mapbox")

tm_shape(World) + 
  tm_polygons("well_being",
    fill.scale = tm_scale_continuous(values = "pu_gn"))
```

<figure>
<img
src="https://r-tmap.github.io/tmap.mapgl/reference/figures/mapbox_well_being.jpg"
alt="tmap mapbox mode" />
<figcaption aria-hidden="true">tmap mapbox mode</figcaption>
</figure>
