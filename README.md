
# tmap.mapgl: two new tmap modes: mapbox and maplibre <img src="man/figures/logo.png" align="right" height="139" alt="" />

[tmap](https://r-tmap.github.io/tmap/) is a R package for visualizing
spatial data. This package is an extension. It features two new modes:
`"mapbox"` and `"maplibre"`.

## Installation

This package is in development so the development version of both
**tmap** and **tmap.mapgl** are required.

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

For `"mapbox"` an API key is required, which is free for personal use.

#### mapbox API key

1.  Go to [Mapbox Access
    Tokens](https://docs.mapbox.com/help/getting-started/access-tokens/)
    and follow the instructions
2.  Set the API key as a system environment variable. This can be done
    by
    1.  running `usethis::edit_r_environ()` which creates/opens a text
        file named `.Renviron` in your user’s home folder
    2.  adding the line `MAPBOX_PUBLIC_TOKEN="abc123"`, where abc123
        should be replaced by your API key obtained in step 1.

## Example

``` r
library(tmap)
library(tmap.mapgl)

tmap_mode("maplibre")

tm_shape(World) + 
  tm_polygons("well_being")
```
