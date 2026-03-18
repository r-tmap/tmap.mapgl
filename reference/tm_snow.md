# Map layer: let it snow!

Map layer that generates snow.

## Usage

``` r
tm_snow(
  density = 0.85,
  intensity = 1,
  color = "#ffffff",
  opacity = 1,
  center_thinning = 0.4,
  direction = c(0, 50),
  flake_size = 0.71,
  vignette = 0.3,
  vignette_color = "#ffffff"
)
```

## Arguments

- density:

  A number between 0 and 1 controlling the snow particles density.
  Default is 0.85.

- intensity:

  A number between 0 and 1 controlling the snow particles movement
  speed. Default is 1.0.

- color:

  A string specifying the color of the snow particles. Default is
  "#ffffff".

- opacity:

  A number between 0 and 1 controlling the snow particles opacity.
  Default is 1.0.

- center_thinning:

  A number between 0 and 1 controlling the thinning factor of snow
  particles from center. Default is 0.4.

- direction:

  A numeric vector of length 2 defining the azimuth and polar angles of
  the snow direction. Default is c(0, 50).

- flake_size:

  A number between 0 and 5 controlling the snow flake particle size.
  Default is 0.71.

- vignette:

  A number between 0 and 1 controlling the snow vignette screen-space
  effect. Default is 0.3.

- vignette_color:

  A string specifying the snow vignette screen-space corners tint color.
  Default is "#ffffff".

## Value

tmap layer

## Details

Arguments and their default values have been taken from
\[mapbox::set_snow()\] (package version 0.2.2)

## See also

\[tm_rain()\]
