% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapgl_components.R
\name{tm_geocoder}
\alias{tm_geocoder}
\title{Map component: (credits) text}
\usage{
tm_geocoder(stack, position, group_id, z)
}
\arguments{
\item{stack}{stack with other map components, either `"vertical"` or `"horizontal"`.}

\item{position}{The position specification of the component: an object created with `tm_pos_in()` or `tm_pos_out()`. Or, as a shortcut, a vector of two values, specifying the x and y coordinates. The first is `"left"`, `"center"` or `"right"` (or upper case, meaning tighter to the map frame), the second `"top"`, `"center"` or `"bottom"`. Numeric values are also supported, where 0, 0 means left bottom and 1, 1 right top. See also \href{https://r-tmap.github.io/tmap/articles/adv_positions}{vignette about positioning}. In case multiple components should be combined (stacked), use `group_id` and specify `component` in [tm_comp_group()].}

\item{group_id}{Component group id name. All components (e.g. legends, titles, etc) with the same `group_id` will be grouped. The specifications of how they are placed (e.g. stacking, margins etc.) are determined in [tm_comp_group()] where its argument `id` should correspond to `group_id`.}

\item{z}{z index, e.g. the place of the component relative to the other componets}
}
\description{
Map component that adds a text, typically used as credits. This function is the same as [tm_title()] but with different default values.
}
\seealso{
\href{https://r-tmap.github.io/tmap/articles/basics_components}{Vignette about components}
}
