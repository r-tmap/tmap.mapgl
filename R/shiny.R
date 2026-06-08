# internal helpers -----------------------------------------------------------

.mapgl_output_fun <- function(gs) switch(gs,
										 Maplibre = mapgl::maplibreOutput,
										 Mapbox   = mapgl::mapboxglOutput)

.mapgl_proxy_fun <- function(gs) switch(gs,
										Maplibre = mapgl::maplibre_proxy,
										Mapbox   = mapgl::mapboxgl_proxy)

# renderTmap -----------------------------------------------------------------


.renderTmapGS_mapgl <- function(x, expr, env, ...) {
	expr <- bquote(utils::getFromNamespace("print.tmap", "tmap")(.(expr), in.shiny = TRUE))
	htmlwidgets::shinyRenderWidget(expr, .mapgl_output_fun(x$gs), env, quoted = TRUE)
}

#' @exportS3Method tmap::renderTmapGS
renderTmapGS.Maplibre <- .renderTmapGS_mapgl

#' @exportS3Method tmap::renderTmapGS
renderTmapGS.Mapbox <- .renderTmapGS_mapgl

# tmapOutput -----------------------------------------------------------------

.tmapOutputGS_mapgl <- function(x, outputId, width, height, ...) {
	.mapgl_output_fun(x$gs)(outputId = outputId, width = width, height = height)
}

#' @exportS3Method tmap::tmapOutputGS
tmapOutputGS.Maplibre <- .tmapOutputGS_mapgl

#' @exportS3Method tmap::tmapOutputGS
tmapOutputGS.Mapbox <- .tmapOutputGS_mapgl

# tmapProxy ------------------------------------------------------------------

.tmapProxyGS_mapgl <- function(x, mapId, session, tmobj, ...) {
	print(tmobj,
			   m = .mapgl_proxy_fun(x$gs)(mapId, session),  # maplibre_proxy / mapboxgl_proxy
			   show = FALSE, in.shiny = TRUE, proxy = TRUE)
}


#' @exportS3Method tmap::tmapProxyGS
tmapProxyGS.Maplibre <- .tmapProxyGS_mapgl

#' @exportS3Method tmap::tmapProxyGS
tmapProxyGS.Mapbox <- .tmapProxyGS_mapgl


mapgl_layer_ids = function(e, zindex) {
	reg = e$layer_zindex
	if (is.null(reg) || !length(reg)) return(character(0))
	unlist(reg[as.character(zindex)], use.names = FALSE)
}


