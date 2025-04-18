\donttest{
library(tmap.mapgl)
tmap_mode("maplibre")
NLD_dist$pop_dens = NLD_dist$population / NLD_dist$area
tm_shape(NLD_dist) +
	tm_polygons_3d(height = "pop_dens",
				   fill = "edu_appl_sci",
				   fill.scale = tm_scale_intervals(style = "kmeans", values = "-pu_gn"),
				   fill.legend = tm_legend("Univeristy degree")) +
	tm_maplibre(pitch = 45)
}
