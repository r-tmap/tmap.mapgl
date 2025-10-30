frc = function(row, col) paste0(sprintf("%02d", row), "_", sprintf("%02d", col))

select_sf = function(shpTM, dt) {
	shp = shpTM$shp
	stid = shpTM$tmapID

	dtid = dt$tmapID__

	tid = intersect(stid, dtid)

	d = data.table::data.table(sord = seq_along(tid), ord = dt$ord__[match(tid, dtid)], tid = tid)
	if ("ord" %in% names(d)) {
		data.table::setkeyv(d, cols = c("ord", "sord"))
	} else {
		data.table::setkeyv(d, cols = "sord")
	}
	sid = match(d$tid, stid)

	shpSel = shp[sid] #st_cast(shp[match(tid, tmapID)], "MULTIPOLYGON")

	# assign prop_ vectors to data dt (to be used in plotting) e.g. prop_angle is determined in tmapTransCentroid when along.lines = TRUE
	prop_vars = names(shpTM)[substr(names(shpTM), 1, 5) == "prop_"]
	if (length(prop_vars)) {
		for (p in prop_vars) {
			pname = substr(p, 6, nchar(p))
			dt[[pname]] = shpTM[[p]][sid]
		}
	}

	dt = dt[match(d$tid, dtid), ]
	list(shp = shpSel, dt = dt)
}

impute_gp = function(gp, dt) {
	dtn = setdiff(names(dt), c("tmapID__", paste0("by", 1L:3L, "__")))

	cols = paste0("__", dtn)
	gp1 = sapply(gp, "[[", 1)
	gpids = which(gp1 %in% cols)
	#gp[gpids] = as.list(dt[, dtn, with = FALSE])

	for (i in gpids) gp[i] = as.list(dt[, dtn[match(gp1[i], cols)], with = FALSE])
	gp
}

rescale_gp = function(gp, scale, skip = character()) {
	if ("lwd" %in% names(gp) && (!"lwd" %in% skip)) gp$lwd = gp$lwd * scale
	if ("size" %in% names(gp) && (!"size" %in% skip)) gp$size = gp$size * sqrt(scale)
	if ("cex" %in% names(gp) && (!"cex" %in% skip)) gp$cex = gp$cex * sqrt(scale)
	gp
}

findZoom = function(b) {
	## calculate zoom level
	# borrowed from https://github.com/dkahle/ggmap/blob/master/R/calc_zoom.r
	lon_diff = b[3] - b[1]
	lat_diff = b[4] - b[2]

	zoomlon = ceiling(log2(360 * 2/lon_diff))
	zoomlat = ceiling(log2(180 * 2/lat_diff))
	zoom = as.integer(min(zoomlon, zoomlat))
}


process_color <- function(col, alpha=NA, sepia_intensity=0, saturation=1, color_vision_deficiency_sim="none") {
	#if (length(col)>100) browser()
	isFactor <- is.factor(col)

	if (isFactor) {
		x <- as.integer(col)
		col <- levels(col)
	}

	res <- t(col2rgb(col, alpha=TRUE))

	# set alpha values
	if (!is.na(alpha)) res[res[,4] != 0, 4] <- alpha * 255

	# convert to sepia
	if (sepia_intensity!=0) {
		conv_matrix <- matrix(c(.393, .769, .189,
								.349, .686, .168,
								.272, .534, .131), ncol=3, byrow=FALSE)
		res[,1:3] <-  (res[,1:3] %*% conv_matrix) * sepia_intensity + res[,1:3] * (1-sepia_intensity)
		res[res>255] <- 255
		res[res<0] <- 0
	}

	# convert to black&white
	if (saturation!=1) {
		res[,1:3] <- (res[,1:3] %*% matrix(c(.299, .587, .114), nrow=3, ncol=3))  * (1-saturation) + res[,1:3] * saturation
		res[res>255] <- 255
		res[res<0] <- 0
	}
	if (all(res[,4]==255)) res <- res[,-4, drop=FALSE]

	new_cols <- do.call("rgb", c(unname(as.data.frame(res)), list(maxColorValue=255)))

	rlang::check_installed("colorspace")
	# color blind sim
	sim_colors = switch(color_vision_deficiency_sim,
						deutan = colorspace::deutan,
						protan = colorspace::protan,
						tritan = colorspace::tritan,
						function(x) x)

	new_cols2 = sim_colors(new_cols)

	if (isFactor) {
		new_cols2[x]
	} else {
		new_cols2
	}
}


pchs = stats::setNames(c(seq(0L, 25L, 1L), seq(100L, 109L, 1L)),
					   c(c('open-rect', 'open-circle', 'open-triangle', 'simple-plus',
					   	'simple-cross', 'open-diamond', 'open-down-triangle', 'cross-rect',
					   	'simple-star', 'plus-diamond', 'plus-circle', 'hexagram', 'plus-rect',
					   	'cross-circle', 'triangle-rect', 'solid-rect', 'solid-circle-md',
					   	'solid-triangle', 'solid-diamond', 'solid-circle-bg', 'solid-circle-sm', 'circle',
					   	'rect', 'diamond', 'triangle', 'down-triangle'
					   ),
					   c('rect', 'circle', 'triangle', 'plus', 'cross', 'diamond', 'star', 'stadium', 'line', 'polygon')
					   ))


get_pch_names = function(x) {
	if (is.numeric(x)) {
		if (!(all(x %in% pchs | x > 999))) stop("Unknown symbol values", call. = FALSE)
		y = names(pchs)[match(x, pchs)]
		y[x > 999] = x[x>999]
		y
	} else {
		if (!all(x %in% names(pchs))) stop("Unknown symbol values", call. = FALSE)
		x
	}
}


get_option_class = function(o, class = NULL, spatial_class = TRUE) {
	is_spatial = !spatial_class || (any(names(o) %in% c("stars", "sf", "sfc", "raster", "terra", "sp", "dimensions")))
	if (!is.null(class) && is_spatial) { # && is.list(o)
		mtch = which(names(o) %in% class)
		if (!length(mtch)) mtch = which(names(o) == "")[1]
		o = o[[mtch]]
	}
	o
}

# from stars package
is_regular_grid = function (x) {
	has_raster(x) && !(has_rotate_or_shear(x) || is_rectilinear(x) ||
					   	is_curvilinear(x))
}

has_raster = function (x) {
	if (inherits(x, "stars"))
		x = stars::st_dimensions(x)
	!is.null(r <- attr(x, "raster")) && all(r$dimensions %in%
												names(x))
}

has_rotate_or_shear = function (x) {
	dimensions = stars::st_dimensions(x)
	if (has_raster(x)) {
		r = attr(dimensions, "raster")
		!anyNA(r$affine) && any(r$affine != 0)
	}
	else FALSE
}

is_curvilinear = function (x) {
	d = stars::st_dimensions(x)
	has_raster(x) && isTRUE(attr(d, "raster")$curvilinear)
}

is_rectilinear = function (x) {
	d = stars::st_dimensions(x)
	if (has_raster(x) && !is_curvilinear(x)) {
		xy = attr(d, "raster")$dimensions
		dimx = d[[xy[1]]]
		dimy = d[[xy[2]]]
		(is.na(dimx$delta) || is.na(dimy$delta)) && (!regular_intervals(dimx$values) ||
													 	!regular_intervals(dimy$values))
	}
	else FALSE
}

regular_intervals = function (x, epsilon = 1e-10) {
	if (length(x) <= 1) {
		FALSE
	} else {
		ud = if (is.atomic(x))
			unique(diff(x))
		else {
			if (identical(tail(x$end, -1), head(x$start, -1)))
				x$end - x$start
			else return(FALSE)
		}
		abs(diff(range(ud))/mean.default(ud)) < epsilon
	}
}

consider_global = function (x, th = 0.6)
{
	b = sf::st_bbox(x)
	# in case margins are applied
	if (sf::st_is_longlat(b)) {
		b["xmin"] = max(b["xmin"], -180)
		b["xmax"] = min(b["xmax"], 180)
		b["ymin"] = max(b["ymin"], -90)
		b["ymax"] = min(b["ymax"], 90)
	}
	if (b$xmin == b$xmax || b$ymin == b$ymax) return(FALSE)
	earth_surface = 5.1e+14
	area = as.numeric(sf::st_area(sf::st_as_sfc(b)))
	area > (earth_surface * 0.6)
}


number_text_lines = function (txt)
{
	if (is.character(txt)) {
		length(strsplit(txt, "\n")[[1]])
	}
	else 1
}



fancy_breaks <- function(vec, as.count = FALSE, interval.disjoint = FALSE, intervals=FALSE, interval.closure="left", fun=NULL, scientific=FALSE, big.num.abbr = c("mln" = 6, "bln" = 9), prefix = "", suffix = "", text.separator="to", text.less.than=c("less", "than"), text.less.than_as.prefix = TRUE, text.or.more=c("or", "more"), text.or.more_as.prefix = FALSE, text.align="left", text.to.columns=FALSE, digits=NA, html.escape = TRUE, ...) {
	args <- list(...)
	n <- length(vec)

	args$called = NULL

	digits_defined = !is.na(digits)

	if (!intervals) {
		as.count = FALSE
		interval.disjoint = FALSE
	}

	if (inherits(vec, c("POSIXct", "POSIXlt", "Date"))) {
		x = format(vec)
	} else if (!is.null(fun) && !interval.disjoint) {
		if (as.count) {
			steps <- (vec[-1] - vec[-n])
			vec <- c(vec, vec - 1L, vec + 1L) # needed for: {1, 2, ... 9}
			digits <- 0
		}
		x <- do.call(fun, list(vec))
		if (as.count) {
			x1 <- x[1:(n-1)]
			x2 <- x[(n+2):(2*n)]
			x1p1 <- x[(2*n+1):(3*n-1)]
		}
	} else if (all(is.infinite(vec))) {
		x <- as.character(vec)
	} else {
		# calculate magnitude, needed to determine digits and big number abbreviations
		vec_fin <- unique(vec[!is.infinite(vec)])
		frm <- gsub(" ", "", sprintf("%20.10f", abs(vec_fin)))
		mag <- max(nchar(frm)-11)

		if (as.count) {
			steps <- (vec[-1] - vec[-n])
			vec <- c(vec, vec - 1L, vec + 1L) # needed for: {1, 2, ... 9}
			digits <- 0
		} else {
			# get number of decimals (which is number of decimals in vec, which is reduced when mag is large)
			ndec <- max(10 - nchar(frm) + nchar(sub("0+$","",frm)))
			if (!digits_defined) {
				digits <- max(min(ndec, 4-mag), 0)

				# add sign to frm
				frm_sign <- unique(paste0(ifelse(vec_fin<0, "-", "+"), frm))

				# test if number of digits is sufficient for unique labels
				if (!scientific) {
					while (anyDuplicated(substr(frm_sign, 1, nchar(frm_sign)-10 + digits)) && (digits < 10)) {
						digits <- digits + 1
					}

					# if (interval.disjoint) {
					# 	## NEW
					# 	#update vec_fin
					# 	# add 'to' numbers and see if digits should be increased
					# 	vec_fin2 = c(vec_fin, vec_fin[-1] - 10^-digits)
					# 	while (anyDuplicated(na.omit(vec_fin2))) {
					# 		digits = digits + 1
					# 		vec_fin2 = c(vec_fin, vec_fin[-1] - 10^-digits)
					# 	}
					# 	vec = c(vec, vec - 10^-digits)
					# }

				}

				# } else {
				# 	if (interval.disjoint) {
				# 		vec = c(vec, vec - 10^-digits)
				# 	}
			}
		}


		if ((!is.null(fun) && interval.disjoint)) {
			x <- do.call(fun, list(vec))
		} else if (!scientific || as.count) {

			# check whether big number abbrevations should be used
			ext <- ""
			if (!is.na(big.num.abbr[1])) {
				big.num.abbr <- sort(big.num.abbr, decreasing = TRUE)
				for (i in 1:length(big.num.abbr)) {
					o <- unname(big.num.abbr[i])
					if ((mag > o && all(vec - floor(vec/(10^o))*(10^o) < 1))) { #mag>(o+2) || removed
						vec <- vec / (10^o)
						ext <- paste0(" ", names(big.num.abbr)[i])
						break
					}
				}
			}

			if (interval.disjoint) {

				if (!digits_defined) {
					# update digits
					vec_fin <- unique(vec[!is.infinite(vec)])
					vec_fin2 = c(vec_fin, vec_fin[-1] - 10^-digits)
					while (anyDuplicated(na.omit(vec_fin2))) {
						digits = digits + 1
						vec_fin2 = c(vec_fin, vec_fin[-1] - 10^-digits)
					}
				}
				# add to points
				vec = c(vec, head(vec, -1) - 10^-digits, tail(vec, 1))
			}

			# set default values
			if (!("big.mark" %in% names(args))) args$big.mark <- ","
			if (!("format" %in% names(args))) args$format <- "f"
			if (!("preserve.width" %in% names(args))) args$preserve.width <- "none"

			x = do.call("formatC", c(list(x=vec, digits=digits), args))
			if (as.count || interval.disjoint) {
				x[(n+2):(2*n)] = paste0(x[(n+2):(2*n)], ext)
			} else {
				x = paste0(x, ext)
			}
			x <- paste0(prefix, x, suffix)
		} else {
			if (!("format" %in% names(args))) args$format <- "g"
			x <- do.call("formatC", c(list(x=vec, digits=digits), args))
		}

		if (as.count) {
			x1 <- x[1:(n-1)]
			x2 <- x[(n+2):(2*n)]
			x1p1 <- x[(2*n+1):(3*n-1)]
		} else if (interval.disjoint) {
			x1 = x[1:(n-1)]
			x2 = x[(n+2):(2*n)]


		}
		# x <- formatC(vec, format = "f", digits = 0)
		# x1 <- x[-n]
		# x2 <- formatC(vec[-1] - 1L, format = "f", digits = 0)
		# xs <- (vec[-1] - vec[-n])
		# x1p1 <- formatC(vec[-n] + 1L, format = "f", digits = 0)
	}

	if (intervals) {
		if (scientific) {
			if (as.count) {
				# discrete
				lbls <- paste("{", x1, "}", sep = "")
				lbls[steps == 2] <- paste("{", x1[steps == 2], ", ", x2[steps == 2], "}", sep="")
				lbls[steps > 2] <- paste("{", x1[steps > 2], ", ", x1p1[steps > 2], ", ..., ", x2[steps > 2], "}", sep="")
			} else {
				# continuous
				if (interval.closure=="left") {
					lbls <- paste("[", x[-n], ", ", x[-1], ")", sep="")
					lbls[n-1] <- paste(substr(lbls[n-1], 1, nchar(lbls[n-1])-1), "]", sep="")
				} else {
					lbls <- paste("(", x[-n], ", ", x[-1], "]", sep="")
					lbls[1] <- paste("[", substr(lbls[1], 2, nchar(lbls[1])), sep="")
				}
			}


		} else {
			if (as.count) {
				lbls <- x1
				lbls[steps>1] <- paste(x1[steps>1], x2[steps>1], sep = paste0(" ", text.separator, " "))
				if (vec[n]==Inf) lbls[n-1] <- paste2(x1[n-1], paste(text.or.more, collapse = " "), flipped = text.or.more_as.prefix)
			} else if (interval.disjoint) {
				lbls <- paste(x1, x2, sep = paste0(" ", text.separator, " "))
				if (vec[1]==-Inf) lbls[1] <- paste2(x1[2], paste(text.less.than, collapse = " "), flipped = text.less.than_as.prefix)
				if (vec[n]==Inf) lbls[n-1] <- paste2(x1[n-1], paste(text.or.more, collapse = " "), flipped = text.or.more_as.prefix)
			} else {
				x[vec==-Inf] <- ""

				lbls <- paste(x[-n], x[-1], sep = paste0(" ", text.separator, " "))
				if (vec[1]==-Inf) lbls[1] <- paste2(x[2], paste(text.less.than, collapse = " "), flipped = text.less.than_as.prefix)
				if (vec[n]==Inf) lbls[n-1] <- paste2(x[n-1], paste(text.or.more, collapse = " "), flipped = text.or.more_as.prefix)
			}

			if (text.to.columns) {
				#xtra <- as.numeric(!is.na(text.align) && text.align=="right")


				nc1 <- nchar(paste(x[-n], " ", sep = "")) + 1
				nc2 <- rep(nchar(paste(text.separator, " ", sep = "")), n-1)

				lbls_breaks <- matrix(c(nc1, nc1+nc2), ncol=2)

				if (vec[1]==-Inf) {
					if (length(text.less.than)==1) {
						lbls_breaks[1,] <- rep(nchar(paste(text.less.than[1], " ", sep = "")) + 1, 2)
					} else {
						lbls_breaks[1,] <- cumsum(c(nchar(paste(text.less.than[1], " ", sep = "")) + 1, nchar(text.less.than[2])+1))
					}
				}
				if (vec[n]==Inf) {
					if (length(text.or.more)==1) {
						lbls_breaks[n-1,] <- rep(nchar(paste(x[n-1], " ", sep = "")) + 1, 2)
					} else {
						lbls_breaks[n-1,] <- cumsum(c(nchar(paste(x[n-1], " ", sep = "")) + 1, nchar(text.or.more[1])+1))
					}

				}
				attr(lbls, "brks") <- lbls_breaks
			}




		}
	}

	y <- if (intervals) lbls else x
	attr(y, "align") <- text.align
	y
}
