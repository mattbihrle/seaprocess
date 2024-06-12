#' Find index of nearest value in vector
#'
#' @param vec
#' @param vals
#'
#' @return
#' @export
#'
#' @examples
find_near <- function(vec,vals) {

  locs <- rep(NA,length(vals))
  for (i in 1:length(vals)) {
    if(!is.na(vals[i])) {
      locs[i] <- which.min(abs(vec-vals[i]))
    } else {
      locs[i] <- NA
    }
  }

  return(locs)

}


#' Convert wind speed and wind direction to u and v
#'
#' @param ws input wind speed vector
#' @param wd input wind direction vector
#'
#' @return
#' @export
#'
#' @examples
wswd_to_uv <- function(ws,wd) {

  v = -ws*cos(wd*pi/180)
  u = -ws*sin(wd*pi/180)

  out <- tibble::tibble(u = u, v = v)

}


#' U and V to wind speed and wind direction
#'
#' @param u
#' @param v
#'
#' @return
#' @export
#'
#' @examples
uv_to_wswd <- function(u,v) {

  ws <- sqrt(u^2+v^2)
  wd <- 90 - atan2(v,u)*180/pi
  wd[wd<0&!is.na(wd)] <- wd[wd<0&!is.na(wd)] + 360

  out <- list(ws = ws, wd = wd)

  return(out)
}

DIM <- function(x) if(is.null(dim(x))) length(x) else dim(x)


add_file_cruiseID <- function(filename, cruiseID) {

  if(!is.null(filename) & !is.null(cruiseID)) {
    file_dir <- dirname(filename)
    filename <- basename(filename)
    filename <- paste0(cruiseID,"_",filename)
    filename <- file.path(file_dir,filename)
  } else {
    stop("filename or cruiseID are not set")
  }

  return(filename)

}

#' Plot Limits
#'
#' Experimental, Takes an elg file and outputs a vector of the limits of the max
#' and min lats and lons with a buffer for plotting.
#'
#' @param elg
#'
#' @return
#' @export
#'
#' @examples
plot_limits <- function(elg) {
  lat_max <- max(elg$lat, na.rm = T) + 3
  lat_min <- min(elg$lat, na.rm = T) -3

  lon <- dplyr::if_else(elg$lon < 0, elg$lon + 360, elg$lon)
  lon <- lon[!is.na(lon)]

lon_east <- dplyr::if_else(max(lon) > 180, max(lon) - 355, max(lon) + 5 )

lon_west <- dplyr::if_else(min(lon) > 180, min(lon) - 365, min(lon) - 5)
#
# if(abs(lon_i) > abs(lon_ii)){
#   lon_max
# }
# min(lon)
#   lon <- dplyr::if_else(elg$lon < 0, elg$lon + 360, elg$lon)
#
#   lon_max <- max(lon) + max(centered_lons) + 3
#   lon_max <- dplyr::if_else(lon_max > 180, lon_max - 360, lon_max)
#
#   lon_min <- min(lon) + min(centered_lons) - 3
#   lon_min <- dplyr::if_else(lon_min > 180, lon_min -360, lon_min)

  limits <- c(lon_west, lon_east, lat_max, lat_min)
  return(limits)
}

#' Center Longitude
#'
#' Experimental, A function that takes a vector of longitudes and centers them on the first longitude in the vector.
#'
#' @param elg
#'
#' @return
#' @export
#'
#' @examples
center_longitude <- function(elg) {
  first_longitude <- elg$lon[1]
  centered_longitudes <- elg$lon - first_longitude
  centered_longitudes[centered_longitudes > 180] <- centered_longitudes[centered_longitudes > 180] - 360
  centered_longitudes[centered_longitudes < -180] <- centered_longitudes[centered_longitudes < -180] + 360
  return(centered_longitudes)
}

