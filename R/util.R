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
#' A rudimentary function that takes a data frame (one of our datasheets) and outputs a vector
#' of the limits of the max and min lats and lons with a buffer for plotting.
#' The output is specifically formatted to match the output required for
#' plotting within ggOceanMaps.
#'
#' @param data data frame with a lat and lon column in decimal degrees.
#' @param lat_pad Numerical, buffer for latitudinal values. Default is 3
#'   degrees.
#' @param lon_pad Numerical, buffer for longitudinal values. Default is 5
#'   degrees.
#'
#' @return a vector of values formatted as c(lon_west, lon_east, lat_max,
#'   lat_min). This vector can be created by hand if needed.
#' @export
#'
#' @examples
plot_limits <- function(data, lat_pad = 3, lon_pad = 5) {
  lat_max <- max(data$lat, na.rm = T) + lat_pad
  lat_min <- min(data$lat, na.rm = T) - lat_pad

  lon <- dplyr::if_else(data$lon < 0, data$lon + 360, data$lon)
  lon <- lon[!is.na(lon)]

lon_east <- dplyr::if_else(max(lon) > 180, max(lon) - (360 - lon_pad), max(lon) + lon_pad)

lon_west <- dplyr::if_else(min(lon) > 180, min(lon) - (360 + lon_pad), min(lon) - lon_pad)

  limits <- c(lon_west, lon_east, lat_max, lat_min)

  if (any(limits > 180)){
    warning(paste("One or more plotting limits exceeds 180 degrees.",
                  "This may result in an odd plot. Adjust lat_pad and lon_pad to fix."))
  }
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

#' Cruise ID to ship name
#'
#' Takes a standard cruise ID and looks for an S or C at the beginning to create
#' a variable called "ship." Used to change processing based on other ship needs
#' since we use cruiseID in all functions.
#'
#'
#' @param cruiseID string formatted as "C___" or S___"
#'
#' @return
#' @export
#'
#' @examples
cruise2ship <- function (cruiseID) {
  #make cruiseid lowercase

  cruiseID <- stringr::str_to_lower(cruiseID)

  # Look for a C or R
  if (stringr::str_detect(cruiseID, "^c")) {
    ship = "cc"
    return(ship)
  }
  if (stringr::str_detect(cruiseID, "^s")) {
    ship = "rcs"
    return(ship)
  }
  if (stringr::str_detect(cruiseID, "^g")) {
    ship = "gene"
    } else {
    warning("Unable to parse ship name from cruiseID.")
  }
return()
}

