#' Read RDI ADCP ensemble file
#'
#' Read in a RDI Acoustic Current Dopler Current Profiler ensemble file with
#' extension .ENS, .ENR, .ENX, .STA, or .LTA.
#'
#' This makes use of the read.adp() function from the oce package which decodes
#' the binary format of a RDI ADCP and creates an oce adcp object. This is then
#' converted to a more readily used format, a list of available fields.
#'
#' @param adcp_file The file name of the ensemble file
#' @param calc_echo Whether or not to calculate the echo intensity from the backscatter
#'
#' @return
#' @export
#'
read_adcp <- function(adcp_file, calc_echo = FALSE, to_tibble = TRUE) {

  # Read the ensemble file using the oce package
  # Remove 'suppressWarnings for warnings messages
  suppressWarnings(
  adcp <- oce::read.adp(adcp_file)
  )

  # Use the speed made good components of the boat trajectory to computer true u and v components
  adcp@data$v[,,1] <- adcp@data$v[,,1] + adcp@data$speedMadeGoodEast
  adcp@data$v[,,2] <- adcp@data$v[,,2] + adcp@data$speedMadeGoodNorth

  # Bespok blanking function for bottom tracking mode
  # if(hasName(adcp@data,"br")) {
  #   dblank = 25
  #   br <- rowMeans(adcp@data$br,na.rm = T) - dblank
  #   iblank <- sea::find_near(adcp@data$distance,br)
  #   iblank[is.na(iblank)] <- 1
  #   nd <- length(adcp@data$distance)
  #   for (i in 1:length(iblank)) {
  #     adcp@data$v[i,iblank[i]:nd,1] <- NA
  #     adcp@data$v[i,iblank[i]:nd,2] <- NA
  #   }
  #
  # }

  # Compute the average latitude of ensemble file by taking mean of first and last latitudes
  lat <- rowMeans(cbind(adcp@data$firstLatitude,adcp@data$lastLatitude))

  # Computer mean longitude of sample based of mean of first and last longitudes
  # Adjusts for passing the anti-meridian
  difflon <- adcp@data$firstLongitude - adcp@data$lastLongitude
  if(length(which(abs(difflon) > 1)) != 0) {
    ii <- which(abs(difflon) > 1)
    for (i in ii) {
      if(difflon[i] > 0) {
        adcp@data$lastLongitude[i] <- adcp@data$lastLongitude[i] + 360
      } else {
        adcp@data$lastLongitude[i] <- adcp@data$lastLongitude[i] - 360
      }
    }
  }
  lon <- rowMeans(cbind(adcp@data$firstLongitude,adcp@data$lastLongitude))
  lon[lon>180] <- lon[lon>180] - 360
  lon[lon< -180] <- lon[lon< -180] + 360

  # select the time vector
  dttm <- adcp@data$time

  # select the depth vector
  d <- adcp@data$distance

  # Extract backscat, quality and percent good
  backscat <- rowMeans(adcp[["a","numeric"]],dims=2)
  quality <- rowMeans(adcp[["q","numeric"]],dims=2)
  percent <- adcp[["g","numeric"]][ , , 4]
  if(is.null(dim(percent))) {
    percent = matrix(percent,nrow = 1, ncol = length(percent))
  }

  # extract temperature and speed of sound
  temp <- adcp@data$temperature
  sound_speed <- adcp@data$soundSpeed

  # Create a list of good values that are non-duplicates in lon and lat
  goodi <- !(duplicated(lon) & duplicated(lat))

  # Put all the values into a list for export
  adcp <- list(u = adcp@data$v[goodi , , 1], v = adcp@data$v[goodi , , 2],
               w = adcp@data$v[goodi , , 3], err = adcp@data$v[goodi , , 4],
               backscat = backscat[goodi,], quality = quality[goodi,], percent = percent[goodi,],
               lon = lon[goodi], lat = lat[goodi], dttm = dttm[goodi], d = d,
               temp = temp[goodi], sound_speed = sound_speed[goodi])


  # calculate the echo intensity
  if(calc_echo) {
    adcp <- calc_echo_amp(adcp)
  }

  if(to_tibble) {
    print(adcp_file)
    adcp <- adcp_to_tibble(adcp)
  }

  # return the adcp object
  return(adcp)

}


#' Read all ADCP Ensemble files in a folder and combine
#'
#' @param adcp_fold
#'
#' @return
#' @export
#'
#' @examples
read_adcp_fold <- function(adcp_fold, file_type = c(".LTA",".STA"),
                               file_select = NULL, calc_echo = FALSE,
                               combine = FALSE, combine_na = TRUE, sort_dttm = FALSE, ...) {

  files <- NULL
  i <- 1
  while(length(files) == 0 & i <= length(file_type)) {
    files <- list.files(adcp_fold,file_type[i])
    i <- i+1
  }

  if(length(files) == 0) {
    stop("No adcp files of specified file_type found in folder")
  }


  if(!is.null(file_select))
    files <- files[select]

  adcp = NULL
  for (i in 1:length(files)) {

    adcp_add <- read_adcp(file.path(adcp_fold,files[i]), calc_echo = calc_echo, ...)

    if(is.null(adcp_add)) {
      warning(paste("CTD in file", files[i], "was not added to complete data"))
      next
    }

    # check if the conversion to tibble should happen (default TRUE)
    if(tibble::is_tibble(adcp_add)) {

      # add a column for filename
      adcp_add <- dplyr::mutate(adcp_add, file = files[i])

      # check to see if this is the first file to be added
      # otherwise bind to previous data
      if(is.null(adcp)) {
        adcp <- adcp_add
      } else {
        adcp <- dplyr::bind_rows(adcp, adcp_add)
      }

      # option to keep the ctds as a list
      # (for troubleshooting - this should not be for default data processing)
    } else {
      if(!is.null(adcp_add)) {
        adcp <- append(adcp,adcp_add)
      }
    }

      if(combine) {
        # find the longest depth vector in the adcp options and assign to dvec
        maxd <- rep(NA,length(adcp_in))
        for (i in 1:length(adcp_in)) {
          maxd[i] <- length(adcp_in[[i]]$d)
        }
        ii <- which.max(maxd)
        dvec <- adcp_in[[ii]]$d

        # loop through adcp objects, interpolate and combine
        for (i in 1:length(adcp_in)) {

          if (i == 1) {
            if(i == ii) {
              adcp_out <- adcp_in[[i]]
            } else {
              adcp_out <- interp_adcp(adcp_in[[i]],dvec)
            }

          } else {

            if(i == ii) {
              adcp_add <- adcp_in[[i]]
            } else {
              adcp_add <- interp_adcp(adcp_in[[i]],dvec)
              if(is.null(adcp_add)) {
                next
              }
            }

            if(combine_na == T) {
              com <- NA
              com_mat <- matrix(NA,1,length(dvec))
            } else {
              com <- NULL
              com_mat <- NULL
            }
            adcp_out$u <- rbind(adcp_out$u,com_mat,adcp_add$u)
            adcp_out$v <- rbind(adcp_out$v,com_mat,adcp_add$v)
            adcp_out$backscat <- rbind(adcp_out$backscat,com_mat,adcp_add$backscat)
            adcp_out$quality <- rbind(adcp_out$quality,com_mat,adcp_add$quality)
            adcp_out$percent <- rbind(adcp_out$percent,com_mat,adcp_add$percent)
            if(calc_echo == TRUE) {
              adcp_out$Idb <- rbind(adcp_out$Idb,com_mat,adcp_add$Idb)
            }

            adcp_out$lon <- c(adcp_out$lon,com,adcp_add$lon)
            adcp_out$lat <- c(adcp_out$lat,com,adcp_add$lat)
            adcp_out$dttm <- c(adcp_out$dttm,mean(c(tail(adcp_out$dttm,1),head(adcp_add$dttm,1)),na.rm=T),adcp_add$dttm)
            adcp_out$temp <- c(adcp_out$temp,com,adcp_add$temp)
            adcp_out$sound_speed <- c(adcp_out$sound_speed,com,adcp_add$sound_speed)
          }
        }

        if(sort_dttm) {
          sorti <- order(adcp_out$dttm)
          adcp_out$u <- adcp_out$u[sorti, ]
          adcp_out$v <- adcp_out$v[sorti, ]
          if(calc_echo == TRUE) {
            adcp_out$Idb <- adcp_out$Idb[sorti, ]
          }
          adcp_out$backscat <- adcp_out$backscat[sorti, ]
          adcp_out$quality <- adcp_out$quality[sorti, ]
          adcp_out$percent <- adcp_out$percent[sorti, ]
          adcp_out$lon <- adcp_out$lon[sorti]
          adcp_out$lat <- adcp_out$lat[sorti]
          adcp_out$dttm <- adcp_out$dttm[sorti]
          adcp_out$temp <- adcp_out$temp[sorti]
          adcp_out$sound_speed <- adcp_out$sound_speed[sorti]
        }

        if(calc_echo == FALSE)
          adcp_out <- calc_echo_amp(adcp_out)

        return(adcp_out)

      }



  }
  return(adcp)

}



#' Interpolate an adcp field to different depths
#'
#' @param adcp
#' @param dvec
#' @param vars
#'
#' @return
#' @export
#'
#' @examples
interp_adcp <- function(adcp,dvec,vars = c("u","v","Idb","backscat","quality","percent")) {

  # Create empty output object
  output <- NULL

  lenv <- dim(adcp[["u"]])[1]
  if(is.null(lenv)) {
    return(NULL)
  }

  for (var in vars) {

    out <- array(NA,c(lenv,length(dvec)))
    for (i in 1:lenv) {
      goodi <- !is.na(adcp[[var]][i,])
      if(sum(goodi)<2) {
        next
      } else {
        if(abs(min(adcp$d[goodi])-min(dvec))<10) {
          rule = 2:1
        } else {
          rule <- 1
        }
        out[i, ] <- approx(adcp$d[goodi],adcp[[var]][i,goodi],dvec,rule=rule)$y
      }
    }
    output[[var]] <- out
  }

  output[["lon"]] <- adcp[["lon"]]
  output[["lat"]] <- adcp[["lat"]]
  output[["dttm"]] <- adcp[["dttm"]]
  output[["temp"]] <- adcp[["temp"]]
  output[["sound_speed"]] <- adcp[["sound_speed"]]
  output[["d"]] <- dvec

  return(output)


}


#' Calculate echo amplitude from backscatter in an adcp object
#'
#' Props to Ali Della Penna for help with this
#'
#'
#' @param adcp
#'
#' @return
#' @export
#'
#' @examples
calc_echo_amp <- function(adcp) {
  bs <- adcp$backscat
  t <- adcp$dttm
  distance <- adcp$d

  Enoise <- min(bs,na.rm = T)  # estimate of the noise as the smallest value we can detect
  E <- bs
  temp <- adcp$temp
  c <- adcp$sound_speed

  # some specs of the instrument
  d_bin <- 10 # size of each depth bin
  theta <- 30 # angle of the transducer
  blank <- 18.67 # blank -> the length where the data are not usable
  alpha <- 0.027; # the sound attenuation coefficient: this changes with temperature and salinity, but at a first approximation it can be seen as constant



  K1 <- 6.1; # transmit power-> needs to be confirmed but from the manifacturer it should be 6.1
  K2 <- 2.2; # system noise constant -> needs to be provided by the manufacturer - this value needs to be confirmed
  # using for now the one found in the Field Service technical paper 003
  Ks <- 1.09*10^5; #% constant depending on frequency - from Field Service technical paper 003
  Tx <- temp  # temperature of the transducer
  Te <- temp # temperature in the water column

  Kc <- 127.3 / (Te + 273); #% conversion factor for each intensity

  P <- 16 # pulse duration: verify this number
  Idb = matrix(data=NA, nrow=length(temp), ncol=length(distance)) # initialize the matrix

  for (tm_cnt in c(1:length(temp))){
    for (depth_level in c(1:length(distance))){
      R <- distance[depth_level]
      # we calculate for each time and each depth the echo return using the equation used in Picco et al., 2016
      Idb[tm_cnt,depth_level] <- 10 * log10((4.47*10^(-20)*K2*Ks*(273+Tx[tm_cnt])*(10^(Kc[tm_cnt]*(E[tm_cnt,depth_level]-Enoise)/10)-1))*R^2/(c[tm_cnt]*P*K1*10^(-2.*alpha*R/10)));
    }
  }

  adcp$Idb <- Idb

  return(adcp)

}


adcp_to_tibble <- function(adcp_data, cruiseID = NULL) {

  nc <- ifelse(is.na(DIM(adcp_data$u)[2]),
               DIM(adcp_data$u)[1],
               DIM(adcp_data$u)[2])

  nb <- ifelse(is.na(DIM(adcp_data$u)[2]),
               1,
               DIM(adcp_data$u)[1])

  spdir <- uv_to_wswd(adcp_data$u,adcp_data$v)

  odv_out <- tibble::tibble(
    station = rep(1:nb, each = nc),
    dttm = rep(adcp_data$dttm, each = nc),
    lon = rep(adcp_data$lon, each = nc),
    lat = rep(adcp_data$lat, each = nc),
    depth = rep(adcp_data$d, nb),
    u = as.vector(t(adcp_data$u)),
    v = as.vector(t(adcp_data$v)),
    w = as.vector(t(adcp_data$v)),
    sp = as.vector(t(spdir$ws)),
    dir = as.vector(t(spdir$wd)),
    err = as.vector(t(adcp_data$err)),
    backscat = as.vector(t(adcp_data$backscat)),
    quality = as.vector(t(adcp_data$quality)),
    percent = as.vector(t(adcp_data$percent)),
    temp = rep(adcp_data$temp, each = nc),
    sound_speed = rep(adcp_data$sound_speed, each = nc)
  )

  return(odv_out)

}
