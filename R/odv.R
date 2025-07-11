#' Wrapper function to format any odv file
#``
#' @param data
#' @param odv_output
#' @param data_type
#' @param cruiseID
#'
#' @return
#' @export
#'
#' @examples
format_odv <- function(data, odv_output, data_type = "CTD", cruiseID = NULL) {
# MB added MN

# MB TODO add in 2MN and TT to format_meter
  if("HC" %in% data_type | "B" %in% data_type ) {
    format_function <- format_bottle_odv
  } else if ("ctd" %in% data_type) {
    format_function <- format_ctd_odv
  } else if ("NT" %in% data_type) {
    format_function <- format_neuston_odv
  } else if ("elg" %in% data_type) {
    format_function <- format_elg_odv
  } else if ("adcp" %in% data_type) {
    format_function <- format_adcp_odv
  } else if ("MN" %in% data_type) {
    format_function <- format_meter_odv
  } else {
    format_function <- format_gen_odv
  }

  odv_out <- format_function(data, odv_output, cruiseID = cruiseID)

  return(odv_out)

}

#' Format ADCP Data for ODV inport
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
format_adcp_odv <- function(data, odv_output, cruiseID = NULL) {

  odv_out <- initialize_odv_tibble(data, cruiseID, type = "C")

  odv_out <- tibble::add_column(odv_out,
                                `Depth [m]` = data$depth,
                                `Echo Amplitude [counts]` = data$backscat,
                                `East Component [mm/s]` = data$u * 1000,
                                `North Component [mm/s]` = data$v * 1000,
                                `Magnitude [mm/s]` = data$sp * 1000,
                                `Direction [deg]` = data$dir,
                                Ensemble = 0)

  write_odv(odv_out, odv_output)

  return(odv_out)

}


#' Format CTD Data for ODV inport
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
format_ctd_odv <- function(data,file,cruiseID = NULL) {

  odv_out <- initialize_odv_tibble(data, cruiseID, type = "C")
  # MB added "data$beam_atten" for xmiss beam attenuation and "data$cdom" for CDOM flour
  #   to be retained in the ctd odv output
  # MB TODO check this function with other cruise data to make sure other sensors
  #   dont mess things up
  odv_out <- tibble::add_column(odv_out,
                                `Depth [m]` = data$dep,
                                `Temperature [~^oC]` = data$temp,
                                `Salinity [psu]` = data$sal,
                                `Potential Density[Kg/m~^3]` = data$sigtheta,
                                `Chl a Fluorescence [V]` = data$fluor,
                                `Oxygen,SBE43[~$m~#mol/kg]` = data$oxygen,
                                `Oxygen [mL/L]` = data$oxygen2,
                                `CDOM Fluorescence [mg/m~^3]` = data$cdom,
                                `PAR Irradience [~$m~#E/m~^2/s]` = data$par,
                                `Beam Attenuation [1/m]` = data$beam_atten)

  readr::write_tsv(odv_out,file)

  return(odv_out)

}


#' Format Hourly Data for ODV inport
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
format_elg_odv <- function(data, odv_output = NULL ,cruiseID = NULL) {
  odv_out <- initialize_odv_tibble(data, cruiseID, type = "C")

  # create lookup table for field names as known to elg_read
  # MB change "depth" to "bot_depth"
  odv_lookup <- tibble::tribble(~varname, ~odvname,
                          "sys_date", "System Date",
                          "sys_time", "System Time",
                          "nav_time", "Nav Time",
                          "nav_lon", "Nav Longitude",
                          "nav_lat", "Nav Latitude",
                          "nav_sog", "Nav Speed Over Ground [knots]",
                          "nav_cog", "Nav Course Over Ground [degrees true]",
                          "nav_quality", "Nav GPS Quality",
                          "lab_time", "Lab Time",
                          "lab_lon", "Lab Longitude",
                          "lab_lat", "Lab Latitude",
                          "lab_sog", "Lab Speed Over Ground [knots]",
                          "lab_cog", "Lab Course Over Ground [knots]",
                          "lab_quality", "Lab GPS Quality",
                          "temp", "Temperature [deg C]",
                          "temp_1min", "Temperature, 1 min avg [deg C]",
                          "temp_60min", "Temperature, 60 min avg [dec C]",
                          "sal", "Salinity [psu]",
                          "sal_1min", "Salinity, 1 min avg [psu]",
                          "sal_60min", "Salinity, 60 min avg [psu]",
                          "sound_vel", "Sound Velocity [m/s]",
                          "fluor", "Rel. Fluorescence",
                          "fluor_1min", "Rel. Fluorescence, 1 min avg",
                          "fluor_60min", "Rel. Fluorescence, 60 min avg",
                          "cdom", "CDOM [counts]",
                          "cdom_1min", "CDOM, 1 min avg [counts]",
                          "cdom_60min", "CDOM, 60 min avg [counts]",
                          "xmiss", "Transmissometer [counts]",
                          "xmiss_1min", "Transmissometer, 1 min avg [counts]",
                          "xmiss_60min", "Transmissometer, 60 min avg [counts]",
                          "wind_sp", "True Wind Speed [knots]",
                          "wind_dir", "True Wind Direction [degrees]",
                          "wind_sp_rel", "Relative Wind Speed [knots]",
                          "wind_dir_rel", "Relative Wind Direction [degrees]",
                          "heading", "Ship's Heading [degrees true]",
                          "pitch", "Pitch [degrees]",
                          "roll", "Roll [degrees]",
                          "bot_depth", "CHIRP depth [m]",
                          "wire_payout", "Wire Payout",
                          "wire_tension", "Wire Tension",
                          "wire_speed", "Wire Speed"
                          )
  # get names from elg, skip dttm, lat, lon; n, filename_first, filename_last if binned
  elg_names <- names(data)[! names(data) %in% c("dttm","lon","lat","n","filename_first","filename_last")]

  # subset elg to relevant columns by name
  odv_names <- dplyr::pull(odv_lookup[odv_lookup$varname %in% elg_names,"odvname"],odvname)

  # subset and rename columns by ODV name
  elg_sub <- data[,elg_names]
  colnames(elg_sub) <- odv_names

  # add Depth [m] in first spot
  odv_out <- tibble::add_column(odv_out, `Depth [m]` = 0)

  # bind data tibble with metadata
  odv_out <- dplyr::bind_cols(odv_out, elg_sub)

  # if wind speed and win dir, add E/W / N/S comp
  if(all(c("wind_sp", "wind_dir") %in% elg_names)){
    odv_out <- tibble::add_column(odv_out,
                                `Wind-E/W Comp. [m/s]` = wswd_to_uv(data$wind_sp,data$wind_dir)$u,
                                `Wind-N/S Comp. [m/s]` = wswd_to_uv(data$wind_sp,data$wind_dir)$v)
  }

  # odv_out <- tibble::add_column(odv_out,
  #                               `Depth [m]` = 0,
  #                               `Temperature [~^oC]` = data$temp,
  #                               `Salinity [PSU]` = data$sal,
  #                               `Fluorescence` = data$fluor,
  #                               `Wind Speed [knots]` = data$wind_sp,
  #                               `Wind Direction [deg]` = data$wind_dir,
  #                               `Wind-E/W Comp. [m/s]` = wswd_to_uv(data$wind_sp,data$wind_dir)$u,
  #                               `Wind-N/S Comp. [m/s]` = wswd_to_uv(data$wind_sp,data$wind_dir)$v,
  #                               `CDOM` = data$cdom,
  #                               `Xmiss` = data$xmiss)

  write_odv(odv_out, odv_output)

  return(odv_out)

}

#' Format Neuston Data for ODV import
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
format_neuston_odv <- function(data,file,cruiseID = NULL) {

  odv_out <- initialize_odv_tibble(data, cruiseID, type = "B")

  odv_out <- tibble::add_column(odv_out,
                                `Depth [m]` = 0,
                                `Temperature [~^oC]` = data$surf_temp_c,
                                `Salinity [PSU]` = data$surf_sal_psu,
                                `Fluorescence` = data$surf_chla_fluor)

  # Add the rest of the data
  # TODO create look-up sheet to find real names
  ii <- which(colnames(data) == "station_distance_m")
  odv_out <- dplyr::bind_cols(odv_out, data[ii:ncol(data)])

  readr::write_tsv(odv_out,file)

  return(odv_out)

}

#MB copy and paste from format neuston

#' Format Meter Net Data for ODV import
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
format_meter_odv <- function(data,file,cruiseID = NULL) {

  odv_out <- initialize_odv_tibble(data, cruiseID, type = "B")

  odv_out <- tibble::add_column(odv_out,
                                `Depth [m]` = 0,
                                `Temperature [~^oC]` = data$surf_temp_c,
                                `Salinity [PSU]` = data$surf_sal_psu,
                                `Fluorescence` = data$surf_chla_fluor)

  # Add the rest of the data
  # TODO create look-up sheet to find real names
  ii <- which(colnames(data) == "station_distance_m")
  odv_out <- dplyr::bind_cols(odv_out, data[ii:ncol(data)])

  readr::write_tsv(odv_out,file)

  return(odv_out)

}

#' Format bottle datasheet for ODV
#'
#' @param data
#' @param file
#' @param cruiseID
#'
#' @return
#' @export
#'
#' @examples
format_bottle_odv <- function(data, file = NULL, cruiseID = NULL) {

  odv_out <- initialize_odv_tibble(data, cruiseID, type = "B")

#MB add the other column names here
  odv_out <- tibble::add_column(odv_out,
                                `Depth [m]` = data$depth_m,
                                `Temperature [~^oC]` = data$temperature_c,
                                `Salinity [psu]` = data$salinity_psu,
                                `Phosphate [uM]` = data$po4_uM,
                                `Nitrate [uM]` = data$no3_uM,
                                `pH` = data$ph,
                                `Chl a [ug/L]` = data$chla_ug.L,
                                `Alkalinity [meq/L]` = data$alk_meq.L,
                                `Potential Density[Kg/m~^3]` = data$sigtheta_kg.m3,
                                `Chl a Fluorescence [V]` = data$chla_fluor_v,
                                `Oxygen,SBE43[~$m~#mol/kg]` = data$oxygen_uM.kg,
                                `Oxygen [mL/L]` = data$oxygen_mL.L,
                                `CDOM Fluorescence [mg/m~^3]` = data$cdom_fluor,
                                `PAR Irradience [~$m~#E/m~^2/s]` = data$par_mE.m2.s,
                                `Beam Attenuation [1/m]` = data$beamAttenuation)

  # Add the rest of the data skipping depth column
  # TODO create look-up sheet to find real names
  # 4.19 MB comment out these

  ##TEST BELOW
  fancy_names <- c("depth_m","temp_c", "sal_psu",
  "po4_uM",
  "no3_uM",
  "ph",
  "chla_ug.L",
  "alk_meq.L",
  "sigma_kg.m3",
  "chla_fluor_v",
  "oxygen_uM.kg",
  "oxygen_mL.L",
 "cdom_fluor",
  "par_mE.m2.s",
  "beamAttenuation")
  ii <- which(colnames(data) == "bottle")
  ii2 <- which(colnames(data) == "depth_m")

  #remove anything between bottle and two before depth_m and any of the "fancy names"
data_test <- dplyr::select(data, -dplyr::any_of(fancy_names) & c((ii+1):(ii2-2)))
  odv_out <- dplyr::bind_cols(odv_out, data_test)
  ## TESTS ABOVE
  # ii <- which(colnames(data) == "bottle")
  # ii2 <- which(colnames(data) == "depth_m")
  #
  # odv_out <- dplyr::bind_cols(odv_out, data[c(ii:(ii2-1), (ii2+1):ncol(data))])

  if(!is.null(file)){
    readr::write_tsv(odv_out,file)
  } else {
    return(odv_out)
  }

}

initialize_odv_tibble <- function(data, cruiseID = NULL, type = "C") {

  # Try and determine the name of the cruise from the data input if not specified. If not, assign "unknown".
  if(is.null(cruiseID)) {
    cruiseID = "unknown"
  }

  # change the value of station depending on whether it has a column in data
  if("station" %in% colnames(data)) {
    station <- data$station
  } else {
    station <- 1:nrow(data)
  }
  #Rename bottom depth to remove "_m" if it exists
  bottom_name <- c(bot_depth = "bot_depth_m")
  data <- dplyr::rename(data, any_of(bottom_name))
  odv_out <- tibble::tibble(Cruise = cruiseID,
                            Station = data$station,
                            Type = type,
                            `mon/day/yr` = format(data$dttm,"%m/%d/%Y"),
                            `hh:mm` = format(data$dttm,"%H:%M"),
                            `Lat [degrees_north]` = data$lat,
                            `Lon [degrees_east]` = data$lon,
                            `Bot. Depth [m]` = data$bot_depth)

  return(odv_out)

}

#' Write formatted data to odv txt file
#'
#' @param data
#' @param odv_output
#'
#' @return
#' @export
#'
#' @examples
write_odv <- function(odv_out, odv_output = NULL) {
  if(!is.null(odv_output)) {
    output <- purrr::safely(readr::write_tsv)(odv_out,odv_output)
    if(!is.null(output$error)) {
      warning("Couldn't export the data to a odv file. Most likely specified directory doesn't exist")
    }
  } else {
    warning("No file written - No output odv file specified")
  }
}

#' Import a txt file to ODV
#'
#' @param odv_txt path to the odv txt file you want to import
#'
#' @return
#' @export
#'
#' @examples
import_odv <- function(odv_txt) {
  odv_txt <- path.expand(odv_txt)
  cmd_file <- stringr::str_replace(odv_txt,".txt",".cmd")
  cmd_line <- paste("open_data_file",odv_txt)
  readr::write_lines(cmd_line,cmd_file)
  system(paste("/Applications/Ocean\\ Data\\ View\\ \\(64bit\\).app/Contents/MacOS/odv4 -x",cmd_file,"-q"))
}

#' Format General ODV
#'
#' Function that takes a datasheet created by SeaProcess and using a large
#' lookup table, attempts to match common column names to well formatted odv
#' names. Any columns that do not match will be left as is. After the matching
#' occurs, it outputs a tab delimited file for import into ODV.
#'
#'
#' @param data dataframe from csv output
#' @param file file path for odv output. Typcially will be
#'   "output/odv/<cruiseID>_<yourdatasheet>.txt"
#' @param cruiseID optional to add cruiseID as a column in odv file.
#'
#' @return
#' @export
#'
#' @examples
format_gen_odv <- function(data,file,cruiseID = NULL) {

  odv_out <- initialize_odv_tibble(data, cruiseID, type = "B")

  odv_out <- tibble::add_column(odv_out,
                                `Depth [m]` = 0,
                                `Temperature [~^oC]` = data$surf_temp_c,
                                `Salinity [PSU]` = data$surf_sal_psu,
                                `Fluorescence` = data$surf_chla_fluor)

  odv_lookup <- c("sys_date"= "System Date",
                  "sys_time"= "System Time",
                  "nav_time"= "Nav Time",
                  "nav_lon"= "Nav Longitude",
                  "nav_lat"= "Nav Latitude",
                  "nav_sog"= "Nav Speed Over Ground [knots]",
                  "nav_cog"= "Nav Course Over Ground [degrees true]",
                  "nav_quality"= "Nav GPS Quality",
                  "lab_time"= "Lab Time",
                  "lab_lon"= "Lab Longitude",
                  "lab_lat"= "Lab Latitude",
                  "lab_sog"= "Lab Speed Over Ground [knots]",
                  "lab_cog"= "Lab Course Over Ground [knots]",
                  "lab_quality"= "Lab GPS Quality",
                  "temp"= "Temperature [deg C]",
                  "temp_1min"= "Temperature, 1 min avg [deg C]",
                  "temp_60min" = "Temperature, 60 min avg [dec C]",
                  "sal"= "Salinity [psu]",
                  "sal_1min"= "Salinity, 1 min avg [psu]",
                  "sal_60min"= "Salinity, 60 min avg [psu]",
                  "sound_vel"= "Sound Velocity [m/s]",
                  "fluor"= "Rel. Fluorescence",
                  "fluor_1min"= "Rel. Fluorescence, 1 min avg",
                  "fluor_60min"= "Rel. Fluorescence, 60 min avg",
                  "cdom"= "CDOM [counts]",
                  "cdom_fluor" = "CDOM [counts]",
                  "cdom_1min"= "CDOM, 1 min avg [counts]",
                  "cdom_60min"= "CDOM, 60 min avg [counts]",
                  "xmiss"= "Transmissometer [counts]",
                  "xmiss_1min"= "Transmissometer, 1 min avg [counts]",
                  "xmiss_60min"= "Transmissometer, 60 min avg [counts]",
                  "wind_sp.*"= "True Wind Speed [knots]",
                  "wind_dir.*"= "True Wind Direction [degrees]",
                  "wind_sp_rel"= "Relative Wind Speed [knots]",
                  "wind_dir_rel"= "Relative Wind Direction [degrees]",
                  "heading.*"= "Ship's Heading [degrees true]",
                  "pitch"= "Pitch [degrees]",
                  "roll"= "Roll [degrees]",
                  "bot_depth.*"= "CHIRP depth [m]",
                  "wire_payout.*"= "Wire Payout",
                  "wire_tension.*"= "Wire Tension",
                  "wire_speed.*"= "Wire Speed",
                  "zooplankton_biovol.*"= "Zooplankton Biovolume [mL]",
                  "moon_phase.*"= "Moon Phase [%]",
                  "zooplankton_biodens.*m2"="Zooplankton Biodensity [mL/m2]",
                  "zoop.*bioden.*m3"= "Zooplankton Biodensity [mL/m3]",
                  "shannon_wiener.*"= "Shannon-Wiener Diversity",
                  "cloud_cover_pct"= "Cloud Cover [%]",
                  "wave_height_ft"= "Wave Height [ft]",
                  "wind_sp.*b.*f.*"= "Wind Speed [BF]",
                  "secchi_depth.*"= "Secchi Depth [m]",
                  "beau.*f.*"= "Beaufort Force",
                  "po4_uM"="Phosphate [uM]",
                  "no3_uM"= "Nitrate [uM]",
                  "chla_ug.L"="Chl a [ug/L]" ,
                  "alk_meq.L"= "Alkalinity [meq/L]",
                  "sigtheta_kg.m3"= "Potential Density[Kg/m~^3]" ,
                  "oxygen_uM.kg"= "Oxygen,SBE43[~$m~#mol/kg]",
                  "oxygen_mL.L"= "Oxygen [mL/L]" ,
                  "par_mE.m2.s"= "PAR Irradience [~$m~#E/m~^2/s]" ,
                  "beam_atten.*"= "Beam Attenuation [1/m]",
                  "beamAttenuation"= "Beam Attenuation [1/m]")


  # get names from datasheet, skip dttm, lat, lon; n, filename_first, filename_last if binned
  datasheet_names <- names(data)[! names(data) %in% c("station", "date", "time_in",
                                                      "time_out", "zd", "dttm","lon","lat","n", "surf_temp_c", "surf_sal_psu", "surf_chla_fluor", "filename_first","filename_last")]

  odv_names <- stringr::str_replace_all(datasheet_names, odv_lookup)


  # subset and rename columns by ODV name
  data_sub <- data[,datasheet_names]
  colnames(data_sub) <- odv_names

  odv_out <- dplyr::bind_cols(odv_out, data_sub)

  readr::write_tsv(odv_out,file)

  return(odv_out)
}
