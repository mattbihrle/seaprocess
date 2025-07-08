#' Create complete data sheet from xls input
#'
#' This function reads in a hand-entered excel data sheet along with a station
#' summary csv file (created using `create_summary()`) and combines them to
#' produce well-formatted csv and odv file outputs.
#'
#' data_input is the only argument that the function *needs* to create an
#' output. The default values for summary_input and the csv and odv outputs are
#' set to work with the default directory configuration of the SEA cruise
#' project so shouldn't need to be set unless you are trying to produce
#' alternative outputs for specific custom cases or trials.
#'
#' data_type can be either a stand-alone value entered into the deployment
#' column of the station summary sheet. Or it can take one of these special
#' cases:
#'
#' * "CTD" for ctd datasheets (default)
#' * "neuston" for neuston datasheets
#' * "bottle" for bottle datasheets
#' * "meter" for meter net datasheets
#' * "<custom>" for custom datasheets you've created and given a unique code to in station summary
#'
#' See "Introduction to SeaProcess" vignette for more information about
#' processing specific to neuston tows and meter nets.
#'
#' @param data_input File path for the .xls file with hand-recorded data values.
#'   These are created by default in the "datasheets" folder and all have the
#'   suffix "_input"
#' @param summary_input File path for the csv summary datasheet produced with
#'   create_summary() (see details below). This defaults to the output/csv
#'   folder in the standard SEA data project organization
#' @param data_type The data type code that will draw data from the summary
#'   sheet (see details below)
#' @param csv_folder The directory path to output the csv file. Set to NULL for
#'   no csv output.
#' @param csv_filename The csv filename to output the data
#' @param odv_folder The directory path to output the odv file. Set to NULL for
#'   no odv output.
#' @param odv_filename The odv .txt filename to output the data
#' @param odv_export Logical defaults to TRUE. Will try to create an ODV
#'   formatted datasheet. If the data type is not neuston, meter, bottle,
#'   hydrocast, or CTD-- Seaprocess will use the format_gen_odv() function to
#'   make an odv output.
#' @param cruiseID Optional string specifying cruise ID (i.e. "S301")
#' @param add_cruise_ID If cruiseID is set, logical to specify whether cruiseID
#'   should be appended to beginning of filenames for csv and odv output
#' @param add_deployment_type logical to tell function whether to add the
#'   default deployment type to the beginning of the filename that is being
#'   created.
#' @param add_deployment_fold logical to tell function whether to add a new
#'   directory to the end of the odv_folder directory path to keep .txt files
#'   separate. Will create the new directory name depending on data_type
#' @param preserve_col_names logical. Function will default to renaming columns
#'   so they are primarily alphanumeric and underscores. If you would like to
#'   toggle this option "off", set to TRUE. The function will make your column
#'   names lowercase but otherwise leave them unchanged. NOTE: This is only
#'   recommended for datasheets that are not automatically created. Setting to
#'   TRUE will cause errors for neuston, meter, bottle, secchi, ctd.
#' @param process_calc logical to tell the function whether to look for
#'   calculation sheets to add to the datasheet. Likely this will mostly be used
#'   with reef data. See ?seaprocess::read_calc_sheet_mb or the "Setup and EOC"
#'   vignette for more information
#' @param calc_folder Defaults to NULL. If you wish to append calculation sheet
#'   information to your datasheet, set this file path to a folder containing
#'   all the calculation sheets you would like to use for that specific
#'   datasheet. This should be autocreated within the 'datasheets' folder
#' @param ... optional arguments to be sent to compile_bottle. Can be used to
#'   process niskins on the wire. See the documentation for that function
#'   (`?seaprocess::compile_bottle()`) for more information.
#'
#'
#' @return If assigned to an object, the function will return the formatted
#'   tibble that was exported to csv
#'
#' @md
#'
#' @export
#'
create_datasheet <- function(data_input, summary_input = "output/csv/summary_datasheet.csv",
                             data_type = "CTD",
                             csv_folder = "output/csv", csv_filename = "datasheet.csv",
                             odv_folder = "output/odv", odv_filename = "datasheet.txt",
                             odv_export = TRUE,
                             cruiseID = NULL, add_cruiseID = TRUE,
                             add_deployment_type = TRUE,
                             add_deployment_subfold = TRUE,
                             preserve_col_names = FALSE, process_calc = FALSE,
                             calc_folder = NULL, ...) {

  if(add_cruiseID == TRUE & !is.null(cruiseID)) {
    if(summary_input == "output/csv/summary_datasheet.csv") {
      summary_input <- add_file_cruiseID(summary_input, cruiseID)
    }
  }


  if(data_type == "bottle") {

    if(add_deployment_type) {
      csv_filename <- paste0("bottle_", csv_filename)
      odv_filename <- paste0("bottle_", odv_filename)
    }
    odv_export <- TRUE
    if(add_deployment_subfold) {
      odv_folder <- file.path(odv_folder,"bottle")
    }
    data_type <- c("HC","SS")

  } else if (data_type == "neuston" | data_type == "NT") {

    if(add_deployment_type) {
      csv_filename <- paste0("neuston_", csv_filename)
      odv_filename <- paste0("neuston_", odv_filename)
    }
    odv_export <- TRUE
    if(add_deployment_subfold) {
      odv_folder <- file.path(odv_folder,"neuston")
    }
    data_type <- "NT"

  } else if (data_type == "CTD") {
    data_type <- c("CTD","HC")
    if(add_deployment_type) {
      csv_filename <- paste0("ctd_", csv_filename)
      odv_filename <- paste0("ctd_", odv_filename)
    }
    odv_export <- FALSE
  } else if (data_type == "meter" | data_type == "MN" | data_type == "TT" |
             data_type == "2MN") {

    if(add_deployment_type) {
      csv_filename <- paste0("meter_", csv_filename)
      odv_filename <- paste0("meter_", odv_filename)
    }
    odv_export <- TRUE
    if(add_deployment_subfold) {
      odv_folder <- file.path(odv_folder,"meter")
    }
    data_type <- c("MN")
  } else {
    if(add_deployment_type) {
      csv_filename <- paste0(stringr::str_to_lower(data_type), "_", csv_filename)
      odv_filename <- paste0(stringr::str_to_lower(data_type), "_", odv_filename)
    }
  }

  # read in the data_input excel sheet datasheet
  data <- readxl::read_excel(data_input)


  #Set all header names to lower case
  colnames(data) <- stringr::str_to_lower(colnames(data))
 if (!preserve_col_names){
   #replace spaces with _
  colnames(data) <- stringr::str_replace_all(colnames(data), " ", "_")

  # replace - with _
  colnames(data) <- stringr::str_replace_all(colnames(data), "-", "_")

  #if moon data exists (Neutson) remove the true/false
  colnames(data) <- stringr::str_remove_all(colnames(data), "_\\(t/f\\)")
  # replace % with 'pct'
  colnames(data) <- stringr::str_replace_all(colnames(data), "%", "pct")

  #replace # with 'count'
  colnames(data) <- stringr::str_replace_all(colnames(data), "#", "count")

  #remove parentheses
  colnames(data) <- stringr::str_remove_all(colnames(data), "\\(")
  colnames(data) <- stringr::str_remove_all(colnames(data), "\\)")

  # remove degree symbol
  colnames(data) <- stringr::str_remove_all(colnames(data), "\u00b0")

  # remove periods
  colnames(data) <- stringr::str_remove_all(colnames(data), "\\.")

  # replace \ with .
  colnames(data) <- stringr::str_replace_all(colnames(data), "\\/", ".")
 }

  # MB add MN and 2MN TT as numeric
  # makes a neuston correction if not enough rows to make good on the excel read
  if(sum(data_type %in% c("NT", "MN", "2MN", "TT"))>0) {
    # Make sure all non note/description/station columns are numeric
    # MB add 'file' 4.17
    suppressWarnings(
      data <- dplyr::mutate(data, dplyr::across(!dplyr::matches("note|desc|stat|file"),as.numeric))
    )
  }
  # read in station summary datasheet
  # TODO: determine what formatting to apply when read in (beyond zd)
  suppressWarnings(
    summary <- readr::read_csv(
      summary_input,
      col_types = readr::cols(
        zd = readr::col_character(),
        time_in = readr::col_time(),
        time_out = readr::col_time(),
        date = readr::col_date()
      )
    )
  )
  problems <- (readr::problems(summary))
  if(nrow(problems) > 0) {
    warning(paste("One or more summary values formatted incorrectly and will be set to 'NA'.", "See table for more information."))
    print(problems)
  }
  # filter by data_type
  summary <- dplyr::filter(summary, deployment %in% data_type)


  # Bottle specific stuff for combining summary, otherwise right join
  if(sum(data_type %in% c("HC", "SS")) > 1) {
    data <- dplyr::mutate(data, bottle = as.character(bottle))

    # add a new column to aid the joining later
    suppressWarnings(
      data <- dplyr::mutate(data,
                            deployment = ifelse(is.na(as.numeric(bottle)), "SS", "HC"))
    )
    check_stations(data, summary, bottle = TRUE)

    #Make water chemistry columns numeric to aid in calculations later on
    suppressWarnings(
      data <- dplyr::mutate(data, dplyr::across(!dplyr::matches("stat|note|deployment|bott"), as.numeric))
    )
    data <- dplyr::right_join(summary, data, by=c("station","deployment"))

    data <- compile_bottle(data, ...)

  } else {
    check_stations(data, summary, bottle = FALSE)
    data <- dplyr::right_join(summary, data, by=c("station"))
  }

  # Neuston specific stuff
  if(sum(data_type %in% "NT")>0) {
    data <- compile_neuston(data)
  }

  # Meter net specific stuff
  # MB added MN, 2MN, and TT
  if(sum(data_type %in% c("MN","2MN", "TT")) > 0) {
    data <- compile_meter(data)
  }
  # Process calculation sheets if that option is selected within create_datasheet
if (process_calc == TRUE){
  data <- read_calc_fold_mb(calc_folder, data)
}

  ##MB add renamed bottle columns, can also double as a master list of variables
  ##to rename if we need to
  units <- c(po4_uM = "po4", no3_uM = "no3", chla_ug.L = "chla", alk_meq.L = "alk",
             depth_m = "depth", temperature_c = "temperature", pressure_db = "pressure",
             chla_fluor_v = "fluorescence", cdom_fluor = "fluorescence2", cdom_fluor = "cdom",
             beamAttenuation = "beam_atten", par_mE.m2.s = "par", oxygen_uM.kg = "oxygen",
             oxygen_mL.L = "oxygen2", salinity_psu = "salinity", theta_c = "theta",
             sigma_kg.m3 = "sigma", sigtheta_kg.m3 = "sigtheta")

  data <- dplyr::rename(data, (any_of(units)))
  # Remove columns we don't need, e.g. deployment
  data <- dplyr::select(data, -deployment)

  # export to csv
  if(!is.null(csv_filename) & !is.null(csv_folder)) {
    csv_output <- file.path(csv_folder, csv_filename)
    if(add_cruiseID == TRUE & !is.null(cruiseID)) {
      csv_output <- add_file_cruiseID(csv_output, cruiseID)
    }

    readr::write_csv(format_csv_output(data),csv_output, na = "NA")
  }
  # export to odv
  if(odv_export) {
    if(!is.null(odv_filename) & !is.null(odv_folder)) {
      odv_output <- file.path(odv_folder, odv_filename)
      if(add_cruiseID == TRUE & !is.null(cruiseID)) {
        odv_output <- add_file_cruiseID(odv_output, cruiseID)
      }
      format_odv(data, odv_output, data_type = data_type, cruiseID = cruiseID)
    }
  } else {
    warning("No ODV output created. For ODV output, set odv_export = TRUE
            or manually import .csv file to ODV. \n If data_type = CTD, this warning can be ignored.")
  }



  return(data)

}


compile_meter <- function(data) {


  data <- dplyr::mutate(data, total_flow = ifelse(is.na(total_flow),
                                                  flow_out - flow_in,
                                                  total_flow),
                        .after = flow_in)

  data <- dplyr::mutate(data, tow_length_m = ifelse(is.na(tow_length_m),
                                                    total_flow * flow_constant,
                                                    tow_length_m),
                        .after = flow_constant)

  data <- dplyr::mutate(data, tow_volume_m3 = ifelse(is.na(tow_volume_m3),
                                                     tow_length_m * net_area_m2,
                                                     tow_volume_m3),
                        .after = net_area_m2)

  data <- dplyr::mutate(data,
                        zooplankton_biodens_ml.m3 = ifelse(is.na(zooplankton_biodens_ml.m3),
                                                           as.numeric(zooplankton_biovol_ml)/tow_volume_m3,
                                                           zooplankton_biodens_ml.m3),
                        .after = zooplankton_biovol_ml)



  #MB copy and paste from compile_neuston
  data <- dplyr::rowwise(data)
  data <- dplyr::mutate(data, total_100_count = ifelse(is.na(total_100_count),
                                                       sum(dplyr::c_across(medusa:other3)),
                                                       total_100_count))

  #MB added a shannon_wiener calculation using the vegan package
  data <- dplyr::mutate(data, shannon_wiener = ifelse(is.na(shannon_wiener),
                                                      vegan::diversity(dplyr::c_across(medusa:other3),
                                                                       index = "shannon", base = 10), shannon_wiener))

  #data <- dplyr::mutate(data, shannon_wiener = sum(dplyr::c_across(medusa:other3)/total_100count * log(dplyr::c_across(medusa:other3)/total_100count)))
  data <- dplyr::ungroup(data)
}

#' Create Neuston datasheet
#'
#' This function combines metadata from the station summary with the hand
#' entered data from 'neuston_input.' Additionally it calculates biodensity
#' (mL/m2) by dividing zooplankton biovolume (mL) by station distance (m), and
#' calculates Shannon-Wiener Diversity using the "vegan" package.
#'
#' For more information on station distance calculation see \link[oce]{geodDist}
#' @export
#' @rdname compile_neuston
compile_neuston <- function(data) {
  #remove wire tension and payout
  data <- dplyr::mutate(data, max_tension = NULL, payout_at_max = NULL)

  # calculate biodensity
  if(length(which(is.na(data$station_distance_m)))>0) {
    warning("One or more tow distances are not available - be sure that they exist in the summary data csv")
  }

  # MB delete station distance/1000 to keep units as mL/m2
  data <- dplyr::mutate(data, zooplankton_biodens_ml.m2 =
                          ifelse(is.na(zooplankton_biodens_ml.m2),

                                 zooplankton_biovol_ml/station_distance_m,

                                 zooplankton_biodens_ml.m2)
  )
  data <- dplyr::relocate(data, zooplankton_biodens_ml.m2, .after = zooplankton_biovol_ml)

  # sum the total 100 count animals
  data <- dplyr::rowwise(data)
  data <- dplyr::mutate(data, total_100_count = ifelse(is.na(total_100_count),
                                                       sum(dplyr::c_across(medusa:other3)),
                                                       total_100_count))

  #MB added a shannon_wiener calculation using the vegan package
  data <- dplyr::mutate(data, shannon_wiener = ifelse(is.na(shannon_wiener),
                                                      vegan::diversity(dplyr::c_across(medusa:other3),
                                                                       index = "shannon", base = 10), shannon_wiener))

  # data <- dplyr::mutate(data, shannon_wiener = sum(dplyr::c_across(medusa:other3)/total_100count * log(dplyr::c_across(medusa:other3)/total_100count)))
  data <- dplyr::ungroup(data)

  # Calculate moon data
  moon_data <- oce::moonAngle(data$dttm,data$lon,data$lat)

  # Add moon info to dataset and set the decimal to zero
  data <- dplyr::mutate(data,
                        moon_phase_pct = ifelse(is.na(moon_phase_pct),
                                                moon_data$illuminatedFraction * 100,
                                                moon_phase_pct),
                        moon_risen = ifelse(is.na(moon_risen), moon_data$altitude > 0,
                                            moon_risen), .before = cloud_cover_pct)
  nodec <- 0
  data <- format_decimal(data, "moon_phase", nodec)
  #move station distance after "heading"
  data <- dplyr::relocate(data, station_distance_m, .after = heading_t)

  #MB TODO: calculate degree min lat/lon for quality control on double checking

  return(data)
}


#' Create bottle file sheet
#'
#' Compiles bottle data from the bottle_input sheet, .ros files and optionally,
#' calculation sheets. To compile data from calculation sheets, set process_calc
#' = TRUE, and define calc_folder path to the folder where calc sheets are
#' stored. Likely this will be the "calc_sheets" folder in the project
#' directory.
#'
#' Also an experimental option to process data from niskins on the wire. For
#' this, ensure there is a "depth" column in your bottle datasheet. Then set
#' niskin_on_wire = TRUE, and define a ctd_folder.
#'
#' @param data dataframe from bottle_input datasheet
#' @param ros_input input folder for the .ros files output by Seabird.
#' @param ctd_folder  input folder for the .cnv files ONLY needed if processing
#'   bottles as niskins on the wire.
#' @param process_calc defaults to FALSE. Optional parameter to process water
#'   chemistry data from calculation sheets. Set to TRUE if desired.
#' @param calc_folder if process_calc = TRUE, folder where calculation sheets
#'   are stored. Each calculation sheet will need a correctly formatted 'output'
#'   tab with station, bottle number, and variable (chla, no3 etc). See "Setup
#'   and EOC" vignette for more information.
#' @param niskin_on_wire logical. Set to TRUE to process niskin data from ctd
#'   sheet and not look for a ros file. When this option is selected you will
#'   need to define ctd_folder = ctd_folder.
#'
#' @return
#' @export
#'
#' @examples
compile_bottle <- function(data, ros_input = NULL, ctd_folder = NULL, calc_folder = NULL,
                           process_calc = FALSE, niskin_on_wire = FALSE) {
  # get just the list of stations that are unique
  stations <- unique(data$station)

  # if no ros_input is given
  if (niskin_on_wire == TRUE) {
  #Process ctd input
    # read in ctd input as an ungrouped data frame
    ctd_input <- read_ctd_fold(ctd_folder) |>
      dplyr::ungroup()
    #remove overlapping columns except for station
    cols <- c("cruise", "lat", "lon", "dttm", "bot_depth", "file")
    ctd_input <- ctd_input |> dplyr::select(!any_of(cols))

    #create a vector of possible names to simplify coming in from the ctd datasheet
    units <- c(oxygen = "oxygen_mL.L", oxygen2 = "oxygen_uM.kg",
               pressure = "pres", salinity = "sal",
               temperature = "temp", fluorescence = "fluor",
               theta = "theta_c", sigtheta = "sigtheta_kg.m3", par = "par_mE.m2.s")
    # # Then rename those columns that exist with the labels with units
    ctd_input <- dplyr::rename(ctd_input, any_of(units))
    #filter out any SS or bottle 13s. They will be dealt with later
    bottle_input <- dplyr::filter(data, bottle != "13")
    bottle_input <- dplyr::filter(bottle_input, bottle != "SS")
      ros_output <- NULL
    #Loop through each station and find the depth row closest to the bottle depth
    for (i in 1:length(stations)) {
      #Isolate just the profile data for the specific station
      cast_data <- dplyr::filter(ctd_input, station == stations[i])
      # Isolate bottle data for the specific station and remove everything but
      # station bottle and depth
      bottle_data <- dplyr::filter(bottle_input, station == stations[i])

      bottle_data <- dplyr::select(bottle_data, c("station", "bottle", "depth"))
      #check to see if there is a bottle for that station. If not, skip that cast
      if(nrow(bottle_data) < 1) {
        next
      } else{
        #If so, find the depth rows closest to the bottle depths
        nearest_row <- find_near(cast_data$dep, bottle_data$depth)
        cast_data <- dplyr::slice(cast_data, nearest_row)

        #add in bottle numbers to cast data
        cast_data <- dplyr::mutate(cast_data, bottle = bottle_data$bottle)
        bottle_data <- dplyr::left_join(bottle_data, cast_data, by = c("station", "bottle"))
        ros_output <- dplyr::bind_rows(ros_output, bottle_data)
      }
    }

      ros_output <- dplyr::select(ros_output, -dep)
      data <- dplyr::select(data, -depth)
  }
  # Go find appropriate bottle files from ctd folder and
  # list all files with *.ros extension in ros_input
  if(!is.null(ros_input)){
  ros_files <- list.files(ros_input,pattern = "\\.ros")

  # Loop through all ros files and
  ros_output <- NULL

  for (i in 1:length(stations)) {

    # select the ros file name than matches the station name from the input data
    ros_file <- ros_files[stringr::str_detect(ros_files,stations[i])]
    if(length(ros_file) > 0) {
      if(length(ros_file) > 1) {
        ros_file <- ros_file[1]
        warning(paste0("More than one ros file in folder matches the stations number ",
                       stations[i],
                       ". Selecting the first file found: ",
                       ros_file[1]))
      }
      # Read in the ros file and combine with datasheet
      ros <- read_ros(file.path(ros_input,ros_file))
      ros <- dplyr::mutate(ros, station = stations[i])
    } else {
      warning(paste("No .ros file found for station",
                    stations[i]))
      ros <- NULL
    }

    if(!is.null(ros)) {
      if(is.null(ros_output)) {
        ros_output <- ros
      } else {
        ros_output <- dplyr::bind_rows(ros_output, ros)
      }
    }

  }
}
  if(!is.null(ros_output)) {
    ros_output$bottle <- as.character(ros_output$bottle)
  }


  # Now that we have our depths and metadata for each bottle in a hydrocast,
  # add all the surface station samples to this

  #Sort out SS and bottle 13s
  bottle_lines <- dplyr::filter(data, bottle == "SS" | bottle == "13")
  if(nrow(bottle_lines) > 0) {
    if(!is.null(ros_output)) {
      #create an empty table, first the correct number of columns
      data_add <- purrr::quietly(tibble::as_tibble)(t(rep(NA_real_, ncol(ros_output))))$result
      # rename columns to have the same names as the ros file
      names(data_add) <- names(ros_output)
      # add the correct number of rows
      data_add <- dplyr::mutate(data_add, count = nrow(bottle_lines))
      data_add <- tidyr::uncount(data_add, count)
      # add the data

      data_add <- dplyr::mutate(data_add,
                                bottle = bottle_lines$bottle,
                                depth = 0,
                                temperature = bottle_lines$surf_temp_c,
                                pressure = 0,
                                salinity = bottle_lines$surf_sal_psu,
                                theta = oce::swTheta(salinity = salinity,
                                                     temperature = temperature,
                                                     pressure = pressure),
                                sigtheta = oce::swSigma0(salinity = salinity,
                                                      temperature = temperature,
                                                      pressure = pressure),
                                station = bottle_lines$station)

      # combine ros with bottle
      all_output <- dplyr::bind_rows(ros_output, data_add)


    } else {
      data_add <- tibble::tibble(bottle = "SS",
                                 depth = 0,
                                 temperature = bottle_lines$surf_temp_c,
                                 pressure = 0,
                                 salinity = bottle_lines$surf_sal_psu,
                                 theta = oce::swTheta(salinity = salinity,
                                                      temperature = temperature,
                                                      pressure = pressure),
                                 sigtheta = oce::swSigma0(salinity = salinity,
                                                       temperature = temperature,
                                                       pressure = pressure),
                                 station = bottle_lines$station)

      # set to be the total output as no ros data exists
      all_output <- data_add
    }
  } else {
    all_output <- ros_output
  }

  output <- dplyr::left_join(data, all_output, by = c("station","bottle"))

  # MB reorder bottles to output shallow to deep
  output <- dplyr::group_by(output, station, bottle)
  output <- dplyr::mutate(output,
                          bottle = factor(bottle,
                                          levels = c("SS", "13", "12", "11", "10", "9",
                                                     "8", "7", "6", "5", "4", "3", "2", "1")))
  output <- dplyr::arrange(output, desc(bottle), .by_group = T)
  output <- dplyr::ungroup(output)
  #Remove max_tension
  output <- dplyr::mutate(output, max_tension = NULL, payout_at_max = NULL)
  # if (process_calc == TRUE){
  #   output <- read_calc_fold_mb(calc_folder, output)
  # }
  #
  # ##MB add renamed bottle columns
  # units <- c(po4_uM = "po4", no3_uM = "no3", chla_ug.L = "chla", alk_meq.L = "alk",
  #            depth_m = "depth", temperature_c = "temperature", pressure_db = "pressure",
  #            chla_fluor_v = "fluorescence", par_mE.m2.s = "par", oxygen_uM.kg = "oxygen",
  #            oxygen_mL.L = "oxygen2", salinity_psu = "salinity", theta_c = "theta",
  #            sigma_kg.m3 = "sigma")
  #
  # output <- dplyr::rename(output, (any_of(units)))

  return(output)

}
#' Check Station
#'
#' Prechecks to make sure every station in the deployment sheet has a matching
#' station in the summary sheet. Sends a warning, not an error upon failure.
#'
#' @param data
#' @param summary
#' @param bottle Logical, TRUE when processing a bottle datasheet. False all other times.
#'
#' @return
#' @export
#'
#' @examples
check_stations <- function(data, summary, bottle = FALSE) {
  if(bottle){
    station_test <- dplyr::anti_join(data, summary, dplyr::join_by(station, deployment))
  } else {
    station_test <- dplyr::anti_join(data, summary, dplyr::join_by(station))
  }
  if(nrow(station_test) != 0) {
    stations <- station_test$station
    #Output a warning for those columns
    warning(paste("Station number",
                  paste0(stations, collapse = ", "),
                  " \n missing from summary datasheet or incorrectly formatted. Correct and rerun."))
  }
}



##Part of function to take in a archive CSV and make it into a readable excel file
#data_input <- "~/GitHub/seaprocess/inst/extdata/Data Sheet CSVs/S285_NT.csv"

#data <- read.csv(data_input)

#colnames(data) <- stringr::str_to_title(colnames(data))
#colnames(data) <- stringr:: str_replace_all(colnames(data), "_", " ")

