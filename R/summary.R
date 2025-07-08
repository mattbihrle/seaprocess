#' Create Station Summary Sheet
#'
#' This function combines hand-entered station metadata with electronically
#' recorded location and environmental data to output a well formatted station
#' summary sheet.
#'
#' @param summary_input The input datasheet that includes the relevent station
#'   and deployment metadata
#' @param elg_input The cruise elg file (or folder of files) for extracting
#'   continuous data from
#' @param csv_folder The directory path to output the csv file. Set to NULL for
#'   no csv output.
#' @param csv_filename The csv filename to output the data
#' @param force_stations logical - set to TRUE if you want to force the output
#'   to have station data at the nearest elg entry regardless of whether it is a
#'   longer time than magdiff from the nearest data row
#' @param cruiseID Optional string specifying cruise ID (i.e. "S301")
#' @param add_cruise_ID If cruiseID is set, logical to specify whether cruiseID
#'   should be appended to beginning of filenames for csv and odv output
#' @param magdiff maximum time difference in seconds between the station time
#'   and the nearest elg time. If greater than this value, look to force_station
#'   as to whether to return a NA or add a value regardless
#' @param skipcheck toggles whether to check through the summary data to make
#'   sure all required fields (deployment, date, time in, zd) all have values.
#'   Defaults to FALSE, set to TRUE to bypass this step.
#' @param process_lci optional to reroute the summary function to process wire
#'   data from the LCI90 raw file (RCS) or the Hydrowinch Tension Files folder
#'   (CC). Also requires the parameter `raw_folder`
#' @param raw_folder location of the .RAW file for the LCI90 created by SCS. The
#'   function will automatically look for a filename containing "LCI90-raw" on
#'   RCS or, on CC, it will look for all files in the defined folder except for
#'   files/folders called "archive files."
#' @param keep a character vector of all the variables from the elg file that
#'   could be added to the summary datasheet. Default is all of the values from
#'   [read_elg()] except 'dttm'. If needed, add additional names from the elg
#'   file *without* units to this vector and your summary sheet. Eg. 'xmiss_counts' would be 'xmiss', 'cdom_fluor'
#'   would be 'cdom'. `create_summary` will try to match column names from this
#'   list with the column names in the summary_input sheet.
#' @param ... optional arguments passed to read_elg like forceGPS. See
#'   `?read_elg` for more information.
#'
#' @return A tibble containing the combined data frames. If csv_folder is set to
#'   a valid output path then a formatted csv file is output also.
#'
#' @export
#'
#' @details During deployments at SEA, we maintain paper data sheets which
#'   recorded station metadata and data from that deployment. These are a vital
#'   component for our data accuracy and redundancy.
#'
#'   In creating electronic datasheets for deployments it is desirable to
#'   combine hand-entered station metadata (station number, station type, zone
#'   description, etc.) with electronically recorded data (location, surface
#'   conditions, etc.) to provide an accurate record of the deployment without
#'   the need to re-enter hand-recorded values of the electronic data.
#'
#'   To do this, [create_summary()] takes in an excel sheet with the bare
#'   minimum of hand-entered data:
#'
#' * Station number
#' * Deployment type
#' * Deployment date/time in (and out)
#' * Zone description (time zone)
#'
#'   [create_summary()] combines this data with the electronically recorded
#'   "event" data which records (amongst other things):
#'
#' * Time
#' * Location
#' * Surface Temperature
#' * Surface Salinity
#' * etc.
#'
#'   The event data is typically stored in a file with an extension elg.
#'   [read_elg()] deals with reading this data in and formatting it properly.
#'
#'   Once the two data frames are read in to R, they are combined using the UTC
#'   time that exists in both data frames.
#'
#' @md
#'
create_summary <- function(summary_input, elg_input,
                           csv_folder = "output/csv",
                           csv_filename = "summary_datasheet.csv",
                           force_stations = TRUE, cruiseID = NULL,
                           add_cruiseID = TRUE, magdiff = 60, skipcheck = FALSE,
                           process_lci = FALSE, raw_folder = NULL,
                           keep = c("lat", "lon", "temp", "fluor", "sal", "bot_depth",
                                    "cdom", "xmiss", "wind_sp","wind_dir",
                                    "heading", "pitch", "roll", "wire_tension",
                                    "filename"), ...) {

  # read in the summary_input xlsx file
  summary <- readxl::read_excel(summary_input, col_types = "text")

  summary_check(summary, skipcheck)

  # combine date and time and convert to R datetime object using specified time zone

  summary <- dplyr::mutate(summary,
                           dttm = lubridate::ymd_hm(
                             paste(summary$date,summary$time_in)
                             ) + lubridate::dhours(as.numeric(summary$zd))
                           )

  # Test to see whether elg_input is a file or a folder and read elg file(s) accordingly
  elg <- get_elg(elg_input, ...)

  # elg <- filter_elg(elg)

  # Test for column names-------------------------------------------------------

  #Use 'keep' and wire and station values to create a vector of possible column names
  sum_val <- c(keep, "max_tension","payout_at_max", "station_distance")

  # Retain the column names that are actually present in the summary sheet
  # This way we can avoid errors if some of these columns are missing
  sum_col <- sum_val[which(sum_val %in% colnames(summary))]

  # filter out rows that have data hand-entered
  summary_hand_enter <- dplyr::filter(summary, !dplyr::if_all(any_of(sum_col),is.na))

  # remove the columns that can be autofilled--these are added back in later
  summary <- dplyr::select(summary, !any_of(sum_col))

  # find all the nearest date time values of summary sheet to the elg file and add these indeces
  # TODO: what happens if any of ii are blank or at beginning or end of the elg?
  # TODO: add fail safe to ensure that times are actually close
  ii <- find_near(elg$dttm, summary$dttm)

  # find which values align closer than 60 seconds
  diff_time <- as.numeric(elg$dttm[ii] - summary$dttm)
  iii <- which(abs(diff_time) > magdiff)

  if(length(iii) > 0) {
    message <- paste("The following station is more than",
                     magdiff,
                     "seconds from nearest elg reading:",
                     paste0(summary$station[iii], "-", summary$deployment[iii],
                            ": ", diff_time[iii], " seconds"), sep = "\n")

    if(force_stations) {
      warning(paste(message,
                    "Setting the station metadata to NA and continuing to run. Run create_summary() without force_stations = TRUE to force a stop here",
                    sep = "\n"))
      ii[iii] <- NA
    } else {
      stop(paste(message,
                 "Please enter the metadata from paper datasheet by hand into the input summary datasheet and rerun",
                 sep = "\n"))
    }
  }

  # Calculate the station distance by extracting the lon/lat along path
  # First, build the dttm_out
  time_diff <- lubridate::time_length(lubridate::hm(summary$time_out) - lubridate::hm(summary$time_in), unit= "minutes")
  time_diff[time_diff<0 & !is.na(time_diff)] <- time_diff[time_diff<0 & !is.na(time_diff)] + 60 * 24
  summary <- dplyr::mutate(summary,dttm_out = dttm + lubridate::minutes(time_diff))


  # find nearest points and calculate along-path distance
  sti <- find_near(elg$dttm, summary$dttm)
  eni <- find_near(elg$dttm, summary$dttm_out)
  sti[iii] <- NA
  eni[iii] <- NA
# Wire processing---------------------------------------------------------------
  if (process_lci) {
    wire_info <- process_wire(summary, raw_folder, cruiseID = cruiseID)

  }
  if (!process_lci & (any(sum_col == "max_tension") | any(sum_col == "payout_at_max"))) {
  # If not processing wire through raw files, try to do it through the elg file

    if (cruise2ship(cruiseID) == "rcs"){
      elg <- dplyr::mutate(elg, wire_payout = wire_payout * -1)
    }
  # Create empty vectors
  max_tension <- rep(NA, length(sti))
  wire_payout <- rep(NA, length(sti))
  # loop through and find max tensions and payout at that time
    for (i in 1:length(sti)) {
      if(is.na(eni[i])) {
        next
      }
      tension_peak <- elg |>
        dplyr::slice(sti[i]:eni[i]) |>
        dplyr::filter(wire_payout >= 0) |>
        dplyr::slice_max(order_by = wire_tension)

      # Show warning for two lines of max tension
      if(nrow(tension_peak) > 1){
        warning("Equal max tension occured at multiple times during:
        ", paste(summary$station[i], summary$deployment[i], sep = "-")
                , "
        Seaprocess will record the first instance.")
      }
      # Ensure there is data before adding
      if(nrow(tension_peak > 0)) {
        suppressWarnings(max_tension[i] <- tension_peak$wire_tension)
        suppressWarnings(wire_payout[i] <- tension_peak$wire_payout)
      } else {
        next
      }

      # max_tension[i] <- max(elg$wire_tension[sti[i]:eni[i]])
      # wire_payout[i] <- elg$wire_payout[max(elg$wire_tension[sti[i]:eni[i]])]
  }
  # Put new vectors into a list to match output from `process_wire`
    wire_info <- list(max_tension = max_tension, wire_payout = wire_payout)
}
  # Once we have tension and payout, check colnames to see if we should add it
    if (any(sum_col == "max_tension")) {
  #MB add max_tension column
  summary <- dplyr::mutate(summary, max_tension = wire_info$max_tension)

  #Remove any resting tension <100
  suppressWarnings(
    summary <- dplyr::mutate(summary,
                             max_tension = ifelse(summary$max_tension > 99,
                                                  summary$max_tension, as.numeric("NA")))
  )
  #Remove max tension from deployments that don't use the wire
    suppressWarnings(
    summary <- dplyr::mutate(summary,
                             max_tension = ifelse(summary$deployment == "NT" |
                                                    summary$deployment == "OBS" |
                                                    summary$deployment == "REEF" |
                                                    summary$deployment == "SS",
                                                  as.numeric("NA"), summary$max_tension))
    )
        if (any(sum_col == "payout_at_max")) {

      #Add payout column
      summary <- dplyr::mutate(summary, payout_at_max = wire_info$wire_payout)


      #Remove payout at max if max_tension is NA
      summary <- dplyr::mutate(summary,
                               payout_at_max = ifelse(summary$max_tension == "NA",
                                                      as.numeric("NA"), summary$payout_at_max))
      }
        } else {
    # If no max tension, but payout listed, make a blank column to aid joining later
    summary <- dplyr::mutate(summary, payout_at_max = as.numeric(NA))
      }

  # Calculate tow length--------------------------------------------------------
  if (any(sum_col == "station_distance")) {
    tow_length <- rep(NA, length(sti))
    for (i in 1:length(sti)) {
      if(is.na(eni[i])) {
        next
      }
      tow_length[i] <- tail(
      oce::geodDist(
        elg$lon[sti[i]:eni[i]],
        elg$lat[sti[i]:eni[i]],
        alongPath = TRUE),1)
    }

  #add tow length in meters to data
  summary <- dplyr::mutate(summary, station_distance = tow_length*1000)
  }
  # Remove variables we dont need
  summary <- dplyr::select(summary, -dttm_out)


  # extract these values and add to the right of summary, add variables from "keep"
  # to have additional variables in the summary output.
  # TODO: make outputs selectable
  elg_to_add <- dplyr::select(elg[ii,], any_of(sum_col))
  summary <- dplyr::bind_cols(summary, elg_to_add)

  # add back in the hand entered values and make sure they are numeric
  if(nrow(summary_hand_enter)>0) {
    summary_hand_enter <- dplyr::mutate(summary_hand_enter,dplyr::across(any_of(sum_col),as.numeric))
  # Check to be sure all columns in summary hand enter are present in summary
    missing_col <- colnames(summary)[which(!colnames(summary) %in% colnames(summary_hand_enter))]
   # Force a stop if the columns don't align

     if (length(missing_col > 0)) {
      stop(missing_col, " missing as a column in station summary sheet.
Either remove from 'keep' in create_summary or add as a column in station summary sheet.")
    }
    #add in all calculated values where there is an NA
    summary_hand_enter <-
      dplyr::rows_patch(summary_hand_enter, summary,
                        by = c("station", "deployment"), unmatched = "ignore")
    #combine updated values with the full summary sheet
    summary <- dplyr::rows_update(summary, summary_hand_enter,
                                  by = c("station", "deployment"))
  }

  # sort by station and relocate distance to end
  summary <- dplyr::arrange(summary, dttm)
  # MB comment out block here
  # summary <- dplyr::relocate(summary, station_distance, .after = tidyselect::last_col())
  # #MB add max tension before station distance
  # summary <- dplyr::relocate(summary, max_tension, .before = station_distance)
  # # Put payout at max after max tension
  # summary <- dplyr::relocate(summary, payout_at_max, .after = max_tension)

  # Rearrange make sure the summary columns are in front
  summary <- dplyr::relocate(summary, any_of(c("dttm", sum_col)),
                                                .after = general_locale)
  # Once finished rearranging columns, rename all to have units


  sum_units <- c(surf_temp_c = "temp", surf_sal_psu = "sal", surf_chla_fluor = "fluor",
                 station_distance_m = "station_distance", bot_depth_m = "bot_depth")

  summary <- dplyr::rename(summary, (any_of(sum_units)))
  # check to ensure that there are no duplicate deployments for any one station
  duplicated_deployments <- dplyr::n_groups(dplyr::group_by(summary, station, deployment)) != nrow(summary)
  if(duplicated_deployments) {
    warning("Summary sheet has defined mulitple of the same deployment for the same station")
  }

  # Output to csv file as long as the folder name exists and the extension is correct
  if(!is.null(csv_filename) & !is.null(csv_folder)) {
    csv_output <- file.path(csv_folder, csv_filename)
    # find the file extension of csv_output
    ext <- tools::file_ext(csv_output)
    # test to see if the extension is csv or CSV
    if(stringr::str_detect("csv|CSV", ext)) {
      # test to see if the filename directory is already in existence
      if(dir.exists(dirname(csv_output))) {
        # create csv output at csv_output location
        if(add_cruiseID & !is.null(cruiseID)) {
          csv_output <- add_file_cruiseID(csv_output, cruiseID)
        }
        readr::write_csv(format_csv_output(summary),csv_output)
      } else {
        stop("csv_output does not direct towards a valid existing folder")
      }
    } else {
      stop("csv_output must include a filename with a .csv extenstion")
    }
  } else {
    warning("csv_output is empty -- no csv file was created")
  }

  return(summary)

}

#' Format for CSV output
#'
#' This is a catch-all function to ensure that everytime a data-frame in this
#' software is exported to a csv that certain fields are formatted correctly
#'
#' @param df Data frame to be exported
#' @param dttm_format dttm_format
#'
#' @return
#' @export
#'
#' @examples
format_csv_output <- function(df, dttm_format = "%Y-%m-%dT%H:%M", dttm_suffix = "Z",
                              ll_dec = 4, temp_dec = 2, sal_dec = 3, fluor_dec = 2) {

  # Format date and time to be in ISO 8601 format including timezone
  if("zd" %in% colnames(df) & "dttm" %in% colnames(df)) {

    df$dttm <- paste0(format(df$dttm - lubridate::dhours(as.numeric(df$zd)), dttm_format),
                      zd_to_tz(df$zd, format_out = TRUE))
  }

  # TODO: create list of all column names and default resolutions

  # Format lon and lat to ~10 m resolution (by default)
  df <- format_decimal(df, "lon", ll_dec)
  df <- format_decimal(df, "lat", ll_dec)

  # Format temp, sal and fluor to set decimal lengths
  df <- format_decimal(df, "surf_temp_c", temp_dec)
  df <- format_decimal(df, "surf_sal_psu", sal_dec)
  df <- format_decimal(df, "surf_chla_fluor", fluor_dec)
  # MB change depth to bot_depth
  df <- format_decimal(df, "bot_depth_m", 1)

  return(df)
}


#' Format to number of decimal places
#'
#' Rather than significant figures as format() provides in its digits = 2 option.
#'
#' @param value
#' @param digits
#'
#' @return
#' @export
#'
#' @examples
format_decimal <- function(df, col_name, digits) {

  if(col_name %in% colnames(df)){
    df[col_name] <- format(round(df[[col_name]],digits), nsmall = digits)
  }

  return(df)
}


#' Zone Description to Time Zone
#'
#' @param zd
#'
#' @return
#' @export
#'
#' @examples
zd_to_tz <- function(zd, format_out = FALSE) {

tz <- (as.numeric(zd) * -1)

if(format_out){

zd_duration <- lubridate::dhours(as.numeric(zd) * -1)
zd_duration <- lubridate::as.period(zd_duration)
zd_duration <- as.character(zd_duration)
#extract hours and minutes
hours <- stringr::str_extract(zd_duration, pattern = "[:digit:]*(?=H)")
minutes <- stringr::str_extract(zd_duration, pattern = "[:digit:]*(?=M)")
#Paste things back together while adding in zeros
timeadd <- paste0(stringr::str_pad(hours,2,pad = "0"), ":",
                  stringr::str_pad(minutes, 2, pad = "0"))
#swap the signs for tz
tz <- dplyr::case_when(
  tz == 0 ~ "Z",
  sign(tz) == 1 ~ paste0("+", timeadd),
  sign(tz) == -1 ~ paste0("-", timeadd),
  TRUE ~ NA_character_
)


  } else {
    tz <- as.character(tz)
    ii <- !stringr::str_sub(tz,1,1) == "-"
    tz[ii] <- paste0("+", tz[ii])

    return(tz)
    # MB USED to format Marquesian ZD
#      tz <- as.character(zd)
#      ##TESTS
#      neg <- stringr::str_which(tz, "\\+")
#      pos <- stringr::str_which(tz, "\\-")
#      # tz[which(is.na(tz))] <- as.character("+99:99")
#      # ii <- !stringr::str_sub(tz,1,1) == "-"
#      if(length(pos) > 0) {
#        stringr::str_sub(tz[pos],1,1) <- "+"
#      }
#
#       if(length(neg) > 0) {
#     stringr::str_sub(tz[neg],1,1) <- "-"
#       }


  }
  return(tz)
}

#' Get elg data from file or folder
#'
#' Takes an elg input and returns a data frame of all elg data fields before
#' they are averaged within the specified file or folder. Typically this will be
#' minute to minute data on SEA vessels.
#'
#' @param elg_input
#'
#' @return
#' @export
#'
#' @examples
get_elg <- function(elg_input, ...) {
  # TODO: add ability to tune reading elg per options provided in that function
  # MB TODO: a possible place for filter_elg?
  # TODO: find way to store Rdata file in local folder so we don't have the delay of loading
  # TODO: confirm consistency in field names

  if(file_test("-f",elg_input)) {
    elg <- read_elg(elg_input, ...)
  } else if(file_test("-d",elg_input)) {
    elg <- read_elg_fold(elg_input, ...)
  } else {
    stop("elg_input is neither a valid filename or folder")
  }

  return(elg)
}


find_tow_length <- function(elg, summary, ii) {



  return(summary)


}

#' Summary Check
#'
#' Ensures each entry in the summary sheet has the needed information to run the
#' create_datasheet functions down the line. Will make sure each station has a
#' deployment, date, time_in, and zd. If not, will show a warning detailing
#' which stations and data to fix.
#'
#' NOTE: does not check formatting. For
#' formatting help, see the notes at the top of the summary_input sheet.
#'
#' @param summary summary data frame to be checked.
#' @param skipcheck defaults to FALSE, set to TRUE within create_summary to skip
#'   this step.
#'
#' @return
#' @export
#'
#' @examples
summary_check <- function(summary = summary, skipcheck = FALSE) {

  if(skipcheck) {
    return(summary)
  } else {

    #Pull indices of na in needed values
    deployment_na <- which(is.na(summary$deployment))
    date_na <- which(is.na(summary$date))
    time_na <- which(is.na(summary$time_in))
    zd_na <- which(is.na(summary$zd))



    if (length(deployment_na > 0)) {
      warning(
        paste("Deployment type missing from: ",
              paste0(summary$station[deployment_na], sep = ",", collapse = " "),
              "correct and rerun create_summary."))
    }


    if((length(date_na)) > 0) {
      warning(
        paste("Date missing from: ",
              paste0(summary$station[date_na], sep = ",", collapse = " "),
              "correct and rerun create_summary."))
    }


    if((length(time_na)) > 0) {
      warning(
        paste("Time_in missing from: ",
              paste0(summary$station[time_na], sep = ",", collapse = " "),
              "correct and rerun create_summary."))
    }


    if((length(zd_na)) > 0) {
      warning(
        paste("ZD missing from: ",
              paste0(summary$station[zd_na], sep = ",", collapse = " "),
              "correct and rerun create_summary.")
      )
    }
    return(summary)
  }
}


#' Process Wire Info
#'
#' A function that works inside `create_summary()` to take a folder with raw
#' tension files, read it into R and find the tension peak and wire payout at
#' that peak for each deployment. It does this by aligning time in and time out
#' from the station summary sheet with the times in the tension files. This
#' means that on CC we must keep the LCI 90 time accurate and on RCS we must
#' keep the datalogger computer time accurate.
#'
#' NOTE: on CC the function will try to read anything in the raw folder EXCEPT a
#' file with the name "archive files." On RCS it is looking for specifically the
#' string "LCI90-raw." If you are having trouble with this function try renaming
#' the files or reach out to the office.
#'
#' @param data dataframe that has a dttm and dttm_out column
#' @param ship either "RCS" or "CC" used for dealing with the different file
#'   names and tension logging methods between both boats.
#' @param raw_folder folder path to where the raw tension files are stored. On
#'   RCS this is typically on the datalogger computer with the name "LCI90-raw,
#'   on CC this is the Hydrowinch Tension Files folder.
#'
#' @return
#' @export
#'
#' @examples
process_wire <- function(data, raw_folder = NULL, cruiseID = NULL) {

  # create ship ID from cruise ID
  ship <- cruise2ship(cruiseID)
  # Proceed only if a tension folder is defined
  if (!is.null(raw_folder)) {

    # RCS file read----------------------------------------------------------------
    if(ship == "rcs") {
      files <- list.files(raw_folder, pattern = "LCI90-raw", full.names = T)
      if (length(files) > 0) {
        suppressWarnings(
        lci <- readr::read_csv(files, show_col_types = F, col_types = "ctcnnnc",
                               col_names = c("date", "time", "string", "tension",
                                             "speed", "payout", "ascii"))
        )

        #create dttm column
        lci <- dplyr::mutate(lci, dttm = lubridate::mdy_hms(paste(lci$date,lci$time)))

        #Swap payout to be positive numbers when gear is in the water
        lci <- dplyr::mutate(lci, payout = lci$payout * -1)
      } else {
        warning(paste("No files found matching the string: 'LCI90-raw'.",
                      "Proceeding to pull max tension and payout from event file (RCS Only)"))
        return()
      }
    }

    # CC file read------------------------------------------------------------------
    if(ship =="cc") {
      # Read all lci files in the raw folder except for "archive files"
      files <- list.files(raw_folder, full.names = T, pattern = ".*[^a|A.* f|Files.*]$")

      if (length(files) > 0) {
        suppressWarnings(
          lci <- readr::read_csv(files, col_names = c("RD", "dttm", "tension", "misc",
                                                    "misc2", "speed", "payout", "checksum"),
                               col_types = "cTnccnnc", show_col_types = F, skip = 2,
                               skip_empty_rows = T)
        )
        # Remove rows that have an NA
        lci <- tidyr::drop_na(lci)
      }
      else {
        warning("Could not find any filenames that do not match 'archive files.'
Please hand enter max tension into station summary sheet.")
        return()
      }
    }

    # Loop to find max tension------------------------------------------------------

    # Create vector of rows that match start and end time.
    sti_t <- find_near(lci$dttm, data$dttm)
    eni_t <- find_near(lci$dttm, data$dttm_out)
    # Create blank vectors to fill
    max_tension <- rep(NA, length(sti_t))
    wire_payout <- rep(NA, length(sti_t))

    for (i in 1:length(sti_t)) {
      if(is.na(eni_t[i])) {
        next
      }
      # Between the start and end time, take only positive payout values
      # and find the maximum
      tension_peak <- lci |>
        dplyr::slice(sti_t[i]:eni_t[i]) |>
        dplyr::filter(payout >= 0) |>
        dplyr::slice_max(order_by = tension)


      # Show warning for two lines of max tension
      if(nrow(tension_peak) > 1){
        warning("Equal max tension occured at multiple times during:
        ", paste(data$station[i], data$deployment[i], sep = "-")
                , "
        Seaprocess will record the first instance.")
      }
      # Ensure there is data before adding
      if(nrow(tension_peak > 0)) {
        suppressWarnings(max_tension[i] <- tension_peak$tension)
        suppressWarnings(wire_payout[i] <- tension_peak$payout)
      } else {
        next
      }
    }
    # Alternatives if there are no raw files to work with-------------------------
  } else {
    if (ship == "rcs") {
      warning(paste("No raw_folder defined. Proceeding to pull tension and payout from event file."))
    }
    if (ship == "cc") {
      warning("No raw_folder defined. Please hand enter max tension into station summary sheet.")

    }
    return()
  }
  # Create a list containing the variables we want
  wire_info <- list(max_tension = max_tension, wire_payout = wire_payout)
  # Send this out to summary input
  return(wire_info)
}
