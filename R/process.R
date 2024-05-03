# Functions that are designed to take the processing from a folder of files to
# a complete data archive and odv output
#

#' Master processing function for ADCP data
#'
#' Function takes in a filepath to a folder containing the cruise adcp files,
#' reads them in, combines them into one data object and then exports to a
#' comma-separated-file and an odv text file.
#'
#' Most likely optional arguments to go in for ...
#'
#' @param adcp_folder filepath to the folder containing adcp files
#' @param cruiseID Optional string specifying cruise ID (i.e. "S301")
#' @param csv_folder The directory path to output the csv file. Set to NULL for
#'   no csv output.
#' @param csv_filename The csv filename to output the data
#' @param odv_folder The directory path to output the odv file. Set to NULL for
#'   no odv output.
#' @param odv_filename The odv .txt filename to output the data
#' @param add_cruise_ID If cruiseID is set, logical to specify whether cruiseID
#'   should be appended to beginning of filenames for csv and odv output
#' @param ... option arguments to be passed to [read_adcp_fold()]. A few
#'   options, but most likely is the file_type which defaults to '.LTA' (long
#'   term averages), but can be changed to ".STA" if you want to read in the
#'   short term averages
#'
#' @return
#' @export
#'
#' @md
#'
#' @examples
process_adcp <- function(adcp_folder, cruiseID = NULL,
                         csv_folder = "output/csv", csv_filename = "adcp.csv",
                         odv_folder = "output/odv/adcp", odv_filename = "adcp.txt",
                         add_cruiseID = TRUE, ...) {

  # Read in all adcp file in the folder
  adcp <- read_adcp_fold(adcp_folder, ...)

  # write adcp data to csv
  # add cruise ID to default output if not already existing
  if(!is.null(csv_filename) & !is.null(csv_folder)) {
    csv_output <- file.path(csv_folder, csv_filename)
    if(add_cruiseID == TRUE & !is.null(cruiseID)) {
      csv_output <- add_file_cruiseID(csv_output, cruiseID)
    }
    safely_write_csv(adcp, csv_output)
  }

  # Convert adcp to odv output
  if(!is.null(odv_folder)) {
    format_odv(adcp, file.path(odv_folder,odv_filename), data_type = "adcp", cruiseID = cruiseID)
  }
}

#' Master processing function for event data
#'
#' Function takes in a filepath to a folder containing the cruise elg files,
#' reads them in, combines them into one data object and then exports to a
#' comma-separated-file and an odv text file.
#'
#' Note that the elg folder doesnt not have to solely contain elg files, the
#' reading function will find all the elg files amoungst the other files
#' exported by SCS
#'
#' @param elg_folder filepath to the folder containg elg files
#' @param cruiseID Optional string specifying cruise ID (i.e. "S301")
#' @param csv_folder The directory path to output the csv file. Set to NULL for
#'   no csv output.
#' @param csv_filename The csv filename to output the data
#' @param odv_folder The directory path to output the odv file. Set to NULL for
#'   no odv output.
#' @param odv_filename The odv .txt filename to output the data
#' @param add_cruise_ID If cruiseID is set, logical to specify whether cruiseID
#'   should be appended to beginning of filenames for csv and odv output
#' @param average_window the averaging window in minutes for the exported file.
#'   Set to NULL for no averaging. Default is 60 minutes.
#' @param min_sal minimum acceptable salinity. Default is 30 psu, will remove
#'   flow through data when salinity drops below this level.
#' @param custom_filter default to FALSE. Set to TRUE and specify
#'   `filter_params` to set custom limits to elg data.
#' @param filter_params custom parameters for elg data. See `?filter_elg` for
#'   more info on formatting
#'
#' @return
#' @export
#'
#' @examples
process_elg <- function(elg_folder, cruiseID = NULL,
                        csv_folder = "output/csv", csv_filename = "elg.csv",
                        odv_folder = "output/odv/elg", odv_filename = "elg.txt",
                        add_cruiseID = TRUE, average_window = 60, min_sal = 30,
                        custom_filter = FALSE,
                        filter_params = c("max_fluor = 30", "min_cdom = 0"), ...) {

  # Read in all the ELG files in the folder
  elg <- read_elg_fold(elg_folder, ...)
  #Filter out erroneous values in minute to minute elg data before averaging

  elg <- filter_elg(elg, min_sal, custom_filter, filter_values = filter_params)

  # Average the elg data (default is to 60 mins)
  if(average_window > 1) {
    elg <- average_elg(elg, average_window = average_window)
  }

  # Output csv file
  # add cruise ID to default output if not already existing
  if(!is.null(csv_filename) & !is.null(csv_folder)) {
    csv_output <- file.path(csv_folder, csv_filename)
    if(add_cruiseID == TRUE & !is.null(cruiseID)) {
      csv_output <- add_file_cruiseID(csv_output, cruiseID)
    }
    safely_write_csv(elg, csv_output)
  }

  # output odv file
  if(!is.null(odv_filename) & !is.null(odv_folder)) {
    odv_output <- file.path(odv_folder, odv_filename)
    if(add_cruiseID == TRUE & !is.null(cruiseID)) {
      odv_output <- add_file_cruiseID(odv_output, cruiseID)
    }
    format_odv(elg, file.path(odv_folder,odv_filename), data_type = "elg", cruiseID = cruiseID)
  }

  return(elg)

}

#' Master processing function for ctd data
#'
#' Function takes in a filepath to a folder containing the cruise cnv CTD files,
#' reads them in, combines them into one data object and then exports to a
#' comma-separated-file and an odv text file.
#'
#' @param ctd_folder filepath to the folder containing ctd cnv files
#' @param cruiseID Optional string specifying cruise ID (i.e. "S301")
#' @param csv_folder The directory path to output the csv file. Set to NULL for
#'   no csv output.
#' @param csv_filename The csv filename to output the data
#' @param odv_folder The directory path to output the odv file. Set to NULL for
#'   no odv output.
#' @param odv_filename The odv .txt filename to output the data
#' @param add_cruise_ID If cruiseID is set, logical to specify whether cruiseID
#'   should be appended to beginning of filenames for csv and odv output
#' @param ... optional arguments passed to [read_ctd_fold()]. Most likely one
#'   you'd set would be the depth_step which is the depth resolution of output
#'   data in meters. Default is 1m.
#'
#' @md
#'
#' @return
#' @export
#'
#' @examples
process_ctd <- function(ctd_folder, cruiseID = NULL,
                        csv_folder = "output/csv", csv_filename = "ctd.csv",
                        odv_folder = "output/odv/ctd", odv_filename = "ctd.txt",
                        add_cruiseID = TRUE, ...) {

  # create data
  ctd <- read_ctd_fold(ctd_folder, cruiseID = cruiseID, ...)

  # output to csv
  if(!is.null(csv_filename) & !is.null(csv_folder)) {
    csv_output <- file.path(csv_folder, csv_filename)
    if(add_cruiseID == TRUE & !is.null(cruiseID)) {
      csv_output <- add_file_cruiseID(csv_output, cruiseID)
    }
    #MB add lines to rename ctd for output
    ctd_units <- ctd |>
      dplyr::rename(oxygen_mL.L = oxygen, oxygen_uM.kg = oxygen2,
                    pres_db = pres, sal_psu = sal,
                    temp_c = temp, chla_fluor = fluor,
                    theta_c = theta, sigtheta_kg.m3 = sigtheta, par_mE.m2.s = par)
    safely_write_csv(ctd_units, csv_output)
  }


  # output odv
  if(!is.null(odv_filename) & !is.null(odv_folder)) {
    odv_output <- file.path(odv_folder, odv_filename)
    if(add_cruiseID == TRUE & !is.null(cruiseID)) {
      odv_output <- add_file_cruiseID(odv_output, cruiseID)
    }
    format_odv(ctd, file.path(odv_folder,odv_filename), data_type = "ctd", cruiseID = cruiseID)
  }

}


safely_write_csv <- function(data, csv_filepath = NULL) {
  print(csv_filepath)
  if(!is.null(csv_filepath)) {
    output <- purrr::safely(readr::write_csv)(data, csv_filepath)
    if(!is.null(output$error)) {
      print(output$error)
      message("Couldn't export the data to a csv file. Most likely specified directory doesn't exist")
    }
  }

}

#' EOC Process
#'
#' Process EOC files
#'
#' A simple function to copy relevant files to the "raw/" directory within the
#' cruise project. It's goal is to have a package of cruise data that is easy to
#' share with students and future scientists without requiring them to wade
#' through heaps of miscellaneous files. In it's current state will only copy
#' files needed to run the cruise processing scripts (.cnv, .ros .elg, .LTA (or
#' STA)).
#'
#' Future improvements could include automatic outputs of cruise track,
#' distance traveled, deployment types etc.
#'
#' @param elg_folder filepath to the folder containing elg files
#' @param ctd_folder filepath to the folder containing ctd cnv files
#' @param ros_folder filepath to the folder containing ros files
#' @param adcp_folder filepath to the folder containing adcp files
#' @param adcp_file_type  type of ADCP data to copy. Defaults to "LTA" but can
#'   accept "STA"
#'
#' @return
#' @export
#'
#' @examples
eoc_process <- function (elg_folder = elg_folder, ctd_folder = ctd_folder,
                         ros_folder = ros_folder, adcp_folder = adcp_folder,
                         adcp_file_type = "LTA" ) {

  elg_files <- list.files(elg_folder, pattern = "\\.elg$", full.names = T)
  files <- as.logical(NULL)
  if (length(elg_files) > 0){
    files <- file.copy(elg_files, "raw/event", overwrite = T)
  } else {
    warning("No elg files found. Check elg_folder filepath.")
  }
  if (length(which(!files)) > 0)  {
    error <- which(!files)
    warning("One or more elg files failed to copy to raw/event: ",
            paste(elg_files[error], collapse = "\n"))
  }
  ctd_files <- list.files(ctd_folder, pattern = "\\.cnv", full.names = T)
  files <- as.logical(NULL)
  if (length(ctd_files) > 0){
    files <- file.copy(ctd_files, "raw/ctd", overwrite = T)
  } else {
    warning("No cnv files found. Check ctd_folder filepath.")
  }
  if (length(which(!files)) > 0)  {
    error <- which(!files)
    warning("One or more cnv files failed to copy to raw/ctd: ",
            paste(ctd_files[error], collapse = "\n"))
  }

  ros_files <- list.files(ros_folder, pattern = "\\.ros", full.names = T)
  files <- as.logical(NULL)
  if (length(ros_files) > 0){
    files <-  file.copy(ros_files, "raw/ctd", overwrite = T)
  } else {
    warning("No ros files found. Check ros_folder filepath.")
  }

  if (length(which(!files)) > 0)  {
    error <- which(!files)
    warning("One or more ros files failed to copy to raw/ctd: ",
            paste(ros_files[error], collapse = "\n"))
  }
  adcp_files <- list.files(adcp_folder, pattern = stringr::str_c("\\.",
                                                                 adcp_file_type), full.names = T)
  files <- as.logical(NULL)
  if (length(adcp_files) > 0){
    files <- file.copy(adcp_files, "raw/adcp", overwrite = T)
  } else {
    warning("No adcp files found. Check adcp_folder filepath and verify adcp_filetype ('LTA' or 'STA').")
  }
  if (length(which(!files)) > 0)  {
    error <- which(!files)
    warning("One or more adcp files failed to copy to raw/adcp: ",
            paste(adcp_files[error], collapse = "\n"))
  }
}
