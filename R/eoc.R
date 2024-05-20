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

#' Process Wire Log
#'
#' Experimental. Cycles through all output csv sheets and compiles wire log data for submission to UNOLS wire pool.Outputs to an EOC folder.
#' *Important* the function must see "max_wire_out" as a column name to include that deployment sheet in the final output.
#'
#' @param csv_folder Where to find the datasheets to compile.
#' @param eoc_folder output for the final wire log.
#' @param cruiseID cruiseID . eg "S314"
#' @param csv_filename Optional filename for the log.
#' @param add_cruiseID Option to append cruiseID to the file name.
#'
#' @return
#' @export
#'
#' @examples
process_wire_log <- function(csv_folder = "output/csv",
                             eoc_folder = "output/eoc",  cruiseID = NULL,
                             csv_filename = "wire_log.csv",
                             add_cruiseID = TRUE) {


  datasheets <- list.files(csv_folder, full.names = T)

  wire_data <- NA


  for(i in 1:length(datasheets)){
    #read in a datasheeet
    iii <- readr::read_csv(datasheets[i], col_names = T, show_col_types = F)
    #see if the datasheet contains a max_wire_out column
    if(any(colnames(iii) == "max_wire_out")){
      #read in wire data and either start a new df or append to the old one
      if(all(is.na(wire_data))){
        wire_data <- dplyr::select(iii, c("station", "dttm", "time_in", "time_out",
                                          "max_tension", "max_wire_out"))

      } else {
        ii <- dplyr::select(iii, c("station", "dttm", "time_in", "time_out",
                                   "max_tension", "max_wire_out"))
        wire_data <- dplyr::rows_append(wire_data, ii)
      }
    }
  }

  ##Arrange by date

  wire_data <-
    wire_data |>
    dplyr::arrange(dttm)

  time_diff <- lubridate::time_length(lubridate::hms(wire_data$time_out) - lubridate::hms(wire_data$time_in), unit= "minutes")
  time_diff[time_diff<0 & !is.na(time_diff)] <- time_diff[time_diff<0 & !is.na(time_diff)] + 60 * 24

  wire_data <- dplyr::mutate(wire_data, deployment_length_min = time_diff)

  # output to csv
  if(!is.null(csv_filename) & !is.null(eoc_folder)) {
    csv_output <- file.path(eoc_folder, csv_filename)
    if(add_cruiseID == TRUE & !is.null(cruiseID)) {
      csv_output <- add_file_cruiseID(csv_output, cruiseID)
    }
    safely_write_csv(wire_data, csv_output)
  }
  return(wire_data)
}
