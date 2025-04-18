# Series of functions that work to read in an event data file

#' Read in SEA data from an ELG event file and return a well formatted output
#'
#' SEA event files contain output from a number of instruments
#' including GPS, flow-through, chirp, etc.
#'
#' @param filein Filepath of the elg file to be read in
#' @param forceGPS option to force the longitude, latitude and time to come from one
#'  or other of the GPS feeds.
#' @param preCheck option to do an initial check of lines to remove any standard issues
#' @param skip number of lines to skip below header
#' @param csv_output optional path to export the processed data to a csv file
#' @keywords
#' @export
#' @examples
read_elg <- function(filein, forceGPS = NULL, preCheck = TRUE, skip = 0,
                     csv_output = NULL,
                     keep = c("dttm","lon","lat","temp","sal","fluor",
                              "cdom","xmiss","wind_sp","wind_dir",
                              "heading","pitch","roll", "wire_tension",
                              "wire_payout", "bot_depth", "filename")) {
  #MB add wire_tension to grab max tension
  # TODO: add in minor interpolation for short gaps of missing values
  # collects names if you need to be able to skip data lines
  if(skip > 0) {
    col_names <- names(readr::read_csv(filein, n_max = 0))
  } else {
    col_names <- TRUE
  }


  # Toggle a preCheck of the elg file for clearly bad lines and commas at end
  if(preCheck) {

    # check for bad lines by checking for number of commas
    liner <- readr::read_lines(filein)
    numcom <- stringr::str_count(liner, "\\,")
    liner <- liner[numcom == numcom[1]]

    # clean and process end of line
    liner <- stringr::str_replace(liner,"\\,$","") # remove the trailing comma on many ELG files
    liner <- stringr::str_replace(liner,"$","\\\n")   # add new line to end of each line
    liner <- stringr::str_c(liner,collapse="")           # collapse vector into single line for read_csv

    # Read in lines using readr package (quicker than base read.csv)
    df <- readr::read_csv(liner,
                          col_types = readr::cols(.default=readr::col_character()),
                          skip = skip,
                          col_names = col_names)

  } else {

    # If no precheck then just read in the file as is
    df <- readr::read_csv(filein,
                          col_types = readr::cols(.default=readr::col_character()),
                          skip = skip,
                          col_names = col_names)

  }

  # Reassign names that have dashes in them to be referenced more easily
  names(df) <- stringr::str_replace_all(names(df),"-",".")
  # MB add bot_depth
  # MB change "depth" to "bot_depth"
  # Construct a data frame of regular expressions and functions used to match col names
  args <- tibble::tribble(~name, ~regex, ~parse_fun,
                          "sys_date", "date", lubridate::mdy,
                          "sys_time", "^time", readr::parse_time,
                          "nav_time", "gps.*nav.*time", readr::parse_character,
                          "nav_lon", c("gps.*nav.*lon", "longitude"), parse_lon,
                          "nav_lat", c("gps.*nav.*lat", "latitude"), parse_lat,
                          "nav_sog", "gps.*nav.*sog", readr::parse_double,
                          "nav_cog", "gps.*nav.*cog", readr::parse_double,
                          "nav_quality", "gps.*nav.*quality", readr::parse_integer,
                          "lab_time", "gps.*lab.*time", readr::parse_character,
                          "lab_lon", "gps.*lab.*lon", parse_lon,
                          "lab_lat", "gps.*lab.*lat", parse_lat,
                          "lab_sog", "gps.*lab.*sog", readr::parse_double,
                          "lab_cog", "gps.*lab.*cog", readr::parse_double,
                          "lab_quality", "gps.*lab.*quality", readr::parse_integer,
                          "temp", c("temp(?!.*[0-9])", "tsal.*temp"), readr::parse_double,
                          "temp_1min", "temp.*1.*min", readr::parse_double,
                          "temp_60min", "temp.*60.*min", readr::parse_double,
                          "sal", c("sali(?!.*[0-9])(?!.*vel)", "tsal.*sal"), readr::parse_double,
                          "sal_1min", "sal.*1.*min", readr::parse_double,
                          "sal_60min", "sal.*60.*min", readr::parse_double,
                          "sound_vel", "tsal.*vel", readr::parse_double,
                          "fluor", "^fluo.*invivo", readr::parse_double,
                          "fluor_1min", "fluo.*chl.*1.*min", readr::parse_double,
                          "fluor_60min", "^fluo.*chl.*60.*min", readr::parse_double,
                          "cdom", "cdom.*raw", readr::parse_double,
                          "cdom_1min", "cdom.*1.*min", readr::parse_double,
                          "cdom_60min", "cdom.*60.*min", readr::parse_double,
                          "xmiss", c("trans.*raw","xmiss.*raw","xmiss.*[^m]", "trans.*1\\.*min"), readr::parse_double,
                          "xmiss_1min", c("trans.*1\\.*min","xmiss.*1\\.*min"), readr::parse_double,
                          "xmiss_60min", c("trans.*60.*min","xmiss.*60.*min"), readr::parse_double,
                          "wind_sp", "true.*wind.*sp", readr::parse_double,
                          "wind_dir", "true.*wind.*dir", readr::parse_double,
                          "wind_sp_rel", "^wind.*sp", readr::parse_double,
                          "wind_dir_rel", "^wind.*dir", readr::parse_double,
                          "heading", c("hdg","heading"), readr::parse_double,
                          "pitch", "pitch", readr::parse_double,
                          "roll", "roll", readr::parse_double,
                          "bot_depth", c("depth","dbt"), readr::parse_double,
                          "wire_payout", "lci90.*payout", readr::parse_double,
                          "wire_tension", "lci90.*tension", readr::parse_double,
                          "wire_speed", "lci90.*spd", readr::parse_double
                          )

  # set up arguments and iterate through columns
  args_in <- tibble::as_tibble(list(df=list(df),regex=args$regex,parse_fun=args$parse_fun))
  namelist <- purrr::as_vector(dplyr::select(args,name))
  # all the parsing happens here

  suppressWarnings(
  output <- purrr::pmap(args_in, parse_field)
)
  # reassign col names and make into tibble
  names(output) <- namelist
  df <- tibble::as_tibble(output)

  # additional parsing for some elements
  df$nav_time <- readr::parse_time(stringr::str_extract(df$nav_time,"[0-9]{6}"),format="%H%M%S")
  df$lab_time <- readr::parse_time(stringr::str_extract(df$lab_time,"[0-9]{6}"),format="%H%M%S")
  df$sys_dttm <- update(df$sys_date, hour = lubridate::hour(df$sys_time),
                        minute = lubridate::minute(df$sys_time),
                        second = lubridate::second(df$sys_time))

  # Make datetimes from GPS using the system datetime
  df <- dplyr::mutate(df, lab_dttm = create_gps_dttm(lab_time,sys_dttm))
  df <- dplyr::mutate(df, nav_dttm = create_gps_dttm(nav_time,sys_dttm))

  if (all(is.na(df$lab_time)) & all(is.na(df$nav_time))){
    warning("Datetime issue - no nav or lab GPS time found. Check elg.")
  }
  # choose master datetime
  # use nav GPS as the default and revert to lab gps and sys time as required
  if(is.null(forceGPS)) {
    lon <- df$nav_lon
    lon[is.na(lon) & !is.na(df$lab_lon)] <- df$lab_lon[is.na(lon) & !is.na(df$lab_lon)]
    lat <- df$nav_lat
    lat[is.na(lat) & !is.na(df$lab_lat)] <- df$lab_lat[is.na(lat) & !is.na(df$lab_lat)]
    dttm <- df$nav_dttm
    dttm[is.na(dttm) & !is.na(df$lab_dttm)] <- df$lab_dttm[is.na(dttm) & !is.na(df$lab_dttm)]
  } else if (forceGPS == 'nav') {
    lon <- df$nav_lon
    lat <- df$nav_lat
    dttm <- df$nav_dttm
  } else if (forceGPS == 'lab') {
    lon <- df$lab_lon
    lat <- df$lab_lat
    dttm <- df$lab_dttm
  }

  # check dttm - if no gps time, revert to sys
  if (all(is.na(dttm))) {
    warning(paste("Datetime issue - no GPS time found for forceGPS option: ",
     forceGPS, ". Reverting to system datetime (sys_dttm).
     Check GPS time availability in elg file.
     Note lack of GPS time in EOC."))
    dttm <- df$sys_dttm
  }

  # add the chosen, lon, lat and dttm
  df <- dplyr::mutate(df,
                      lat = lat,
                      lon = lon,
                      dttm = dttm)

  # rearrange the columns into correct order
  # note: if modifying args above (e.g. adding
  # variables) column indices need to be changed
  # TO DO: reference names instead of indices
  df <- df[,c(47,45,46,42,1,2,44,3:8,43,9:41)]

  # add column with filename
  file <- tail(stringr::str_split(filein, "/")[[1]],1)
  df <- dplyr::mutate(df, filename = file)

  # just keep the specified column names
  colkeep <- colnames(df) %in% keep
  df <- df[, colkeep]

#test to see if any columns from "keep" are entirely NAs
for(i in 1:length(keep)){
  vari_name <- keep[i]
  if(all(is.na(df[vari_name]))){
    warning(paste(vari_name, "not found in ", file))
  } else {
    next
  }
}


  if(!is.null(csv_output)) {
    readr::write_csv(data,csv_output, na = "")
  }

  return(df)

}


#' Parse lon from elg file
#'
#' @param lonin longitude to process
#' @keywords
#' @export
parse_lon <- function(lonin) {

  return(parse_latlon(lonin, var = "lon"))

}

#' Parse lat from elg file
#'
#' Wrapper function for parse_latlon() for latutude case
#'
#' @param latin lat to process
#' @keywords
#' @export
#'
parse_lat <- function(latin) {

  return(parse_latlon(latin, var = "lat"))

}

#' Parse lon and lat strings
#'
#' @param varin
#' @param var
#'
#' @return
#' @export
#'
#' @examples
parse_latlon <- function(varin, var = "lon") {

  # set params based on lat or lon case
  if (var == "lon") {
    deg_char <- 3
    hem_start <- "E"
  } else if (var == "lat") {
    deg_char <- 2
    hem_start <- "N"
  } else {
    stop("var should be lon or lat")
  }

  # force convert varin to character if not already
  varin <- as.character(varin)

  # create a vector of +1 or -1 depending on the hemisphere [could be more robust here]
  hemi <- 2 * (stringr::str_sub(varin, -1, -1) == hem_start) - 1

  # remove hemisphere value from vector
  varin <- stringr::str_sub(varin, 1, -2)

  # create generic regex
  regexp <- paste0("[0-9]{", deg_char+ 2, "}.[0-9]{1,}")

  # remove values that dont match regex
  varin[!stringr::str_detect(varin, regexp)] <- NA

  # convert to varout
  varout <- hemi * as.numeric(stringr::str_sub(varin, 1, deg_char)) +
            hemi * as.numeric(stringr::str_sub(varin, deg_char+1, -1)) / 60

  return(varout)

}

#' Generic field parser - can be used for all elg and excel parsing
#'
#' Returns tibble with found column parsed and formatted
#'
#' @param df input data frame containing column to format
#' @param regex regular expression to locate column name
#' @param parse_fun function to use in parsing data in function
#' @param ... optional arguments to be passed to parse_fun
#'
#' @return
#' @export
#'
#' @examples
parse_field <- function(df, regex, parse_fun, ...) {

  # Convert all column names to lowercase
  df_names <- stringr::str_to_lower(names(df))

  # Find and parse field based on regex of column names
  for (i in 1:length(regex)) {
    ii <- stringr::str_which(df_names,regex[i])
    if(length(ii) > 0) break
  }

  # if there are multiple matches return just the first value
  if(length(ii) > 1) {
    ii <- ii[1]
  }

  # parse the selected column based on the parser given to function
  if (length(ii)==0) {
    warning(paste("One field not found in ELG file. Setting all values to NA:",regex[i]))
    output <- parse_fun(rep("NA",nrow(df)),...)
  } else {
    output <- parse_fun(df[[ii]],...)
  }

}


#' Read multiple ELG files from a folder
#'
#' @param root_folder
#'
#' @return
#' @export
#'
#' @examples
read_elg_fold <- function(root_folder, sort_elg = TRUE, ...) {
  # get all file names in folder
  files <- list.files(root_folder,pattern = '\\.elg$')

  # Set up blank data structure
  elg <- NULL

  # as long as there are files in the folder continue
  if(length(files) > 0) {

    # loop through all the files
    for (i in 1:length(files)) {

      # file name to be read in
      filein <- file.path(root_folder,files[i])

      # read in the file, but return NULL is not possible
      elg_add <- purrr::possibly(read_elg, NULL)(filein, ...)

      # if data has content, add it to the previous data
      if(!is.null(elg_add)) {
        if(i==1) {
          elg <- elg_add
        } else {
          elg <- dplyr::bind_rows(elg, elg_add)
        }

      # if elg_add is empty acknowledge but move on
      } else {
        warning(paste("elg file:",filein,"could not be read in and is not being added to the collection"))
      }
    }

    # Check for if no files could be read in
    if(is.null(elg)) {
      stop("elg files exist in specified folder, but none could be read in")
    }

    # optional sorting
    if(sort_elg) {
      elg <- dplyr::arrange(elg, dttm)
    }

  } else {
    # error message lets user know that no elg files were found so no data could be read in.
    stop("No elg files in specified folder.")
  }


  return(elg)

}


#' Create a GPS DateTime field
#'
#' ELG files typically have a GPS time, but no GPS data.
#' This function takes the system datetime field already parsed and returns a well formated GPS datetime.
#'
#' @param gps_time Raw GPS time field
#' @param sys_dttm system datetime
#'
#' @return
#' @export
#'
#' @examples
create_gps_dttm <- function(gps_time, sys_dttm) {

  if(length(which(is.na(gps_time))) < length(gps_time) &
     length(which(!is.na(gps_time))) > 100) {
    sys_time <- readr::parse_time(format(sys_dttm, "%H:%M:%S"))
    difft <- gps_time - sys_time
    goodi <- !is.na(difft)
    dayoffi <- difft < -8000
    x <- 1:length(difft)
    lf <- lsfit(x[goodi & !dayoffi],difft[goodi & !dayoffi])
    difft <- x*lf[[1]][2] + lf[[1]][1]
    gps_dttm = sys_dttm+difft
  } else {
    gps_dttm = readr::parse_datetime(rep("",length(gps_time)))
  }
  return(gps_dttm)
}

#' Average time series data
#'
#' @param data
#' @param average_window
#' @return
#' @export
#'
#' @examples
average_elg <- function(data, average_window = 60) {

  if(is.null(average_window)) {
    message("No time averaging applied to elg output data")
    return(data)
  }

  if(average_window<2) {
    warning("Cannot average data to window smaller than 2 minutues - returning original data")
    return(data)
  }

  data <- dplyr::mutate(data, roundtime = lubridate::round_date(dttm, unit = paste(average_window,"minute")))
  data <- dplyr::group_by(data, roundtime)

  #Adjust for crossing the antimeridian
  data <- fix_dateline(data)
  data_out <- dplyr::summarise(data,
                               dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), ~mean(.x, na.rm = TRUE)),
                               n = dplyr::n(),
                               filename_first = dplyr::first(filename),
                               filename_last = dplyr::last(filename))

  data_out <- dplyr::mutate(data_out, dttm = roundtime, .before=1)
  data_out <- dplyr::select(data_out, -roundtime)

  # could test for this before the averaging and then change back after

  # check for time gaps in average data and add them back in
  data_out <- fill_time_gaps(data_out, average_window = average_window)

  # finally make sure data is sorted by increasing datetime
  data_out <- dplyr::arrange(data_out, dttm)

  return(data_out)

}

fill_time_gaps <- function(data, average_window) {

  # set up a datetime vector that spans the data
  test_time_vector <- seq(min(data$dttm, na.rm = TRUE),
                          max(data$dttm, na.rm = TRUE),
                          by = 60 * average_window)

  # test to see which places are missing
  time_gaps_i <- test_time_vector %in% data$dttm

  # if gaps are found, plug them with NAs
  if(length(which(!time_gaps_i)) > 0) {

    # create vector of missing times
    times_to_fill <- test_time_vector[!time_gaps_i]

    # create a row of blank values and replicate to the number of rows of missing times
    na_row_add <- purrr::quietly(tibble::as_tibble)(t(rep(NA_real_, ncol(data))))$result
    names(na_row_add) <- names(data)
    na_row_add <- dplyr::mutate(na_row_add, count = length(times_to_fill))
    na_tibble_add <- tidyr::uncount(na_row_add, count)
    na_tibble_add <- dplyr::mutate(na_tibble_add,
                                   filename_first = NA_character_,
                                   filename_last = NA_character_)

    # add the missing times to the datetime column and add to the original data
    na_tibble_add$dttm <- times_to_fill
    data <- dplyr::bind_rows(data, na_tibble_add)

    # sort the data to ensure gaps come sequentially
    data <- dplyr::arrange(data, dttm)
  }

  return(data)

}

##|
# tidyselect::vars_select_helpers$where(lubridate::is.Date) |
#   tidyselect::vars_select_helpers$where(lubridate::is.POSIXct) |
#   tidyselect::vars_select_helpers$where(lubridate::is.difftime),

#'Filter elg data
#'
#'Filter out flow through data primarily based on min salinity threshold
#'
#'SEA techs regularly backflush or clean components of our flow-through system
#'while under way. When this happens, the most telling value is salinity
#'however, all of our flow-through data is inaccurate for the time freshwater is
#'running through the system. This function takes a minimum salinity and filters
#'out data from all flow-through instruments (tsal, cdom, xmiss, chla, fluor)
#'while the salinity is below the minimum acceptable threshold. NOTE: if
#'instruments on the flow through change, the parameter 'flow_thr' will need to
#'be updated.
#'
#'@param elg compiled minute to minute elg data
#'@param min_sal minimum acceptable salinity default to 30 psu
#'@param custom default to FALSE. if TRUE, user will have the option to set
#'  ranges for all elg output variables.
#'@param flow_thr names of instruments on the flow through.
#'@param filter_values a list of filter values for when `custom_filter = TRUE`.
#'  The function will take a `filter_values` argument and convert it to
#'  `filter_values` for the function to use. Specify custom parameters within
#'  `process_elg` using the formatting:
#'  `filter_values = c("max_temp = 35", "min_temp = 15")` etc. Users can
#'  specify `max` and `min` for any numeric column in the .csv output.
#'
#'
#'@return
#'@export
#'
#' @examples
filter_elg <- function(elg, min_sal = 30, custom_filter = FALSE,
                       flow_thr = c("temp", "sal", "fluor", "cdom", "xmiss"),
                       filter_values = c("max_fluor = 30", "min_cdom = 0")) {

  if(custom_filter) {
    for(i in 1:length(filter_values)) {
      # Detect looking for max or minimum
      fmin <- stringr::str_detect(filter_values[i], "min")
      fmax <- stringr::str_detect(filter_values[i], "max")
      #remove white space
      ii <- stringr::str_replace_all(filter_values[i], " ", "")
      #split vector into 3 character strings
      ii <- stringr::str_split_1(ii, pattern = "(m\\w\\w_|=)")

      #extract variable name and max or min value
      vari_name <- ii[2]
      value <- as.double(ii[3])

      #Perform filter
      if(fmin) {
        elg[vari_name] <- ifelse(elg[[vari_name]] < value, NA, elg[[vari_name]])
      }

      if(fmax) {
        elg[vari_name] <- ifelse(elg[[vari_name]] > value, NA, elg[[vari_name]])
      }
    }
    return(elg)
    #Option to create a more customizable filter function here in the future
  }
  else{
    # Turn the salinity, temp, cdom, xmiss and fluor values to NA when sal drops
    # below the min_sal defined in process_elg
    elg <-
      elg |>
      dplyr::mutate(across(any_of(flow_thr), ~ifelse(sal < min_sal, NA, .)))
    return(elg)
  }

}

#'fix_dateline
#'
#'Function to adjust mean longitude while crossing dateline.
#'
#'This first creates a new variable "difflon" of the first lon - last lon in the
#'averaging window. This variable will shoot up to around +/- 360 when the
#'antimeridian(dateline) is crossed otherwise will be much less than 1.
#'
#'Next we filter out those time windows, and apply a correction to them. For
#'each negative longitude in these windows, we add 360, find the mean of all the
#'longitudes, and then subtract 360. Then, we average *just* those specific lons
#'(eg 09:00-10:00). This allows the rest of the averaging to happen through
#'average_elg.
#'
#'NOTE: Because this functions relies on not travelling more than 1 degree of
#'longitude over the averaging window, there is potential for some weird things
#'to happen if it was used with a very long averaging window, in polar regions,
#'with a much faster moving vessel. If errors occur, the 1 degree longitude
#'threshold can be adjusted or the function can be commented out in the base
#'script.
#'
#' @param data
#'
#'
#' @return
#' @export
#'
#' @examples
fix_dateline <- function(data) {

  #create "difflon" variable
  data <-
    data |>
    dplyr::mutate(difflon = abs(dplyr::first(lon)-dplyr::last(lon)))
  #test for if anti meridian is crossed.
  if(length(which(abs(data$difflon) > 1)) != 0){

    #find each row within in an hour that crosses the anti-meridian
    ii <- which(abs(data$difflon) > 1)
    for (i in ii){
      if(data$lon[i] > 0) {
        data$lon[i] <- data$lon[i]
      } else {
        data$lon[i] <- data$lon[i] +360
      }
    }

    #outside of for loop now calculate lon means where the difflon is greater than 1
    data <-
      dplyr::mutate(data, lon = ifelse(abs(difflon) > 1,
                                       mean(lon) - 360,
                                       lon))
  }

   data <- dplyr::select(data, -difflon)

  return(data)
}


