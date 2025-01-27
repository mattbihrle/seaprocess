#TODO eliminate the confusion b/t bottle , bottle2, and bottle_sum. Which is from the csv, which is the utility object, and which is the product?
#TODO make it recognize that all the chl (and future nutrients) are the same variable.
#'Append calculation sheet data to bottle summary
#'
#'This function appends hand-entered and calculated data from calculation sheets
#'to the bottle summary csv. It takes the 'output' sheet on the calculation
#'excel file with whatever header value is used and matches it with station
#'number and bottle number.
#'
#'
#'
#'
#'@param calc_file #the calculation file to be read, chlorophyll, PO4, NO3, pH.
#'  This file must be formatted correctly with an 'output' sheet.
#'@param bottle_sum #the bottle summary from the [create_bottle()] function
#'@param bottle_is_file() #if TRUE, this will read the [bottle.csv] file and
#'  creat the [bottle] object. If FALSE (the default), it will append data to
#'  the bottle object.
#'
#'@return #creates the [bottle] object that will be updated with new data each
#'  time it is run.
#'@export #currently does not export a csv file. The final [bottle] object must
#'  be exported for use in ODV
#'
#'
#'@details This function, and the [read_calc_fold()] function combine the
#'[output] sheet from any calculation sheet (from path ['calc_file]) and the
#'[bottle] object. If [bottle] is not an object (ex. if it is the first time
#'this is run) it will read the [bottle.csv] file from the [bottle_sum]
#'diriectory and create the [bottle] object. [create_bottle()] will overwrite
#'the [bottle.csv] and delete any calc sheet data added in past uses of this
#'function. RUnning this function again will replace these data.
#'
#'
#'
#'
#' @examples
read_calc_sheet <- function(calc_file, bottle_sum, bottle_is_file = FALSE) {

  #read calc sheet as specified in datasheet_examples.R
  calc_sheet <- readxl::read_excel(calc_file, sheet = "output")
  calc_sheet$bottle <- as.character(calc_sheet$bottle)
  colnames(calc_sheet) <- stringr::str_to_lower(colnames(calc_sheet))
  #TODO Make it read the bottle column, even if there are -, blanks or NA, N/A etc. Anything thats not a number is a SS and gets NA


  #if the column names contain "filter" (eg. it is a chlorophyll sheet), then make each filter size its own column with the chl values.
  if(c("filter") %in% colnames(calc_sheet)) {
    calc_sheet <-  pivot_wider(calc_sheet, names_from = "filter size", values_from = "chl")}

#MB add if the column names contain "ph" average any duplicate values and create a new output

  if(c("ph") %in% colnames(calc_sheet)) {

  }
  #if bottle_is_file = TRUE, create an object 'bottle' from the bottle.csv. If FALSE (default), load the object 'bottle_sum'
  if(bottle_is_file == TRUE) {
    bottle <-  readr::read_csv("")



  } else{
    bottle <- bottle_sum
  }

#MB ADD 4/3, make bottle column charcter for SS
  bottle <- dplyr::mutate(bottle, bottle = as.character(bottle))
  #join bottle and calc sheet tibbles into one. Common errors from header typos in the seperate excel sheets. Check spelling.
  #TODO is bottle2 still a valid object? Just use bottle?
  bottle2 <- dplyr::left_join(bottle, calc_sheet, by = c('station', 'bottle'))

  return(bottle2)
}


#'#Read a folder of calculation sheets, and append the data to the bottle
#'summary
#'
#'This function reads the [output] sheet of every excel file in a folder and
#'appends the new values to the bottle summary object [bottle_sum].
#'
#'
#'
#'@param calc_folder #the calculation folder to be read. chlorophyll, PO4, NO3,
#'  pH. All excel files must be formatted correctly with an 'output' sheet.
#'@param bottle_sum #the bottle summary from the [create_bottle()] function
#'@param bottle_is_file #if TRUE (the default) this will read the [bottle.csv]
#'  file and creat the [bottle_sum] object. If FALSE, it will append data to the
#'  bottle object.
#'
#'@return
#'@export #currently does not export an ODV csv file.
#'
#'@details This function, and the [read_calc_file()] function combine the
#'  [output] sheet from any calculation excel file (from path ['calc_file]) and
#'  the [bottle_sum] object. If [bottle] is not an object (ex. if it is the
#'  first time this is run) it will read the [bottle.csv] file from the
#'  [bottle_sum] diriectory and create the [bottle_sum] object.
#'  [create_bottle()] will overwrite the [bottle.csv] and delete any calc sheet
#'  data added in past uses of this function. RUnning this function again will
#'  replace these data. This function will read every excel file in the
#'  specified folder [calc_folder] from [datasheet_examples.R] and append the
#'  [output] sheet to the bottle summary csv. Common errors arise from spelling
#'  typos in the header data of the many calculation excel files, or from
#'  unexpected values in the columns (characters vs. numbers etc.). Individual
#'  files may be run with the [read_calc_file()] function for troubleshooting.
#'
#' @examples
read_calc_fold <- function(calc_folder, bottle_sum, bottle_is_file = TRUE) {

  #if bottle_is_file = TRUE (default), create the object 'bottle_sum' from the bottle.csv. If FALSE, load the object 'bottle_sum'.
  if(bottle_is_file == TRUE) {
    bottle <-  readr::read_csv(bottle_sum)

  } else{
    bottle <- bottle_sum
  }

  #a loop that reads all the excel files in the specified folder and calls the read_calc_file() function on each one.
  calc_files <- list.files(calc_folder,pattern = '\\.xls$|\\.xlsx$')
  if(length(calc_files) > 0) {
    for (i in 1:length(calc_files)) {
      filein <- file.path(calc_folder,calc_files[i])
      bottle <- read_calc_sheet(filein, bottle)
       }
  } else {
    stop("No calc files in specified folder.")
    calc <- NULL
  }
  return(bottle)

  #should we make this auto-output a CSV file?

}
#-----------------------MB EDITS BELOW------------------------------------------

#'Append calculation sheet data to bottle summary
#'
#'This function appends hand-entered and calculated data from calculation sheets
#'to the bottle summary csv. It takes the 'output' sheet on the calculation
#'excel file with whatever header value is used and matches it with station
#'number and bottle number.
#'
#'
#'
#'
#'@param calc_file #the calculation file to be read, chlorophyll, PO4, NO3, pH.
#'  This file must be formatted correctly with an 'output' sheet containing only
#'  station number, bottle number, and paramter name (chla, no3 etc).
#'@param output #the datasheet to output calc sheet data to. This will typically
#'be the bottle_input sheet.
#'
#'@return
#'@export
#'
#'
#'@details This function, and the [read_calc_fold_mb()] function combine the
#'[output] sheet from any calculation sheet (from path ['calc_file']) and the
#'[bottle_input] datasheet. The function will only fill values where there is an
#'"NA" so any numerical values entered in bottle_input will not be overwritten.
#'
#'
#'
#'
#' @examples
read_calc_sheet_mb <- function(calc_file, output) {

  if (any(stringr::str_detect(readxl::excel_sheets(calc_file), "output")) == TRUE) {

  #read calc sheet as specified in datasheet_examples.R
  calc_sheet <- readxl::read_excel(calc_file, sheet = "output")
  calc_sheet$bottle <- as.character(calc_sheet$bottle)
  colnames(calc_sheet) <- stringr::str_to_lower(colnames(calc_sheet))
  #TODO Make it read the bottle column, even if there are -, blanks or NA, N/A etc. Anything thats not a number is a SS and gets NA


  #if the column names contain "filter" (eg. it is a chlorophyll sheet), then make each filter size its own column with the chl values.
  if(c("filter") %in% colnames(calc_sheet)) {
    calc_sheet <-  pivot_wider(calc_sheet, names_from = "filter size", values_from = "chl")}

  #MB add if the column names contain "ph" average any duplicate values and create a new output

  if(c("ph") %in% colnames(calc_sheet)) {
    calc_sheet <- dplyr::group_by(calc_sheet, station, bottle)
    #Calculate averages
    calc_sheet <- dplyr::mutate(calc_sheet, ph_avg = mean(ph))
    #replace average column
    calc_sheet <- dplyr::mutate(calc_sheet, ph = ph_avg)
    calc_sheet <- dplyr::select(calc_sheet, -ph_avg)
    calc_sheet <- dplyr::ungroup(calc_sheet)
    #keep only disinct rows
    calc_sheet <- dplyr::distinct(calc_sheet)
  }

  #combine calc sheet values with the full bottle sheet. Rows patch will only
  # update NA values.

#Pull name of variable on interest
names <- colnames(dplyr::select(calc_sheet, !dplyr::any_of(c("station", "bottle"))))

#See if that column name is also in the output dataframe, if so, run the rows patch
if(any(colnames(output) == names)) {
  # Ensure the variable column in the data sheet is numeric to aid in joining
  # Supress "NAs introduced by coercion" warning
  suppressWarnings(
    output <- dplyr::mutate(output, dplyr::across(dplyr::matches(paste(names)), as.numeric))
  )
  output <- dplyr::rows_patch(output, calc_sheet,
                              by = c("station", "bottle"), unmatched = "ignore")
} else {
  warning(paste("Mismatched column names for", names,". Verify column names in",
                calc_file,"and 'bottle_input'. Calc sheet values will not be updated."))
    }
} else {
    warning(paste("No 'output' sheet found in", calc_file,
                  ". Bottle values will not be updated."))
  }
  return(output)

}

#'#Read a folder of calculation sheets, and append the data to the bottle
#'summary
#'
#'This function reads the [output] sheet of every excel file in a folder and
#'appends the new values to the bottle summary object [bottle_sum].
#'
#'
#'
#'@param calc_folder #the calculation folder to be read. chlorophyll, PO4, NO3,
#'  pH. All excel files must be formatted correctly with an 'output' sheet.
#'
#'@return
#'@export
#'
#'@details This function, and the [read_calc_fold_mb()] function combine the
#'  [output] sheet from any calculation sheet (from path ['calc_file']) and the
#'  [bottle_input] datasheet. The function will only fill values where there is
#'  an "NA" so any numerical values entered in bottle_input will not be
#'  overwritten. This function will read every excel file in the specified
#'  folder [calc_folder] and append the [output] sheet to the bottle summary
#'  csv. Common errors arise from spelling typos in the header data of the many
#'  calculation excel files, or from unexpected values in the columns
#'  (characters vs. numbers etc.). Individual files may be run with the
#'  [read_calc_file()] function for troubleshooting.
#'
#' @examples
read_calc_fold_mb <- function(calc_folder, output) {



  #a loop that reads all the excel files in the specified folder and calls the read_calc_file() function on each one.
  calc_files <- list.files(calc_folder,pattern = '\\.xls$|\\.xlsx$')
  if(length(calc_files) > 0) {
    for (i in 1:length(calc_files)) {
      filein <- file.path(calc_folder,calc_files[i])
      output <- read_calc_sheet_mb(filein, output)
    }
  } else {
    stop("No calc files in specified folder.")
    calc <- NULL
  }
  return(output)

  #should we make this auto-output a CSV file?

}

#' Read Net Calculation Sheet
#'
#' Not in use. Similar to our other calculation sheet function but does not
#' require or look for a column labelled "bottle." Instead your output sheet
#' would have just 'station' and 'parameter' to worry about.
#'
#' @param calc_file
#' @param output
#'
#' @return
#' @export
#'
#' @examples
read_calc_sheet_net <- function(calc_file, output) {

  if (any(stringr::str_detect(readxl::excel_sheets(calc_file), "output")) == TRUE) {

    #read calc sheet as specified in datasheet_examples.R
    calc_sheet <- readxl::read_excel(calc_file, sheet = "output")
    colnames(calc_sheet) <- stringr::str_to_lower(colnames(calc_sheet))

    #combine calc sheet values with the full datasheet. Rows patch will only
    # update NA values.
    #Pull name of variable on interest
    names <- colnames(dplyr::select(calc_sheet, !dplyr::any_of(c("station"))))

    #See if that column name is also in the output dataframe, if so, run the rows patch
    if(any(colnames(output) == names)) {
      # Ensure the variable column in the data sheet is numeric to aid in joining
      # Supress "NAs introduced by coercion" warning
      suppressWarnings(
        output <- dplyr::mutate(output, dplyr::across(dplyr::matches(paste(names)), as.numeric))
      )
      output <- dplyr::rows_patch(output, calc_sheet,
                                  by = c("station"), unmatched = "ignore")
    } else {
      warning(paste("Mismatched column names for", names,". Verify column names in",
                    calc_file,"and 'deployment input'. Calc sheet values will not be updated."))
    }
  } else {
    warning(paste("No 'output' sheet found in", calc_file,
                  ". Values will not be updated."))
  }
  return(output)

}
