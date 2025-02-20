# Initialize a cruise
initialize_cruise <- function(path, ...) {

  initialize_master(path, ...)

}


# Initialize an example cruise
initialize_example <- function(path, ...) {

  initialize_master(path, cruiseID = "C285C",
                    initial_folder = "initial_example", ...)


}

# master initializing function
initialize_master <- function(path, cruiseID = NULL,
                              initial_folder =  "initial_dir", select_ship,
                              neuston, meter, secchi, obs, niskins_on_wire, ctd,
                              free_ctd,
                              ...) {
  if(is.null(cruiseID)) {
    cruiseID <- basename(path)
  }

  # ensure path exists but don't overwrite project
  if(!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  } else {
    stop("Project already exists")
  }

  # copy contents of template directory to project folder
  file.copy(list.files(system.file("extdata",initial_folder,package="seaprocess"), full.names = TRUE),
            path,
            recursive = TRUE)

  # Create raw input data paths
  if(!dir.exists(file.path(path,"raw"))) {
    dir.create(file.path(path,"raw"))
    dir.create(file.path(path,"raw","ctd"))
    dir.create(file.path(path,"raw","adcp"))
    dir.create(file.path(path,"raw","event"))
  }

  # Create output paths
  if(!dir.exists(file.path("output"))) {
    dir.create(file.path(path,"output"))
    dir.create(file.path(path,"output","csv"))
    dir.create(file.path(path,"output","odv"))
    dir.create(file.path(path, "output", "plots"))
    dir.create(file.path(path,"output","odv","bottle"))
    dir.create(file.path(path,"output","odv","adcp"))
    dir.create(file.path(path,"output","odv","ctd"))
    dir.create(file.path(path,"output","odv","elg"))
    if (neuston){
    dir.create(file.path(path,"output","odv","neuston"))
    }
    if (meter) {
      dir.create(file.path(path,"output","odv","meter"))
    }
  }

  ## TODO add cruise metadata contents
  lines <- readr::read_lines(file.path(path,"process_data.R"))

  # Make ship specific changes to process_data.R
    if (select_ship == "CC") {
      # Make blank any line with '#RCS' and the first line below it
      ii <- stringr::str_which(lines, "\\#RCS")
      lines <- lines[-c(ii, ii + 1)]

      #Remove the process_lci parameters from 'create_summary'
      #find the line below 'Create Summary'
      ii <- stringr::str_which(lines, "# Create Summary datasheet") + 1
      # Input the function we want
      lines[ii] <-
        stringr::str_replace(lines[ii], ".*", "create_summary(summary_input, elg_folder, cruiseID = cruiseID)")
      # Replace anything below it with white space
      lines <- lines[-c(ii + 1)]
    }

    if (select_ship == "RCS") {
      ii <- stringr::str_which(lines, "\\#RCS")
      lines <- lines[-ii]
    }

  # Remove and edit datasheet lines if not needed

   if(!neuston) {
      # Remove datasheet line from process data script
      ii <- stringr::str_which(lines, "# Neuston datasheet")
      lines <- lines[-c(ii:(ii + 2))]
      # Remove excel sheet from project directory
      list.files()
    }

    if(!meter) {
      ii <- stringr::str_which(lines, "# Meter datasheet")
      lines <- lines[-c(ii:(ii + 2))]
    }

    if(!secchi) {
      ii <- stringr::str_which(lines, "# Secchi datasheet")
      lines <- lines[-c(ii:(ii + 2))]
    }

    #Deal with CTD data

    if(!ctd){
      ii <- stringr::str_which(lines, "# CTD datasheet")
      lines <- lines[-c(ii:(ii+2))]
    }

    if(niskins_on_wire){
      ii <- stringr::str_which(lines, "ros_input = ros_folder")
      lines[ii] <- stringr::str_replace(lines[ii],
                                        "ros_input = ros_folder",
                                        "niskin_on_wire = T, ctd_folder = ctd_folder")
    }
    if(free_ctd) {
      ii <- stringr::str_which(lines, "# Bottle datasheet")
      lines <- lines[-c(ii:(ii + 5))]
    }

    if(!obs) {
      ii <- stringr::str_which(lines, "# Obs")
      #Loop from bottom of lines to top removing three lines associated with #Obs
      for (i in length(ii):1){
        lines <- lines[-c(ii[i]:(ii[i]+2))]
      }
    }
  # add cruiseID as master processing param
  ii <- stringr::str_which(lines, "^cruiseID \\<-")
  lines[ii] <- stringr::str_replace(lines[ii], '\\"\\"', paste0('\\"',cruiseID,'\\"'))

  # add cruise ID to all datasheets
  ii <- stringr::str_which(lines, "datasheets/")
  lines[ii] <- stringr::str_replace_all(lines[ii], "datasheets/", paste0("datasheets/",cruiseID,"_"))

  # add cruiseID to session info script (and any others with <cruiseID>)
  ii <- stringr::str_which(lines, "<cruiseID>")
  lines[ii] <- stringr::str_replace_all(lines[ii], "<cruiseID>", paste0(cruiseID))

   # write this to file
  readr::write_lines(lines, file.path(path,"process_data.R"))

  #Add cruiseID to the process_eoc script
  lines_eoc <- readr::read_lines(file.path(path,"/eoc/process_eoc.R"))

  # add cruiseID as master processing param
  ii <- stringr::str_which(lines_eoc, "^cruiseID \\<-")
  lines_eoc[ii] <- stringr::str_replace(lines_eoc[ii], '\\"\\"', paste0('\\"',cruiseID,'\\"'))
  readr::write_lines(lines_eoc, file.path(path,"/eoc/process_eoc.R"))

  ## TODO append cruise ID to all files
  cruise <- basename(path)
  old_names <- c(list.files(file.path(path,"datasheets"), full.names = T, recursive = T),
                 list.files(path, full.names = T, recursive = F, pattern = "*.R"),
                 list.files(file.path(path, "eoc"), full.names = T, recursive = F, pattern = "*.R"))
  new_names <- file.path(dirname(old_names),paste0(cruiseID,"_",basename(old_names)))
  file.rename(old_names, new_names)


}
