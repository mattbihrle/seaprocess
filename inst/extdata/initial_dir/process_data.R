### Master processing file for the cruise
###

# This script is intended to be edited. You wont mess anything up with the raw
# data or data entered into the input datasheets by changing anything in here.
# You *may* end up overwriting some output data, but that is all
# recoverable.


# Loading the processing library ------------------------------------------

library(seaprocess)



# Set up code parameters --------------------------------------------------

# Enter the cruise ID and the folder paths for where the data are stored.
#
# Filepaths likely to looks something like "Z:\\Data" for data stored on other
# machines and "ctd" for data folders in this project directory

cruiseID <- ""

# Data folders
#
# elg_folder will most likely be on the datalogger computer. This folder has to
# just contain data from this cruise. It can have other files other than .elg
# files in there, but shouldn't have data from previous cruises
#
# ctd_folder can be the local ctd folder where you can put .cnv and .ros files
# in from other "Cnv" folder or you direct the code to this other folder
# directly
#
# ros_folder can be the same as ctd_folder
#
# adcp_folder will probably be on the remote adcp machine. It can have all the
# ADCP files in there, but will only read, by default the LTA files
#
# calc_folder is within the "datasheets" folder. See documentation for "Calculation Sheets"
# witin "Setup and Use" for more info
#
elg_folder <- "<enter-location-of-elg-folder>"
ctd_folder <- "<enter-location-of-ctd-folder>"
ros_folder <- "<enter-location-of-ros-folder>"
adcp_folder <- "<enter-location-of-adcp-folder>"
calc_folder <- "<enter-location-of-calc-folder>"
#RCS
raw_folder <- "<enter-location-of-lci_raw-folder>"

# Datasheets
#
# These are stored within this project under the "datasheets" folder.
# other_input is there for you to include other data sheets that you need to
# include in the processing beyond the default ones included in this processing
# script
#
summary_input <- "datasheets/summary_input.xls"
ctd_input <- "datasheets/ctd_input.xls"
neuston_input <- "datasheets/neuston_input.xls"
meter_input <- "datasheets/meter_input.xls"
bottle_input <- "datasheets/bottle_input.xls"
secchi_input <- "datasheets/secchi_input.xls"
other_input <- ""

# Obs Datasheets
obs_input <- "datasheets/obs_input.xls"
obs_summary_input <- "datasheets/obs_summary_input.xls"


# Process non-datasheet data sources --------------------------------------

# Process elg (event file) data
process_elg(elg_folder, cruiseID = cruiseID)

# Process ctd data
process_ctd(ctd_folder, cruiseID = cruiseID)

# Process ADCP data
process_adcp(adcp_folder, cruiseID = cruiseID)


# Process datasheets ------------------------------------------------------

# Start with creating a robust summary sheet of all deployments. This summary
# sheet will then be used by subsequent functions to add metadata to their
# datasheets

# Create Summary datasheet
create_summary(summary_input, elg_folder, cruiseID = cruiseID,
               process_lci = TRUE, raw_folder = lci_raw_folder)

# CTD datasheet
create_datasheet(ctd_input, data_type = "CTD",
                 cruiseID = cruiseID)

# Neuston datasheet
create_datasheet(neuston_input, data_type = "neuston",
                 cruiseID = cruiseID)

# Meter datasheet
create_datasheet(meter_input, data_type = "meter",
                 cruiseID = cruiseID)

# Bottle datasheet
create_datasheet(bottle_input, data_type = "bottle",
                 ros_input = ros_folder,
                 process_calc = TRUE, calc_folder = calc_folder,
                 cruiseID = cruiseID)

# Secchi datasheet
create_datasheet(secchi_input, data_type = "SD",
                 cruiseID = cruiseID)

# Obs Summary Input
create_summary(obs_summary_input, elg_folder, cruiseID = cruiseID,
               csv_filename = "obs_summary_datasheet.csv")

# Obs datasheet
create_datasheet(obs_input, summary_input = "output/csv/<cruiseID>_obs_summary_datasheet.csv",
                 data_type = "OBS", cruiseID = cruiseID)

# Station Plots-----------------------------------------------------------------

# Uncomment one or all of these lines to run a script to generate station plots.
# Please navigate to the the 'plot_scripts' folder to make the necessary updates
# to cruiseID and output folder before running

# source("plot_scripts/StationPlotsNoBathymetry.R")
# source("plot_scripts/StationPlots.R")
# source("plot_scripts/ft_plots.R")

# Print Session Info------------------------------------------------------------

# Set cruiseID and run this line to log package versions used to analyze data in
# this script. This should be run at EOC, but will not do any harm if it is
# regularly run with the rest of the processing script.

source("eoc/<cruiseID>_session_info.R")
