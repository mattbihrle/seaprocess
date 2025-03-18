#----------------EOC Processing-------------------------------------------------

# This script is intended to setup the cruise project to be used easily
# post-cruise AND create any cruise summary reports that need to go back
# to the office.

# First, load the processing library and define cruise ID.

library(seaprocess)

cruiseID <- ""

# Then, move the raw data. Step one is to tell the function where to find raw
# cruise data. You should be able to just copy and paste from
# "<cruiseID>_process_data.R"

elg_folder <- "<enter-location-of-elg-folder>"
ctd_folder <- "<enter-location-of-ctd-folder>"
ros_folder <- "<enter-location-of-ros-folder>"
adcp_folder <- "<enter-location-of-adcp-folder>"

#Next run EOC process which will copy data into the "raw/" directory within your
#cruise.


eoc_process(elg_folder = elg_folder, ctd_folder = ctd_folder,
            ros_folder = ros_folder, adcp_folder = adcp_folder,
            adcp_file_type = "LTA")


# Then, overwrite the block of code pointing to the elg/adcp/ctd files on
# different machines and tell the package to find them in the "raw/" directory.
#
# Copy and paste the block below into "<cruiseID>_process_data.R" and either
# delete or comment out the old one

elg_folder <- "raw/event"
ctd_folder <- "raw/ctd"
ros_folder <- "raw/ctd"
adcp_folder <- "raw/adcp"

# Finally, run through ALL of "<cruiseID>_process_data.R" once more to ensure
# there are no issues.
# Once this is done:
# Print Session Info------------------------------------------------------------

# Set cruiseID and run this line to log package versions used to analyze data in
# this script. This should be run at EOC, but will not do any harm if it is
# regularly run with the rest of the processing script.

source("eoc/<cruiseID>_session_info.R")



# Step two: Create a wire log for UNOLS reporting. Running the function
# below will generate a wire use report saved to the `output/csv` folder. NOTE:
# the function is looking for the column name "max_wire_out" so any wire
# deployments will need that column in their datasheet to be included.

process_wire_log(cruiseID = cruiseID)
