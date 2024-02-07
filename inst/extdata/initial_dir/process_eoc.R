#----------------EOC Processing-------------------------------------------------

# This script is intended to setup the cruise project to be used easily
# post-cruise. First, tell the function where to find raw cruise data. You should
# be able to just copy and paste from "__process_data.R"

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

# Copy and paste the block below into "__process_data.R" and either delete or
# comment out the old one

elg_folder <- "raw/event"
ctd_folder <- "raw/ctd"
ros_folder <- "raw/ctd"
adcp_folder <- "raw/adcp"

# Finally, run through ALL of "__process_data.R" to ensure there are no issues.



