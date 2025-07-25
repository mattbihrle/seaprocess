% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{create_summary}
\alias{create_summary}
\title{Create Station Summary Sheet}
\usage{
create_summary(
  summary_input,
  elg_input,
  csv_folder = "output/csv",
  csv_filename = "summary_datasheet.csv",
  force_stations = TRUE,
  cruiseID = NULL,
  add_cruiseID = TRUE,
  magdiff = 60,
  skipcheck = FALSE,
  process_lci = FALSE,
  raw_folder = NULL,
  keep = c("lat", "lon", "temp", "fluor", "sal", "bot_depth", "cdom", "xmiss", "wind_sp",
    "wind_dir", "heading", "pitch", "roll", "wire_tension", "filename"),
  ...
)
}
\arguments{
\item{summary_input}{The input datasheet that includes the relevent station
and deployment metadata}

\item{elg_input}{The cruise elg file (or folder of files) for extracting
continuous data from}

\item{csv_folder}{The directory path to output the csv file. Set to NULL for
no csv output.}

\item{csv_filename}{The csv filename to output the data}

\item{force_stations}{logical - set to TRUE if you want to force the output
to have station data at the nearest elg entry regardless of whether it is a
longer time than magdiff from the nearest data row}

\item{cruiseID}{Optional string specifying cruise ID (i.e. "S301")}

\item{magdiff}{maximum time difference in seconds between the station time
and the nearest elg time. If greater than this value, look to force_station
as to whether to return a NA or add a value regardless}

\item{skipcheck}{toggles whether to check through the summary data to make
sure all required fields (deployment, date, time in, zd) all have values.
Defaults to FALSE, set to TRUE to bypass this step.}

\item{process_lci}{optional to reroute the summary function to process wire
data from the LCI90 raw file (RCS) or the Hydrowinch Tension Files folder
(CC). Also requires the parameter \code{raw_folder}}

\item{raw_folder}{location of the .RAW file for the LCI90 created by SCS. The
function will automatically look for a filename containing "LCI90-raw" on
RCS or, on CC, it will look for all files in the defined folder except for
files/folders called "archive files."}

\item{keep}{a character vector of all the variables from the elg file that
could be added to the summary datasheet. Default is all of the values from
\code{\link[=read_elg]{read_elg()}} except 'dttm'. If needed, add additional names from the elg
file \emph{without} units to this vector and your summary sheet. Eg. 'xmiss_counts' would be 'xmiss', 'cdom_fluor'
would be 'cdom'. \code{create_summary} will try to match column names from this
list with the column names in the summary_input sheet.}

\item{...}{optional arguments passed to read_elg like forceGPS. See
\code{?read_elg} for more information.}

\item{add_cruise_ID}{If cruiseID is set, logical to specify whether cruiseID
should be appended to beginning of filenames for csv and odv output}
}
\value{
A tibble containing the combined data frames. If csv_folder is set to
a valid output path then a formatted csv file is output also.
}
\description{
This function combines hand-entered station metadata with electronically
recorded location and environmental data to output a well formatted station
summary sheet.
}
\details{
During deployments at SEA, we maintain paper data sheets which
recorded station metadata and data from that deployment. These are a vital
component for our data accuracy and redundancy.

In creating electronic datasheets for deployments it is desirable to
combine hand-entered station metadata (station number, station type, zone
description, etc.) with electronically recorded data (location, surface
conditions, etc.) to provide an accurate record of the deployment without
the need to re-enter hand-recorded values of the electronic data.

To do this, \code{\link[=create_summary]{create_summary()}} takes in an excel sheet with the bare
minimum of hand-entered data:
\itemize{
\item Station number
\item Deployment type
\item Deployment date/time in (and out)
\item Zone description (time zone)

\code{\link[=create_summary]{create_summary()}} combines this data with the electronically recorded
"event" data which records (amongst other things):
\item Time
\item Location
\item Surface Temperature
\item Surface Salinity
\item etc.

The event data is typically stored in a file with an extension elg.
\code{\link[=read_elg]{read_elg()}} deals with reading this data in and formatting it properly.

Once the two data frames are read in to R, they are combined using the UTC
time that exists in both data frames.
}
}
