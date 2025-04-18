% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/elg.R
\name{filter_elg}
\alias{filter_elg}
\title{Filter elg data}
\usage{
filter_elg(
  elg,
  min_sal = 30,
  custom_filter = FALSE,
  flow_thr = c("temp", "sal", "fluor", "cdom", "xmiss"),
  filter_values = c("max_fluor = 30", "min_cdom = 0")
)
}
\arguments{
\item{elg}{compiled minute to minute elg data}

\item{min_sal}{minimum acceptable salinity default to 30 psu}

\item{flow_thr}{names of instruments on the flow through.}

\item{filter_values}{a list of filter values for when `custom_filter = TRUE`.
The function will take a `filter_values` argument and convert it to
`filter_values` for the function to use. Specify custom parameters within
`process_elg` using the formatting:
`filter_values = c("max_temp = 35", "min_temp = 15")` etc. Users can
specify `max` and `min` for any numeric column in the .csv output.}

\item{custom}{default to FALSE. if TRUE, user will have the option to set
ranges for all elg output variables.}
}
\description{
Filter out flow through data primarily based on min salinity threshold
}
\details{
SEA techs regularly backflush or clean components of our flow-through system
while under way. When this happens, the most telling value is salinity
however, all of our flow-through data is inaccurate for the time freshwater is
running through the system. This function takes a minimum salinity and filters
out data from all flow-through instruments (tsal, cdom, xmiss, chla, fluor)
while the salinity is below the minimum acceptable threshold. NOTE: if
instruments on the flow through change, the parameter 'flow_thr' will need to
be updated.
}
