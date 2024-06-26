% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{summary_check}
\alias{summary_check}
\title{Summary Check}
\usage{
summary_check(summary = summary, skipcheck = FALSE)
}
\arguments{
\item{summary}{summary data frame to be checked.}

\item{skipcheck}{defaults to FALSE, set to TRUE within create_summary to skip
this step.}
}
\description{
Ensures each entry in the summary sheet has the needed information to run the
create_datasheet functions down the line. Will make sure each station has a
deployment, date, time_in, and zd. If not, will show a warning detailing
which stations and data to fix.
}
\details{
NOTE: does not check formatting. For
formatting help, see the notes at the top of the summary_input sheet.
}
