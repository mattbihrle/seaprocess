% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ctd.R
\name{read_ctd}
\alias{read_ctd}
\title{Read and process CTD data}
\usage{
read_ctd(
  cnv_file,
  pmin = 1,
  p = 1,
  to_tibble = TRUE,
  cruiseID = NULL,
  depth_vec = NULL,
  depth_step = 1,
  ...
)
}
\arguments{
\item{cnv_file}{file path to the CNV file to be read}

\item{pmin}{minimum cut off pressure for ctdTrim}

\item{p}{pressure bins for ctdDecimate}

\item{...}{additional arguments passed to called functions}
}
\description{
Wrapper function for oce functions read.ctd, ctdTrim, and ctdDecimate.
Extracts and adds metadata from header that is unique to SEA headers.
}
