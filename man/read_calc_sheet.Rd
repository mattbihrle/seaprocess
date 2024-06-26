% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcsheet.R
\name{read_calc_sheet}
\alias{read_calc_sheet}
\title{Append calculation sheet data to bottle summary}
\usage{
read_calc_sheet(calc_file, bottle_sum, bottle_is_file = FALSE)
}
\arguments{
\item{calc_file}{#the calculation file to be read, chlorophyll, PO4, NO3, pH.
This file must be formatted correctly with an 'output' sheet.}

\item{bottle_sum}{#the bottle summary from the [create_bottle()] function}

\item{bottle_is_file()}{#if TRUE, this will read the [bottle.csv] file and
creat the [bottle] object. If FALSE (the default), it will append data to
the bottle object.}
}
\value{
#creates the [bottle] object that will be updated with new data each
 time it is run.
}
\description{
This function appends hand-entered and calculated data from calculation sheets
to the bottle summary csv. It takes the 'output' sheet on the calculation
excel file with whatever header value is used and matches it with station
number and bottle number.
}
\details{
This function, and the [read_calc_fold()] function combine the
[output] sheet from any calculation sheet (from path ['calc_file]) and the
[bottle] object. If [bottle] is not an object (ex. if it is the first time
this is run) it will read the [bottle.csv] file from the [bottle_sum]
diriectory and create the [bottle] object. [create_bottle()] will overwrite
the [bottle.csv] and delete any calc sheet data added in past uses of this
function. RUnning this function again will replace these data.
}
