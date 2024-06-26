% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Geo_extract.R
\name{Geo_extract}
\alias{Geo_extract}
\title{Geo_extract}
\usage{
Geo_extract(ISO, Level, dataset, timestep)
}
\arguments{
\item{ISO}{ISO3 code of your targeted country}

\item{Level}{Level of your targeted ADM shape file}

\item{dataset}{A string representing the dataset to be used (e.g., "cru-ts").}

\item{timestep}{Time resolution for the data (e.g., "yearly", "monthly").}
}
\value{
A data frame/csv object that contains geodata for the specified time period.
}
\description{
Provides a CSV data of any geo data for every coumtry in the world.
}
\examples{
data <- Geo_extract(ISO = "VNM", Level = "2", dataset = "cru-ts", timestep = "yearly")
}
