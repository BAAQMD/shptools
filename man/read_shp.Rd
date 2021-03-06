% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_shp.R
\name{read_shp}
\alias{read_shp}
\title{Read a layer from a shapefile}
\usage{
read_shp(dsn = ".", layer = NULL, ..., verbose = getOption("verbose"))
}
\arguments{
\item{dsn}{(character) path to folder or ZIP file}

\item{layer}{(character) layer to extract}

\item{...}{further arguments to \code{\link[sf:read_sf]{sf::read_sf()}}}

\item{verbose}{(logical)}
}
\value{
an \code{sf} object
}
\description{
Read a layer from a shapefile
}
\examples{
zip_file <- "~/Dropbox/ca_co_ab_dis.zip"
sf_obj <- read_shp(zip_file, layer = "CoAbDis")
mapview(sf_obj)

}
