#' Read a layer from a shapefile
#'
#' @param dsn (character) path to folder or ZIP file
#' @param layer (character) layer to extract
#' @param ... further arguments to [sf::read_sf()]
#' @param verbose (logical)
#'
#' @return an `sf` object
#'
#' @examples
#' shp_zip <- "~/GitHub/BAAQMD/ARB/tmp/ca_co_ab_dis.zip"
#' sf_obj <- read_shp(shp_zip, layer = "CoAbDis")
#' show(sf_obj)
#'
#' @export
read_shp <- function (dsn, layer, ..., verbose = getOption("verbose")) {

  msg <- function (...) if(isTRUE(verbose)) message("[read_shp] ", ...)

  try(dsn <- normalizePath(dsn)) # because `readOGR()` doesn't like "~" (home directory)
  msg("reading layer ", layer, " from ", dsn)

  #
  # Allow for `dsn` to name a zipped shapefile.
  #
  # In this case, extract the contents to a (temporary) directory
  # and then proceed as normal.
  #
  is_zipfile <- function (dsn) {
    str_detect(dsn, regex("\\.zip$", ignore_case = TRUE))
  }

  if (is_zipfile(dsn)) {
    dsn <- unzip_only(dsn, pattern = layer, junkpaths = TRUE, verbose = verbose)
  }

  sf_obj <- read_sf(dsn = dsn, layer = layer, ...)
  comment(sf_obj) <- str_c("Imported from ", dsn, " ", format(Sys.time()))
  return(sf_obj)

}
