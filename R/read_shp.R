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
#' zip_file <- "~/Dropbox/ca_co_ab_dis.zip"
#' sf_obj <- read_shp(zip_file, layer = "CoAbDis")
#' mapview(sf_obj)
#'
#' @export
read_shp <- function (dsn = ".", layer = NULL, ..., verbose = getOption("verbose")) {

  msg <- function (...) if(isTRUE(verbose)) message("[read_shp] ", ...)

  if (str_begins(dsn, "http://")) {
    url_parts <- httr::parse_url(dsn)
    destfile <- tempfile(fileext = ".zip")
    msg("saving copy as: ", destfile)
    downloader::download(dsn, destfile = destfile)
    dsn <- destfile
  }

  try(dsn <- normalizePath(dsn)) # because `readOGR()` doesn't like "~" (home directory)

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

  if (is.null(layer)) {
    # default to the first layer
    available_layers <- rgdal::ogrListLayers(dsn)
    layer <- first(available_layers)
    if (length(available_layers) > 1) {
      msg("defaulting to layer: ", layer)
      msg("other layers available: ", str_csv(setdiff(available_layers, layer)))
    }
  }

  msg("importing layer ", layer, " from ", dsn)

  sf_obj <- read_sf(dsn = dsn, layer = layer, ...)
  comment(sf_obj) <- str_c("Imported from ", dsn, " ", format(Sys.time()))
  return(sf_obj)

}
