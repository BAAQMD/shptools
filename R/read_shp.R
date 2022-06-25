#' Read a layer from a shapefile
#'
#' @param dsn (character) path to folder or ZIP file
#' @param layer (character) layer to extract
#' @param crs (optional) passed to [sf::st_transform()]
#' @param ... passed to [sf::read_sf()]
#' @param verbose (logical)
#'
#' @return an `sf` object
#'
#' @importFrom httr parse_url
#' @importFrom downloader download
#' @importFrom rgdal ogrListLayers
#' @importFrom stringr str_starts str_detect str_c regex
#' @importFrom sf read_sf st_transform
#'
#' @examples
#' zip_file <- "~/Dropbox/ca_co_ab_dis.zip"
#' sf_obj <- read_shp(zip_file, layer = "CoAbDis")
#' mapview(sf_obj)
#'
#' @export
read_shp <- function (
  dsn = ".",
  layer = NULL,
  crs = NULL,
  ...,
  verbose = getOption("verbose")
) {

  msg <- function (...) if(isTRUE(verbose)) message("[read_shp] ", ...)

  if (stringr::str_starts(dsn, "http://")) {
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
    stringr::str_detect(
      dsn,
      stringr::regex("\\.zip$", ignore_case = TRUE))
  }

  rmdir <- function (path) {
    msg("deleting ", path)
    base::unlink(path, recursive = TRUE)
  }

  if (is_zipfile(dsn)) {
    exdir <- unzip_only(dsn, pattern = layer, junkpaths = TRUE, verbose = verbose)
    dsn <- exdir
    on.exit(rmdir(exdir))
  }

  if (is.null(layer)) {
    # default to the first layer
    available_layers <- rgdal::ogrListLayers(dsn)
    layer <- available_layers[[1]]
    if (length(available_layers) > 1) {
      other_layers <- setdiff(available_layers, layer)
      msg("defaulting to layer: ", layer)
      msg("other layers available: ", stringr::str_c(other_layers, collapse = ", "))
    }
  }

  msg("importing layer ", layer, " from ", dsn)

  sf_obj <- read_sf(dsn = dsn, layer = layer, ...)

  if (isFALSE(is.null(crs))) {
    sf_obj <- sf::st_transform(sf_obj, crs = crs)
  }

  comment(sf_obj) <- dsn
  attr(sf_obj, "dsn") <- dsn
  attr(sf_obj, "layer") <- layer

  return(sf_obj)

}
