#' Write geodata to a shapefile
#'
#' @param geodata (sf or sp object)
#' @param dsn (character) generally, a path on disk; see [sf::st_write()]
#' @param layer (character) layer name; see [sf::st_write()]
#' @param ... further arguments to [sf::write_sf()]
#'
#' @examples
#' require(ARB)
#' dsn <- tempdir()
#' write_shp(ARB_CoAbDis, dsn = dsn, layer = "CoAbDis", delete_layer = TRUE)
#'
#' @return `geodata`, invisibly (to allow for chaining)
#'
#' @export
write_shp <- function (
  geodata,
  dsn = getwd(),
  layer = deparse(substitute(geodata)),
  ...,
  verbose = TRUE
) {

  msg <- function (...) if(isTRUE(verbose)) message("[write_shp] ", ...)

  if (!dir.exists(dsn)) {
    msg("creating directory: ", dsn)
    dir.create(dsn, recursive = TRUE)
  }

  dsn <- normalizePath(dsn, mustWork = TRUE)

  write_sf(
    geodata,
    dsn = dsn,
    layer = layer,
    driver = "ESRI Shapefile",
    quiet = !verbose,
    ...)

  # To allow for chaining, return the original input (invisibly)
  return(invisible(geodata))

}
