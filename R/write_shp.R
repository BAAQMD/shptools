#' Write geodata to a shapefile
#'
#' @param geodata (sf or sp object)
#' @param dsn (character) generally, a path on disk; see [sf::st_write()]
#' @param layer (character) layer name; see [sf::st_write()]
#' @param ... further arguments to [sf::write_sf()]
#' @param verbose (logical)
#'
#' @importFrom sf write_sf
#' @importFrom digest digest
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

  msg("writing ", nrow(geodata), " features to ", file.path(dsn, layer))
  msg("md5 is: ", digest::digest(geodata, algo = "md5"))

  sf::write_sf(
    geodata,
    dsn = dsn,
    layer = layer,
    driver = "ESRI Shapefile",
    quiet = !verbose,
    ...)

  # To allow for chaining, return the original input (invisibly)
  return(invisible(geodata))

}
