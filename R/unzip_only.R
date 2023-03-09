#' Unzip only files matching a pattern
#'
#' @param zip_file (character) path to ZIP file
#' @param pattern (character) see [utils::glob2rx] and [stringr::regex]
#' @param junkpaths (logical) if `TRUE`, use only the basename of the stored filepath (discard subdirectory names)
#' @param ... further arguments to [utils::unzip]
#' @param verbose (logical)
#'
#' @importFrom stringr str_trunc str_csv
#'
#' @examples
#' zip_file <- "/Users/dholstius/GitHub/BAAQMD/ARB/tmp/ca_co_ab_dis.zip"
#' exdir <- unzip_only(zip_file, pattern = "CoAbDis", verbose = TRUE)
#' show(exdir)
#'
#' @export
unzip_only <- function (zip_file, pattern = NULL, junkpaths = FALSE, ..., verbose = getOption("verbose")) {

  msg <- function (...) if(isTRUE(verbose)) message("[unzip_only] ", ...)

  if (is.null(pattern)) {
    pattern <- ".*"
  }

  exdir <- tempfile(pattern = basename(zip_file)) # NOT tempdir()
  dir.create(exdir)
  msg("exdir() is: ", exdir)
  msg("pattern is: ", pattern)

  # Get the names of all files in the ZIP archive. Don't unzip it just yet.
  file_set <- unzip(zip_file, list = TRUE)
  file_names <- file_set[["Name"]]
  which_match <- which(str_detect(file_names, pattern = pattern))
  matching_files <- file_names[which_match]
  msg("matching files are: ", str_trunc(str_csv(matching_files), width = 60))

  # Create a temporary subdirectory.
  # Unzip only those files matching `pattern`.
  unzip(zip_file,
        files = matching_files,
        exdir = exdir,
        junkpaths = junkpaths,
        ...)

  return(exdir)

}
