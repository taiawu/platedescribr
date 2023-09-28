#' Get path to platedefinr examples
#'
#' platedefinr comes with a number of sample files in its 'inst/extdata' directory. Use `platedescribr_example()` to retrieve the path to an available example.
#' example.
#'
#' This documentation is minimally modified from the analogous file in the vroom package. (see https://github.com/tidyverse/vroom/blob/main/R/example.R)
#'
#' @param path Name of file.
#'
#' @return
#' @export
#'
#' @examples
#' # Get path to one example
#' platedescribr_example("dose_response.csv")
#'
platedescribr_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "platedescribr"))
  } else {
    system.file("extdata", path, package = "platedescribr", mustWork = TRUE)
  }
}
