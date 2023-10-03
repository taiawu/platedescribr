#' Get path to tidyplate examples
#'
#' tidyplate comes with a number of sample files in its 'inst/extdata' directory. Use `platedescribr_example()` to retrieve the path to an available example.
#' example.
#'
#' This documentation is minimally modified from the analogous file in the vroom package. (see https://github.com/tidyverse/vroom/blob/main/R/example.R)
#'
#' @param path Name of file.
#'
#' @return Path to the supplied file.
#' @export
#'
#' @examples
#' # Get path to one example
#' # tidyplate_example("dose_response.csv")
#'
tidyplate_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "tidyplate"))
  } else {
    system.file("extdata", path, package = "tidyplate", mustWork = TRUE)
  }
}
