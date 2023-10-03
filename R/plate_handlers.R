#' Wells to rows ## DOCUMENTATION NOT FINISHED######
#'
#' Given a well name (e.g. "A1"), extract the plate row ("A").
#'
#' @family well name handlers
#'
#' @param well_names A vector of well names.
#'
#' @return A vector containing only the character elements of each well name.
#' @export
#'
#' @examples
#' wells_to_rows(c("A1", "A2", "B1", "B2"))
#' # [1] "A" "A" "B" "B"
#'
wells_to_rows <- function(well_names){
  gsub('[0-9]+', '', well_names)
}


#' Wells to cols ## DOCUMENTATION NOT FINISHED######
#'
#' Given a well name (e.g. "A1"), extract the plate column (1).
#'
#' @param well_names A vector containing only the character elements of each well name.
#'
#' @return A vector containing only the numeric elements of each well name.
#' @export
#'
#' @examples
#' wells_to_cols(c("A1", "A2", "B1", "B2"))
#' # [1] 1 2 1 2
#'
wells_to_cols <- function(well_names){
  as.numeric(gsub("\\D", "", well_names))
}

#' Expand wells ## DOCUMENTATION NOT FINISHED######
#'
#' @param dim1 first dimension to expand
#' @param dim2 second dimension to expand
#' @param .leading_zeroes whether or not to add leading zeroes
#'
#' @importFrom dplyr arrange mutate if_else
#'
#' @return vector of well names
#' @export
#'
#' @examples
#'
#' expand_wells(c("A", "B"), c(8:10))
#' # [1] "A8"  "B8"  "A9"  "B9"  "A10" "B10""
#'
#' # leading zeroes can be added to well names
#' expand_wells(c("A", "B"), c(8:10), .leading_zeroes = TRUE)
#' # [1] "A08" "B08" "A09" "B09" "A10" "B10"
#'
expand_wells <- function(dim1, dim2, .leading_zeroes = FALSE) {
  expand.grid("cols" = dim1, "rows" = dim2)
  do.call(paste0, dplyr::arrange(expand.grid("cols" = dim1, "rows" = dim2), "cols"))

  if(.leading_zeroes){
    expanded <- arrange(expand.grid("cols" = dim1, "rows" = dim2), "cols") |>
      mutate("rows" = if_else(.data$rows < 10,
                            true = paste0("0", as.character(.data$rows)),
                            false = as.character(.data$rows))) |>
      do.call(paste0, args =  _)

  } else {
    expanded <- do.call(paste0, arrange(expand.grid("cols" = dim1, "rows" = dim2), "cols"))
  }

  expanded
}


#' Add empty wells ### DOCUMENTATION NOT FINISHED######
#'
#' Add wells missing from the input layout. These wells are assigned `NA` for all variables.
#'
#' @param layout A layout tibble
#' @param plate_type the type of plate
#' @param .well_col A string--the name of the column that holds the well
#' @param ... Unused, for extensibility
#'
#' @importFrom dplyr tibble right_join mutate
#' @importFrom rlang :=
#'
#'
#' @return the same layout, but with all wells in the chosen plate type
#' @export
add_empty_wells <- function(layout,
                            plate_type = "384",
                            .well_col = "well",
                            ...) {

  # generate well names for full plate
  wells <- tibble("{.well_col}" := well_names(plate_type, ...))

  filled <- right_join(layout, wells, by = .well_col)

  # add plate row and column, if layout has these
  if ("row" %in% names(layout) & "column" %in% names(layout)) {
    filled <- filled |>
      mutate("row" = wells_to_rows(.data[[.well_col]]), # will [[.well_col]] work in a package?
             "column" = wells_to_cols(.data[[.well_col]])) # will [[.well_col]] work in a package?
  }

  filled
}

#' Get the names of wells from a given plate  ### DOCUMENTATION NOT FINISHED######
#'
#' @param platetype type of plate
#' @param ... Unused, for extensibility
#' @param dim1 plate rows
#' @param dim2 plate cols
#' @param .leading_zeroes add leading zeroes?
#' @param .as_factor make the output a factor?
#'
#' @return a vector of well names.
#' @export
#'
#' @examples
#' well_names(platetype = 384)
#'
well_names <- function(platetype, ..., dim1 = NULL, dim2 = NULL, .leading_zeroes = FALSE, .as_factor = FALSE) {
  # function to create a string of well names for a desired plate type

  # accept number or character input
  platetype <- as.character(platetype)

  # guide unusable custom dimensions
  if(platetype == "custom") {
    if(is.null(dim1) | is.null(dim2)) {
      stop("For 'custom' plate types, you must supply a vector to both `dim1` and `dim2`.")
    }
  }

  if(!platetype %in% c("384", "96", "custom")){
    stop(paste0("Unknown `.platetype` supplied: ", platetype, ". Please specify one of: '96', '384', or 'custom'."))
  }

  # Generate well names  --------------------------------------------------------------
  # generate all possible combinations of supplied vectors as
  wells <- switch(platetype, # accept number or character
                  "384" = expand_wells(LETTERS[1:16], c(1:24), .leading_zeroes),
                  "96" = expand_wells(LETTERS[1:8], c(1:12), .leading_zeroes),
                  "custom" = expand_wells(dim1, dim2), .leading_zeroes)

  # turn into a factor if requested.
  if(.as_factor){ # order determined by first vector; see expand_wells() above
    wells <- factor(wells, levels = wells)
  }

  wells
}

