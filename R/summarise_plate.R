#' Print a summary of plate contents
#'
#' Given a layout, create a printed summary of layout contents.
#'
#' @param layout A tibble of a tidied layout #### UPDATE THIS DOCUMENTATION
#' @param plate_notes A list containing notes, as strings, to be included in the summary.
#' @param ... Unused, for extensibility.
#'
#' @importFrom dplyr n_distinct
#' @importFrom glue glue_col
#'
#' @return A printable summary of the contents of the plate, including
#' * The number of wells
#'
#' * The number of unique experimental conditions
#'
#' * The name of each experimental variable
#'
#' * The value(s) for each experimental variable, and the number of wells in which that value occurs.
#'
#' * Any user-specified notes on the experiment.
#'
#' @export
#'
summarise_plate <- function(layout, plate_notes = "none", ...) {
  # get a few important values
  n_wells <- n_distinct(layout$well)
  n_variables <- length(names(layout)[!names(layout) %in% c("well", "row", "column", "condition")])
  variables <- names(layout)[!names(layout) %in% c("well", "row", "column", "condition")]

  # get plate heading
  plate_heading <- glue_col(.literal = TRUE, "Experiment containing {green {n_wells} wells}, with {green {n_variables}} {yellow experimental variables}")

  plate_msgs <- lapply(variables, summarise_variable, layout = layout)

  all_notes <- tryCatch(glue_col(.literal = TRUE, "{glue_collapse(plate_notes, sep = '\n ')}"), error = function(e) {
    "Notes could not be printed."
  })

  var_summary <- glue_col(.literal = TRUE, "{plate_heading} \n{glue_collapse(plate_msgs, sep = '\n \n')} \n \n{magenta {bold Notes:}} \n{plate_notes}")

  var_summary
}

#' Print a summary of a variable's contents
#'
#' Given a layout, prints the name of a given variable, and the number of wells
#' in which each of its values appears. Used as a part of [summarise_plate()].
#'
#' @inheritParams summarise_plate
#' @param variable_name The name of the column containing the experimental
#'   variable to be summarised, as a string.
#' @param ... Unused, for extensibility.
#'
#' @importFrom dplyr select distinct group_by tally mutate pull
#' @importFrom tidyselect all_of
#' @importFrom glue glue_col
#' @return A glued string stating the variable name, and the number of wells in
#'   which each value of that variable occurs.
#' @export
#'
summarise_variable <-
  function(layout, variable_name, ...) {
    # get stuff like "cmpd1 in 24 wells"
    var_counts <-
      layout |>
      select(all_of(c("well", variable_name))) |>
      distinct() |>
      group_by(.data[[variable_name]]) |>
      tally() |>
      mutate("msg" = paste0(
        "--- {blue ", .data[[variable_name]],
        "} in ", .data$n, " wells\n"
      )) |>
      pull("msg")

    # don't include variable summaries which cannot be glued
    tryCatch(
      {
        var_name <- glue_col(.literal = TRUE, "{yellow {bold {variable_name}}} (type: {glue_collapse(typeof(layout[[variable_name]]))}), includes: \n {glue_col({glue_collapse(var_counts)})}\n")
      },
      error = function(e) {
        var_name <- glue_col(.literal = TRUE, "{yellow {bold {variable_name}}} (type: {glue_collapse(typeof(layout[[variable_name]]))}) -- variable couldn't be printed. Does it contain special characters? \n")
      },
      finally = function(f) {
        var_name <- glue_col("variable could not be printed")
      }
    )
  }
