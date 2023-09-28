test_that("all accepted file extensions still read as before", {
  withr::local_options(width = 20)

  expect_snapshot( # for csv
    waldo::compare(read_layout(test_path("reads_csv.csv")),
                   read_layout(test_path("reads_csv.csv")))
  )

  expect_snapshot( # for txt
    waldo::compare(read_layout(test_path("reads_plain_text.txt")),
                   read_layout(test_path("reads_plain_text.txt")))
  )

  expect_snapshot( # for xls
    waldo::compare(read_layout(test_path("reads_xls.xls")),
                   read_layout(test_path("reads_xls.xls")))
  )

  expect_snapshot( # for xlsx
    waldo::compare(read_layout(test_path("reads_xlsx.xlsx")),
                   read_layout(test_path("reads_xlsx.xlsx")))
  )

})

test_that("missing-well variations still read as before", {
  withr::local_options(width = 20)

  expect_snapshot( # mixture of empty and Empty
    waldo::compare(read_layout(test_path("case_insensitivity_to_Empty_empty.xlsx")),
                   read_layout(test_path("case_insensitivity_to_Empty_empty.xlsx")))
  )

  expect_snapshot( # all Empty
    waldo::compare(read_layout(test_path("drop_wells_if_all_variables_Empty.xlsx")),
                   read_layout(test_path("drop_wells_if_all_variables_Empty.xlsx")))
  )

  expect_snapshot( # mixture of Empty, empty, or NA
    waldo::compare(read_layout(test_path("drop_wells_if_mixed_empty_and_no_entry.xlsx")),
                   read_layout(test_path("drop_wells_if_mixed_empty_and_no_entry.xlsx")))
  )

  expect_snapshot( # no entries / NA
    waldo::compare(read_layout(test_path("drop_wells_with_no_values.xlsx")),
                   read_layout(test_path("drop_wells_with_no_values.xlsx")))
  )

  expect_snapshot( # don't drop wells unless all variables are empty
    waldo::compare(read_layout(test_path("retain_well_with_single_missing_variable.xlsx")),
                   read_layout(test_path("retain_well_with_single_missing_variable.xlsx")))
  )

})
