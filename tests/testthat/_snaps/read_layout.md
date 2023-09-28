# all accepted file extensions still read as before

    Code
      waldo::compare(
        read_layout(
          test_path(
            "reads_csv.csv")),
        read_layout(
          test_path(
            "reads_csv.csv")))
    Output
      v No differences

---

    Code
      waldo::compare(
        read_layout(
          test_path(
            "reads_plain_text.txt")),
        read_layout(
          test_path(
            "reads_plain_text.txt")))
    Output
      v No differences

---

    Code
      waldo::compare(
        read_layout(
          test_path(
            "reads_xls.xls")),
        read_layout(
          test_path(
            "reads_xls.xls")))
    Output
      v No differences

---

    Code
      waldo::compare(
        read_layout(
          test_path(
            "reads_xlsx.xlsx")),
        read_layout(
          test_path(
            "reads_xlsx.xlsx")))
    Output
      v No differences

# missing-well variations still read as before

    Code
      waldo::compare(
        read_layout(
          test_path(
            "case_insensitivity_to_Empty_empty.xlsx")),
        read_layout(
          test_path(
            "case_insensitivity_to_Empty_empty.xlsx")))
    Output
      v No differences

---

    Code
      waldo::compare(
        read_layout(
          test_path(
            "drop_wells_if_all_variables_Empty.xlsx")),
        read_layout(
          test_path(
            "drop_wells_if_all_variables_Empty.xlsx")))
    Output
      v No differences

---

    Code
      waldo::compare(
        read_layout(
          test_path(
            "drop_wells_if_mixed_empty_and_no_entry.xlsx")),
        read_layout(
          test_path(
            "drop_wells_if_mixed_empty_and_no_entry.xlsx")))
    Output
      v No differences

---

    Code
      waldo::compare(
        read_layout(
          test_path(
            "drop_wells_with_no_values.xlsx")),
        read_layout(
          test_path(
            "drop_wells_with_no_values.xlsx")))
    Output
      v No differences

---

    Code
      waldo::compare(
        read_layout(
          test_path(
            "retain_well_with_single_missing_variable.xlsx")),
        read_layout(
          test_path(
            "retain_well_with_single_missing_variable.xlsx")))
    Output
      v No differences

