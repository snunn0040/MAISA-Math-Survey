library(quarto)

# Minimum student responses for a school to get its own one-pager.
# Schools below this are skipped entirely (not just per-item suppressed).
MIN_RESPONSES <- 10

# Count distinct student responses per school, straight from the cleaned
# data so the strings match exactly (run prep_data.R first to create
# responses.csv).
school_counts <- readr::read_csv("responses.csv", show_col_types = FALSE) |>
  dplyr::filter(!is.na(school)) |>
  dplyr::group_by(school) |>
  dplyr::summarise(n_students = dplyr::n_distinct(student_id), .groups = "drop") |>
  dplyr::arrange(school)

skipped <- dplyr::filter(school_counts, n_students < MIN_RESPONSES)
if (nrow(skipped) > 0) {
  message("Skipping ", nrow(skipped), " school(s) with < ", MIN_RESPONSES,
          " responses:")
  message(paste0("  ", skipped$school, " (n = ", skipped$n_students, ")",
                 collapse = "\n"))
}

schools <- school_counts |>
  dplyr::filter(n_students >= MIN_RESPONSES) |>
  dplyr::pull(school)

dir.create("onepagers", showWarnings = FALSE)

for (s in schools) {
  file_safe <- gsub("[^A-Za-z0-9]+", "_", s)
  message("Rendering: ", s)

  quarto::quarto_render(
    input          = "math_onepager.qmd",
    execute_params = list(school = s),
    output_file    = paste0(file_safe, ".pdf")
  )

  # quarto_render writes next to the .qmd; move into the onepagers/ folder
  file.rename(paste0(file_safe, ".pdf"),
              file.path("onepagers", paste0(file_safe, ".pdf")))
}

message("Done. PDFs are in ./onepagers/")
