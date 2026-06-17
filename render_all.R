library(quarto)

# Minimum student responses for a district to get its own one-pager.
# Districts below this are skipped entirely (not just per-item suppressed).
MIN_RESPONSES <- 10

# Count distinct student responses per district, straight from the cleaned
# data so the strings match exactly (run prep_data.R first to create
# responses.csv).
district_counts <- readr::read_csv("responses.csv", show_col_types = FALSE) |>
  dplyr::filter(!is.na(district)) |>
  dplyr::group_by(district) |>
  dplyr::summarise(n_students = dplyr::n_distinct(student_id), .groups = "drop") |>
  dplyr::arrange(district)

skipped <- dplyr::filter(district_counts, n_students < MIN_RESPONSES)
if (nrow(skipped) > 0) {
  message("Skipping ", nrow(skipped), " district(s) with < ", MIN_RESPONSES,
          " responses:")
  message(paste0("  ", skipped$district, " (n = ", skipped$n_students, ")",
                 collapse = "\n"))
}

districts <- district_counts |>
  dplyr::filter(n_students >= MIN_RESPONSES) |>
  dplyr::pull(district)

dir.create("onepagers", showWarnings = FALSE)

for (d in districts) {
  file_safe <- gsub("[^A-Za-z0-9]+", "_", d)
  message("Rendering: ", d)

  quarto::quarto_render(
    input          = "math_onepager.qmd",
    execute_params = list(district = d),
    output_file    = paste0(file_safe, ".pdf")
  )

  # quarto_render writes next to the .qmd; move into the onepagers/ folder
  file.rename(paste0(file_safe, ".pdf"),
              file.path("onepagers", paste0(file_safe, ".pdf")))
}

message("Done. PDFs are in ./onepagers/")
