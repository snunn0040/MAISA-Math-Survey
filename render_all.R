library(quarto)

# Derive the school list straight from the cleaned data so the
# strings match exactly (run prep_data.R first to create responses.csv).
schools <- readr::read_csv("responses.csv", show_col_types = FALSE) |>
  dplyr::distinct(school) |>
  dplyr::arrange(school) |>
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
