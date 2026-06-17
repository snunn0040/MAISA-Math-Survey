library(quarto)

# Render a SINGLE district's one-pager for quick testing, instead of the
# whole batch (see render_all.R for the full run).
#
# Set DISTRICT to the district you want. Leave it as NULL to just grab the
# first district found in responses.csv.
DISTRICT <- NULL

available <- readr::read_csv("responses.csv", show_col_types = FALSE) |>
  dplyr::filter(!is.na(district)) |>
  dplyr::distinct(district) |>
  dplyr::arrange(district) |>
  dplyr::pull(district)

if (is.null(DISTRICT)) {
  DISTRICT <- available[1]
  message("DISTRICT not set; using first available: ", DISTRICT)
} else if (!DISTRICT %in% available) {
  stop("District '", DISTRICT, "' not found. Available districts:\n  ",
       paste(available, collapse = "\n  "))
}

dir.create("onepagers", showWarnings = FALSE)
file_safe <- gsub("[^A-Za-z0-9]+", "_", DISTRICT)

message("Rendering: ", DISTRICT)
quarto::quarto_render(
  input          = "math_onepager.qmd",
  execute_params = list(district = DISTRICT),
  output_file    = paste0(file_safe, ".pdf")
)

# quarto_render writes next to the .qmd; move into the onepagers/ folder
file.rename(paste0(file_safe, ".pdf"),
            file.path("onepagers", paste0(file_safe, ".pdf")))

message("Done. ./onepagers/", file_safe, ".pdf")
