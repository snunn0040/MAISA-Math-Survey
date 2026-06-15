library(tidyverse)

# ============================================================
# prep_data.R
# Raw Qualtrics to data we can use
# ============================================================

raw <- read_csv("maisa23hSS_values_061226.csv", show_col_types = FALSE)

# --- 1. Inspect first, fill in the maps below ---------------
# Uncomment and run these two lines once to see your real columns
# and a sample of values, then edit item_map / likert_map to match.
# print(names(raw))
# glimpse(raw)

# --- 2. Drop Qualtrics' two extra header rows ---------------
# Row 1 = full question text, row 2 = {"ImportId":...} JSON.
# Eyeball head(raw) first and confirm these are the junk rows
# (some exports don't include them -- if so, delete this slice()).
raw <- raw |> slice(-(1:2))

# --- 3. Map raw question columns -> item codes --------------

item_map <- c(
  "Q6"  = "enjoy",        "Q7"  = "interesting",  "Q10"  = "important",
  "Q8"  = "math_person",  "Q9"  = "can_solve",
  "Q11"  = "belonging",    "Q12"  = "valued",       "Q14" = "many_ways",
  "Q15" = "mistakes",     "Q16" = "comfortable",
  "Q17" = "explain",      "Q18" = "classmates",   "Q19" = "teacher_talks"
)

# --- 4. Recode text responses -> 1-5 -----------------------
# Uncomment if responses are words; else set responses_are_text <- FALSE.
responses_are_text <- FALSE
###likert_map <- c(
###  "Strongly disagree"          = 1,
###  "Disagree"                   = 2,
###  "Neither agree nor disagree" = 3,
###  "Agree"                      = 4,
###  "Strongly agree"             = 5
###)
school_lookup <- read_csv("school_lookup.csv", show_col_types = FALSE) |>
  transmute(school_code = as.character(school_code),  # <- code column in your file
            school_name = school_name)                # <- name column in your file

# --- 5. Pivot to long + clean ------------------------------
responses <- raw |>
  rename(student_id = ResponseId,   
         school     = Q22) |>        # the "what school" column
  mutate(school_code = str_trim(as.character(school_code))) |>
  left_join(school_lookup, by = "school_code") |>
  rename(school = school_name) |>
  select(student_id, school, all_of(names(item_map))) |>
  pivot_longer(all_of(names(item_map)),
               names_to = "qcol", values_to = "response") |>
  mutate(
    item     = unname(item_map[qcol]),
    response = if (responses_are_text) {
      as.integer(unname(likert_map[str_trim(response)]))
    } else {
      as.integer(response)
    }
  ) |>
  select(school, student_id, item, response)

# --- 6. Quick sanity checks (printed to console) -----------
unmapped <- responses |> filter(is.na(response))
if (nrow(unmapped) > 0) {
  warning(nrow(unmapped),
          " responses became NA -- check that every answer text is in likert_map.")
  print(count(unmapped, item), n = Inf)
}

missing_items <- setdiff(unname(item_map), unique(responses$item))
if (length(missing_items) > 0) {
  warning("These item codes never appeared: ",
          paste(missing_items, collapse = ", "))
}

cat("\nSchools found (copy these into render_all.R):\n")
print(sort(unique(responses$school)))
cat("\nResponses per school:\n")
print(count(responses, school))

# --- 7. Write the tidy file the report reads ---------------
write_csv(responses, "responses.csv")
cat("\nWrote responses.csv (", nrow(responses), " rows).\n", sep = "")
