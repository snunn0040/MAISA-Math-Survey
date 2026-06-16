library(tidyverse)
library(lavaan)

# ============================================================
# cfa.R
# Confirmatory factor analysis of the math-perceptions survey.
# 4 factors, 13 items, all schools pooled.
# Factor structure = the original construct grouping documented in
# math_onepager.qmd / prep_data.R comments.
# Reads the tidy long file written by prep_data.R (responses.csv).
# ============================================================

# --- 1. Load tidy responses (long: school, student_id, item, response) ---
responses <- read_csv("responses.csv", show_col_types = FALSE)

# --- 2. Reverse-score the one reverse-coded item -----------
# 4-point Likert (1-4), so the reflection is (4 + 1) - x = 5 - x.
# teacher_talks: high = teacher does most talking = less student-centered.
# After reversing, high = more student-centered, matching the others.
LIKERT_MAX <- 4L
responses <- responses |>
  mutate(response = if_else(item == "teacher_talks",
                            (LIKERT_MAX + 1L) - response,
                            response))

# --- 3. Pivot to wide: one row per student, one column per item ---
wide <- responses |>
  select(student_id, item, response) |>
  pivot_wider(names_from = item, values_from = response)

item_cols <- c("enjoy", "interesting", "math_person", "can_solve", "important",
               "belonging", "valued", "comfortable",
               "many_ways", "mistakes", "explain", "classmates", "teacher_talks")

stopifnot(all(item_cols %in% names(wide)))

# Declare the 13 items as ordered factors so lavaan uses the WLSMV
# estimator (correct for 4-point Likert). Pairwise deletion is the
# default for ordered data in lavaan.
wide <- wide |> mutate(across(all_of(item_cols), ordered))

# --- 4. Specify the 4-factor measurement model -------------
# Original construct grouping (math_onepager.qmd). Every factor has
# >= 2 indicators, so there is no single-indicator identification
# problem; `important` lives with the attitudes items here.
cfa_model <- '
  attitudes =~ enjoy + interesting + important
  identity  =~ math_person + can_solve
  belonging =~ belonging + valued + many_ways + mistakes + comfortable
  discourse =~ explain + classmates + teacher_talks
'

# --- 5. Fit -------------------------------------------------
fit <- cfa(
  model       = cfa_model,
  data        = wide,
  ordered     = item_cols,   # 4-point Likert -> ordinal / WLSMV
  estimator   = "WLSMV",
  missing     = "pairwise"
)

# --- 6. Report ---------------------------------------------
# Fit indices: use the *scaled* / robust versions for WLSMV.
cat("\n===== Model fit (robust/scaled indices for WLSMV) =====\n")
print(fitMeasures(fit, c("chisq.scaled", "df.scaled", "pvalue.scaled",
                         "cfi.scaled", "tli.scaled",
                         "rmsea.scaled", "rmsea.ci.lower.scaled",
                         "rmsea.ci.upper.scaled", "srmr")))
# Rules of thumb: CFI/TLI >= .95 good (>= .90 acceptable),
# RMSEA <= .06 good (<= .08 acceptable), SRMR <= .08.

cat("\n===== Standardized loadings & factor correlations =====\n")
print(standardizedSolution(fit), nd = 3)

cat("\n===== Full summary =====\n")
summary(fit, fit.measures = TRUE, standardized = TRUE)

# --- 7. (Optional) save outputs ----------------------------
# saveRDS(fit, "cfa_fit.rds")
