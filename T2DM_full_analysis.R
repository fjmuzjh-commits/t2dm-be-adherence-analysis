
# ==============================================================================
# T2DM_full_analysis.R
# Complete, reproducible R analysis project for SCI submission
# ===============================================================================
# Run from a clean R session at the project root:
#   rm(list = ls())
#   setwd("path/to/T2DM_R_project")
#   source("T2DM_full_analysis.R", encoding = "UTF-8")
# ===============================================================================

options(stringsAsFactors = FALSE)
options(warn = 1)
if (identical(getOption("repos")[["CRAN"]], "@CRAN@") || is.null(getOption("repos")[["CRAN"]])) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}
set.seed(20260714)

PROJECT_ROOT <- normalizePath(".", winslash = "/", mustWork = TRUE)
INPUT_FILE <- file.path(PROJECT_ROOT, "input", "T2DM_SCI_final_data_v1.2.csv")
OUTPUT_DIR <- file.path(PROJECT_ROOT, "output")
TABLE_DIR <- file.path(OUTPUT_DIR, "tables")
DIAG_DIR <- file.path(OUTPUT_DIR, "diagnostics")
MODEL_DIR <- file.path(OUTPUT_DIR, "model_objects")
LOG_DIR <- file.path(OUTPUT_DIR, "logs")
LOG_FILE <- file.path(LOG_DIR, "analysis_log.txt")
INSTALL_MISSING_PACKAGES <- TRUE
required_packages <- c("psych", "lavaan", "writexl", "lmtest", "sandwich")

for (d in c(OUTPUT_DIR, TABLE_DIR, DIAG_DIR, MODEL_DIR, LOG_DIR)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
}
if (file.exists(LOG_FILE)) file.remove(LOG_FILE)

source(file.path(PROJECT_ROOT, "functions.R"), encoding = "UTF-8")
analysis_start_time <- timestamp_now()

run_analysis <- function() {
  log_msg("Analysis started.")
  install_and_load(required_packages, INSTALL_MISSING_PACKAGES)
  log_msg("R packages loaded.")
  assert_true(file.exists(INPUT_FILE), paste0("Input data file not found: ", INPUT_FILE))
  raw <- read_csv_robust(INPUT_FILE)
  log_msg("CSV imported: ", nrow(raw), " rows and ", ncol(raw), " columns.")

  expected_columns <- c(
    "study_id", "sex", "age_years", "residence", "smoking", "alcohol_use", "occupation",
    "prior_diabetes_hospitalization", "marital_status", "insulin_use", "living_arrangement",
    "health_insurance", "insurance_type", "education", "monthly_income_group", "monthly_income_original",
    "diabetes_duration_group", "complication_count_reported", "diabetic_nephropathy",
    "cardiovascular_cerebrovascular", "peripheral_neuropathy", "diabetic_eye_complication", "diabetic_foot",
    paste0("med", 1:4), paste0("diet", 1:4), paste0("exercise", 1:4), paste0("monitor", 1:4), paste0("followup", 1:4),
    paste0("RH", 1:3), paste0("AH", 1:3), paste0("TP", 1:3), paste0("OC", 1:3), paste0("SP", 1:3), paste0("RP", 1:2),
    "age_group", "representativeness_mean", "availability_mean", "time_preference_mean", "overconfidence_mean",
    "social_preference_mean", "risk_preference_mean", "framing_category", "medication_adherence", "diet_adherence",
    "exercise_adherence", "monitoring_adherence", "followup_adherence", "adherence_total", "adherence_good_ge40",
    "listed_complication_count", "any_complication", "complication_count_discordant", "qc_high_similarity_pair"
  )
  assert_columns(raw, expected_columns)
  for (v in expected_columns) raw[[v]] <- suppressWarnings(as.numeric(raw[[v]]))
  assert_true(all(vapply(raw[, expected_columns, drop = FALSE], is.numeric, logical(1))), "Some expected numeric columns could not be converted.")

  adherence_items <- c(paste0("med", 1:4), paste0("diet", 1:4), paste0("exercise", 1:4), paste0("monitor", 1:4), paste0("followup", 1:4))
  be_items <- c(paste0("RH", 1:3), paste0("AH", 1:3), paste0("TP", 1:3), paste0("OC", 1:3), paste0("SP", 1:3), paste0("RP", 1:2))

  # Data quality control
  age_group_check <- ifelse(raw$age_years < 45, 1, ifelse(raw$age_years < 60, 2, 3))
  duplicate_rows <- sum(duplicated(raw))
  # IMPORTANT: keep observed values in a list before formatting.
  # A plain c(...) vector would coerce logical TRUE/FALSE values to 1/0 because
  # numeric values are present, which makes valid checks appear to fail.
  format_qc_value <- function(x) {
    if (is.logical(x) && length(x) == 1) return(ifelse(isTRUE(x), "TRUE", "FALSE"))
    if (is.numeric(x) && length(x) == 1) return(format(x, scientific = FALSE, trim = TRUE))
    as.character(x)
  }

  qc_names <- c(
    "Final sample size", "Number of columns", "Duplicate study IDs", "Duplicate rows", "Missing cells",
    "Minimum adherence total", "Maximum adherence total", "Events: adherence_total >=40", "Non-events: adherence_total <40",
    "Any complication: yes", "Any complication: no", "Framing category 0", "Framing category 1", "Framing category 2",
    "Adherence total equals sum of 20 items", "Medication subscale sum check", "Diet subscale sum check", "Exercise subscale sum check",
    "Monitoring subscale sum check", "Follow-up subscale sum check", "Age-group derivation check", "BE mean score derivation check",
    "Complication-count discordance flags", "High-similarity QC flags"
  )

  qc_observed <- list(
    nrow(raw), ncol(raw), sum(duplicated(raw$study_id)), duplicate_rows, sum(is.na(raw)),
    min(raw$adherence_total), max(raw$adherence_total), sum(raw$adherence_good_ge40 == 1), sum(raw$adherence_good_ge40 == 0),
    sum(raw$any_complication == 1), sum(raw$any_complication == 0), sum(raw$framing_category == 0), sum(raw$framing_category == 1), sum(raw$framing_category == 2),
    nearly_equal(rowSums(raw[, adherence_items]), raw$adherence_total),
    nearly_equal(rowSums(raw[, paste0("med", 1:4)]), raw$medication_adherence),
    nearly_equal(rowSums(raw[, paste0("diet", 1:4)]), raw$diet_adherence),
    nearly_equal(rowSums(raw[, paste0("exercise", 1:4)]), raw$exercise_adherence),
    nearly_equal(rowSums(raw[, paste0("monitor", 1:4)]), raw$monitoring_adherence),
    nearly_equal(rowSums(raw[, paste0("followup", 1:4)]), raw$followup_adherence),
    nearly_equal(age_group_check, raw$age_group),
    all(c(
      nearly_equal(rowMeans(raw[, paste0("RH", 1:3)]), raw$representativeness_mean),
      nearly_equal(rowMeans(raw[, paste0("AH", 1:3)]), raw$availability_mean),
      nearly_equal(rowMeans(raw[, paste0("TP", 1:3)]), raw$time_preference_mean),
      nearly_equal(rowMeans(raw[, paste0("OC", 1:3)]), raw$overconfidence_mean),
      nearly_equal(rowMeans(raw[, paste0("SP", 1:3)]), raw$social_preference_mean),
      nearly_equal(rowMeans(raw[, paste0("RP", 1:2)]), raw$risk_preference_mean)
    )),
    sum(raw$complication_count_discordant == 1), sum(raw$qc_high_similarity_pair == 1)
  )

  data_checks <- data.frame(
    check = qc_names,
    observed = vapply(qc_observed, format_qc_value, character(1)),
    expected_or_note = c("232", "79", "0", "0", "0", ">=20", "<=60", "152", "80", "116", "116", "112", "56", "64", rep("TRUE", 8), "Audit flag only", "QC flag only"),
    stringsAsFactors = FALSE
  )
  # Determine QC status one row at a time.
  # Do NOT use vectorized ifelse() with as.numeric() here: vectorized evaluation
  # attempts numeric conversion for non-numeric rows such as TRUE/FALSE and
  # audit notes, producing coercion warnings and possible NA values.
  qc_status_one <- function(check_name, observed_value, expected_value) {
    if (check_name %in% c("Complication-count discordance flags", "High-similarity QC flags")) {
      return("INFO")
    }
    if (check_name == "Minimum adherence total") {
      return(ifelse(!is.na(suppressWarnings(as.numeric(observed_value))) &&
                      suppressWarnings(as.numeric(observed_value)) >= 20, "PASS", "FAIL"))
    }
    if (check_name == "Maximum adherence total") {
      return(ifelse(!is.na(suppressWarnings(as.numeric(observed_value))) &&
                      suppressWarnings(as.numeric(observed_value)) <= 60, "PASS", "FAIL"))
    }
    if (identical(as.character(observed_value), as.character(expected_value))) {
      return("PASS")
    }
    "FAIL"
  }
  data_checks$status <- mapply(qc_status_one, data_checks$check, data_checks$observed,
                               data_checks$expected_or_note, USE.NAMES = FALSE)
  # Export QC results immediately, including if a check fails.
  write_csv_utf8(data_checks, file.path(TABLE_DIR, "data_quality_checks.csv"))
  assert_true(all(data_checks$status[data_checks$status != "INFO"] == "PASS"), "One or more data quality checks failed; review output/tables/data_quality_checks.csv")
  log_msg("Data quality control passed.")

  # Variable coding and reference groups
  dat <- raw
  dat$sex_f <- factor(dat$sex, levels = c(1, 2), labels = c("Male", "Female"))
  dat$residence_f <- factor(dat$residence, levels = c(1, 2), labels = c("Rural", "Urban"))
  dat$smoking_f <- factor(dat$smoking, levels = c(1, 2, 3), labels = c("Current smoker", "Never smoker", "Former smoker"))
  dat$alcohol_f <- factor(dat$alcohol_use, levels = c(1, 2, 3), labels = c("Current drinker", "Non-drinker", "Former drinker"))
  dat$occupation_f <- factor(dat$occupation, levels = c(2, 1, 3, 4), labels = c("Retired", "Employed", "Self-employed", "Unemployed"))
  dat$prior_hosp_f <- factor(dat$prior_diabetes_hospitalization, levels = c(1, 2), labels = c("Yes", "No"))
  dat$marital_f <- factor(dat$marital_status, levels = c(1, 2, 3, 4), labels = c("Unmarried", "Married", "Divorced", "Widowed"))
  dat$insulin_f <- factor(dat$insulin_use, levels = c(1, 2), labels = c("Yes", "No"))
  dat$living_f <- factor(dat$living_arrangement, levels = c(1, 2, 3, 4), labels = c("Living alone", "Living with spouse and children", "Living with spouse", "Living with children"))
  dat$insurance_type_f <- factor(dat$insurance_type, levels = c(2, 3, 4, 5), labels = c("Urban employee basic medical insurance", "Urban and rural resident basic medical insurance", "Commercial medical insurance", "Other medical insurance"))
  dat$age_group_f <- factor(dat$age_group, levels = c(1, 2, 3), labels = c("<45 years", "45-59 years", ">=60 years"))
  dat$education_f <- factor(dat$education, levels = c(1, 2, 3, 4), labels = c("Primary school or below", "Junior high school", "High school/technical secondary school", "College or above"))
  dat$income_f <- factor(dat$monthly_income_group, levels = c(1, 2, 3, 4), labels = c("<3000", "3000-4999", "5000-7999", ">=8000"))
  dat$duration_f <- factor(dat$diabetes_duration_group, levels = c(1, 2, 3, 4, 5), labels = c("<=5 years", "6-10 years", "11-15 years", "16-20 years", ">20 years"))
  dat$complication_f <- factor(dat$any_complication, levels = c(0, 1), labels = c("No", "Yes"))
  dat$age_group_trend <- as.numeric(dat$age_group)
  dat$education_trend <- as.numeric(dat$education)
  dat$income_trend <- as.numeric(dat$monthly_income_group)
  dat$duration_trend <- as.numeric(dat$diabetes_duration_group)

  coding_table <- read.csv(file.path(PROJECT_ROOT, "variable_coding.csv"), stringsAsFactors = FALSE, check.names = FALSE)

  # Table 1 and Table 3 descriptive statistics
  continuous_labels <- c(age_years = "Age, years", adherence_total = "Treatment adherence total", medication_adherence = "Medication adherence",
                         diet_adherence = "Dietary adherence", exercise_adherence = "Exercise adherence", monitoring_adherence = "Self-monitoring adherence",
                         followup_adherence = "Follow-up adherence", representativeness_mean = "Representativeness bias", availability_mean = "Availability bias",
                         overconfidence_mean = "Overconfidence", time_preference_mean = "Time preference", social_preference_mean = "Social preference", risk_preference_mean = "Risk preference")
  theoretical_ranges <- c(adherence_total = "20-60", medication_adherence = "4-12", diet_adherence = "4-12", exercise_adherence = "4-12",
                          monitoring_adherence = "4-12", followup_adherence = "4-12", representativeness_mean = "1-5", availability_mean = "1-5",
                          overconfidence_mean = "1-5", time_preference_mean = "1-5", social_preference_mean = "1-5", risk_preference_mean = "1-5")
  table3a <- continuous_summary(dat, c("adherence_total", "medication_adherence", "diet_adherence", "exercise_adherence", "monitoring_adherence", "followup_adherence"), continuous_labels, theoretical_ranges)
  table3b <- continuous_summary(dat, c("representativeness_mean", "availability_mean", "overconfidence_mean", "time_preference_mean", "social_preference_mean", "risk_preference_mean"), continuous_labels, theoretical_ranges)
  table3c <- rbind(
    categorical_summary(dat, "adherence_good_ge40", c("0" = "<40", "1" = ">=40"), "Treatment adherence category"),
    categorical_summary(dat, "framing_category", c("0" = "No apparent framing effect (RP1=RP2)", "1" = "Greater risk preference under gain frame (RP1>RP2)", "2" = "Greater risk preference under loss frame (RP1<RP2)"), "Framing-effect category")
  )
  table1_cont <- continuous_summary(dat, c("age_years"), continuous_labels, NULL)
  table1_cat <- do.call(rbind, list(
    categorical_summary(dat, "age_group", c("1" = "<45 years", "2" = "45-59 years", "3" = ">=60 years"), "Age group"),
    categorical_summary(dat, "sex", c("1" = "Male", "2" = "Female"), "Sex"),
    categorical_summary(dat, "residence", c("1" = "Rural", "2" = "Urban"), "Residence"),
    categorical_summary(dat, "marital_status", c("1" = "Unmarried", "2" = "Married", "3" = "Divorced", "4" = "Widowed"), "Marital status"),
    categorical_summary(dat, "prior_diabetes_hospitalization", c("1" = "Yes", "2" = "No"), "Prior diabetes hospitalization"),
    categorical_summary(dat, "health_insurance", c("1" = "Yes"), "Health insurance"),
    categorical_summary(dat, "insurance_type", c("2" = "Urban employee basic medical insurance", "3" = "Urban and rural resident basic medical insurance", "4" = "Commercial medical insurance", "5" = "Other medical insurance"), "Insurance type"),
    categorical_summary(dat, "education", c("1" = "Primary school or below", "2" = "Junior high school", "3" = "High school/technical secondary school", "4" = "College or above"), "Education"),
    categorical_summary(dat, "monthly_income_group", c("1" = "<3000", "2" = "3000-4999", "3" = "5000-7999", "4" = ">=8000"), "Personal monthly income, CNY"),
    categorical_summary(dat, "occupation", c("1" = "Employed", "2" = "Retired", "3" = "Self-employed", "4" = "Unemployed"), "Occupation"),
    categorical_summary(dat, "living_arrangement", c("1" = "Living alone", "2" = "Living with spouse and children", "3" = "Living with spouse", "4" = "Living with children"), "Living arrangement"),
    categorical_summary(dat, "smoking", c("1" = "Current smoker", "2" = "Never smoker", "3" = "Former smoker"), "Smoking status"),
    categorical_summary(dat, "alcohol_use", c("1" = "Current drinker", "2" = "Non-drinker", "3" = "Former drinker"), "Alcohol use"),
    categorical_summary(dat, "diabetes_duration_group", c("1" = "<=5 years", "2" = "6-10 years", "3" = "11-15 years", "4" = "16-20 years", "5" = ">20 years"), "Diabetes duration"),
    categorical_summary(dat, "insulin_use", c("1" = "Yes", "2" = "No"), "Insulin therapy"),
    categorical_summary(dat, "any_complication", c("0" = "No", "1" = "Yes"), "Any diabetes complication"),
    categorical_summary(dat, "diabetic_nephropathy", c("1" = "Yes", "2" = "No"), "Diabetic nephropathy"),
    categorical_summary(dat, "cardiovascular_cerebrovascular", c("1" = "Yes", "2" = "No"), "Cardiovascular/cerebrovascular complication"),
    categorical_summary(dat, "peripheral_neuropathy", c("1" = "Yes", "2" = "No"), "Peripheral neuropathy"),
    categorical_summary(dat, "diabetic_eye_complication", c("1" = "Yes", "2" = "No"), "Diabetic eye complication"),
    categorical_summary(dat, "diabetic_foot", c("1" = "Yes", "2" = "No"), "Diabetic foot")
  ))
  table1_full_audit <- rbind(
    data.frame(section = "Continuous", variable = table1_cont$variable, category = "", n = table1_cont$N, percent = NA_real_, n_percent = table1_cont$mean_SD, raw_variable = table1_cont$raw_variable, stringsAsFactors = FALSE),
    data.frame(section = "Categorical", variable = table1_cat$variable, category = table1_cat$category, n = table1_cat$n, percent = table1_cat$percent, n_percent = table1_cat$n_percent, raw_variable = table1_cat$raw_variable, stringsAsFactors = FALSE)
  )
  # Manuscript Table 1 is intentionally streamlined.
  # Prior hospitalization, health insurance, insurance type, and individual complication
  # items are retained only in the full audit version.
  table1_manuscript_variables <- c(
    "age_years", "age_group", "sex", "residence", "marital_status", "education",
    "monthly_income_group", "occupation", "living_arrangement", "smoking", "alcohol_use",
    "diabetes_duration_group", "insulin_use", "any_complication"
  )
  table1_manuscript <- table1_full_audit[table1_full_audit$raw_variable %in% table1_manuscript_variables,
                                         c("section", "variable", "category", "n", "percent", "n_percent"),
                                         drop = FALSE]
  # Keep the original Table 1 filename as the manuscript-ready version for downstream compatibility.
  table1 <- table1_manuscript
  log_msg("Descriptive tables completed.")

  # Reliability, KMO, Bartlett, and WLSMV CFA
  alpha_results <- do.call(rbind, list(
    cronbach_row(dat, paste0("RH", 1:3), "Behavioral economics questionnaire", "Representativeness bias"),
    cronbach_row(dat, paste0("AH", 1:3), "Behavioral economics questionnaire", "Availability bias"),
    cronbach_row(dat, paste0("OC", 1:3), "Behavioral economics questionnaire", "Overconfidence"),
    cronbach_row(dat, paste0("TP", 1:3), "Behavioral economics questionnaire", "Time preference"),
    cronbach_row(dat, paste0("SP", 1:3), "Behavioral economics questionnaire", "Social preference"),
    cronbach_row(dat, paste0("RP", 1:2), "Behavioral economics questionnaire", "Risk preference"),
    cronbach_row(dat, adherence_items, "Treatment adherence questionnaire", "Total scale"),
    cronbach_row(dat, paste0("med", 1:4), "Treatment adherence questionnaire", "Medication adherence"),
    cronbach_row(dat, paste0("diet", 1:4), "Treatment adherence questionnaire", "Dietary adherence"),
    cronbach_row(dat, paste0("exercise", 1:4), "Treatment adherence questionnaire", "Exercise adherence"),
    cronbach_row(dat, paste0("monitor", 1:4), "Treatment adherence questionnaire", "Self-monitoring adherence"),
    cronbach_row(dat, paste0("followup", 1:4), "Treatment adherence questionnaire", "Follow-up adherence")
  ))
  kmo_bartlett_results <- rbind(kmo_bartlett_row(dat, be_items, "Behavioral economics questionnaire"), kmo_bartlett_row(dat, adherence_items, "Treatment adherence questionnaire"))
  log_msg("Reliability, KMO, and Bartlett tests completed.")

  be_model <- '
    Representativeness =~ RH1 + RH2 + RH3
    Availability       =~ AH1 + AH2 + AH3
    Overconfidence     =~ OC1 + OC2 + OC3
    TimePreference     =~ TP1 + TP2 + TP3
    SocialPreference   =~ SP1 + SP2 + SP3
    RiskPreference     =~ RP1 + RP2
  '
  adherence_5f_model <- '
    Medication =~ med1 + med2 + med3 + med4
    Diet       =~ diet1 + diet2 + diet3 + diet4
    Exercise   =~ exercise1 + exercise2 + exercise3 + exercise4
    Monitoring =~ monitor1 + monitor2 + monitor3 + monitor4
    Followup   =~ followup1 + followup2 + followup3 + followup4
  '
  adherence_1f_model <- paste("GeneralAdherence =~", paste(adherence_items, collapse = " + "))
  log_msg("Fitting behavioral economics six-factor WLSMV CFA.")
  fit_be <- lavaan::cfa(be_model, data = dat, ordered = be_items, estimator = "WLSMV", parameterization = "theta", std.lv = TRUE, missing = "pairwise")
  log_msg("Fitting adherence five-factor WLSMV CFA.")
  fit_adh_5f <- lavaan::cfa(adherence_5f_model, data = dat, ordered = adherence_items, estimator = "WLSMV", parameterization = "theta", std.lv = TRUE, missing = "pairwise")
  log_msg("Fitting adherence one-factor WLSMV CFA.")
  fit_adh_1f <- lavaan::cfa(adherence_1f_model, data = dat, ordered = adherence_items, estimator = "WLSMV", parameterization = "theta", std.lv = TRUE, missing = "pairwise")
  assert_true(isTRUE(lavaan::lavInspect(fit_be, "converged")), "Behavioral economics CFA did not converge.")
  assert_true(isTRUE(lavaan::lavInspect(fit_adh_5f, "converged")), "Adherence five-factor CFA did not converge.")
  assert_true(isTRUE(lavaan::lavInspect(fit_adh_1f, "converged")), "Adherence one-factor CFA did not converge.")
  cfa_fit_results <- rbind(extract_cfa_fit(fit_be, "Behavioral economics questionnaire", "Six-factor model"),
                           extract_cfa_fit(fit_adh_5f, "Treatment adherence questionnaire", "Five-factor model"),
                           extract_cfa_fit(fit_adh_1f, "Treatment adherence questionnaire", "One-factor model"))
  cfa_load_be <- extract_cfa_loadings(fit_be, "Behavioral economics questionnaire", "Six-factor model")
  cfa_load_adh5 <- extract_cfa_loadings(fit_adh_5f, "Treatment adherence questionnaire", "Five-factor model")
  cfa_load_adh1 <- extract_cfa_loadings(fit_adh_1f, "Treatment adherence questionnaire", "One-factor model")
  cfa_loadings <- rbind(cfa_load_be, cfa_load_adh5, cfa_load_adh1)
  ave_cr_results <- rbind(extract_ave_cr(cfa_load_be), extract_ave_cr(cfa_load_adh5))
  table2a <- rbind(
    data.frame(section = "Cronbach alpha", alpha_results, KMO = NA_real_, Bartlett_chisq = NA_real_, Bartlett_df = NA_real_, Bartlett_p_value = NA_real_, AVE = NA_real_, CR = NA_real_, stringsAsFactors = FALSE),
    data.frame(section = "KMO and Bartlett", instrument = kmo_bartlett_results$instrument, dimension = "Total item set", number_of_items = kmo_bartlett_results$number_of_items,
               raw_alpha = NA_real_, standardized_alpha = NA_real_, average_interitem_correlation = NA_real_, KMO = kmo_bartlett_results$KMO,
               Bartlett_chisq = kmo_bartlett_results$Bartlett_chisq, Bartlett_df = kmo_bartlett_results$Bartlett_df,
               Bartlett_p_value = kmo_bartlett_results$Bartlett_p_value, AVE = NA_real_, CR = NA_real_, stringsAsFactors = FALSE),
    data.frame(section = "AVE and CR", instrument = ave_cr_results$instrument, dimension = ave_cr_results$factor, number_of_items = ave_cr_results$number_of_items,
               raw_alpha = NA_real_, standardized_alpha = NA_real_, average_interitem_correlation = NA_real_, KMO = NA_real_, Bartlett_chisq = NA_real_, Bartlett_df = NA_real_, Bartlett_p_value = NA_real_,
               AVE = ave_cr_results$AVE, CR = ave_cr_results$CR, stringsAsFactors = FALSE)
  )
  table2a$loading_min <- NA_real_
  table2a$loading_max <- NA_real_
  ave_rows <- which(table2a$section == "AVE and CR")
  table2a$loading_min[ave_rows] <- ave_cr_results$loading_min
  table2a$loading_max[ave_rows] <- ave_cr_results$loading_max
  capture_to_file(summary(fit_be, fit.measures = TRUE, standardized = TRUE), file.path(LOG_DIR, "CFA_BE_six_factor_summary.txt"))
  capture_to_file(summary(fit_adh_5f, fit.measures = TRUE, standardized = TRUE), file.path(LOG_DIR, "CFA_adherence_five_factor_summary.txt"))
  capture_to_file(summary(fit_adh_1f, fit.measures = TRUE, standardized = TRUE), file.path(LOG_DIR, "CFA_adherence_one_factor_summary.txt"))
  saveRDS(fit_be, file.path(MODEL_DIR, "CFA_BE_six_factor_fit.rds"))
  saveRDS(fit_adh_5f, file.path(MODEL_DIR, "CFA_adherence_five_factor_fit.rds"))
  saveRDS(fit_adh_1f, file.path(MODEL_DIR, "CFA_adherence_one_factor_fit.rds"))
  log_msg("WLSMV CFA completed.")

  # Welch univariable analysis
  univariable_results <- do.call(rbind, list(
    univariable_result(dat, "sex", dat$sex_f, "Sex", FALSE),
    univariable_result(dat, "age_group", dat$age_group_f, "Age group", TRUE),
    univariable_result(dat, "residence", dat$residence_f, "Residence", TRUE),
    univariable_result(dat, "smoking", dat$smoking_f, "Smoking status", TRUE),
    univariable_result(dat, "alcohol_use", dat$alcohol_f, "Alcohol use", TRUE),
    univariable_result(dat, "occupation", factor(dat$occupation, levels = c(1, 2, 3, 4), labels = c("Employed", "Retired", "Self-employed", "Unemployed")), "Occupation", TRUE),
    univariable_result(dat, "prior_diabetes_hospitalization", dat$prior_hosp_f, "Prior diabetes hospitalization", FALSE),
    univariable_result(dat, "marital_status", dat$marital_f, "Marital status", FALSE),
    univariable_result(dat, "insulin_use", dat$insulin_f, "Insulin therapy", TRUE),
    univariable_result(dat, "living_arrangement", dat$living_f, "Living arrangement", TRUE),
    univariable_result(dat, "education", dat$education_f, "Education", TRUE),
    univariable_result(dat, "monthly_income_group", dat$income_f, "Personal monthly income", TRUE),
    univariable_result(dat, "diabetes_duration_group", dat$duration_f, "Diabetes duration", TRUE),
    univariable_result(dat, "any_complication", dat$complication_f, "Diabetes complications", FALSE)
  ))
  s1_overall <- univariable_results[univariable_results$test != "", c("variable", "raw_variable", "test", "statistic", "numerator_df", "denominator_df", "p_value", "p_display", "selected_p_lt_0_05", "included_in_fixed_main_model")]
  log_msg("Welch univariable analysis completed.")

  # Pearson correlation matrix
  correlation_vars <- c("adherence_total", "representativeness_mean", "availability_mean", "overconfidence_mean", "time_preference_mean", "social_preference_mean", "risk_preference_mean")
  corr <- correlation_with_p(dat, correlation_vars)
  corr_r_df <- matrix_to_df(corr$r, "variable")
  corr_p_df <- matrix_to_df(corr$p, "variable")
  corr_symmetry_check <- data.frame(check = c("r matrix symmetric", "p matrix symmetric", "diagonal of r equals 1"),
                                    status = c(isTRUE(all.equal(corr$r, t(corr$r))), isTRUE(all.equal(corr$p, t(corr$p))), all(diag(corr$r) == 1)), stringsAsFactors = FALSE)
  log_msg("Pearson correlation analysis completed.")

  # Regression models
  main_formula <- adherence_total ~ representativeness_mean + availability_mean + overconfidence_mean + time_preference_mean + social_preference_mean + risk_preference_mean + age_group_trend + residence_f + smoking_f + alcohol_f + occupation_f + living_f + education_trend + income_trend + insulin_f + duration_trend
  repro_formula <- update(main_formula, . ~ . - residence_f)
  logistic_formula <- adherence_good_ge40 ~ representativeness_mean + availability_mean + overconfidence_mean + time_preference_mean + social_preference_mean + risk_preference_mean + age_group_trend + sex_f + insulin_f
  full_category_formula <- adherence_total ~ representativeness_mean + availability_mean + overconfidence_mean + time_preference_mean + social_preference_mean + risk_preference_mean + age_group_f + residence_f + smoking_f + alcohol_f + occupation_f + living_f + education_f + income_f + insulin_f + duration_f

  term_display <- c("(Intercept)" = "Intercept", "representativeness_mean" = "Representativeness bias", "availability_mean" = "Availability bias", "overconfidence_mean" = "Overconfidence", "time_preference_mean" = "Time preference", "social_preference_mean" = "Social preference", "risk_preference_mean" = "Risk preference", "age_group_trend" = "Age group", "residence_fUrban" = "Urban residence", "smoking_fNever smoker" = "Never smoker", "smoking_fFormer smoker" = "Former smoker", "alcohol_fNon-drinker" = "Non-drinker", "alcohol_fFormer drinker" = "Former drinker", "occupation_fEmployed" = "Employed", "occupation_fSelf-employed" = "Self-employed", "occupation_fUnemployed" = "Unemployed", "living_fLiving with spouse and children" = "Living with spouse and children", "living_fLiving with spouse" = "Living with spouse", "living_fLiving with children" = "Living with children", "education_trend" = "Education", "income_trend" = "Personal monthly income", "insulin_fNo" = "No insulin therapy", "duration_trend" = "Diabetes duration", "sex_fFemale" = "Female")
  term_unit <- c("(Intercept)" = "", "representativeness_mean" = "Per 1-point increase in mean score (1-5)", "availability_mean" = "Per 1-point increase in mean score (1-5)", "overconfidence_mean" = "Per 1-point increase in mean score (1-5)", "time_preference_mean" = "Per 1-point increase in mean score (1-5)", "social_preference_mean" = "Per 1-point increase in mean score (1-5)", "risk_preference_mean" = "Per 1-point increase in mean score (1-5)", "age_group_trend" = "Per one-category increase (1-3)", "residence_fUrban" = "Reference: rural residence", "smoking_fNever smoker" = "Reference: current smoker", "smoking_fFormer smoker" = "Reference: current smoker", "alcohol_fNon-drinker" = "Reference: current drinker", "alcohol_fFormer drinker" = "Reference: current drinker", "occupation_fEmployed" = "Reference: retired", "occupation_fSelf-employed" = "Reference: retired", "occupation_fUnemployed" = "Reference: retired", "living_fLiving with spouse and children" = "Reference: living alone", "living_fLiving with spouse" = "Reference: living alone", "living_fLiving with children" = "Reference: living alone", "education_trend" = "Per one-category increase (1-4)", "income_trend" = "Per one-category increase (1-4)", "insulin_fNo" = "Reference: insulin therapy", "duration_trend" = "Per one-category increase (1-5)", "sex_fFemale" = "Reference: male")
  term_display <- c(term_display,
    "age_group_f45-59 years" = "Age group: 45-59 years", "age_group_f>=60 years" = "Age group: >=60 years",
    "education_fJunior high school" = "Education: junior high school",
    "education_fHigh school/technical secondary school" = "Education: high school/technical secondary school",
    "education_fCollege or above" = "Education: college or above",
    "income_f3000-4999" = "Income: 3000-4999 CNY", "income_f5000-7999" = "Income: 5000-7999 CNY", "income_f>=8000" = "Income: >=8000 CNY",
    "duration_f6-10 years" = "Diabetes duration: 6-10 years", "duration_f11-15 years" = "Diabetes duration: 11-15 years",
    "duration_f16-20 years" = "Diabetes duration: 16-20 years", "duration_f>20 years" = "Diabetes duration: >20 years")
  term_unit <- c(term_unit,
    "age_group_f45-59 years" = "Reference: <45 years", "age_group_f>=60 years" = "Reference: <45 years",
    "education_fJunior high school" = "Reference: primary school or below",
    "education_fHigh school/technical secondary school" = "Reference: primary school or below",
    "education_fCollege or above" = "Reference: primary school or below",
    "income_f3000-4999" = "Reference: <3000 CNY", "income_f5000-7999" = "Reference: <3000 CNY", "income_f>=8000" = "Reference: <3000 CNY",
    "duration_f6-10 years" = "Reference: <=5 years", "duration_f11-15 years" = "Reference: <=5 years",
    "duration_f16-20 years" = "Reference: <=5 years", "duration_f>20 years" = "Reference: <=5 years")

  log_msg("Fitting primary 22-parameter linear model including residence.")
  lm_main <- lm(main_formula, data = dat)
  table4 <- extract_linear_results(lm_main, term_display, term_unit)
  table4_fit <- extract_linear_stats(lm_main, 22, "Primary linear model including residence")
  table4_vif <- data.frame(term = names(coefficient_vif(lm_main)), VIF = as.numeric(coefficient_vif(lm_main)), stringsAsFactors = FALSE)
  table4_vif$variable <- ifelse(table4_vif$term %in% names(term_display), unname(term_display[table4_vif$term]), table4_vif$term)
  hc3_main <- hc3_table(lm_main, term_display, term_unit)
  plot_lm_diagnostics(lm_main, DIAG_DIR)
  bp <- lmtest::bptest(lm_main)
  reset <- lmtest::resettest(lm_main, power = 2:3, type = "fitted")
  infl <- influence.measures(lm_main)
  influential_cases <- data.frame(study_id = dat$study_id, cooks_distance = cooks.distance(lm_main), leverage = hatvalues(lm_main), standardized_residual = rstandard(lm_main), studentized_residual = rstudent(lm_main), influence_flag = apply(infl$is.inf, 1, any), stringsAsFactors = FALSE)
  main_diagnostics <- data.frame(
    diagnostic = c("N", "Residual df", "Durbin-Watson statistic", "Maximum coefficient-level VIF", "Condition number", "Maximum absolute standardized residual", "Maximum absolute studentized residual", "Maximum Cook's distance", "Maximum leverage", "Cases with Cook's distance > 4/N", "Cases with |standardized residual| > 3", "Breusch-Pagan statistic", "Breusch-Pagan df", "Breusch-Pagan p value", "Ramsey RESET statistic", "Ramsey RESET numerator df", "Ramsey RESET denominator df", "Ramsey RESET p value"),
    value = c(nobs(lm_main), df.residual(lm_main), durbin_watson_stat(lm_main), max(coefficient_vif(lm_main)), model_condition_number(lm_main), max(abs(rstandard(lm_main))), max(abs(rstudent(lm_main))), max(cooks.distance(lm_main)), max(hatvalues(lm_main)), sum(cooks.distance(lm_main) > 4 / nobs(lm_main)), sum(abs(rstandard(lm_main)) > 3), unname(bp$statistic), unname(bp$parameter), bp$p.value, unname(reset$statistic), unname(reset$parameter[1]), unname(reset$parameter[2]), reset$p.value),
    stringsAsFactors = FALSE
  )
  capture_to_file(summary(lm_main), file.path(LOG_DIR, "linear_main_22_parameter_summary.txt"))
  saveRDS(lm_main, file.path(MODEL_DIR, "linear_main_22_parameter_model.rds"))

  log_msg("Fitting full-category sensitivity model for ordinal covariates.")
  lm_full_category <- lm(full_category_formula, data = dat)
  sensitivity_full_category <- extract_linear_results(lm_full_category, term_display, term_unit)
  sensitivity_full_category_fit <- extract_linear_stats(lm_full_category, 30, "Sensitivity linear model with ordinal covariates entered as full categorical factors")
  sensitivity_full_category_vif <- data.frame(term = names(coefficient_vif(lm_full_category)), VIF = as.numeric(coefficient_vif(lm_full_category)), stringsAsFactors = FALSE)
  sensitivity_full_category_vif$variable <- ifelse(sensitivity_full_category_vif$term %in% names(term_display), unname(term_display[sensitivity_full_category_vif$term]), sensitivity_full_category_vif$term)
  factor_level_display <- c(term_display,
    "age_group_f" = "Age group", "education_f" = "Education", "income_f" = "Personal monthly income", "duration_f" = "Diabetes duration",
    "residence_f" = "Residence", "smoking_f" = "Smoking status", "alcohol_f" = "Alcohol use", "occupation_f" = "Occupation",
    "living_f" = "Living arrangement", "insulin_f" = "Insulin therapy")
  sensitivity_full_category_gvif <- factor_level_gvif(lm_full_category, factor_level_display)
  trend_vs_full_category <- compare_linear_models(lm_main, lm_full_category, "Primary ordinal trend model", "Full categorical covariate sensitivity model")
  full_category_reset <- reset_result_row(lm_full_category, "Full categorical covariate sensitivity model")
  core_be_coefficients_comparison <- compare_core_behavioral_coefficients(table4, sensitivity_full_category)
  capture_to_file(summary(lm_full_category), file.path(LOG_DIR, "linear_full_category_ordinal_covariate_sensitivity_summary.txt"))
  saveRDS(lm_full_category, file.path(MODEL_DIR, "linear_full_category_ordinal_covariate_sensitivity_model.rds"))

  log_msg("Fitting 21-parameter reproduction model excluding residence.")
  lm_repro <- lm(repro_formula, data = dat)
  s3 <- extract_linear_results(lm_repro, term_display, term_unit)
  s3_fit <- extract_linear_stats(lm_repro, 21, "Reproduction linear model excluding residence")
  s3_vif <- data.frame(term = names(coefficient_vif(lm_repro)), VIF = as.numeric(coefficient_vif(lm_repro)), stringsAsFactors = FALSE)
  s3_vif$variable <- ifelse(s3_vif$term %in% names(term_display), unname(term_display[s3_vif$term]), s3_vif$term)
  capture_to_file(summary(lm_repro), file.path(LOG_DIR, "linear_reproduction_21_parameter_summary.txt"))
  saveRDS(lm_repro, file.path(MODEL_DIR, "linear_reproduction_21_parameter_model.rds"))

  log_msg("Fitting logistic sensitivity analysis.")
  glm_warnings <- character(0)
  glm_s4 <- withCallingHandlers(glm(logistic_formula, data = dat, family = binomial(link = "logit")), warning = function(w) { glm_warnings <<- c(glm_warnings, conditionMessage(w)); invokeRestart("muffleWarning") })
  s4 <- extract_logistic_results(glm_s4, term_display, term_unit)
  s4_fit <- extract_logistic_stats(glm_s4, dat, 9, "Logistic sensitivity analysis: adherence score >=40")
  s4_fit$warnings <- ifelse(length(glm_warnings) == 0, "None", paste(unique(glm_warnings), collapse = " | "))
  s4_separation <- s4_separation_check(glm_s4, dat)
  s4_fit$separation_screen <- ifelse(any(s4_separation$separation_signal == "Yes"), "Review for possible complete/quasi-complete separation or numerical instability", "No obvious complete/quasi-complete separation signal")
  capture_to_file(summary(glm_s4), file.path(LOG_DIR, "logistic_sensitivity_summary.txt"))
  saveRDS(glm_s4, file.path(MODEL_DIR, "logistic_sensitivity_model.rds"))
  log_msg("Regression models completed.")

  # Frozen-result validation.
  # Expected values are used only as independent acceptance checks; no calculated
  # statistical result is overwritten or rounded to force agreement.
  validation_row <- function(check, observed, expected, tolerance = NA_real_, criterion = NULL) {
    if (!is.null(criterion)) {
      status <- ifelse(isTRUE(criterion), "PASS", "FAIL")
    } else if (is.na(tolerance)) {
      status <- ifelse(identical(as.character(observed), as.character(expected)), "PASS", "FAIL")
    } else {
      status <- ifelse(isTRUE(abs(as.numeric(observed) - as.numeric(expected)) <= tolerance), "PASS", "FAIL")
    }
    data.frame(
      check = check,
      observed = as.character(observed),
      expected = as.character(expected),
      tolerance = ifelse(is.na(tolerance), "", as.character(tolerance)),
      status = status,
      stringsAsFactors = FALSE
    )
  }
  coefficient_direction_row <- function(term, expected_direction, expected_significance = NULL) {
    row <- table4[table4$term == term, , drop = FALSE]
    is_significant <- isTRUE(row$p_value[1] < 0.05)
    sign_ok <- switch(expected_direction,
      negative = isTRUE(row$B[1] < 0),
      positive = isTRUE(row$B[1] > 0),
      `not statistically significant` = !is_significant,
      FALSE
    )
    sig_ok <- if (is.null(expected_significance)) TRUE else identical(is_significant, expected_significance)
    validation_row(
      paste0("Primary linear model direction: ", row$variable[1]),
      paste0("B=", signif(row$B[1], 8), "; p=", signif(row$p_value[1], 8)),
      paste0(expected_direction, if (!is.null(expected_significance) && expected_significance) "; p<0.05" else ""),
      criterion = sign_ok && sig_ok
    )
  }
  validation_checks <- do.call(rbind, list(
    validation_row("N", nrow(dat), 232),
    validation_row("Table 4 R-squared", table4_fit$R_squared[1], 0.775, tolerance = 0.001),
    validation_row("Table 4 adjusted R-squared", table4_fit$adjusted_R_squared[1], 0.752, tolerance = 0.001),
    validation_row("Table 4 F statistic", table4_fit$F_statistic[1], 32.754, tolerance = 0.001),
    validation_row("Table 4 numerator df", table4_fit$numerator_df[1], 22),
    validation_row("Table 4 denominator df", table4_fit$denominator_df[1], 209),
    validation_row("S3 F statistic", s3_fit$F_statistic[1], 34.409, tolerance = 0.001),
    validation_row("S3 numerator df", s3_fit$numerator_df[1], 21),
    validation_row("S3 denominator df", s3_fit$denominator_df[1], 210),
    validation_row("S4 events", s4_fit$events[1], 152),
    validation_row("S4 non-events", s4_fit$non_events[1], 80),
    validation_row("S4 McFadden R-squared", s4_fit$McFadden_R_squared[1], 0.701, tolerance = 0.001),
    validation_row("S4 LR chi-square", s4_fit$likelihood_ratio_chisq[1], 209.629, tolerance = 0.001),
    validation_row("S4 AIC", s4_fit$AIC[1], 109.274, tolerance = 0.001),
    coefficient_direction_row("availability_mean", "negative", TRUE),
    coefficient_direction_row("overconfidence_mean", "negative", TRUE),
    coefficient_direction_row("time_preference_mean", "negative", TRUE),
    coefficient_direction_row("social_preference_mean", "positive", TRUE),
    coefficient_direction_row("representativeness_mean", "not statistically significant"),
    coefficient_direction_row("risk_preference_mean", "not statistically significant")
  ))
  validation_lines <- c(
    "T2DM R project validation report",
    paste0("Generated: ", timestamp_now()),
    "",
    paste0("Overall status: ", ifelse(all(validation_checks$status == "PASS"), "PASS", "FAIL")),
    "",
    apply(validation_checks, 1, function(x) {
      paste0(x[["status"]], " | ", x[["check"]], " | observed=", x[["observed"]],
             " | expected=", x[["expected"]], ifelse(nzchar(x[["tolerance"]]), paste0(" | tolerance=", x[["tolerance"]]), ""))
    })
  )
  write_text(validation_lines, file.path(PROJECT_ROOT, "validation_report.txt"))
  write_csv_utf8(validation_checks, file.path(TABLE_DIR, "validation_checks.csv"))
  assert_true(all(validation_checks$status == "PASS"), "Frozen-result validation failed; review validation_report.txt and output/tables/validation_checks.csv")
  log_msg("Frozen-result validation passed.")

  # Manifest, software versions, and checksums
  package_versions <- data.frame(package = required_packages, version = vapply(required_packages, function(pkg) as.character(utils::packageVersion(pkg)), character(1)), stringsAsFactors = FALSE)
  manifest <- data.frame(
    field = c("Analysis start time", "Analysis end time", "Project root", "Input file", "Input MD5", "Rows", "Columns", "Total missing cells", "Primary outcome", "Sensitivity event", "Sensitivity events/non-events", "Primary linear model", "Reproduction linear model", "Logistic sensitivity model", "Full categorical sensitivity model", "CFA estimator", "CFA fit index policy", "CFA ordered indicators", "Post hoc item deletion", "Model objects directory", "Diagnostics directory", "Tables directory", "Validation report", "Excel workbook policy"),
    value = c(analysis_start_time, timestamp_now(), ".", relative_path(INPUT_FILE), unname(tools::md5sum(INPUT_FILE)), nrow(dat), ncol(raw), sum(is.na(raw)), "adherence_total (continuous score, theoretical range 20-60)", "adherence_good_ge40: 1 if adherence_total >=40", paste0(sum(dat$adherence_good_ge40 == 1), "/", sum(dat$adherence_good_ge40 == 0)), paste(deparse(main_formula), collapse = " "), paste(deparse(repro_formula), collapse = " "), paste(deparse(logistic_formula), collapse = " "), paste(deparse(full_category_formula), collapse = " "), "WLSMV", "Scaled WLSMV indices only: chisq.scaled, df.scaled, pvalue.scaled, cfi.scaled, tli.scaled, rmsea.scaled, rmsea CI scaled, and SRMR", "All questionnaire items treated as ordered categorical indicators", "None", relative_path(MODEL_DIR), relative_path(DIAG_DIR), relative_path(TABLE_DIR), "validation_report.txt; output/tables/validation_checks.csv", "Clean table-only workbook written with writexl; no inserted images, comments, drawing, or vmlDrawing parts"),
    stringsAsFactors = FALSE
  )

  # Required CSV exports
  write_csv_utf8(table1, file.path(TABLE_DIR, "Table1_participant_characteristics.csv"))
  write_csv_utf8(table1_manuscript, file.path(TABLE_DIR, "Table1_participant_characteristics_manuscript.csv"))
  write_csv_utf8(table1_full_audit, file.path(TABLE_DIR, "Table1_participant_characteristics_full_audit.csv"))
  write_csv_utf8(table2a, file.path(TABLE_DIR, "Table2A_reliability_AVE_CR.csv"))
  write_csv_utf8(cfa_fit_results, file.path(TABLE_DIR, "Table2B_WLSMV_CFA_fit.csv"))
  write_csv_utf8(cfa_load_be, file.path(TABLE_DIR, "CFA_BE_standardized_loadings.csv"))
  write_csv_utf8(cfa_load_adh5, file.path(TABLE_DIR, "CFA_adherence_five_factor_standardized_loadings.csv"))
  write_csv_utf8(cfa_load_adh1, file.path(TABLE_DIR, "CFA_adherence_one_factor_standardized_loadings.csv"))
  write_csv_utf8(table3a, file.path(TABLE_DIR, "Table3A_adherence_descriptives.csv"))
  write_csv_utf8(table3b, file.path(TABLE_DIR, "Table3B_behavioral_economics_descriptives.csv"))
  write_csv_utf8(table3c, file.path(TABLE_DIR, "Table3C_classification_counts.csv"))
  write_csv_utf8(univariable_results, file.path(TABLE_DIR, "Supplementary_Table_S1_Welch_univariable.csv"))
  write_csv_utf8(s1_overall, file.path(TABLE_DIR, "S1_Welch_overall_tests.csv"))
  write_csv_utf8(corr_r_df, file.path(TABLE_DIR, "Supplementary_Table_S2_Pearson_r.csv"))
  write_csv_utf8(corr_p_df, file.path(TABLE_DIR, "Supplementary_Table_S2_Pearson_P.csv"))
  write_csv_utf8(table4, file.path(TABLE_DIR, "Table4_main_22_parameter_linear_model.csv"))
  write_csv_utf8(table4_fit, file.path(TABLE_DIR, "Table4_main_model_fit.csv"))
  write_csv_utf8(table4_vif, file.path(TABLE_DIR, "Table4_VIF.csv"))
  write_csv_utf8(hc3_main, file.path(TABLE_DIR, "Main_model_HC3_robust_SE.csv"))
  write_csv_utf8(main_diagnostics, file.path(TABLE_DIR, "Main_model_diagnostics.csv"))
  write_csv_utf8(sensitivity_full_category, file.path(TABLE_DIR, "Sensitivity_ordinal_covariates_full_category_model.csv"))
  write_csv_utf8(sensitivity_full_category_fit, file.path(TABLE_DIR, "Sensitivity_ordinal_covariates_full_category_model_fit.csv"))
  write_csv_utf8(sensitivity_full_category_vif, file.path(TABLE_DIR, "Sensitivity_ordinal_covariates_full_category_VIF.csv"))
  write_csv_utf8(sensitivity_full_category_gvif, file.path(TABLE_DIR, "Sensitivity_ordinal_covariates_full_category_GVIF.csv"))
  write_csv_utf8(trend_vs_full_category, file.path(TABLE_DIR, "Sensitivity_trend_vs_full_category_model_comparison.csv"))
  write_csv_utf8(full_category_reset, file.path(TABLE_DIR, "Sensitivity_full_category_model_RESET.csv"))
  write_csv_utf8(core_be_coefficients_comparison, file.path(TABLE_DIR, "Sensitivity_core_BE_coefficients_comparison.csv"))
  write_csv_utf8(influential_cases, file.path(TABLE_DIR, "Main_model_influential_observations.csv"))
  write_csv_utf8(s3, file.path(TABLE_DIR, "Supplementary_Table_S3_21_parameter_model.csv"))
  write_csv_utf8(s3_fit, file.path(TABLE_DIR, "S3_replication_model_fit.csv"))
  write_csv_utf8(s3_vif, file.path(TABLE_DIR, "S3_VIF.csv"))
  write_csv_utf8(s4, file.path(TABLE_DIR, "Supplementary_Table_S4_logistic_sensitivity.csv"))
  write_csv_utf8(s4_fit, file.path(TABLE_DIR, "S4_logistic_model_fit.csv"))
  write_csv_utf8(s4_separation, file.path(TABLE_DIR, "S4_complete_or_quasi_complete_separation_check.csv"))
  write_csv_utf8(data_checks, file.path(TABLE_DIR, "data_quality_checks.csv"))
  write_csv_utf8(corr_symmetry_check, file.path(TABLE_DIR, "S2_correlation_symmetry_check.csv"))
  write_csv_utf8(package_versions, file.path(PROJECT_ROOT, "package_versions.csv"))
  write_csv_utf8(manifest, file.path(PROJECT_ROOT, "analysis_manifest.txt"))
  capture_to_file(sessionInfo(), file.path(PROJECT_ROOT, "sessionInfo.txt"))

  # Complete clean Excel workbook.
  # The workbook is exported with writexl as tables only. Diagnostic plots remain
  # in output/diagnostics and are not inserted into Excel. This avoids stale
  # drawing/vmlDrawing relationships and other invalid workbook relationships.
  sheet_list <- list(
    manifest = manifest, package_versions = package_versions, data_checks = data_checks, validation = validation_checks, variable_coding = coding_table,
    Table1 = table1, Table1_manuscript = table1_manuscript, Table1_full_audit = table1_full_audit,
    Table2A = table2a, Table2B_CFA_fit = cfa_fit_results, CFA_BE_loadings = cfa_load_be,
    CFA_Adh5_loadings = cfa_load_adh5, CFA_Adh1_loadings = cfa_load_adh1,
    Table3A = table3a, Table3B = table3b, Table3C = table3c,
    S1_Welch = univariable_results, S1_overall = s1_overall, S2_Pearson_r = corr_r_df, S2_Pearson_P = corr_p_df,
    Table4 = table4, Table4_fit = table4_fit, Table4_VIF = table4_vif, HC3_SE = hc3_main, Main_diagnostics = main_diagnostics,
    Sens_fullcat = sensitivity_full_category, Sens_fullcat_fit = sensitivity_full_category_fit,
    Sens_fullcat_VIF = sensitivity_full_category_vif, Sens_fullcat_GVIF = sensitivity_full_category_gvif,
    Sens_model_compare = trend_vs_full_category, Sens_fullcat_RESET = full_category_reset, Sens_BE_compare = core_be_coefficients_comparison,
    Influential_cases = influential_cases, S3 = s3, S3_fit = s3_fit, S3_VIF = s3_vif, S4 = s4, S4_fit = s4_fit, S4_sep_check = s4_separation
  )
  sheet_list <- lapply(sheet_list, safe_df)
  names(sheet_list) <- make_unique_sheet_names(names(sheet_list))
  writexl::write_xlsx(sheet_list, path = file.path(OUTPUT_DIR, "T2DM_R_complete_results.xlsx"))

  # Checksums for reproducibility. Public checksum paths are project-relative.
  checksum_files <- c(INPUT_FILE, file.path(PROJECT_ROOT, "T2DM_full_analysis.R"), file.path(PROJECT_ROOT, "functions.R"), file.path(PROJECT_ROOT, "validation_report.txt"), file.path(OUTPUT_DIR, "T2DM_R_complete_results.xlsx"))
  checksum_files <- checksum_files[file.exists(checksum_files)]
  checks <- data.frame(file = vapply(checksum_files, relative_path, character(1)), md5 = unname(tools::md5sum(checksum_files)), stringsAsFactors = FALSE)
  writeLines(paste(checks$file, checks$md5, sep = "	"), con = file.path(PROJECT_ROOT, "checksums.txt"), useBytes = TRUE)

  log_msg("All required CSV files, diagnostics, model objects, logs, and Excel workbook exported.")
  log_msg("Analysis completed successfully.")
  invisible(list(data = dat, fit_be = fit_be, fit_adherence_5f = fit_adh_5f, fit_adherence_1f = fit_adh_1f, lm_main = lm_main, lm_repro = lm_repro, glm_s4 = glm_s4, lm_full_category = lm_full_category))
}

analysis_result <- tryCatch(
  run_analysis(),
  error = function(e) {
    error_lines <- c(
      paste0("Time: ", timestamp_now()),
      paste0("Error: ", conditionMessage(e)),
      "", "sys.calls():", capture.output(print(sys.calls())),
      "", "traceback():", capture.output(traceback()),
      "", "sessionInfo():", capture.output(sessionInfo())
    )
    write_text(error_lines, file.path(LOG_DIR, "ERROR_traceback_and_sessionInfo.txt"))
    log_msg("Analysis failed: ", conditionMessage(e))
    stop(e)
  }
)
