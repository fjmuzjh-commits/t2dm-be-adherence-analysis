
# ==============================================================================
# Utility functions for T2DM_full_analysis.R
# ===============================================================================

timestamp_now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

log_msg <- function(...) {
  msg <- paste0("[", timestamp_now(), "] ", paste(..., collapse = ""))
  cat(msg, "\n")
  if (exists("LOG_FILE", envir = .GlobalEnv)) {
    cat(msg, "\n", file = get("LOG_FILE", envir = .GlobalEnv), append = TRUE)
  }
  invisible(msg)
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

write_text <- function(x, path) {
  ensure_dir(dirname(path))
  writeLines(enc2utf8(as.character(x)), con = path, useBytes = TRUE)
}

capture_to_file <- function(expr, path) {
  write_text(capture.output(expr), path)
}

install_and_load <- function(pkgs, install_missing = TRUE) {
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    if (!install_missing) {
      stop("Missing R packages: ", paste(missing_pkgs, collapse = ", "), call. = FALSE)
    }
    log_msg("Installing missing R packages: ", paste(missing_pkgs, collapse = ", "))
    install.packages(missing_pkgs, dependencies = TRUE)
  }
  invisible(lapply(pkgs, function(pkg) suppressPackageStartupMessages(library(pkg, character.only = TRUE))))
}

read_csv_robust <- function(path) {
  encodings <- c("UTF-8-BOM", "UTF-8", "GB18030")
  last_error <- NULL
  for (enc in encodings) {
    ans <- tryCatch(
      read.csv(path, fileEncoding = enc, check.names = FALSE, stringsAsFactors = FALSE,
               na.strings = c("", "NA", "N/A")),
      error = function(e) { last_error <<- e; NULL }
    )
    if (!is.null(ans)) return(ans)
  }
  stop("CSV import failed: ", conditionMessage(last_error), call. = FALSE)
}

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

assert_columns <- function(data, columns) {
  missing_cols <- setdiff(columns, names(data))
  if (length(missing_cols) > 0) stop("Missing variables: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

nearly_equal <- function(x, y, tolerance = 1e-10) {
  isTRUE(all.equal(as.numeric(x), as.numeric(y), tolerance = tolerance, check.attributes = FALSE))
}

format_p <- function(p) {
  ifelse(is.na(p), NA_character_, ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

mean_sd_text <- function(x, digits = 2) {
  sprintf(paste0("%.", digits, "f ± %.", digits, "f"), mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}

observed_range_text <- function(x, digits = 2) {
  rng <- range(x, na.rm = TRUE)
  sprintf(paste0("%.", digits, "f-%." , digits, "f"), rng[1], rng[2])
}

safe_numeric_df <- function(data, columns, object_name = "data") {
  out <- data[, columns, drop = FALSE]
  bad <- columns[!vapply(out, is.numeric, logical(1))]
  if (length(bad) > 0) stop(object_name, " contains nonnumeric columns: ", paste(bad, collapse = ", "), call. = FALSE)
  out
}

safe_df <- function(x) {
  if (is.null(x)) return(data.frame(Message = "No output", stringsAsFactors = FALSE))
  if (is.matrix(x)) x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!is.data.frame(x)) x <- as.data.frame(x, stringsAsFactors = FALSE)
  names(x) <- make.unique(names(x), sep = "_")
  for (j in seq_along(x)) {
    if (is.factor(x[[j]])) x[[j]] <- as.character(x[[j]])
    if (is.list(x[[j]])) x[[j]] <- vapply(x[[j]], function(z) paste(as.character(z), collapse = "; "), character(1))
    if (is.numeric(x[[j]])) x[[j]][!is.finite(x[[j]])] <- NA_real_
  }
  if (ncol(x) == 0) x <- data.frame(Message = "No columns", stringsAsFactors = FALSE)
  x
}

write_csv_utf8 <- function(data, path) {
  data <- safe_df(data)
  ensure_dir(dirname(path))
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw("\xEF\xBB\xBF"), con)
  write.csv(data, con, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

sanitize_sheet_name <- function(x) {
  x <- gsub("[\\\\/:*?\\[\\]]", "_", x)
  substr(x, 1, 31)
}

add_df_sheet <- function(wb, sheet_name, data) {
  sheet_name <- sanitize_sheet_name(sheet_name)
  if (sheet_name %in% names(wb)) openxlsx::removeWorksheet(wb, sheet_name)
  openxlsx::addWorksheet(wb, sheet_name)
  data <- safe_df(data)
  if (nrow(data) == 0) data <- data.frame(Message = "No rows returned", stringsAsFactors = FALSE)
  openxlsx::writeData(wb, sheet_name, data, startRow = 1, headerStyle = openxlsx::createStyle(
    fontColour = "#FFFFFF", fgFill = "#1F4E78", halign = "center", valign = "center",
    textDecoration = "bold", border = "Bottom"))
  openxlsx::addFilter(wb, sheet_name, rows = 1, cols = seq_len(ncol(data)))
  openxlsx::freezePane(wb, sheet_name, firstActiveRow = 2)
  widths <- vapply(seq_len(ncol(data)), function(j) {
    vals <- c(names(data)[j], as.character(utils::head(data[[j]], 500)))
    vals[is.na(vals)] <- ""
    min(45, max(10, max(nchar(vals, type = "width"), na.rm = TRUE) + 2))
  }, numeric(1))
  openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncol(data)), widths = widths)
  invisible(sheet_name)
}

continuous_summary <- function(data, variables, labels = NULL, theoretical_ranges = NULL) {
  do.call(rbind, lapply(variables, function(v) {
    x <- data[[v]]
    data.frame(
      variable = if (!is.null(labels) && v %in% names(labels)) labels[[v]] else v,
      raw_variable = v,
      N = sum(!is.na(x)), missing = sum(is.na(x)), mean = mean(x, na.rm = TRUE),
      SD = sd(x, na.rm = TRUE), minimum = min(x, na.rm = TRUE), maximum = max(x, na.rm = TRUE),
      theoretical_range = if (!is.null(theoretical_ranges) && v %in% names(theoretical_ranges)) theoretical_ranges[[v]] else "",
      mean_SD = mean_sd_text(x), observed_range = observed_range_text(x),
      stringsAsFactors = FALSE
    )
  }))
}

categorical_summary <- function(data, variable, labels = NULL, variable_label = NULL) {
  x <- data[[variable]]
  if (!is.null(labels)) x <- factor(x, levels = names(labels), labels = unname(labels))
  tab <- table(x, useNA = "ifany")
  data.frame(
    variable = ifelse(is.null(variable_label), variable, variable_label),
    raw_variable = variable,
    category = names(tab),
    n = as.integer(tab),
    percent = 100 * as.integer(tab) / length(x),
    n_percent = sprintf("%d (%.1f)", as.integer(tab), 100 * as.integer(tab) / length(x)),
    stringsAsFactors = FALSE
  )
}

coefficient_vif <- function(model) {
  X <- model.matrix(model)
  X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  if (ncol(X) == 0) return(numeric(0))
  if (ncol(X) == 1) return(setNames(1, colnames(X)))
  vifs <- vapply(seq_len(ncol(X)), function(j) {
    target <- X[, j]
    others <- X[, -j, drop = FALSE]
    aux <- lm(target ~ others)
    r2 <- summary(aux)$r.squared
    if (is.na(r2) || r2 >= 1) Inf else 1 / (1 - r2)
  }, numeric(1))
  names(vifs) <- colnames(X)
  vifs
}

standardized_beta <- function(model) {
  X <- model.matrix(model)
  X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  y <- model.response(model.frame(model))
  b <- coef(model)[-1]
  sx <- apply(X, 2, sd)
  sy <- sd(y)
  b * sx[names(b)] / sy
}

durbin_watson_stat <- function(model) {
  e <- residuals(model)
  sum(diff(e)^2) / sum(e^2)
}

model_condition_number <- function(model) {
  X <- model.matrix(model)
  X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  kappa(scale(X), exact = TRUE)
}

extract_linear_results <- function(model, display_map = NULL, unit_map = NULL) {
  sm <- summary(model)
  coef_mat <- sm$coefficients
  ci <- confint(model)
  vifs <- coefficient_vif(model)
  std_b <- standardized_beta(model)
  terms <- rownames(coef_mat)
  data.frame(
    term = terms,
    variable = ifelse(!is.null(display_map) & terms %in% names(display_map), unname(display_map[terms]), terms),
    category_or_unit = ifelse(!is.null(unit_map) & terms %in% names(unit_map), unname(unit_map[terms]), ""),
    B = coef_mat[, "Estimate"], SE = coef_mat[, "Std. Error"],
    standardized_beta = c(NA_real_, std_b[terms[-1]]),
    CI_lower = ci[terms, 1], CI_upper = ci[terms, 2],
    p_value = coef_mat[, "Pr(>|t|)"], p_display = format_p(coef_mat[, "Pr(>|t|)"]),
    VIF = c(NA_real_, vifs[terms[-1]]),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

extract_linear_stats <- function(model, expected_parameters, model_name) {
  sm <- summary(model)
  f <- sm$fstatistic
  n_parameters <- length(coef(model)) - 1
  assert_true(n_parameters == expected_parameters, paste0(model_name, " should have ", expected_parameters, " predictor parameters; observed ", n_parameters))
  data.frame(
    model = model_name, N = nobs(model), predictor_parameters = n_parameters,
    R_squared = sm$r.squared, adjusted_R_squared = sm$adj.r.squared,
    F_statistic = unname(f[1]), numerator_df = unname(f[2]), denominator_df = unname(f[3]),
    model_p_value = pf(f[1], f[2], f[3], lower.tail = FALSE),
    residual_standard_error = sm$sigma, residual_df = df.residual(model),
    AIC = AIC(model), BIC = BIC(model), log_likelihood = as.numeric(logLik(model)),
    Durbin_Watson = durbin_watson_stat(model),
    maximum_coefficient_VIF = max(coefficient_vif(model), na.rm = TRUE),
    condition_number = model_condition_number(model),
    stringsAsFactors = FALSE
  )
}

extract_logistic_results <- function(model, display_map = NULL, unit_map = NULL) {
  coef_mat <- summary(model)$coefficients
  terms <- rownames(coef_mat)
  z <- qnorm(0.975)
  lower <- coef_mat[, "Estimate"] - z * coef_mat[, "Std. Error"]
  upper <- coef_mat[, "Estimate"] + z * coef_mat[, "Std. Error"]
  vifs <- coefficient_vif(model)
  data.frame(
    term = terms,
    variable = ifelse(!is.null(display_map) & terms %in% names(display_map), unname(display_map[terms]), terms),
    category_or_unit = ifelse(!is.null(unit_map) & terms %in% names(unit_map), unname(unit_map[terms]), ""),
    B_log_odds = coef_mat[, "Estimate"], SE = coef_mat[, "Std. Error"],
    adjusted_OR = exp(coef_mat[, "Estimate"]), CI_lower = exp(lower), CI_upper = exp(upper),
    z_value = coef_mat[, "z value"], p_value = coef_mat[, "Pr(>|z|)"], p_display = format_p(coef_mat[, "Pr(>|z|)"]),
    VIF = c(NA_real_, vifs[terms[-1]]),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

extract_logistic_stats <- function(model, data, expected_parameters, model_name) {
  mf <- model.frame(model)
  y_name <- names(mf)[1]
  null_model <- glm(as.formula(paste(y_name, "~ 1")), data = mf, family = binomial(link = "logit"))
  ll_full <- as.numeric(logLik(model)); ll_null <- as.numeric(logLik(null_model))
  lr <- 2 * (ll_full - ll_null)
  lr_df <- attr(logLik(model), "df") - attr(logLik(null_model), "df")
  probs <- fitted(model)
  n_parameters <- length(coef(model)) - 1
  assert_true(n_parameters == expected_parameters, paste0(model_name, " should have ", expected_parameters, " predictor parameters; observed ", n_parameters))
  data.frame(
    model = model_name, N = nobs(model), events = sum(data$adherence_good_ge40 == 1), non_events = sum(data$adherence_good_ge40 == 0),
    predictor_parameters = n_parameters, converged = isTRUE(model$converged),
    McFadden_R_squared = 1 - ll_full / ll_null,
    likelihood_ratio_chisq = lr, likelihood_ratio_df = lr_df,
    likelihood_ratio_p = pchisq(lr, df = lr_df, lower.tail = FALSE),
    AIC = AIC(model), BIC = BIC(model), log_likelihood = ll_full,
    maximum_coefficient_VIF = max(coefficient_vif(model), na.rm = TRUE),
    minimum_fitted_probability = min(probs), maximum_fitted_probability = max(probs),
    coefficients_abs_gt_10 = sum(abs(coef(model)[-1]) > 10),
    stringsAsFactors = FALSE
  )
}

exact_fit_measure <- function(fit, measure_name) {
  fm <- lavaan::fitMeasures(fit)
  if (measure_name %in% names(fm) && is.finite(fm[[measure_name]])) return(unname(fm[[measure_name]]))
  NA_real_
}

extract_cfa_fit <- function(fit, instrument, model_name) {
  # WLSMV fit indices are reported consistently with scaled statistics only.
  # Do not mix robust and scaled indices across models.
  chisq_scaled <- exact_fit_measure(fit, "chisq.scaled")
  df_scaled <- exact_fit_measure(fit, "df.scaled")
  data.frame(
    instrument = instrument,
    model = model_name,
    estimator = "WLSMV",
    converged = isTRUE(lavaan::lavInspect(fit, "converged")),
    chisq_scaled = chisq_scaled,
    df_scaled = df_scaled,
    chisq_df_scaled = ifelse(is.na(df_scaled) | df_scaled == 0, NA_real_, chisq_scaled / df_scaled),
    pvalue_scaled = exact_fit_measure(fit, "pvalue.scaled"),
    cfi_scaled = exact_fit_measure(fit, "cfi.scaled"),
    tli_scaled = exact_fit_measure(fit, "tli.scaled"),
    rmsea_scaled = exact_fit_measure(fit, "rmsea.scaled"),
    rmsea_ci_lower_scaled = exact_fit_measure(fit, "rmsea.ci.lower.scaled"),
    rmsea_ci_upper_scaled = exact_fit_measure(fit, "rmsea.ci.upper.scaled"),
    srmr = exact_fit_measure(fit, "srmr"),
    stringsAsFactors = FALSE
  )
}

extract_cfa_loadings <- function(fit, instrument, model_name) {
  # Use lavaan standardizedSolution(type = "std.all") from the fitted WLSMV model
  # as the single source for standardized loadings, AVE, and CR.
  pe <- as.data.frame(lavaan::parameterEstimates(fit, standardized = FALSE, ci = TRUE), stringsAsFactors = FALSE)
  load <- pe[pe$op == "=~", , drop = FALSE]
  std <- as.data.frame(lavaan::standardizedSolution(fit, type = "std.all"), stringsAsFactors = FALSE)
  std_load <- std[std$op == "=~", c("lhs", "op", "rhs", "est.std"), drop = FALSE]
  load <- merge(load, std_load, by = c("lhs", "op", "rhs"), all.x = TRUE, sort = FALSE)
  get_num <- function(nm) if (nm %in% names(load)) as.numeric(load[[nm]]) else rep(NA_real_, nrow(load))
  get_chr <- function(nm) if (nm %in% names(load)) as.character(load[[nm]]) else rep(NA_character_, nrow(load))
  data.frame(
    instrument = instrument,
    model = model_name,
    factor = get_chr("lhs"),
    op = get_chr("op"),
    item = get_chr("rhs"),
    estimate = get_num("est"),
    SE = get_num("se"),
    z = get_num("z"),
    p_value = get_num("pvalue"),
    CI_lower = get_num("ci.lower"),
    CI_upper = get_num("ci.upper"),
    standardized_loading = get_num("est.std"),
    stringsAsFactors = FALSE
  )
}

extract_ave_cr <- function(loadings_table) {
  factors <- unique(loadings_table$factor)
  do.call(rbind, lapply(factors, function(f) {
    lambda <- loadings_table$standardized_loading[loadings_table$factor == f]
    error_var <- 1 - lambda^2
    data.frame(
      instrument = unique(loadings_table$instrument[loadings_table$factor == f])[1],
      model = unique(loadings_table$model[loadings_table$factor == f])[1],
      factor = f, number_of_items = length(lambda), loading_min = min(lambda, na.rm = TRUE), loading_max = max(lambda, na.rm = TRUE),
      AVE = sum(lambda^2, na.rm = TRUE) / (sum(lambda^2, na.rm = TRUE) + sum(error_var, na.rm = TRUE)),
      CR = sum(lambda, na.rm = TRUE)^2 / (sum(lambda, na.rm = TRUE)^2 + sum(error_var, na.rm = TRUE)),
      stringsAsFactors = FALSE
    )
  }))
}

cronbach_row <- function(data, items, instrument, dimension) {
  item_data <- safe_numeric_df(data, items, paste(instrument, dimension))
  alpha_obj <- suppressWarnings(psych::alpha(item_data, check.keys = FALSE, warnings = FALSE, na.rm = TRUE))
  data.frame(instrument = instrument, dimension = dimension, number_of_items = length(items),
             raw_alpha = unname(alpha_obj$total$raw_alpha), standardized_alpha = unname(alpha_obj$total$std.alpha),
             average_interitem_correlation = unname(alpha_obj$total$average_r), stringsAsFactors = FALSE)
}

kmo_bartlett_row <- function(data, items, instrument) {
  item_data <- safe_numeric_df(data, items, instrument)
  R <- as.matrix(cor(item_data, use = "pairwise.complete.obs")); storage.mode(R) <- "double"
  kmo <- psych::KMO(R); bart <- psych::cortest.bartlett(R, n = nrow(item_data))
  data.frame(instrument = instrument, number_of_items = length(items), N = nrow(item_data), KMO = unname(kmo$MSA),
             Bartlett_chisq = unname(bart$chisq), Bartlett_df = unname(bart$df), Bartlett_p_value = unname(bart$p.value),
             determinant_correlation_matrix = det(R), stringsAsFactors = FALSE)
}

univariable_result <- function(data, variable, group_factor, variable_label, included_in_fixed_main) {
  y <- data$adherence_total
  group_factor <- droplevels(group_factor)
  summaries <- do.call(rbind, lapply(levels(group_factor), function(level_name) {
    values <- y[group_factor == level_name]
    data.frame(variable = variable_label, raw_variable = variable, category = level_name, n = length(values),
               mean = mean(values), SD = sd(values), mean_SD = mean_sd_text(values), stringsAsFactors = FALSE)
  }))
  if (nlevels(group_factor) == 2) {
    test <- t.test(y ~ group_factor, var.equal = FALSE)
    test_name <- "Welch independent-samples t test"; statistic <- unname(test$statistic)
    numerator_df <- unname(test$parameter); denominator_df <- NA_real_; p_value <- test$p.value
  } else {
    test <- oneway.test(y ~ group_factor, var.equal = FALSE)
    test_name <- "Welch one-way ANOVA"; statistic <- unname(test$statistic)
    numerator_df <- unname(test$parameter[1]); denominator_df <- unname(test$parameter[2]); p_value <- test$p.value
  }
  summaries$test <- ""; summaries$statistic <- NA_real_; summaries$numerator_df <- NA_real_; summaries$denominator_df <- NA_real_
  summaries$p_value <- NA_real_; summaries$p_display <- ""; summaries$selected_p_lt_0_05 <- ""; summaries$included_in_fixed_main_model <- ""
  summaries$test[1] <- test_name; summaries$statistic[1] <- statistic; summaries$numerator_df[1] <- numerator_df; summaries$denominator_df[1] <- denominator_df
  summaries$p_value[1] <- p_value; summaries$p_display[1] <- format_p(p_value); summaries$selected_p_lt_0_05[1] <- ifelse(p_value < 0.05, "Yes", "No")
  summaries$included_in_fixed_main_model[1] <- ifelse(included_in_fixed_main, "Yes", "No")
  summaries
}

correlation_with_p <- function(data, variables) {
  mat <- as.matrix(data[, variables, drop = FALSE]); storage.mode(mat) <- "double"
  r <- cor(mat, use = "pairwise.complete.obs", method = "pearson")
  p <- matrix(NA_real_, length(variables), length(variables), dimnames = list(variables, variables))
  n <- matrix(NA_integer_, length(variables), length(variables), dimnames = list(variables, variables))
  for (i in seq_along(variables)) for (j in seq_along(variables)) {
    keep <- complete.cases(mat[, i], mat[, j]); n[i, j] <- sum(keep)
    p[i, j] <- if (i == j) 0 else cor.test(mat[keep, i], mat[keep, j], method = "pearson")$p.value
  }
  list(r = r, p = p, n = n)
}

matrix_to_df <- function(mat, row_name = "variable") {
  out <- as.data.frame(mat, stringsAsFactors = FALSE, check.names = FALSE)
  out <- cbind(setNames(data.frame(rownames(mat), stringsAsFactors = FALSE), row_name), out)
  rownames(out) <- NULL
  out
}

hc3_table <- function(model, display_map = NULL, unit_map = NULL) {
  vc <- sandwich::vcovHC(model, type = "HC3")
  ct <- lmtest::coeftest(model, vcov. = vc)
  ci_low <- ct[, 1] - qnorm(0.975) * ct[, 2]
  ci_up <- ct[, 1] + qnorm(0.975) * ct[, 2]
  terms <- rownames(ct)
  data.frame(
    term = terms,
    variable = ifelse(!is.null(display_map) & terms %in% names(display_map), unname(display_map[terms]), terms),
    category_or_unit = ifelse(!is.null(unit_map) & terms %in% names(unit_map), unname(unit_map[terms]), ""),
    B = ct[, 1], HC3_SE = ct[, 2], statistic = ct[, 3], p_value = ct[, 4], p_display = format_p(ct[, 4]),
    HC3_CI_lower = ci_low, HC3_CI_upper = ci_up,
    stringsAsFactors = FALSE, row.names = NULL
  )
}

plot_lm_diagnostics <- function(model, diagnostics_dir) {
  ensure_dir(diagnostics_dir)
  write_plot_pair <- function(file_stem, plot_fun) {
    png(file.path(diagnostics_dir, paste0(file_stem, ".png")), width = 1400, height = 1000, res = 160)
    tryCatch(plot_fun(), finally = dev.off())
    pdf(file.path(diagnostics_dir, paste0(file_stem, ".pdf")), width = 8.75, height = 6.25)
    tryCatch(plot_fun(), finally = dev.off())
    invisible(TRUE)
  }
  write_plot_pair("main_residuals_vs_fitted", function() plot(model, which = 1))
  write_plot_pair("main_normal_QQ_plot", function() plot(model, which = 2))
  write_plot_pair("main_scale_location_plot", function() plot(model, which = 3))
  write_plot_pair("main_residuals_vs_leverage", function() plot(model, which = 5))
  write_plot_pair("main_studentized_residuals", function() {
    plot(rstudent(model), ylab = "Studentized residuals", xlab = "Observation", main = "Studentized residuals")
    abline(h = c(-3, 3), lty = 2)
  })
  write_plot_pair("main_cooks_distance", function() {
    plot(cooks.distance(model), type = "h", ylab = "Cook's distance", xlab = "Observation", main = "Cook's distance")
    abline(h = 4 / nobs(model), lty = 2)
  })
  write_plot_pair("main_leverage", function() {
    plot(hatvalues(model), type = "h", ylab = "Leverage", xlab = "Observation", main = "Leverage values")
    abline(h = 2 * length(coef(model)) / nobs(model), lty = 2)
  })
  invisible(TRUE)
}

# Compare nested linear models and summarize model-fit changes.
compare_linear_models <- function(trend_model, categorical_model, trend_name = "Ordinal trend model", categorical_name = "Full categorical covariate model") {
  sm_trend <- summary(trend_model)
  sm_cat <- summary(categorical_model)
  nested_test <- anova(trend_model, categorical_model)
  data.frame(
    comparison = paste(trend_name, "vs", categorical_name),
    trend_predictor_parameters = length(coef(trend_model)) - 1,
    categorical_predictor_parameters = length(coef(categorical_model)) - 1,
    trend_R_squared = sm_trend$r.squared,
    categorical_R_squared = sm_cat$r.squared,
    delta_R_squared = sm_cat$r.squared - sm_trend$r.squared,
    trend_adjusted_R_squared = sm_trend$adj.r.squared,
    categorical_adjusted_R_squared = sm_cat$adj.r.squared,
    delta_adjusted_R_squared = sm_cat$adj.r.squared - sm_trend$adj.r.squared,
    trend_AIC = AIC(trend_model),
    categorical_AIC = AIC(categorical_model),
    delta_AIC_categorical_minus_trend = AIC(categorical_model) - AIC(trend_model),
    trend_BIC = BIC(trend_model),
    categorical_BIC = BIC(categorical_model),
    delta_BIC_categorical_minus_trend = BIC(categorical_model) - BIC(trend_model),
    nested_F_statistic = nested_test$F[2],
    nested_numerator_df = nested_test$Df[2],
    nested_denominator_df = nested_test$Res.Df[2],
    nested_p_value = nested_test$`Pr(>F)`[2],
    nested_p_display = format_p(nested_test$`Pr(>F)`[2]),
    stringsAsFactors = FALSE
  )
}

reset_result_row <- function(model, model_name) {
  reset <- lmtest::resettest(model, power = 2:3, type = "fitted")
  data.frame(
    model = model_name,
    RESET_statistic = unname(reset$statistic),
    numerator_df = unname(reset$parameter[1]),
    denominator_df = unname(reset$parameter[2]),
    p_value = reset$p.value,
    p_display = format_p(reset$p.value),
    stringsAsFactors = FALSE
  )
}

compare_core_behavioral_coefficients <- function(trend_results, categorical_results) {
  core_terms <- c(
    "representativeness_mean", "availability_mean", "overconfidence_mean",
    "time_preference_mean", "social_preference_mean", "risk_preference_mean"
  )
  trend <- trend_results[trend_results$term %in% core_terms, c("term", "variable", "B", "SE", "standardized_beta", "CI_lower", "CI_upper", "p_value"), drop = FALSE]
  cat <- categorical_results[categorical_results$term %in% core_terms, c("term", "B", "SE", "standardized_beta", "CI_lower", "CI_upper", "p_value"), drop = FALSE]
  names(trend) <- c("term", "variable", "trend_B", "trend_SE", "trend_standardized_beta", "trend_CI_lower", "trend_CI_upper", "trend_p_value")
  names(cat) <- c("term", "full_category_B", "full_category_SE", "full_category_standardized_beta", "full_category_CI_lower", "full_category_CI_upper", "full_category_p_value")
  out <- merge(trend, cat, by = "term", all = TRUE, sort = FALSE)
  out$B_difference_full_category_minus_trend <- out$full_category_B - out$trend_B
  out$standardized_beta_difference_full_category_minus_trend <- out$full_category_standardized_beta - out$trend_standardized_beta
  out$trend_p_display <- format_p(out$trend_p_value)
  out$full_category_p_display <- format_p(out$full_category_p_value)
  out
}

s4_separation_check <- function(model, data) {
  probs <- fitted(model)
  coef_values <- coef(model)
  se_values <- summary(model)$coefficients[, "Std. Error"]
  X <- model.matrix(model)
  rank_full <- qr(X)$rank == ncol(X)
  y <- data$adherence_good_ge40
  event <- y == 1
  nonevent <- y == 0

  continuous_vars <- c("representativeness_mean", "availability_mean", "overconfidence_mean",
                       "time_preference_mean", "social_preference_mean", "risk_preference_mean", "age_group_trend")
  continuous_screen <- do.call(rbind, lapply(continuous_vars, function(v) {
    ev <- data[[v]][event]
    ne <- data[[v]][nonevent]
    separated <- (max(ev, na.rm = TRUE) < min(ne, na.rm = TRUE)) || (max(ne, na.rm = TRUE) < min(ev, na.rm = TRUE))
    data.frame(
      screen = "Continuous univariable range screen",
      variable = v,
      level = "",
      events = sum(event),
      non_events = sum(nonevent),
      event_range = paste0(min(ev, na.rm = TRUE), " to ", max(ev, na.rm = TRUE)),
      non_event_range = paste0(min(ne, na.rm = TRUE), " to ", max(ne, na.rm = TRUE)),
      separation_signal = ifelse(separated, "Yes", "No"),
      stringsAsFactors = FALSE
    )
  }))

  categorical_specs <- list(
    age_group = c("1" = "<45 years", "2" = "45-59 years", "3" = ">=60 years"),
    sex = c("1" = "Male", "2" = "Female"),
    insulin_use = c("1" = "Yes", "2" = "No")
  )
  categorical_screen <- do.call(rbind, lapply(names(categorical_specs), function(v) {
    labels <- categorical_specs[[v]]
    do.call(rbind, lapply(names(labels), function(code) {
      keep <- as.character(data[[v]]) == code
      ev <- sum(data$adherence_good_ge40[keep] == 1)
      ne <- sum(data$adherence_good_ge40[keep] == 0)
      data.frame(
        screen = "Categorical zero-cell screen",
        variable = v,
        level = labels[[code]],
        events = ev,
        non_events = ne,
        event_range = "",
        non_event_range = "",
        separation_signal = ifelse(ev == 0 || ne == 0, "Yes", "No"),
        stringsAsFactors = FALSE
      )
    }))
  }))

  global_checks <- data.frame(
    screen = "Global model diagnostics",
    variable = c("glm_converged", "model_matrix_full_rank", "minimum_fitted_probability", "maximum_fitted_probability",
                 "any_fitted_probability_below_1e-8", "any_fitted_probability_above_1_minus_1e-8",
                 "maximum_absolute_coefficient_excluding_intercept", "maximum_coefficient_SE", "any_NA_coefficient", "any_SE_above_10"),
    level = "",
    events = sum(event),
    non_events = sum(nonevent),
    event_range = "",
    non_event_range = "",
    separation_signal = c(
      ifelse(isTRUE(model$converged), "No", "Yes"),
      ifelse(rank_full, "No", "Yes"),
      format(min(probs), scientific = TRUE),
      format(max(probs), scientific = TRUE),
      ifelse(any(probs < 1e-8), "Yes", "No"),
      ifelse(any(probs > 1 - 1e-8), "Yes", "No"),
      format(max(abs(coef_values[-1]), na.rm = TRUE), scientific = FALSE),
      format(max(se_values, na.rm = TRUE), scientific = FALSE),
      ifelse(any(is.na(coef_values)), "Yes", "No"),
      ifelse(any(se_values > 10, na.rm = TRUE), "Yes", "No")
    ),
    stringsAsFactors = FALSE
  )

  out <- rbind(global_checks, categorical_screen, continuous_screen)
  out$interpretation <- ifelse(out$separation_signal == "Yes",
                               "Potential complete/quasi-complete separation or numerical instability signal; review only, do not automatically delete observations.",
                               "No signal on this screen.")
  out
}


# Convert an absolute project path to a project-relative public path.
relative_path <- function(path) {
  p <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(PROJECT_ROOT, winslash = "/", mustWork = FALSE)
  if (identical(p, root)) return(".")
  prefix <- paste0(root, "/")
  if (startsWith(p, prefix)) return(sub(prefix, "", p, fixed = TRUE))
  p
}

# Create Excel-safe, unique sheet names <=31 characters.
make_unique_sheet_names <- function(x) {
  x <- vapply(x, sanitize_sheet_name, character(1))
  out <- character(length(x))
  seen <- character(0)
  for (i in seq_along(x)) {
    base <- substr(x[i], 1, 31)
    candidate <- base
    k <- 1
    while (candidate %in% seen) {
      suffix <- paste0("_", k)
      candidate <- paste0(substr(base, 1, 31 - nchar(suffix)), suffix)
      k <- k + 1
    }
    out[i] <- candidate
    seen <- c(seen, candidate)
  }
  out
}

# Factor-level GVIF based on the coefficient correlation matrix.
# This complements coefficient-level/dummy-level VIF and is especially useful
# for multi-df categorical predictors in the full-category sensitivity model.
factor_level_gvif <- function(model, display_map = NULL) {
  X <- model.matrix(model)
  assign_vec <- attr(X, "assign")
  term_labels <- attr(stats::terms(model), "term.labels")

  keep <- colnames(X) != "(Intercept)"
  X <- X[, keep, drop = FALSE]
  assign_vec <- assign_vec[keep]
  if (ncol(X) == 0) {
    return(data.frame(term = character(0), variable = character(0), Df = integer(0), GVIF = numeric(0), GVIF_adjusted = numeric(0), model_matrix_columns = character(0), stringsAsFactors = FALSE))
  }

  V <- stats::vcov(model)
  non_intercept <- rownames(V) != "(Intercept)"
  V <- V[non_intercept, non_intercept, drop = FALSE]
  V <- V[colnames(X), colnames(X), drop = FALSE]
  R <- stats::cov2cor(V)

  safe_logdet <- function(M) {
    if (ncol(M) == 0) return(0)
    d <- determinant(M, logarithm = TRUE)
    if (!is.finite(as.numeric(d$modulus)) || d$sign <= 0) return(NA_real_)
    as.numeric(d$modulus)
  }

  logdet_R <- safe_logdet(R)
  rows <- lapply(seq_along(term_labels), function(i) {
    cols <- which(assign_vec == i)
    term <- term_labels[i]
    if (length(cols) == 0 || is.na(logdet_R)) {
      gvif <- NA_real_
    } else {
      other_cols <- setdiff(seq_len(ncol(R)), cols)
      logdet_term <- safe_logdet(R[cols, cols, drop = FALSE])
      logdet_other <- safe_logdet(R[other_cols, other_cols, drop = FALSE])
      gvif <- ifelse(any(is.na(c(logdet_term, logdet_other))), NA_real_, exp(logdet_term + logdet_other - logdet_R))
    }
    df <- length(cols)
    data.frame(
      term = term,
      variable = ifelse(!is.null(display_map) && term %in% names(display_map), unname(display_map[term]), term),
      Df = df,
      GVIF = gvif,
      GVIF_adjusted = ifelse(is.na(gvif), NA_real_, gvif^(1 / (2 * df))),
      model_matrix_columns = paste(colnames(X)[cols], collapse = "; "),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
