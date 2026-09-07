is_numeric_vector <- function(x) {
  is.numeric(x) && is.null(dim(x)) && !is.object(x)
}

check_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be TRUE or FALSE.", name), call. = FALSE)
  }
}

check_number <- function(x, name, positive = FALSE) {
  if (!is_numeric_vector(x) || length(x) != 1L || !is.finite(x) ||
      (positive && x <= 0)) {
    stop(sprintf("`%s` must be one %sfinite number.", name,
                 if (positive) "positive " else ""), call. = FALSE)
  }
}

as_model_list <- function(mods) {
  if (is.matrix(mods) && is.numeric(mods)) {
    model_names <- colnames(mods)
    mods <- lapply(seq_len(ncol(mods)), function(i) mods[, i])
    names(mods) <- model_names
  } else if (is_numeric_vector(mods)) {
    mods <- list(Model = mods)
  }
  if (!is.list(mods) || !length(mods)) {
    stop("`mods` must contain at least one numeric prediction vector; use a vector, list, matrix, or data frame.", call. = FALSE)
  }
  if (!all(vapply(mods, is_numeric_vector, logical(1)))) {
    stop("Every element of `mods` must be a numeric vector (one model per column).", call. = FALSE)
  }
  nm <- names(mods)
  if (is.null(nm)) nm <- rep("", length(mods))
  if (anyNA(nm) || anyDuplicated(nm[nzchar(nm)])) {
    stop("Model names must be unique and not NA.", call. = FALSE)
  }
  for (i in which(!nzchar(nm))) {
    candidate <- paste0("Model ", i)
    while (candidate %in% nm) candidate <- paste0(candidate, "_")
    nm[i] <- candidate
  }
  names(mods) <- nm
  as.list(mods)
}

prepare_models <- function(mods, obs, na.rm) {
  check_flag(na.rm, "na.rm")
  mods <- as_model_list(mods)
  if (!is_numeric_vector(obs) || !length(obs)) {
    stop("`obs` must be a non-empty numeric vector.", call. = FALSE)
  }
  if (any(vapply(mods, length, integer(1)) != length(obs))) {
    stop("Every prediction vector must have the same length as `obs`.", call. = FALSE)
  }
  if (any(is.infinite(obs)) || any(vapply(mods, function(x) any(is.infinite(x)), logical(1)))) {
    stop("Observations and predictions must not contain infinite values.", call. = FALSE)
  }
  mods
}

paired_values <- function(pred, obs, na.rm) {
  keep <- if (na.rm) stats::complete.cases(pred, obs) else rep(TRUE, length(obs))
  list(pred = pred[keep], obs = obs[keep])
}

validate_plot_sizes <- function(label, point_size, label_size) {
  check_flag(label, "label")
  check_number(point_size, "point_size", TRUE)
  check_number(label_size, "label_size", TRUE)
}

validate_diagram_arguments <- function(colorval, colorval.name, x.axis_begin,
                                       x.axis_end, y.axis_end, by, label,
                                       n_models) {
  if (!is.null(colorval) && (!is_numeric_vector(colorval) ||
      length(colorval) != n_models || any(!is.finite(colorval)))) {
    stop("`colorval` must be finite numeric values, with one value per model in model order.", call. = FALSE)
  }
  if (!is.null(colorval.name) && (!is.character(colorval.name) ||
      length(colorval.name) != 1L || is.na(colorval.name))) {
    stop("`colorval.name` must be NULL or one character string.", call. = FALSE)
  }
  check_number(x.axis_begin, "x.axis_begin / axis_begin")
  check_number(x.axis_end, "x.axis_end / axis_end")
  check_number(y.axis_end, "y.axis_end", TRUE)
  if (x.axis_begin >= x.axis_end) {
    stop("Axis limits require x.axis_begin < x.axis_end (axis_begin < axis_end).", call. = FALSE)
  }
  check_number(by, "by", TRUE)
  if (!is.finite((x.axis_end - x.axis_begin) / by) ||
      max((x.axis_end - x.axis_begin) / by, y.axis_end / by) > 10000) {
    stop("`by` would generate more than 10000 ticks; increase it or reduce the axis range.", call. = FALSE)
  }
  check_flag(label, "label")
}

# Scaling before squaring avoids overflow for large but finite observations.
root_mean_square <- function(x) {
  scale <- max(abs(x))
  if (scale == 0) return(0)
  scale * sqrt(mean((x / scale)^2))
}

pair_moments <- function(pred, obs) {
  scale <- max(abs(c(pred, obs)))
  if (scale == 0) scale <- 1
  p <- pred / scale
  o <- obs / scale
  pc <- p - mean(p)
  oc <- o - mean(o)
  sp <- root_mean_square(pc)
  so <- root_mean_square(oc)
  if ((sp == 0 && any(pred != pred[1])) ||
      (so == 0 && any(obs != obs[1]))) {
    stop("Input dynamic range exceeds numeric precision; rescale the inputs.", call. = FALSE)
  }
  r <- if (sp > 0 && so > 0) mean((pc / sp) * (oc / so)) else 0
  list(scale = scale, p = p, o = o, sp = sp, so = so,
       r = max(-1, min(1, r)))
}

# Prepare equally sized numeric vectors and, by default, remove incomplete
# observations as complete pairs. `NULL` denotes incomplete data with
# na.rm = FALSE, for which scalar metrics are undefined.
prepare_metric_vectors <- function(..., na.rm = TRUE) {
  values <- list(...)
  check_flag(na.rm, "na.rm")
  if (!length(values) || !all(vapply(values, is_numeric_vector, logical(1)))) {
    stop("All inputs must be numeric vectors.", call. = FALSE)
  }
  lengths <- vapply(values, length, integer(1))
  if (!lengths[1L] || any(lengths != lengths[1L])) {
    stop("All inputs must be non-empty numeric vectors of the same length.",
         call. = FALSE)
  }
  if (any(vapply(values, function(x) any(is.infinite(x)), logical(1)))) {
    stop("Inputs must not contain infinite values.", call. = FALSE)
  }
  keep <- stats::complete.cases(values)
  if (!na.rm && any(!keep)) return(NULL)
  if (na.rm) values <- lapply(values, `[`, keep)
  values
}

check_probability <- function(x, name) {
  if (!is_numeric_vector(x) || length(x) != 1L || !is.finite(x) ||
      x <= 0 || x >= 1) {
    stop(sprintf("`%s` must be one finite number strictly between 0 and 1.", name),
         call. = FALSE)
  }
}

check_positive_vector <- function(x, name) {
  if (any(!is.finite(x)) || any(x <= 0)) {
    stop(sprintf("`%s` must contain finite, strictly positive values.", name),
         call. = FALSE)
  }
}

prepare_interval_vectors <- function(obs, lower, upper, na.rm = TRUE) {
  values <- prepare_metric_vectors(obs = obs, lower = lower, upper = upper,
                                   na.rm = na.rm)
  if (is.null(values)) return(NULL)
  if (any(values$lower > values$upper)) {
    stop("`lower` must be less than or equal to `upper` for every complete pair.",
         call. = FALSE)
  }
  values
}

prepare_interval_bounds <- function(lower, upper, na.rm = TRUE) {
  values <- prepare_metric_vectors(lower = lower, upper = upper, na.rm = na.rm)
  if (is.null(values)) return(NULL)
  if (any(values$lower > values$upper)) {
    stop("`lower` must be less than or equal to `upper` for every complete pair.",
         call. = FALSE)
  }
  values
}

prepare_predictive_sd_vectors <- function(obs, pred, predictive_sd,
                                          na.rm = TRUE) {
  if (!is_numeric_vector(predictive_sd)) {
    stop("`predictive_sd` must be a numeric vector.", call. = FALSE)
  }
  if (any(!is.na(predictive_sd) & (!is.finite(predictive_sd) | predictive_sd <= 0))) {
    stop("`predictive_sd` must contain finite, strictly positive values.",
         call. = FALSE)
  }
  prepare_metric_vectors(obs = obs, pred = pred, predictive_sd = predictive_sd,
                         na.rm = na.rm)
}

prepare_distribution_matrix <- function(obs, distribution, na.rm = TRUE,
                                        min_columns = 1L) {
  if (!is_numeric_vector(obs)) stop("`obs` must be a numeric vector.", call. = FALSE)
  if (!(is.matrix(distribution) || is.data.frame(distribution)) ||
      !all(vapply(as.data.frame(distribution), is.numeric, logical(1)))) {
    stop("`distribution` must be a numeric matrix or data frame with one row per observation.", call. = FALSE)
  }
  distribution <- as.matrix(distribution)
  storage.mode(distribution) <- "double"
  if (nrow(distribution) != length(obs)) {
    stop("`obs` and `distribution` must have the same number of rows.", call. = FALSE)
  }
  if (ncol(distribution) < min_columns) {
    stop("`distribution` has too few predictive-distribution values.", call. = FALSE)
  }
  if (any(!is.na(distribution) & !is.finite(distribution)) ||
      any(!is.na(obs) & !is.finite(obs))) {
    stop("Inputs must not contain infinite values.", call. = FALSE)
  }
  keep <- !is.na(obs) & stats::complete.cases(distribution)
  if (!na.rm && !all(keep)) return(NULL)
  list(obs = obs[keep], distribution = distribution[keep, , drop = FALSE])
}

prepare_quantiles <- function(obs, quantiles, levels, na.rm = TRUE) {
  if (!is_numeric_vector(levels) || !length(levels) || any(!is.finite(levels)) ||
      any(levels <= 0 | levels >= 1) || is.unsorted(levels, strictly = TRUE)) {
    stop("`levels` must be strictly increasing finite probabilities between zero and one.", call. = FALSE)
  }
  x <- prepare_distribution_matrix(obs, quantiles, na.rm)
  if (ncol(as.matrix(quantiles)) != length(levels)) {
    stop("`levels` must have one value for every quantile column.", call. = FALSE)
  }
  if (is.null(x)) return(NULL)
  if (nrow(x$distribution) && any(apply(x$distribution, 1, is.unsorted))) {
    stop("Each row of `quantiles` must be non-decreasing across `levels`.", call. = FALSE)
  }
  x
}
