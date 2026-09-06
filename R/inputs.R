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
