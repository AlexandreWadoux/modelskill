#' Mean error (ME) of quantitative predictions
#'
#' Mean error (ME; also called bias) is the mean signed difference between
#' observations and predictions, calculated as observation minus prediction.
#'
#' \deqn{\mathrm{ME} = \frac{1}{n}\sum_{i = 1}^{n}(obs_i - pred_i)}
#'
#' An ME of zero indicates no average systematic error. Negative values indicate
#' overprediction on average, whereas positive values indicate underprediction
#' on average. ME has the same units as the response variable. Opposing errors
#' can cancel, so interpret ME together with an unsigned error measure such as
#' [mae()] or [rmse()]. Missing pairs are removed when `na.rm = TRUE`; otherwise
#' the result is `NA` when any pair is missing.
#'
#' @param obs Numeric observation vector.
#' @param pred Numeric prediction vector paired with `obs`.
#' @param na.rm Logical; remove incomplete pairs?
#' @return One numeric value.
#' @references Legates, D. R. and McCabe, G. J. (1999). Evaluating the use of
#'   goodness-of-fit measures in hydrologic and hydroclimatic model validation.
#'   *Water Resources Research*, 35(1), 233-241.
#'   <doi:10.1029/1998WR900018>
#' @examples bias(c(1, 2, 3), c(1, 3, 2))
#' @export
bias <- function(obs, pred, na.rm = TRUE) metric_value(obs, pred, na.rm, "bias")

#' Mean absolute error
#'
#' Mean absolute error (MAE) is the average absolute difference between
#' observations and predictions.
#'
#' \deqn{\mathrm{MAE} = \frac{1}{n}\sum_{i = 1}^{n}|obs_i - pred_i|}
#'
#' MAE is non-negative and has the same units as the response variable. Zero
#' indicates perfect predictions; smaller values indicate smaller typical
#' prediction errors. Unlike mean error (ME), positive and negative errors
#' cannot cancel. MAE gives each error equal weight and is therefore less
#' sensitive to unusually large errors than [rmse()]. Interpret MAE alongside
#' [bias()] to assess both typical error magnitude and systematic
#' over- or underprediction. Missing-value handling follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Willmott, C. J. and Matsuura, K. (2005). Advantages of the mean
#'   absolute error (MAE) over the root mean square error (RMSE) in assessing
#'   average model performance. *Climate Research*, 30, 79-82.
#'   <doi:10.3354/cr030079>
#'
#'   Hodson, T. O. (2022). Root mean square error (RMSE) or mean absolute error
#'   (MAE): When to use them or not. *Geoscientific Model Development*, 15, 5481-5487. <doi:10.5194/gmd-15-5481-2022>
#' @examples mae(1:3, c(1, 3, 2))
#' @export
mae <- function(obs, pred, na.rm = TRUE) metric_value(obs, pred, na.rm, "mae")

#' Mean squared error
#'
#' Mean squared error (MSE) is the mean squared difference between observations
#' and predictions.
#'
#' \deqn{\mathrm{MSE} = \frac{1}{n}\sum_{i = 1}^{n}(obs_i - pred_i)^2}
#'
#' MSE is non-negative and zero indicates perfect predictions. Smaller values
#' indicate better agreement. Squaring gives larger errors disproportionately
#' more influence, making MSE sensitive to large deviations. Its units are the
#' squared units of the response variable, so [rmse()] is usually easier to
#' interpret on the original response scale. Missing-value handling follows
#' [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Hodson, T. O. (2022). Root mean square error (RMSE) or mean
#'   absolute error (MAE): When to use them or not. *Geoscientific Model
#'   Development*, 15, 5481-5487. <doi:10.5194/gmd-15-5481-2022>
#' @examples mse(1:3, c(1, 3, 2))
#' @export
mse <- function(obs, pred, na.rm = TRUE) metric_value(obs, pred, na.rm, "mse")

#' Root mean squared error
#'
#' Root mean squared error (RMSE) is the square root of mean squared error.
#'
#' \deqn{\mathrm{RMSE} = \sqrt{\frac{1}{n}\sum_{i = 1}^{n}(obs_i - pred_i)^2}}
#'
#' RMSE is non-negative, has the same units as the response variable, and is
#' zero for perfect predictions. Smaller values indicate better agreement.
#' Squaring means that a small number of large errors can disproportionately
#' increase RMSE. Therefore, when errors are skewed or asymmetric, interpret
#' RMSE together with [mae()] and inspect the error distribution rather than
#' relying on RMSE alone. Missing-value handling follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Legates, D. R. and McCabe, G. J. (1999). Evaluating the use of
#'   goodness-of-fit measures in hydrologic and hydroclimatic model validation.
#'   *Water Resources Research*, 35(1), 233-241.
#'   <doi:10.1029/1998WR900018>
#'
#'   Armstrong, J. S. (2001). Evaluating forecasting methods. In J. S.
#'   Armstrong (Ed.), *Principles of Forecasting* (pp. 443-472). Springer.
#'   <doi:10.1007/978-0-306-47630-3_20>
#'
#'   Willmott, C. J. and Matsuura, K. (2005). Advantages of the mean absolute
#'   error (MAE) over the root mean square error (RMSE) in assessing average
#'   model performance. *Climate Research*, 30, 79-82.
#'   <doi:10.3354/cr030079>
#'
#'   Hodson, T. O. (2022). Root mean square error (RMSE) or mean absolute error
#'   (MAE): When to use them or not. *Geoscientific Model Development*, 15, 5481-5487. <doi:10.5194/gmd-15-5481-2022>
#' @examples rmse(1:3, c(1, 3, 2))
#' @export
rmse <- function(obs, pred, na.rm = TRUE) metric_value(obs, pred, na.rm, "rmse")

#' Normalized root mean squared error
#'
#' Normalized RMSE (NRMSE) divides [rmse()] by the sample standard deviation of
#' observations.
#'
#' \deqn{\mathrm{NRMSE}=
#' \frac{\sqrt{n^{-1}\sum_{i=1}^{n}(obs_i-pred_i)^2}}
#' {\sqrt{(n-1)^{-1}\sum_{i=1}^{n}(obs_i-\bar{obs})^2}}.}
#'
#' NRMSE is unitless and zero is ideal. Smaller values indicate less error
#' relative to the observed variation. A value of one means RMSE equals one
#' observed sample standard deviation. This package uses this normalization to
#' remain consistent with its diagram statistics. It returns `NA` with a
#' warning when fewer than two valid pairs remain or the observations have zero
#' variance. Missing-value handling follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Taylor, K. E. (2001). Summarizing multiple aspects of model
#'   performance in a single diagram. *Journal of Geophysical Research*, 106,
#'   7183-7192. <doi:10.1029/2000JD900719>
#' @examples nrmse(1:3, c(1, 3, 2))
#' @export
nrmse <- function(obs, pred, na.rm = TRUE) metric_value(obs, pred, na.rm, "nrmse")

#' Centered root mean squared error
#'
#' Centered RMSE (cRMSE) is the root mean square difference after removing the
#' mean error from the paired errors.
#'
#' \deqn{\mathrm{cRMSE}=\sqrt{\frac{1}{n}\sum_{i=1}^{n}
#' \left[(obs_i-pred_i)-\frac{1}{n}\sum_{j=1}^{n}(obs_j-pred_j)\right]^2}.}
#'
#' cRMSE is non-negative, has the response units, and zero is ideal. It measures
#' disagreement in pattern and spread independently of a constant mean bias.
#' Interpret it with [bias()] because two models with identical cRMSE can have
#' different systematic errors. Missing-value handling follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Taylor, K. E. (2001). Summarizing multiple aspects of model
#'   performance in a single diagram. *Journal of Geophysical Research*, 106,
#'   7183-7192. <doi:10.1029/2000JD900719>
#' @examples crmse(1:3, c(1, 3, 2))
#' @export
crmse <- function(obs, pred, na.rm = TRUE) metric_value(obs, pred, na.rm, "crmse")

#' Pearson correlation
#'
#' Pearson product-moment correlation between observations and predictions.
#'
#' \deqn{r = \frac{\sum_{i = 1}^{n}(obs_i - \bar{obs})(pred_i - \bar{pred})}
#' {\sqrt{\sum_{i = 1}^{n}(obs_i - \bar{obs})^2\sum_{i = 1}^{n}(pred_i - \bar{pred})^2}}}
#'
#' Correlation ranges from -1 to 1: one indicates a perfect increasing linear
#' association, minus one a perfect decreasing linear association, and zero no
#' linear association. It returns `NA` with a warning when fewer than two valid
#' pairs remain or either vector has zero variance. Correlation is
#' unaffected by additive bias and proportional scaling, so it measures pattern
#' association rather than agreement or prediction accuracy. Interpret it with
#' [bias()], [rmse()], and an agreement measure such as [ccc()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Willmott, C. J. (1984). On the evaluation of model performance
#'   in physical geography. In G. L. Gaile and C. J. Willmott (Eds.), *Spatial
#'   Statistics and Models* (pp. 443-460). D. Reidel.
#'
#'   Legates, D. R. and McCabe, G. J. (1999). Evaluating the use of
#'   goodness-of-fit measures in hydrologic and hydroclimatic model validation.
#'   *Water Resources Research*, 35(1), 233-241.
#'   <doi:10.1029/1998WR900018>
#' @examples correlation(1:3, c(1, 3, 2))
#' @export
correlation <- function(obs, pred, na.rm = TRUE) metric_value(obs, pred, na.rm, "correlation")

#' Squared Pearson correlation
#'
#' Squared Pearson correlation is the square of [correlation()].
#'
#' \deqn{r^2=
#' \frac{\left[\sum_{i=1}^{n}(obs_i-\bar{obs})(pred_i-\bar{pred})\right]^2}
#' {\sum_{i=1}^{n}(obs_i-\bar{obs})^2\sum_{i=1}^{n}(pred_i-\bar{pred})^2}.}
#'
#' It ranges from zero to one and summarizes the strength, but not the sign, of
#' linear association. It describes the dispersion of predictions and
#' observations around their fitted linear relationship rather than their
#' departure from the 1:1 line. Consequently, `r2()` is insensitive to additive
#' bias and proportional scaling: a value of one can occur even when predictions
#' are systematically biased or have a different scale from the observations.
#' It should therefore not be interpreted as a general measure of predictive
#' agreement or accuracy.
#'
#' Do not confuse lowercase `r2()` with [nse()], [mec()], or uppercase [R2()],
#' which are model-efficiency statistics and are sensitive to departures from
#' the line of equality. It returns `NA` with a warning when fewer than two
#' valid pairs remain or either vector has zero variance.
#' @inheritParams bias
#' @return One numeric value.
#' @references Willmott, C. J. (1984). On the evaluation of model performance
#'   in physical geography. In G. L. Gaile and C. J. Willmott (Eds.), *Spatial
#'   Statistics and Models* (pp. 443-460). D. Reidel.
#'
#'   Legates, D. R. and McCabe, G. J. (1999). Evaluating the use of
#'   goodness-of-fit measures in hydrologic and hydroclimatic model validation.
#'   *Water Resources Research*, 35(1), 233-241.
#'   <doi:10.1029/1998WR900018>
#' @examples r2(1:3, c(1, 3, 2))
#' @export
r2 <- function(obs, pred, na.rm = TRUE) metric_value(obs, pred, na.rm, "r2")

#' Nash-Sutcliffe efficiency
#'
#' Nash-Sutcliffe efficiency (NSE; also called the model efficiency coefficient,
#' MEC) compares the prediction squared error with the squared error from using
#' the observed mean as a constant prediction.
#'
#' \deqn{\mathrm{NSE} = 1 - \frac{\sum_{i = 1}^{n}(obs_i - pred_i)^2}
#' {\sum_{i = 1}^{n}(obs_i - \bar{obs})^2}}
#'
#' One is ideal; zero means the predictions are no better than predicting the
#' observed mean; negative values indicate worse performance than that
#' benchmark. NSE returns `NA` with a warning when fewer than two valid pairs
#' remain or the observations have zero variance. It is sensitive to
#' large errors because it uses squared differences. NSE, [mec()], and uppercase
#' `R2()` are identical in this package; they are not lowercase [r2()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Nash, J. E. and Sutcliffe, J. V. (1970). River flow forecasting
#'   through conceptual models part I: A discussion of principles. *Journal of
#'   Hydrology*, 10, 282-290. <doi:10.1016/0022-1694(70)90255-6>
#'
#'   Janssen, P. H. M. and Heuberger, P. S. C. (1995). Calibration of
#'   process-oriented models. *Ecological Modelling*, 83, 55-66.
#'   <doi:10.1016/0304-3800(95)00084-9>
#'
#'   Legates, D. R. and McCabe, G. J. (1999). Evaluating the use of
#'   goodness-of-fit measures in hydrologic and hydroclimatic model validation.
#'   *Water Resources Research*, 35(1), 233-241.
#'   <doi:10.1029/1998WR900018>
#' @examples nse(1:3, c(1, 3, 2))
#' @export
nse <- function(obs, pred, na.rm = TRUE) {
  metric_value(obs, pred, na.rm, "nse", metric = "NSE")
}

#' Model efficiency coefficient
#'
#' Alias for [nse()]. MEC, NSE, and the uppercase R-squared efficiency `R2()`
#' are the same statistic. They must not be confused with lowercase `r2()`,
#' the squared Pearson correlation.
#'
#' \deqn{\mathrm{MEC}=1-\frac{\sum_{i=1}^{n}(obs_i-pred_i)^2}
#' {\sum_{i=1}^{n}(obs_i-\bar{obs})^2}.}
#'
#' Its interpretation and references are given in [nse()]. It returns `NA`
#' with a warning under the same undefined conditions as [nse()].
#' @inheritParams bias
#' @return One numeric value.
#' @examples mec(1:3, c(1, 3, 2))
#' @export
mec <- function(obs, pred, na.rm = TRUE) {
  metric_value(obs, pred, na.rm, "nse", metric = "MEC")
}

#' Model-efficiency R-squared
#'
#' `R2()` is the model-efficiency coefficient and is identical to [nse()] and
#' [mec()] in this package. It compares the squared prediction error with the
#' squared deviation of the observations from their mean:
#'
#' \deqn{
#' R^2 =
#' 1 -
#' \frac{
#' \sum_{i=1}^{n}(obs_i-pred_i)^2
#' }{
#' \sum_{i=1}^{n}(obs_i-\bar{obs})^2
#' }.
#' }
#'
#' The statistic has a direct benchmark interpretation. A value of 1 indicates
#' perfect agreement between observations and predictions. A value of 0 means
#' that predicting the observed mean for every observation performs equally
#' well according to squared error. Negative values indicate that the observed
#' mean provides a better prediction than the model.
#'
#' Uppercase `R2()` must not be confused with lowercase [r2()], which is the
#' squared Pearson correlation coefficient. Squared Pearson correlation
#' measures the strength of linear association and is insensitive to additive
#' and proportional differences between observations and predictions. It can
#' therefore equal one even for strongly biased predictions. In contrast,
#' `R2()` is sensitive to deviations from the line of equality and therefore
#' measures predictive performance rather than linear association alone.
#'
#' Because `R2()` is based on squared errors, individual large prediction
#' errors can have a disproportionate influence on its value. It should
#' therefore generally be interpreted together with complementary measures
#' such as [bias()], [mae()], [rmse()], and [r2()] rather than as a standalone
#' measure of predictive performance.
#'
#' `R2()` returns `NA` with a warning when fewer than two valid observation-
#' prediction pairs remain or when the observations have zero variance.
#' Missing-value handling follows [bias()].
#'
#' @rdname efficiency_r2
#' @inheritParams bias
#' @return One numeric value. The optimum is 1; values may be negative and are
#'   not bounded below.
#'
#' @references
#' Wadoux, A. M. J.-C., Walvoort, D. J. J. and Brus, D. J. (2022).
#' An integrated approach for the evaluation of quantitative soil maps through
#' Taylor and solar diagrams. *Geoderma*, 405, 115332.
#' <doi:10.1016/j.geoderma.2021.115332>
#'
#' Janssen, P. H. M. and Heuberger, P. S. C. (1995). Calibration of
#' process-oriented models. *Ecological Modelling*, 83, 55-66.
#' <doi:10.1016/0304-3800(95)00084-9>
#'
#' Nash, J. E. and Sutcliffe, J. V. (1970). River flow forecasting through
#' conceptual models part I: A discussion of principles. *Journal of
#' Hydrology*, 10, 282-290.
#' <doi:10.1016/0022-1694(70)90255-6>
#'
#' Legates, D. R. and McCabe, G. J. (1999). Evaluating the use of
#' goodness-of-fit measures in hydrologic and hydroclimatic model validation.
#' *Water Resources Research*, 35(1), 233-241.
#' <doi:10.1029/1998WR900018>
#'
#' @seealso [r2()], [nse()], [mec()], [bias()], [mae()], [rmse()]
#'
#' @examples
#' obs <- c(1, 2, 3, 4, 5)
#'
#' # Perfect predictions
#' R2(obs, obs)
#'
#' # Additive bias: r2 remains 1, whereas R2 decreases
#' pred <- obs + 1
#' r2(obs, pred)
#' R2(obs, pred)
#'
#' # Predictions can perform worse than using the observed mean
#' R2(obs, rev(obs))
#'
#' @export
R2 <- function(obs, pred, na.rm = TRUE) {
  metric_value(obs, pred, na.rm, "nse", metric = "R2")
}

#' Prediction-to-observation standard deviation ratio
#'
#' The standard deviation ratio compares the sample standard deviation of
#' predictions with that of observations.
#'
#' \deqn{\mathrm{SD\ ratio}=
#' \sqrt{\frac{\sum_{i=1}^{n}(pred_i-\bar{pred})^2}
#' {\sum_{i=1}^{n}(obs_i-\bar{obs})^2}}.}
#'
#' The ratio is non-negative and one indicates equal variability. Values below
#' one indicate that predictions have less variability than the observations;
#' values above one indicate that predictions have more variability than the
#' observations. It assesses variability, not mean bias or association, and
#' returns `NA` with a warning when fewer than two valid pairs remain or the
#' observations have zero variance. Missing-value handling follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Taylor, K. E. (2001). Summarizing multiple aspects of model
#'   performance in a single diagram. *Journal of Geophysical Research*, 106,
#'   7183-7192. <doi:10.1029/2000JD900719>
#' @examples sd_ratio(1:3, c(1, 3, 2))
#' @export
sd_ratio <- function(obs, pred, na.rm = TRUE) metric_value(obs, pred, na.rm, "sd_ratio")

#' Lin's concordance correlation coefficient
#'
#' Lin's concordance correlation coefficient (CCC; \eqn{\rho_c}) measures
#' agreement between observations and predictions by combining Pearson
#' correlation with differences in location and scale. Unlike Pearson
#' correlation alone, CCC evaluates how closely paired values approach the
#' line of equality.
#'
#' \deqn{
#' \rho_c =
#' \frac{
#' 2\sum_{i=1}^{n}(obs_i-\bar{obs})(pred_i-\bar{pred})
#' }{
#' \sum_{i=1}^{n}(obs_i-\bar{obs})^2+
#' \sum_{i=1}^{n}(pred_i-\bar{pred})^2+
#' n(\bar{obs}-\bar{pred})^2
#' }.
#' }
#'
#' CCC ranges from -1 to 1, with 1 indicating perfect agreement. Values
#' decrease as observations and predictions differ in linear association,
#' mean, or scale. Negative values indicate negative concordance.
#'
#' CCC can also be expressed as
#'
#' \deqn{\rho_c = r C_b,}
#'
#' where \eqn{r} is the Pearson correlation coefficient and \eqn{C_b} is a
#' bias-correction factor that accounts for departures from the line of
#' equality. Consequently, CCC incorporates both association and agreement
#' into a single statistic.
#'
#' When CCC is used to evaluate predictive models, its value is most
#' informative when considered together with complementary statistics.
#' Different combinations of correlation, mean bias, and scale differences
#' can produce similar CCC values, so the coefficient alone does not identify
#' which component is responsible for disagreement between observations and
#' predictions. In addition, CCC depends partly on the variability of the
#' reference observations. Direct comparison of CCC values obtained from
#' substantially different datasets or target populations should therefore
#' be made with caution.
#'
#' For prediction-model evaluation, CCC can usefully be reported alongside
#' measures describing individual aspects of predictive performance, such as
#' [bias()], [mae()], [rmse()], [correlation()], or [R2()]. This allows the
#' overall concordance indicated by CCC to be interpreted together with the
#' magnitude and sources of prediction error.
#'
#' Population variances (divisor \eqn{n}) are used, matching the package
#' convention. The function returns `NA` with a warning when fewer than two
#' valid observation-prediction pairs remain or when the observations have
#' zero variance. It returns zero for constant predictions when observations
#' vary. Missing-value handling follows [bias()].
#'
#' @inheritParams bias
#' @return One numeric value between -1 and 1, with 1 indicating perfect
#'   agreement.
#'
#' @references
#' Lin, L. I.-K. (1989). A concordance correlation coefficient to evaluate
#' reproducibility. *Biometrics*, 45, 255-268.
#' <doi:10.2307/2532051>
#'
#' Wadoux, A. M. J.-C. and Minasny, B. (2024). Some limitations of the
#' concordance correlation coefficient to characterise model accuracy.
#' *Ecological Informatics*, 83, 102820.
#' <doi:10.1016/j.ecoinf.2024.102820>
#'
#' @seealso [correlation()], [bias()], [mae()], [rmse()], [R2()]
#'
#' @examples
#' obs <- c(1, 2, 3, 4, 5)
#'
#' # Perfect agreement
#' ccc(obs, obs)
#'
#' # Systematic bias reduces concordance
#' ccc(obs, obs + 1)
#'
#' # Compare with Pearson correlation
#' correlation(obs, obs + 1)
#' ccc(obs, obs + 1)
#'
#' @export
ccc <- function(obs, pred, na.rm = TRUE) {
  metric_value(obs, pred, na.rm, "ccc")
}

extended_components <- function(obs, pred, na.rm = TRUE) {
  x <- prepare_metric_vectors(obs = obs, pred = pred, na.rm = na.rm)
  if (is.null(x) || !length(x$obs)) return(stats::setNames(rep(NA_real_, 13), c("mdae", "rpd", "rpiq", "sep", "rer", "mape", "mpe", "smape", "msle", "rmsle", "rae", "rrmse", "willmott_d")))
  error <- x$obs - x$pred; root <- sqrt(mean(error^2)); n <- length(error)
  obs_sd <- if (n < 2L) NA_real_ else stats::sd(x$obs)
  obs_iqr <- stats::IQR(x$obs)
  obs_range <- diff(range(x$obs))
  out <- c(mdae = stats::median(abs(error)),
    rpd = if (n < 2) NA_real_ else if (root == 0) if (obs_sd == 0) NA_real_ else Inf else obs_sd / root,
    rpiq = if (root == 0) if (obs_iqr == 0) NA_real_ else Inf else obs_iqr / root,
    sep = if (n < 2) NA_real_ else sqrt(sum((error - mean(error))^2) / (n - 1)),
    rer = if (root == 0) if (obs_range == 0) NA_real_ else Inf else obs_range / root,
    mape = if (any(x$obs == 0)) NA_real_ else 100 * mean(abs(error / x$obs)),
    mpe = if (any(x$obs == 0)) NA_real_ else 100 * mean(error / x$obs),
    smape = 100 * mean(ifelse(x$obs == 0 & x$pred == 0, 0, 2 * abs(error) / (abs(x$obs) + abs(x$pred))), na.rm = TRUE),
    msle = if (any(x$obs < 0 | x$pred < 0)) NA_real_ else mean((log1p(x$obs) - log1p(x$pred))^2),
    rmsle = NA_real_,
    rae = if (sum(abs(x$obs - mean(x$obs))) == 0) NA_real_ else sum(abs(error)) / sum(abs(x$obs - mean(x$obs))),
    rrmse = if (mean(x$obs) == 0) NA_real_ else 100 * root / abs(mean(x$obs)),
    willmott_d = NA_real_)
  out["rmsle"] <- sqrt(out["msle"])
  potential_error <- sum((abs(x$pred - mean(x$obs)) + abs(x$obs - mean(x$obs)))^2)
  out["willmott_d"] <- if (potential_error == 0) NA_real_ else 1 - sum(error^2) / potential_error
  out
}

extended_metric_label <- function(name) {
  labels <- c(mdae = "MdAE", rpd = "RPD", rpiq = "RPIQ", sep = "SEP",
    rer = "RER", mape = "MAPE", mpe = "MPE", smape = "sMAPE",
    msle = "MSLE", rmsle = "RMSLE", rae = "RAE", rrmse = "RRMSE",
    willmott_d = "Willmott's d")
  unname(labels[[name]])
}

extended_metric_undefined_reason <- function(x, name) {
  n <- length(x$obs)
  error <- x$obs - x$pred
  root <- sqrt(mean(error^2))
  if (name == "sep" && n < 2L) return("fewer than two valid observation-prediction pairs remain")
  if (name %in% c("mape", "mpe") && any(x$obs == 0)) return("observations contain zero values")
  if (name %in% c("msle", "rmsle") && any(x$obs < 0 | x$pred < 0)) {
    return("observations or predictions contain negative values")
  }
  if (name == "rrmse" && mean(x$obs) == 0) return("the mean of the observations is zero")
  if (name == "rae" && sum(abs(x$obs - mean(x$obs))) == 0) return("the observations are constant, giving a zero denominator")
  if (name == "willmott_d") {
    denominator <- sum((abs(x$pred - mean(x$obs)) + abs(x$obs - mean(x$obs)))^2)
    if (denominator == 0) return("the potential-error denominator is zero")
  }
  if (name == "rpd") {
    if (n < 2L) return("fewer than two valid observation-prediction pairs remain")
    if (root == 0 && stats::sd(x$obs) == 0) return("the observations have zero variance and RMSE is zero")
  }
  if (name == "rpiq" && root == 0 && stats::IQR(x$obs) == 0) {
    return("the observation interquartile range and RMSE are both zero")
  }
  if (name == "rer" && root == 0 && diff(range(x$obs)) == 0) {
    return("the observation range and RMSE are both zero")
  }
  NULL
}

extended_metric_value <- function(obs, pred, na.rm, name) {
  x <- prepare_metric_vectors(obs = obs, pred = pred, na.rm = na.rm)
  metric <- extended_metric_label(name)
  if (is.null(x)) return(undefined_metric(metric, missing_pair_reason(na.rm)))
  if (!length(x$obs)) {
    return(undefined_metric(metric, "no valid observation-prediction pairs remain after missing-value handling"))
  }
  reason <- extended_metric_undefined_reason(x, name)
  if (!is.null(reason)) return(undefined_metric(metric, reason))
  unname(extended_components(x$obs, x$pred, na.rm = FALSE)[[name]])
}

#' Median absolute error
#'
#' Median absolute error (MdAE) is the median absolute prediction error.
#'
#' \deqn{\mathrm{MdAE} = \mathrm{median}_{i=1,\ldots,n}(|obs_i-pred_i|).}
#'
#' MdAE has response units, is non-negative, and zero is ideal. It describes a
#' typical error while being less sensitive to extreme errors than [mae()] or
#' [rmse()]. Missing-value handling follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Hyndman, R. J. and Koehler, A. B. (2006). Another look at
#'   measures of forecast accuracy. *International Journal of Forecasting*, 22,
#'   679-688. <doi:10.1016/j.ijforecast.2006.03.001>
#' @family prediction metrics
#' @export
mdae <- function(obs, pred, na.rm = TRUE) {
  extended_metric_value(obs, pred, na.rm, "mdae")
}

#' Ratio of performance to deviation
#'
#' Ratio of performance to deviation (RPD) scales RMSE by the sample standard
#' deviation of observations.
#'
#' \deqn{\mathrm{RPD}=
#' \frac{\sqrt{(n-1)^{-1}\sum_{i=1}^{n}(obs_i-\bar{obs})^2}}
#' {\sqrt{n^{-1}\sum_{i=1}^{n}(obs_i-pred_i)^2}}.}
#'
#' RPD is non-negative and larger values indicate lower error relative to
#' observed variation. For perfect predictions with nonzero observed variation
#' it returns `Inf`, the mathematically defined ratio with zero RMSE, without a
#' warning. It returns `NA` with a warning for fewer than two retained pairs or
#' for the indeterminate zero-over-zero case. Missing-value handling follows
#' [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Bellon-Maurel, V., Fernandez-Ahumada, E., Palagos, B., Roger,
#'   J.-M., and McBratney, A. (2010). Critical review of chemometric indicators
#'   commonly used for assessing the quality of the prediction of soil
#'   attributes by NIR spectroscopy. *Trends in Analytical Chemistry*, 29,
#'   1073-1081. <doi:10.1016/j.trac.2010.05.006>
#' @family prediction metrics
#' @export
rpd <- function(obs, pred, na.rm = TRUE) {
  extended_metric_value(obs, pred, na.rm, "rpd")
}

#' Ratio of performance to interquartile distance
#'
#' Ratio of performance to interquartile distance (RPIQ) scales RMSE by the
#' interquartile range of observations.
#'
#' \deqn{\mathrm{RPIQ}=
#' \frac{Q_{0.75}(obs)-Q_{0.25}(obs)}
#' {\sqrt{n^{-1}\sum_{i=1}^{n}(obs_i-pred_i)^2}}.}
#'
#' RPIQ is non-negative and larger values indicate lower error relative to the
#' middle 50 percent of observed values. It is less influenced by extremes than
#' [rpd()]. R uses default type-7 quartiles. For perfect predictions with a
#' nonzero interquartile range it returns `Inf`; it returns `NA` with a warning
#' for the indeterminate zero-over-zero case. Missing-value handling follows
#' [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Bellon-Maurel et al. (2010). See [rpd()].
#' @family prediction metrics
#' @export
rpiq <- function(obs, pred, na.rm = TRUE) {
  extended_metric_value(obs, pred, na.rm, "rpiq")
}

#' Standard error of prediction
#'
#' Standard error of prediction (SEP) is the sample standard deviation of
#' prediction errors after removing their mean error (ME).
#'
#' \deqn{\mathrm{SEP}=\sqrt{\frac{1}{n-1}\sum_{i=1}^{n}
#' \left[(obs_i-pred_i)-\frac{1}{n}\sum_{j=1}^{n}(obs_j-pred_j)\right]^2}.}
#'
#' SEP has response units, is non-negative, and zero is ideal. Unlike RMSE, it
#' removes constant bias. It returns `NA` with a warning when fewer than two
#' retained pairs remain. Missing-value handling follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Bellon-Maurel et al. (2010). See [rpd()].
#' @family prediction metrics
#' @export
sep <- function(obs, pred, na.rm = TRUE) {
  extended_metric_value(obs, pred, na.rm, "sep")
}

#' Range-to-RMSE ratio
#'
#' Range-to-RMSE ratio (RER) scales RMSE by the observed range.
#'
#' \deqn{\mathrm{RER}=\frac{\max_{i}(obs_i)-\min_{i}(obs_i)}
#' {\sqrt{n^{-1}\sum_{i=1}^{n}(obs_i-pred_i)^2}}.}
#'
#' RER is non-negative and larger values indicate smaller error relative to the
#' observed range. It is sensitive to extreme observations. For perfect
#' predictions with a nonzero range it returns `Inf`; it returns `NA` with a
#' warning for the indeterminate zero-over-zero case. Missing-value handling
#' follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Bellon-Maurel et al. (2010). See [rpd()].
#' @family prediction metrics
#' @export
rer <- function(obs, pred, na.rm = TRUE) {
  extended_metric_value(obs, pred, na.rm, "rer")
}

#' Mean absolute percentage error
#'
#' Mean absolute percentage error (MAPE) averages absolute error relative to
#' each observation.
#'
#' \deqn{\mathrm{MAPE}=\frac{100}{n}\sum_{i=1}^n
#' \left|\frac{obs_i-pred_i}{obs_i}\right|.}
#'
#' MAPE is a non-negative percentage; zero is ideal. It returns `NA` with a
#' warning when any observation is zero and can
#' disproportionately weight errors near zero. Missing-value handling follows
#' [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Hyndman and Koehler (2006). See [mdae()].
#' @family prediction metrics
#' @export
mape <- function(obs, pred, na.rm = TRUE) {
  extended_metric_value(obs, pred, na.rm, "mape")
}

#' Mean percentage error
#'
#' Mean percentage error (MPE) is signed mean error relative to observations,
#' reported in percent.
#'
#' \deqn{\mathrm{MPE}=\frac{100}{n}\sum_{i=1}^n\frac{obs_i-pred_i}{obs_i}.}
#'
#' MPE is unbounded and zero is ideal. For strictly positive observations,
#' positive values indicate underprediction. Relative errors can cancel; MPE is
#' undefined for zero observations, returning `NA` with a warning, and unstable
#' near zero. Missing-value handling follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Hyndman and Koehler (2006). See [mdae()].
#' @family prediction metrics
#' @export
mpe <- function(obs, pred, na.rm = TRUE) {
  extended_metric_value(obs, pred, na.rm, "mpe")
}

#' Symmetric mean absolute percentage error
#'
#' Symmetric mean absolute percentage error (sMAPE) scales absolute error by
#' absolute observation and prediction sizes.
#'
#' \deqn{\mathrm{sMAPE}=\frac{100}{n}\sum_{i=1}^n
#' \frac{2|obs_i-pred_i|}{|obs_i|+|pred_i|}.}
#'
#' sMAPE ranges from zero to 200 percent; zero is ideal. A pair of zeros
#' contributes zero. Missing-value handling follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Hyndman and Koehler (2006). See [mdae()].
#' @family prediction metrics
#' @export
smape <- function(obs, pred, na.rm = TRUE) {
  extended_metric_value(obs, pred, na.rm, "smape")
}

#' Mean squared logarithmic error
#'
#' Mean squared logarithmic error (MSLE) averages squared differences on the
#' log1p scale.
#'
#' \deqn{\mathrm{MSLE}=\frac{1}{n}\sum_{i=1}^n
#' [\log(1+obs_i)-\log(1+pred_i)]^2.}
#'
#' MSLE is non-negative and zero is ideal. It emphasizes relative differences
#' and requires non-negative observations and predictions; otherwise it returns
#' `NA` with a warning. The log1p convention is a package choice. Missing-value
#' handling follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Hodson, T. O. (2022). Root mean square error (RMSE) or mean
#'   absolute error (MAE): When to use them or not. *Geoscientific Model
#'   Development*, 15, 5481-5487. <doi:10.5194/gmd-15-5481-2022>
#' @family prediction metrics
#' @export
msle <- function(obs, pred, na.rm = TRUE) {
  extended_metric_value(obs, pred, na.rm, "msle")
}

#' Root mean squared logarithmic error
#'
#' Root mean squared logarithmic error (RMSLE) is the square root of [msle()].
#'
#' \deqn{\mathrm{RMSLE}=\sqrt{\frac{1}{n}\sum_{i=1}^n
#' [\log(1+obs_i)-\log(1+pred_i)]^2}.}
#'
#' RMSLE is non-negative and zero is ideal. It has the same non-negative input
#' requirement and log1p convention as [msle()], returning `NA` with a warning
#' when either input contains a negative value. Missing-value handling follows
#' [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Hodson (2022). See [msle()].
#' @family prediction metrics
#' @export
rmsle <- function(obs, pred, na.rm = TRUE) {
  extended_metric_value(obs, pred, na.rm, "rmsle")
}

#' Relative absolute error
#'
#' Relative absolute error (RAE) compares total absolute error with total
#' absolute error from predicting the observed mean.
#'
#' \deqn{\mathrm{RAE}=\frac{\sum_{i=1}^n|obs_i-pred_i|}
#' {\sum_{i=1}^n|obs_i-\bar{obs}|}.}
#'
#' RAE is non-negative and zero is ideal. One equals the observed-mean
#' absolute-error benchmark; values above one are worse. It returns `NA` with a
#' warning for constant observations, whose benchmark denominator is zero.
#' Missing-value handling follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Hyndman and Koehler (2006). See [mdae()].
#' @family prediction metrics
#' @export
rae <- function(obs, pred, na.rm = TRUE) {
  extended_metric_value(obs, pred, na.rm, "rae")
}

#' Relative root mean squared error
#'
#' Relative root mean squared error (RRMSE) expresses RMSE as a percentage of
#' the absolute observed mean.
#'
#' \deqn{\mathrm{RRMSE}=100\,
#' \frac{\sqrt{n^{-1}\sum_{i=1}^{n}(obs_i-pred_i)^2}}{|\bar{obs}|}.}
#'
#' RRMSE is non-negative and zero is ideal. It returns `NA` with a warning for
#' a zero observed mean and is unstable when that mean is near zero. This mean-normalized convention
#' differs from the standard-deviation normalization in [nrmse()]. Missing-value
#' handling follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Willmott, C. J., Ackleson, S. G., Davis, R. E., Feddema, J. J.,
#'   Klink, K. M., Legates, D. R., O'Donnell, J., and Rowe, C. M. (1985).
#'   Statistics for the evaluation and comparison of models. *Journal of
#'   Geophysical Research*, 90, 8995-9005.
#'   <doi:10.1029/JC090iC05p08995>
#' @family prediction metrics
#' @export
rrmse <- function(obs, pred, na.rm = TRUE) {
  extended_metric_value(obs, pred, na.rm, "rrmse")
}

#' Willmott's index of agreement
#'
#' Willmott's original index of agreement, d, compares squared error with a
#' potential-error denominator based on the observed mean.
#'
#' \deqn{d=1-\frac{\sum_{i=1}^n(obs_i-pred_i)^2}
#' {\sum_{i=1}^n(|pred_i-\bar{obs}|+|obs_i-\bar{obs}|)^2}.}
#'
#' For finite inputs, d ranges from zero to one and one is ideal. The index can
#' be strongly influenced by large errors. It returns `NA` with a warning when
#' its denominator is zero, including identical constant observations and predictions. Missing-value
#' handling follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Willmott et al. (1985). See [rrmse()].
#' @family prediction metrics
#' @export
willmott_d <- function(obs, pred, na.rm = TRUE) {
  extended_metric_value(obs, pred, na.rm, "willmott_d")
}

#' Quantile (pinball) loss
#'
#' Quantile loss evaluates a prediction for a specified conditional quantile.
#' With quantile level `tau`, it is
#'
#' \deqn{L_\tau = \frac{1}{n}\sum_{i=1}^{n}
#' \begin{cases}\tau(obs_i-pred_i), & obs_i-pred_i \geq 0\\
#' (\tau-1)(obs_i-pred_i), & obs_i-pred_i < 0.\end{cases}}
#'
#' It is non-negative and zero is ideal. Underprediction is penalized more when
#' `level` is high; overprediction is penalized more when `level` is low. At
#' `level = 0.5`, it equals one-half of [mae()]. Missing-value handling follows
#' [bias()]. It returns `NA` with a warning when no valid pairs remain.
#' @inheritParams bias
#' @param level Quantile level strictly between zero and one.
#' @return One numeric loss; lower is better.
#' @references Koenker, R. and Bassett, G. (1978). Regression quantiles.
#'   *Econometrica*, 46, 33-50. <doi:10.2307/1913643>
#' @export
pinball_loss <- function(obs, pred, level = .5, na.rm = TRUE) {
  check_probability(level, "level")
  x <- prepare_metric_vectors(obs = obs, pred = pred, na.rm = na.rm)
  if (is.null(x)) return(undefined_metric("Pinball loss", missing_pair_reason(na.rm)))
  if (!length(x$obs)) return(undefined_metric("Pinball loss", "no valid observation-prediction pairs remain after missing-value handling"))
  error <- x$obs - x$pred
  mean(ifelse(error >= 0, level * error, (level - 1) * error))
}

#' Kling-Gupta efficiency
#'
#' Kling-Gupta efficiency (KGE) combines correlation, variability ratio, and
#' mean ratio. The component definitions below use the same paired observations
#' and predictions as the main score.
#'
#' \deqn{r=\frac{\sum_{i=1}^{n}(obs_i-\bar{obs})(pred_i-\bar{pred})}
#' {\sqrt{\sum_{i=1}^{n}(obs_i-\bar{obs})^2\sum_{i=1}^{n}(pred_i-\bar{pred})^2}},
#' \quad \alpha=\sqrt{\frac{\sum_{i=1}^{n}(pred_i-\bar{pred})^2}
#' {\sum_{i=1}^{n}(obs_i-\bar{obs})^2}},
#' \quad \beta=\frac{\bar{pred}}{\bar{obs}}.}
#' \deqn{\mathrm{KGE}=1-\sqrt{(r-1)^2+(\alpha-1)^2+(\beta-1)^2}.}
#'
#' One is ideal. Values closer to one indicate agreement in linear association,
#' spread, and mean. The range is unbounded below and at most one. Zero is
#' not the observed-mean benchmark used for NSE. KGE is undefined when the
#' observed mean or either vector's standard deviation is zero, or fewer than
#' two pairs remain; it returns `NA` with a warning in those cases. As with NSE, avoid treating KGE as the only measure of model
#' quality; inspect its components and complementary error metrics.
#' @inheritParams bias
#' @return One numeric value; one is ideal.
#' @references Gupta, H. V., Kling, H., Yilmaz, K. K., and Martinez, G. F.
#'   (2009). Decomposition of the mean squared error and NSE performance
#'   criteria: Implications for improving hydrological modelling. *Journal of
#'   Hydrology*, 377, 80-91. <doi:10.1016/j.jhydrol.2009.08.003>
#' @export
kge <- function(obs, pred, na.rm = TRUE) {
  x <- prepare_metric_vectors(obs = obs, pred = pred, na.rm = na.rm)
  if (is.null(x)) return(undefined_metric("KGE", missing_pair_reason(na.rm)))
  if (!length(x$obs)) {
    return(undefined_metric("KGE", "no valid observation-prediction pairs remain after missing-value handling"))
  }
  reason <- kge_undefined_reason(x)
  if (!is.null(reason)) return(undefined_metric("KGE", reason))
  1 - sqrt((stats::cor(x$obs, x$pred) - 1)^2 + (stats::sd(x$pred) / stats::sd(x$obs) - 1)^2 + (mean(x$pred) / mean(x$obs) - 1)^2)
}

kge_undefined_reason <- function(x) {
  if (length(x$obs) < 2L) return("fewer than two valid observation-prediction pairs remain")
  if (mean(x$obs) == 0) return("the mean of the observations is zero")
  if (is_constant(x$obs)) return("the observations have zero variance")
  if (is_constant(x$pred)) return("the predictions have zero variance")
  NULL
}

undefined_metric <- function(metric, reason) {
  warning(sprintf("%s is undefined because %s.", metric, reason), call. = FALSE)
  NA_real_
}

missing_pair_reason <- function(na.rm) {
  if (na.rm) {
    "no valid observation-prediction pairs remain after missing-value handling"
  } else {
    "inputs contain missing values and `na.rm = FALSE`"
  }
}

metric_label <- function(name) {
  labels <- c(bias = "ME", mae = "MAE", mse = "MSE", rmse = "RMSE",
    nrmse = "NRMSE", crmse = "cRMSE", correlation = "Correlation",
    r2 = "r2", nse = "NSE", sd_ratio = "SD ratio", ccc = "CCC")
  unname(labels[[name]])
}

is_constant <- function(x) length(x) > 0L && all(x == x[[1L]])

core_metric_undefined_reason <- function(x, name) {
  n <- length(x$obs)
  if (name %in% c("nrmse", "nse", "sd_ratio", "correlation", "r2", "ccc") && n < 2L) {
    return("fewer than two valid observation-prediction pairs remain")
  }
  if (name %in% c("nrmse", "nse", "sd_ratio", "correlation", "r2", "ccc") && is_constant(x$obs)) {
    return("the observations have zero variance")
  }
  if (name %in% c("correlation", "r2") && is_constant(x$pred)) {
    return("the predictions have zero variance")
  }
  NULL
}

metric_value <- function(obs, pred, na.rm, name, metric = metric_label(name)) {
  values <- prepare_metric_vectors(obs = obs, pred = pred, na.rm = na.rm)
  if (is.null(values)) return(undefined_metric(metric, missing_pair_reason(na.rm)))
  if (!length(values$obs)) {
    return(undefined_metric(metric, "no valid observation-prediction pairs remain after missing-value handling"))
  }
  reason <- core_metric_undefined_reason(values, name)
  if (!is.null(reason)) return(undefined_metric(metric, reason))
  metric_components(values$obs, values$pred)[[name]]
}

# Shared calculations for individual metrics and model_metrics().
metric_components <- function(obs, pred) {
  empty <- stats::setNames(rep(NA_real_, 13),
    c("bias", "mae", "mse", "rmse", "nrmse", "crmse", "correlation",
      "r2", "nse", "sd_ratio", "ccc", "cb", "n"))
  n <- length(obs)
  if (!n) return(empty)
  m <- pair_moments(pred, obs)
  error <- m$o - m$p
  result <- empty
  result["n"] <- n
  result["bias"] <- mean(error) * m$scale
  result["mae"] <- mean(abs(error)) * m$scale
  result["rmse"] <- root_mean_square(error) * m$scale
  result["mse"] <- result["rmse"]^2
  result["crmse"] <- root_mean_square(error - mean(error)) * m$scale
  if (n < 2L || m$so == 0) return(result)
  sample_obs_sd <- m$so * m$scale * sqrt(n / (n - 1))
  result["nrmse"] <- result["rmse"] / sample_obs_sd
  result["sd_ratio"] <- m$sp / m$so
  result["nse"] <- 1 - (root_mean_square(error) / m$so)^2
  if (m$sp > 0) {
    result["correlation"] <- m$r
    result["r2"] <- m$r^2
  }
  shift <- mean(m$p) - mean(m$o)
  denom_scale <- max(m$sp, m$so, abs(shift))
  sp <- m$sp / denom_scale
  so <- m$so / denom_scale
  result["cb"] <- 2 * sp * so / (sp^2 + so^2 + (shift / denom_scale)^2)
  result["ccc"] <- if (m$sp > 0) m$r * result["cb"] else 0
  result
}
