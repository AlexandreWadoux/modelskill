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
#' remain consistent with its diagram statistics. NRMSE is undefined for
#' constant observations. Missing-value handling follows [bias()].
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
#' linear association. It is undefined for a constant vector. Correlation is
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
#' linear association. A value of one can occur despite additive bias or
#' proportional scale differences, so it is not a measure of agreement or
#' prediction accuracy. Do not confuse lowercase `r2()` with [nse()], [mec()],
#' or uppercase `R2()`, which are aliases for model efficiency.
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
#' benchmark. NSE is undefined for constant observations. It is sensitive to
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
nse <- function(obs, pred, na.rm = TRUE) metric_value(obs, pred, na.rm, "nse")

#' Model efficiency coefficient
#'
#' Alias for [nse()]. MEC, NSE, and the uppercase R-squared efficiency `R2()`
#' are the same statistic. They must not be confused with lowercase `r2()`,
#' the squared Pearson correlation.
#'
#' \deqn{\mathrm{MEC}=1-\frac{\sum_{i=1}^{n}(obs_i-pred_i)^2}
#' {\sum_{i=1}^{n}(obs_i-\bar{obs})^2}.}
#'
#' Its interpretation and references are given in [nse()].
#' @inheritParams bias
#' @return One numeric value.
#' @examples mec(1:3, c(1, 3, 2))
#' @export
mec <- function(obs, pred, na.rm = TRUE) nse(obs, pred, na.rm)

#' Coefficient of determination / efficiency R-squared
#'
#' Alias for [nse()] and `mec()`. This uppercase `R2()` is the model-efficiency
#' coefficient; lowercase `r2()` remains squared Pearson correlation. Its
#' interpretation and references are given in [nse()].
#'
#' \deqn{R^2=1-\frac{\sum_{i=1}^{n}(obs_i-pred_i)^2}
#' {\sum_{i=1}^{n}(obs_i-\bar{obs})^2}.}
#' @rdname efficiency_r2
#' @inheritParams bias
#' @return One numeric value.
#' @examples R2(1:3, c(1, 3, 2))
#' @export
R2 <- function(obs, pred, na.rm = TRUE) nse(obs, pred, na.rm)

#' Prediction-to-observation standard deviation ratio
#'
#' The standard deviation ratio compares the sample standard deviation of
#' predictions with that of observations.
#'
#' \deqn{\mathrm{SD\ ratio}=
#' \sqrt{\frac{\sum_{i=1}^{n}(pred_i-\bar{pred})^2}
#' {\sum_{i=1}^{n}(obs_i-\bar{obs})^2}}.}
#'
#' The ratio is non-negative and one indicates equal spread. Values below one
#' indicate under-dispersed predictions; values above one indicate
#' over-dispersed predictions. It assesses spread, not mean bias or association,
#' and is undefined for constant observations. Missing-value handling follows
#' [bias()].
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
#' Lin's concordance correlation coefficient (CCC) combines Pearson correlation
#' with agreement in location and scale.
#'
#' \deqn{\rho_c=\frac{2\sum_{i=1}^{n}(obs_i-\bar{obs})(pred_i-\bar{pred})}
#' {\sum_{i=1}^{n}(obs_i-\bar{obs})^2+
#' \sum_{i=1}^{n}(pred_i-\bar{pred})^2+n(\bar{obs}-\bar{pred})^2}.}
#'
#' CCC ranges from -1 to one and equals one for perfect agreement. Values near
#' zero indicate little concordance; negative values indicate discordant linear
#' association. Unlike Pearson correlation, CCC is reduced by mean and scale
#' differences. Population variances (divisor n) are used, matching the
#' established package convention. The package returns NA for constant
#' observations or fewer than two pairs, and zero for constant predictions
#' with varying observations. Missing-value handling follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Lin, L. I.-K. (1989). A concordance correlation coefficient to
#'   evaluate reproducibility. *Biometrics*, 45, 255-268.
#'   <doi:10.2307/2532051>
#' @examples ccc(1:3, c(1, 3, 2))
#' @export
ccc <- function(obs, pred, na.rm = TRUE) metric_value(obs, pred, na.rm, "ccc")

extended_components <- function(obs, pred, na.rm = TRUE) {
  x <- prepare_metric_vectors(obs = obs, pred = pred, na.rm = na.rm)
  if (is.null(x) || !length(x$obs)) return(stats::setNames(rep(NA_real_, 13), c("mdae", "rpd", "rpiq", "sep", "rer", "mape", "mpe", "smape", "msle", "rmsle", "rae", "rrmse", "willmott_d")))
  error <- x$obs - x$pred; root <- sqrt(mean(error^2)); n <- length(error)
  out <- c(mdae = stats::median(abs(error)),
    rpd = if (n < 2 || root == 0) NA_real_ else stats::sd(x$obs) / root,
    rpiq = if (root == 0) NA_real_ else stats::IQR(x$obs) / root,
    sep = if (n < 2) NA_real_ else sqrt(sum((error - mean(error))^2) / (n - 1)),
    rer = if (root == 0) NA_real_ else diff(range(x$obs)) / root,
    mape = if (any(x$obs == 0)) NA_real_ else mean(abs(error / x$obs)),
    mpe = if (any(x$obs == 0)) NA_real_ else 100 * mean(error / x$obs),
    smape = mean(ifelse(x$obs == 0 & x$pred == 0, 0, 2 * abs(error) / (abs(x$obs) + abs(x$pred))), na.rm = TRUE),
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
mdae <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["mdae"])

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
#' observed variation. It returns NA for perfect predictions rather than
#' infinity, and with fewer than two retained pairs. Missing-value handling
#' follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Bellon-Maurel, V., Fernandez-Ahumada, E., Palagos, B., Roger,
#'   J.-M., and McBratney, A. (2010). Critical review of chemometric indicators
#'   commonly used for assessing the quality of the prediction of soil
#'   attributes by NIR spectroscopy. *Trends in Analytical Chemistry*, 29,
#'   1073-1081. <doi:10.1016/j.trac.2010.05.006>
#' @family prediction metrics
#' @export
rpd <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["rpd"])

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
#' [rpd()]. R uses default type-7 quartiles. It is NA for perfect predictions.
#' Missing-value handling follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Bellon-Maurel et al. (2010). See [rpd()].
#' @family prediction metrics
#' @export
rpiq <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["rpiq"])

#' Standard error of prediction
#'
#' Standard error of prediction (SEP) is the sample standard deviation of
#' prediction errors after removing their mean error (ME).
#'
#' \deqn{\mathrm{SEP}=\sqrt{\frac{1}{n-1}\sum_{i=1}^{n}
#' \left[(obs_i-pred_i)-\frac{1}{n}\sum_{j=1}^{n}(obs_j-pred_j)\right]^2}.}
#'
#' SEP has response units, is non-negative, and zero is ideal. Unlike RMSE, it
#' removes constant bias. At least two retained pairs are required. Missing-value
#' handling follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Bellon-Maurel et al. (2010). See [rpd()].
#' @family prediction metrics
#' @export
sep <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["sep"])

#' Range-to-RMSE ratio
#'
#' Range-to-RMSE ratio (RER) scales RMSE by the observed range.
#'
#' \deqn{\mathrm{RER}=\frac{\max_{i}(obs_i)-\min_{i}(obs_i)}
#' {\sqrt{n^{-1}\sum_{i=1}^{n}(obs_i-pred_i)^2}}.}
#'
#' RER is non-negative and larger values indicate smaller error relative to the
#' observed range. It is sensitive to extreme observations and is NA for perfect
#' predictions. Missing-value handling follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Bellon-Maurel et al. (2010). See [rpd()].
#' @family prediction metrics
#' @export
rer <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["rer"])

#' Mean absolute percentage error
#'
#' Mean absolute percentage error (MAPE) averages absolute error relative to
#' each observation.
#'
#' \deqn{\mathrm{MAPE}=\frac{1}{n}\sum_{i=1}^n
#' \left|\frac{obs_i-pred_i}{obs_i}\right|.}
#'
#' MAPE is a non-negative fraction; zero is ideal, and multiplying by 100 gives
#' percent. It is NA for zero observations and can disproportionately weight
#' errors near zero. Missing-value handling follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Hyndman and Koehler (2006). See [mdae()].
#' @family prediction metrics
#' @export
mape <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["mape"])

#' Mean percentage error
#'
#' Mean percentage error (MPE) is signed mean error relative to observations,
#' reported in percent.
#'
#' \deqn{\mathrm{MPE}=\frac{100}{n}\sum_{i=1}^n\frac{obs_i-pred_i}{obs_i}.}
#'
#' MPE is unbounded and zero is ideal. For strictly positive observations,
#' positive values indicate underprediction. Relative errors can cancel; MPE is
#' NA for zero observations and unstable near zero. Missing-value handling
#' follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Hyndman and Koehler (2006). See [mdae()].
#' @family prediction metrics
#' @export
mpe <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["mpe"])

#' Symmetric mean absolute percentage error
#'
#' Symmetric mean absolute percentage error (sMAPE) scales absolute error by
#' absolute observation and prediction sizes.
#'
#' \deqn{\mathrm{sMAPE}=\frac{1}{n}\sum_{i=1}^n
#' \frac{2|obs_i-pred_i|}{|obs_i|+|pred_i|}.}
#'
#' sMAPE ranges from zero to two; zero is ideal. Multiply by 100 for percent.
#' A pair of zeros contributes zero. Missing-value handling follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Hyndman and Koehler (2006). See [mdae()].
#' @family prediction metrics
#' @export
smape <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["smape"])

#' Mean squared logarithmic error
#'
#' Mean squared logarithmic error (MSLE) averages squared differences on the
#' log1p scale.
#'
#' \deqn{\mathrm{MSLE}=\frac{1}{n}\sum_{i=1}^n
#' [\log(1+obs_i)-\log(1+pred_i)]^2.}
#'
#' MSLE is non-negative and zero is ideal. It emphasizes relative differences
#' and requires non-negative observations and predictions; otherwise it is NA.
#' The log1p convention is a package choice. Missing-value handling follows
#' [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Hodson, T. O. (2022). Root mean square error (RMSE) or mean
#'   absolute error (MAE): When to use them or not. *Geoscientific Model
#'   Development*, 15, 5481-5487. <doi:10.5194/gmd-15-5481-2022>
#' @family prediction metrics
#' @export
msle <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["msle"])

#' Root mean squared logarithmic error
#'
#' Root mean squared logarithmic error (RMSLE) is the square root of [msle()].
#'
#' \deqn{\mathrm{RMSLE}=\sqrt{\frac{1}{n}\sum_{i=1}^n
#' [\log(1+obs_i)-\log(1+pred_i)]^2}.}
#'
#' RMSLE is non-negative and zero is ideal. It has the same non-negative input
#' requirement and log1p convention as [msle()]. Missing-value handling follows
#' [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Hodson (2022). See [msle()].
#' @family prediction metrics
#' @export
rmsle <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["rmsle"])

#' Relative absolute error
#'
#' Relative absolute error (RAE) compares total absolute error with total
#' absolute error from predicting the observed mean.
#'
#' \deqn{\mathrm{RAE}=\frac{\sum_{i=1}^n|obs_i-pred_i|}
#' {\sum_{i=1}^n|obs_i-\bar{obs}|}.}
#'
#' RAE is non-negative and zero is ideal. One equals the observed-mean
#' absolute-error benchmark; values above one are worse. It is NA for constant
#' observations. Missing-value handling follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Hyndman and Koehler (2006). See [mdae()].
#' @family prediction metrics
#' @export
rae <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["rae"])

#' Relative root mean squared error
#'
#' Relative root mean squared error (RRMSE) expresses RMSE as a percentage of
#' the absolute observed mean.
#'
#' \deqn{\mathrm{RRMSE}=100\,
#' \frac{\sqrt{n^{-1}\sum_{i=1}^{n}(obs_i-pred_i)^2}}{|\bar{obs}|}.}
#'
#' RRMSE is non-negative and zero is ideal. It is NA for a zero observed mean
#' and unstable when that mean is near zero. This mean-normalized convention
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
rrmse <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["rrmse"])

#' Willmott's index of agreement
#'
#' Willmott's original index of agreement, d, compares squared error with a
#' potential-error denominator based on the observed mean.
#'
#' \deqn{d=1-\frac{\sum_{i=1}^n(obs_i-pred_i)^2}
#' {\sum_{i=1}^n(|pred_i-\bar{obs}|+|obs_i-\bar{obs}|)^2}.}
#'
#' For finite inputs, d ranges from zero to one and one is ideal. The index can
#' be strongly influenced by large errors. It is NA when its denominator is zero,
#' including identical constant observations and predictions. Missing-value
#' handling follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Willmott et al. (1985). See [rrmse()].
#' @family prediction metrics
#' @export
willmott_d <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["willmott_d"])

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
#' [bias()].
#' @inheritParams bias
#' @param level Quantile level strictly between zero and one.
#' @return One numeric loss; lower is better.
#' @references Koenker, R. and Bassett, G. (1978). Regression quantiles.
#'   *Econometrica*, 46, 33-50. <doi:10.2307/1913643>
#' @export
pinball_loss <- function(obs, pred, level = .5, na.rm = TRUE) {
  check_probability(level, "level"); x <- prepare_metric_vectors(obs = obs, pred = pred, na.rm = na.rm)
  if (is.null(x)) return(NA_real_); error <- x$obs - x$pred
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
#' two pairs remain. As with NSE, avoid treating KGE as the only measure of model
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
  if (is.null(x) || length(x$obs) < 2 || mean(x$obs) == 0 || stats::sd(x$obs) == 0 || stats::sd(x$pred) == 0) return(NA_real_)
  1 - sqrt((stats::cor(x$obs, x$pred) - 1)^2 + (stats::sd(x$pred) / stats::sd(x$obs) - 1)^2 + (mean(x$pred) / mean(x$obs) - 1)^2)
}

metric_value <- function(obs, pred, na.rm, name) {
  values <- prepare_metric_vectors(obs = obs, pred = pred, na.rm = na.rm)
  if (is.null(values)) return(NA_real_)
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
