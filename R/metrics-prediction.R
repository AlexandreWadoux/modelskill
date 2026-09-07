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
#'   (MAE): When to use them or not. *Geoscientific Model Development
#'   Discussions*, 2022, 1-10. <doi:10.5194/gmd-2022-1>
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
#'   Development Discussions*, 2022, 1-10. <doi:10.5194/gmd-2022-1>
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
#'   (MAE): When to use them or not. *Geoscientific Model Development
#'   Discussions*, 2022, 1-10. <doi:10.5194/gmd-2022-1>
#' @examples rmse(1:3, c(1, 3, 2))
#' @export
rmse <- function(obs, pred, na.rm = TRUE) metric_value(obs, pred, na.rm, "rmse")

#' Normalized root mean squared error
#'
#' Normalized RMSE (NRMSE) divides [rmse()] by the sample standard deviation of
#' observations.
#'
#' \deqn{\mathrm{NRMSE} = \frac{\mathrm{RMSE}}{s_{obs}}}
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
#' \deqn{\mathrm{cRMSE} = \sqrt{\frac{1}{n}\sum_{i = 1}^{n}
#' [(obs_i - pred_i) - \mathrm{ME}]^2}}
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
#' \deqn{r^2 = r \times r}
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
#' the squared Pearson correlation. Its equation, interpretation, and references
#' are given in [nse()].
#' @inheritParams bias
#' @return One numeric value.
#' @examples mec(1:3, c(1, 3, 2))
#' @export
mec <- function(obs, pred, na.rm = TRUE) nse(obs, pred, na.rm)

#' Coefficient of determination / efficiency R-squared
#'
#' Alias for [nse()] and `mec()`. This uppercase `R2()` is the model-efficiency
#' coefficient; lowercase `r2()` remains squared Pearson correlation. Its
#' equation, interpretation, and references are given in [nse()].
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
#' \deqn{\mathrm{SD\ ratio} = \frac{s_{pred}}{s_{obs}}}
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
#' \deqn{\rho_c = \frac{2s_{obs,pred}}
#' {s_{obs}^2 + s_{pred}^2 + (\bar{obs} - \bar{pred})^2}}
#'
#' CCC ranges from -1 to one and equals one for perfect agreement. Values near
#' zero indicate little concordance; negative values indicate discordant linear
#' association. Unlike Pearson correlation, CCC is reduced by mean and scale
#' differences. Population variances (divisor n) are used, matching the
#' established package convention. Missing-value handling follows [bias()].
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
  if (is.null(x) || !length(x$obs)) return(stats::setNames(rep(NA_real_, 10), c("mdae", "rpd", "rpiq", "sep", "rer", "mape", "smape", "msle", "rmsle", "rae")))
  error <- x$obs - x$pred; root <- sqrt(mean(error^2)); n <- length(error)
  out <- c(mdae = stats::median(abs(error)),
    rpd = if (n < 2 || root == 0) NA_real_ else stats::sd(x$obs) / root,
    rpiq = if (root == 0) NA_real_ else stats::IQR(x$obs) / root,
    sep = if (n < 2) NA_real_ else sqrt(sum((error - mean(error))^2) / (n - 1)),
    rer = if (root == 0) NA_real_ else diff(range(x$obs)) / root,
    mape = if (any(x$obs == 0)) NA_real_ else mean(abs(error / x$obs)),
    smape = mean(ifelse(x$obs == 0 & x$pred == 0, 0, 2 * abs(error) / (abs(x$obs) + abs(x$pred))), na.rm = TRUE),
    msle = if (any(x$obs < 0 | x$pred < 0)) NA_real_ else mean((log1p(x$obs) - log1p(x$pred))^2),
    rmsle = NA_real_,
    rae = if (sum(abs(x$obs - mean(x$obs))) == 0) NA_real_ else sum(abs(error)) / sum(abs(x$obs - mean(x$obs))))
  out["rmsle"] <- sqrt(out["msle"])
  out
}

#' Extended continuous-prediction metrics
#'
#' These metrics complement the core error and agreement statistics. Let
#' `e_i = obs_i - pred_i`, and let RMSE denote [rmse()].
#'
#' \describe{
#' \item{`mdae()`}{Median absolute error:
#' \deqn{\mathrm{MdAE} = \mathrm{median}(|e_i|).}
#' It has response units; zero is ideal. It describes a typical absolute error
#' robustly, so large outliers have less influence than on MAE or RMSE.}
#' \item{`rpd()`}{Ratio of performance to deviation:
#' \deqn{\mathrm{RPD} = s_{obs} / \mathrm{RMSE}.}
#' Larger values indicate error small relative to observed variation; it is
#' undefined for perfect predictions in this implementation rather than
#' returning infinity.}
#' \item{`rpiq()`}{Ratio of performance to interquartile distance:
#' \deqn{\mathrm{RPIQ} = \mathrm{IQR}(obs) / \mathrm{RMSE}.}
#' Larger values are better; it is less affected by extreme observations than
#' RPD.}
#' \item{`sep()`}{Bias-corrected standard error of prediction:
#' \deqn{\mathrm{SEP} = \sqrt{\sum(e_i - \mathrm{ME})^2/(n-1)}.}
#' It has response units; zero is ideal and constant bias is removed.}
#' \item{`rer()`}{Ratio of error to range:
#' \deqn{\mathrm{RER} = (\max(obs)-\min(obs))/\mathrm{RMSE}.}
#' Larger values are better, but the observed range makes it sensitive to
#' extremes.}
#' \item{`mape()`}{Mean absolute percentage error:
#' \deqn{\mathrm{MAPE} = n^{-1}\sum|e_i / obs_i|.}
#' It is unitless; zero is ideal. It is undefined when any observation is zero
#' and can overemphasize errors at small observed values.}
#' \item{`smape()`}{Symmetric mean absolute percentage error:
#' \deqn{\mathrm{sMAPE} = n^{-1}\sum 2|e_i|/(|obs_i|+|pred_i|).}
#' It is unitless; zero is ideal. A pair of zeros contributes zero.}
#' \item{`msle()`}{Mean squared logarithmic error:
#' \deqn{\mathrm{MSLE} = n^{-1}\sum[\log(1+obs_i)-\log(1+pred_i)]^2.}
#' It is non-negative; zero is ideal and emphasizes relative differences. It
#' requires non-negative observations and predictions.}
#' \item{`rmsle()`}{Root mean squared logarithmic error:
#' \deqn{\mathrm{RMSLE} = \sqrt{\mathrm{MSLE}}.}
#' It is non-negative; zero is ideal and has the log-scale interpretation of
#' MSLE.}
#' \item{`rae()`}{Relative absolute error:
#' \deqn{\mathrm{RAE} = \sum|e_i| / \sum|obs_i-\bar{obs}|.}
#' Zero is ideal; one equals the absolute-error benchmark from predicting the
#' observed mean; values above one are worse.}
#' }
#'
#' MAPE is undefined for zero observations; log metrics require non-negative
#' values. Missing-value handling follows [bias()].
#' @inheritParams bias
#' @return One numeric value.
#' @references Willmott, C. J. and Matsuura, K. (2005). Advantages of the mean
#'   absolute error (MAE) over the root mean square error (RMSE) in assessing
#'   average model performance. *Climate Research*, 30, 79-82.
#'   <doi:10.3354/cr030079>
#'
#'   Bellon-Maurel, V., Fernandez-Ahumada, E., Palagos, B., Roger, J.-M., and
#'   McBratney, A. (2010). Critical review of chemometric indicators commonly
#'   used for assessing the quality of the prediction of soil attributes by NIR
#'   spectroscopy. *Trends in Analytical Chemistry*, 29, 1073-1081.
#'   <doi:10.1016/j.trac.2010.05.006>
#'
#'   Hyndman, R. J. and Koehler, A. B. (2006). Another look at measures of
#'   forecast accuracy. *International Journal of Forecasting*, 22, 679-688.
#'   <doi:10.1016/j.ijforecast.2006.03.001>
#' @name extended_prediction_metrics
NULL

#' @rdname extended_prediction_metrics
#' @export
mdae <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["mdae"])
#' @rdname extended_prediction_metrics
#' @export
rpd <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["rpd"])
#' @rdname extended_prediction_metrics
#' @export
rpiq <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["rpiq"])
#' @rdname extended_prediction_metrics
#' @export
sep <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["sep"])
#' @rdname extended_prediction_metrics
#' @export
rer <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["rer"])
#' @rdname extended_prediction_metrics
#' @export
mape <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["mape"])
#' @rdname extended_prediction_metrics
#' @export
smape <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["smape"])
#' @rdname extended_prediction_metrics
#' @export
msle <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["msle"])
#' @rdname extended_prediction_metrics
#' @export
rmsle <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["rmsle"])
#' @rdname extended_prediction_metrics
#' @export
rae <- function(obs, pred, na.rm = TRUE) unname(extended_components(obs, pred, na.rm)["rae"])

#' Quantile (pinball) loss
#'
#' Quantile loss evaluates a prediction for a specified conditional quantile.
#' With `e_i = obs_i - pred_i` and quantile level `tau`, it is
#'
#' \deqn{L_\tau = \frac{1}{n}\sum_{i=1}^{n}
#' \begin{cases}\tau e_i, & e_i \geq 0\\
#' (\tau-1)e_i, & e_i < 0.\end{cases}}
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
#' mean ratio. Let `r` be [correlation()], `alpha = s_pred / s_obs`, and
#' `beta = mean(pred) / mean(obs)`.
#'
#' \deqn{\mathrm{KGE} = 1 - \sqrt{(r-1)^2 + (\alpha-1)^2 + (\beta-1)^2}}
#'
#' One is ideal. Values closer to one indicate agreement in linear association,
#' spread, and mean. KGE is undefined when observed means or standard deviations
#' are zero. As with NSE, avoid treating KGE as the only measure of model
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
