#' Time series for a two-species (logistic) model system. Unidirectional forcing from x to y.
#'
#' Source: Sugihara, G., May, R., Ye, H., Hsieh, C. H., Deyle, E., Fogarty, M., & Munch, S. (2012).
#' Detecting causality in complex ecosystems. Science, 338(6106), 496-500.
#'
#' @param n The number of time steps that will be generated.
#' @param n Number of points in the time series.
#' @param xi The initial value for x
#' @param yi The initival value for y
#' @param Rx The value of the parameter Rx
#' @param Ry The value of the parameter Ry
#' @param Bxy Parameter controlling the sensitivity of x to changes in y.
#' @param Byx Parameter controlling the sensitivity of y to changes in x.
#' @add.timestep Whether or not to add a timestep column to the returned data frame.
#'     Defaults to FALSE.
#' @plot Whether to plot the dataset as a scatterplot matrix. Defaults to FALSE
#' @return A dataframe containing columns time series for x and y
#'     (and optionally "t", a timestep column).
#'
#' Defaults values are as in the original paper for figure 2A. Figure 2B in the original paper
#' uses 1000 different parameterizations with R and B chosen on the intervals
#' [3.6, 4.0] and [0, 0.4], respectively.
logistic_map_twospecies_unidir <- function(n = 1000,
                                           xi = 0.2,
                                           yi = 0.4,
                                           Rx = 3.7,
                                           Ry = 3.7,
                                           Bxy = 0,
                                           Byx = 0.32,
                                           add.timestep = FALSE,
                                           plot = FALSE) {
  x = vector(length = n)
  y = vector(length = n)
  x[1] = xi
  y[1] = yi

  for (i in 2:n) {
    x[i] = x[i - 1]*(Rx - Rx * x[i - 1] - Bxy * y[i - 1])
    y[i] = y[i - 1]*(Ry - Ry * y[i - 1] - Byx * x[i - 1])
  }

  if (plot & add.timestep) {
    p = ggplot2::ggplot() +
      ggplot2::geom_line(data = df, mapping = ggplot2::aes(x = t, y = x, col = "x")) +
      ggplot2::geom_line(data = df, mapping = ggplot2::aes(x = t, y = y, col = "y")) +
      ggplot2::theme_bw() +
      ggplot2::ylab("value") +
      ggplot2::ggtitle("Unidirectional logistic map (Sugihara et al., 2012)")
    print(p)
  } else if (plot & !add.timestep) {
    warning("cannot plot without adding timestep.")
  }
  return(df)
}


