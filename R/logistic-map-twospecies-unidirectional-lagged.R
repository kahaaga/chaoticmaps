#' Time series for a two-species model system. Unidirectional forcing from x introduces synchrony in y.
#'
#' Source: Ye, H. et al. (2015). Distinguishing time-delayed causal interactions
#' using convergent cross mapping. Sci. Rep. 5, 14750; doi: 10.1038/srep14750 (2015).
#' Default values are as in the original paper.
#'
#' @param n The number of time steps that will be generated.
#' @param xi Initial value of time series x.
#' @param yi Initial value of time series y.
#' @param Rx Parameter controlling the influence of x at timestep i on itself at timestep i+1
#' @param Ry Parameter controlling the influence of y at timestep i on itself at timestep i+1
#' @param Rxy Parameter controlling the influence of x at timestep i on y at timestep i+delay
#' @param delay The delay of the effect of x on y
#' @param add.timestep Whether or not to add a timestep column to the returned data frame.
#'     Defaults to FALSE.
#' @param plot Whether to plot the dataset as a scatterplot matrix. Defaults to FALSE
#' @return A dataframe containing columns time series for x and y
#'     (and optionally "t", a timestep column).
#'
logistic_map_twospecies_unidir_lagged <- function(
    n =  3000,
    xi = sample(seq(0.01, .99, 0.001), 1),
    yi = sample(seq(0.01, .99, 0.001), 1),
    Rx = sample(seq(3.57, 3.82, 0.001), size = 1),
    Rxy = sample(seq(.7, .9, 0.001), size = 1),
    Ry = sample(seq(3.57, 3.82, 0.001), size = 1),
    time.delay = 0,
    add.timestep = FALSE,
    plot = FALSE) {
  x = vector(length = n)
  y = vector(length = n)
  x[1] = xi
  y[1] = yi

  for (i in 1:max(max(time.delay), 1) + 1) {
    x[i] = xi
    y[i] = yi
  }

  for (i in max(max(time.delay), 1) + 2:n) {
    x[i] = x[i - 1] * (Rx - Rx * x[i - 1] - Rx * x[i - 1])
    y[i] = y[i - 1] * (Ry - Ry * y[i - 1] - Rxy * x[i - 1 - time.delay])
  }

  if (add.timestep) {
    timestep = 1:length(x)
    df = data.frame(timestep, x, y)
    names(df) = c("t", "x", "y")
  } else {
    df = data.frame(x,y)
    names(df) = c("x", "y")
  }


  df = df[1:n, ]

  if (any(!is.finite(x)) | any(!is.finite(y))) {
    return(logistic_map_twospecies_unidir_lagged(n = n,
                                                 time.delay = time.delay,
                                               plot = plot,
                                               add.timestep = add.timestep))
  }


  if (plot & add.timestep) {
    p = ggplot2::ggplot() +
      ggplot2::geom_line(data = df, mapping = ggplot2::aes(x = t, y = x, col = "x")) +
      ggplot2::geom_line(data = df, mapping = ggplot2::aes(x = t, y = y, col = "y")) +
      ggplot2::theme_bw() +
      ggplot2::ylab("value") +
      ggplot2::ggtitle("Logistic map (x forces y)")
    print(p)
  } else if (plot & !add.timestep) {
    warning("cannot plot without adding timestep.")
  }
  return(df)
}
