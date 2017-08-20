#' Four-species logistic map system.
#' Transitive unidirectional forcing (y1 -> y2 -> y3 -> y4).
#'
#' Source: Ye, H. et al. (2015). Distinguishing time-delayed causal interactions
#' using convergent cross mapping. Sci. Rep. 5, 14750; doi: 10.1038/srep14750 (2015)
#'
#' This function allows you to express your love of cats.
#' @param n The number of time steps that will be generated.
#' @param y1.i Initial value of time series y1.
#' @param y2.i Initial value of time series y2.
#' @param y3.i Initial value of time series y3.
#' @param y4.i Initial value of time series y4.
#' @param Ry1y1 Parameter controlling the influence of y1 at timestep i on itself at timestep i+1
#' @param Ry2y2 Parameter controlling the influence of y2 at timestep i on itself at timestep i+1
#' @param Ry3y3 Parameter controlling the influence of y1 at timestep i on itself at timestep i+1
#' @param Ry4y4 Parameter controlling the influence of y1 at timestep i on itself at timestep i+1
#' @param Ry1y2 Parameter controlling the influence of y1 at timestep i on y2 at timestep i+1
#' @param Ry2y3 Parameter controlling the influence of y2 at timestep i on y3 at timestep i+1
#' @param Ry3y4 Parameter controlling the influence of y3 at timestep i on y4 at timestep i+1
#' @add.timestep Whether or not to add a timestep column to the returned data frame.
#'     Defaults to FALSE.
#' @plot Whether to plot the dataset as a scatterplot matrix. Defaults to FALSE
#' @return A dataframe containing columns time series for y1, y2, y3 and y4
#'     (and optionally "t", a timestep column).
#'
#' Defaults values are as in the original paper.
logistic_map_fourspecies_transitive <- function (n = 3000,
                                     y1.i = 0.4, y2.i = 0.4,
                                     y3.i = 0.4, y4.i = 0.4,
                                     Ry1y1 = 3.9, Ry1y2 = 0.4,
                                     Ry2y2 = 3.6, Ry2y3 = 0.4,
                                     Ry3y3 = 3.6, Ry3y4 = 0.35,
                                     Ry4y4 = 3.8,
                                     add.timestep = FALSE,
                                     plot = FALSE) {
  y1 = vector(length = n)
  y2 = vector(length = n)
  y3 = vector(length = n)
  y4 = vector(length = n)
  y1[1] = y1.i
  y2[1] = y2.i
  y3[1] = y3.i
  y4[1] = y4.i

  for (i in 2:n) {
    y1[i] = y1[i - 1] * (3.9 - Ry1y1 * y1[i - 1])
    y2[i] = y2[i - 1] * (3.6 - Ry1y2 * y1[i - 1] - Ry2y2 * y2[i - 1])
    y3[i] = y3[i - 1] * (3.6 - Ry2y3 * y2[i - 1] - Ry3y3 * y3[i - 1])
    y4[i] = y4[i - 1] * (3.8 - Ry3y4 * y3[i - 1] - Ry4y4 * y4[i - 1])
  }

  if (add.timestep) {
    timestep = 1:n
    df = data.frame(timestep, y1, y2, y3, y4)
    names(df) = c("t", "y1", "y2", "y3", "y4")
  } else {
    df = data.frame(y1, y2, y3, y4)
    names(df) = c("y1", "y2", "y3", "y4")
  }
  if (plot) plot(df, cex = 0.2, col = grDevices::rgb(0.5, 0.8, 0.9, 0.7))
  return(df)
}
