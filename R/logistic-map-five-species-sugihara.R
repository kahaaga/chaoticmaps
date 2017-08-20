
#' Time series for a two-species model system. Unidirectional forcing from x introduces synchrony in y.
#'
#' Source: Sugihara, G., May, R., Ye, H., Hsieh, C. H., Deyle, E., Fogarty, M., & Munch, S. (2012).
#' Detecting causality in complex ecosystems. science, 338(6106), 496-500.
#'
#' @param n The number of time steps that will be generated.
#' @param y1 Initial value of time series y1.
#' @param y2 Initial value of time series y2.
#' @param y3 Initial value of time series y3.
#' @param y4 Initial value of time series y4.
#' @param y5 Initial value of time series y5.
#' @param Ry1y1 Parameter controlling the influence of y1 at timestep i on itself at timestep i+1.
#' @param Ry2y2 Parameter controlling the influence of y2 at timestep i on itself at timestep i+1.
#' @param Ry3y3 Parameter controlling the influence of y3 at timestep i on itself at timestep i+1.
#' @param Ry4y4 Parameter controlling the influence of y4 at timestep i on itself at timestep i+1.
#' @param Ry5y5 Parameter controlling the influence of y5 at timestep i on itself at timestep i+1.
#' @param Ry1y2 Parameter controlling the influence of y1 at timestep i on on y2 at timestep i+1.
#' @param Ry1y3 Parameter controlling the influence of y1 at timestep i on on y3 at timestep i+1.
#' @param Ry1y4 Parameter controlling the influence of y1 at timestep i on on y4 at timestep i+1.
#' @param Ry1y5 Parameter controlling the influence of y1 at timestep i on on y5 at timestep i+1.
#' @param Ry2y1 Parameter controlling the influence of y2 at timestep i on on y1 at timestep i+1.
#' @param Ry2y3 Parameter controlling the influence of y2 at timestep i on on y3 at timestep i+1.
#' @param Ry2y4 Parameter controlling the influence of y2 at timestep i on on y4 at timestep i+1.
#' @param Ry2y5 Parameter controlling the influence of y2 at timestep i on on y5 at timestep i+1.
#' @param Ry3y1 Parameter controlling the influence of y3 at timestep i on on y1 at timestep i+1.
#' @param Ry3y2 Parameter controlling the influence of y3 at timestep i on on y2 at timestep i+1.
#' @param Ry3y4 Parameter controlling the influence of y3 at timestep i on on y4 at timestep i+1.
#' @param Ry3y5 Parameter controlling the influence of y3 at timestep i on on y5 at timestep i+1.
#' @param Ry4y1 Parameter controlling the influence of y4 at timestep i on on y1 at timestep i+1.
#' @param Ry4y2 Parameter controlling the influence of y4 at timestep i on on y2 at timestep i+1.
#' @param Ry4y3 Parameter controlling the influence of y4 at timestep i on on y3 at timestep i+1.
#' @param Ry4y5 Parameter controlling the influence of y4 at timestep i on on y5 at timestep i+1.
#' @add.timestep Whether or not to add a timestep column to the returned data frame.
#'     Defaults to FALSE.
#' @plot Whether to plot the dataset as a scatterplot matrix. Defaults to FALSE
#' @return A dataframe containing columns time series for x and y
#'     (and optionally "t", a timestep column).
#'
#' Subsystem y1, y2 and y3 is the forcing subsystem, that interacts unidirectionally
#' with y4 and y5. y4 and y5 do not interact with each other and do not influence y1, y2 and
#' y3. Initial conditions are not provided in the original paper, but are all set to 0.4 here.
logistic_map_fivespecies_sugihara <- function (n = 3000,
                                 y1.i = 0.4, y2.i = 0.4, y3.i = 0.4, y4.i = 0.4, y5.i = 0.4,
                                 Ry1y1 = 4.00, Ry1y2 = 0.31, Ry1y3 = 0.636, Ry1y4 = 0.111, Ry1y5 = 0.082,
                                 Ry2y2 = 3.10, Ry2y1 = 2.00, Ry2y3 = 0.636, Ry2y4 = 0.011, Ry2y5 = 0.111,
                                 Ry3y3 = 2.12, Ry3y1 = 0.40, Ry3y2 = 0.930, Ry3y4 = 0.131, Ry3y5 = 0.125,
                                 Ry4y4 = 3.80, Ry4y5 = 4.10,
                                 Ry5y5 = 4.10,
                                 add.timestep = FALSE,
                                 plot = FALSE) {
  y1 = vector(length = n)
  y2 = vector(length = n)
  y3 = vector(length = n)
  y4 = vector(length = n)
  y5 = vector(length = n)

  y1[1] = y1.i
  y2[1] = y2.i
  y3[1] = y3.i
  y4[1] = y4.i
  y5[1] = y5.i

  for (i in 2:n) {
    y1[i] = y1[i - 1] * (Ry1y1 - Ry1y1 * y1[i - 1] - Ry2y1 * y2[i - 1] - Ry3y1 * y3[i - 1])
    y2[i] = y2[i - 1] * (Ry2y2 - Ry1y2 * y1[i - 1] - Ry2y2 * y2[i - 1] - Ry3y1 * y3[i - 1])
    y3[i] = y3[i - 1] * (Ry3y3 + Ry1y3 * y1[i - 1] + Ry2y3 * y2[i - 1] - Ry3y3 * y3[i - 1])
    y4[i] = y4[i - 1] * (Ry4y4 - Ry1y4 * y1[i - 1] - Ry2y4 * y2[i - 1] + Ry3y4 * y4[i - 1] - Ry4y4 * y4[i - 1])
    y5[i] = y5[i - 1] * (Ry5y5 - Ry1y5 * y1[i - 1] - Ry2y5 * y2[i - 1] - Ry3y5 * y3[i - 1] - Ry5y5 * y5[i - 1])
  }

  if (add.timestep) {
    timestep = 1:n
    df = data.frame(timestep, y1, y2, y3, y4, y5)
    names(df) = c("t", "y1", "y2", "y3", "y4", "y5")
  } else {
    df = data.frame(y1, y2, y3, y4, y5)
    names(df) = c("y1", "y2", "y3", "y4", "y5")
  }
  if (plot) plot(df, cex = 0.2, col = grDevices::rgb(0.5, 0.8, 0.9, 0.7))
  return(df)
}
