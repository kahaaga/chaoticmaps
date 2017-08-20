#' The tinkerbell map. From the Wikipedia article on chaotic maps.
#'
#' @param n Number of points in the time series.
#' @param xi The initial value for x
#' @param yi The initival value for y
#' @param a The value of the parameter a
#' @param b The value of the parameter b
#' @param c The value of the parameter c
#' @param d The value of the parameter d
#' @param add.timestep Add a column indicating the time step?
#' @param plot Plot the time series?
#' @return A n-by-2 data frame containing the values of the x and y components.
#' @export
tinkerbell_map <- function(n = 1000,
                          xi = -0.74,
                          yi = -0.56,
                          a = 0.9,
                          b = -0.6,
                          c = 2,
                          d = 0.5,
                          add.timestep = FALSE,
                          plot = FALSE) {
  x = vector(length = n)
  y = vector(length = n)
  x[1] = xi
  y[1] = yi


  for (i in 2:n) {
    x[i] = x[i - 1]^2 - y[i - 1]^2 + a * x[i - 1] + b * y[i - 1]
    y[i] = 2 * x[i - 1] * y[i - 1] + c * x[i - 1] + d * y[i - 1]
  }

  if (add.timestep) {
    timestep = 1:n
    df = data.frame(timestep, x, y)
    names(df) = c("t", "x", "y")
  } else {
    df = data.frame(x, y)
    names(df) = c("x", "y")
  }

  if (plot & add.timestep) {
    p = ggplot2::ggplot() +
      ggplot2::geom_line(data = df, mapping = ggplot2::aes(x = t, y = x, col = "x")) +
      ggplot2::geom_line(data = df, mapping = ggplot2::aes(x = t, y = y, col = "y")) +
      ggplot2::theme_bw() +
      ggplot2::ylab("value") +
      ggplot2::ggtitle("Tinkerbell map")
    print(p)
  } else if (plot & !add.timestep) {
    warning("cannot plot without adding timestep.")
  }
  return(df)
}
