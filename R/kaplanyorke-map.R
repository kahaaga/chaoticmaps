#' The tinkerbell map. From the Wikipedia article on chaotic maps.
#'
#' @param n Number of points in the time series.
#' @param xi The initial value for x
#' @param yi The initival value for y
#' @param a The value of the parameter a
#' @param b A large prime number
#' @param add.timestep Add a column indicating the time step?
#' @param plot Plot the time series?
#' @return A n-by-2 data frame containing the values of the x and y components.
#' @export
kaplanyorke_map <- function(n = 1000,
                           xi = 0.5,
                           yi = 0.4,
                           b = 104123,
                           alpha = 0.01,
                           add.timestep = FALSE,
                           plot = FALSE) {
  a = vector(length = n)
  x = vector(length = n)
  y = vector(length = n)
  a[1] = alpha
  x[1] = xi
  y[1] = yi

  for (i in 2:n) {
    a[i] = 2 * a[i - 1] %% b
    x[i] = a[i - 1] / b
    y[i] = alpha * y[n - 1] + cos(4 * pi * x[i - 1])
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
      ggplot2::ggtitle("Kaplan-Yorke map")
    print(p)
  } else if (plot & !add.timestep) {
    warning("cannot plot without adding timestep.")
  }
  return(df)
}
