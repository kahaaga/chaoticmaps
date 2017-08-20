#' The Henon map.
#'
#' @param n Number of points in the time series.
#' @param xi The initial value for x
#' @param yi The initival value for y
#' @param a The value of the parameter a
#' @param b The value of the parameter b
#' @param add.timestep Add a column indicating the time step?
#' @param plot Plot the time series?
#' @return A n-by-2 data frame containing the values of the x and y components.
#' @export
henon_map <- function(n=1000,
                     xi = 1,
                     yi = 1,
                     a = 1.4,
                     b = 0.3,
                     add.timestep = FALSE,
                     plot = FALSE) {
  x = vector(length = n)
  y = vector(length = n)
  x[1] = xi
  y[1] = yi

  for (i in 2:n) {
    x[i] = 1 - a * (x[i - 1]^2) + y[i - 1] #- sin(y[i-1]*b)/b*0.1
    y[i] = b * x[i - 1]
  }

  if (add.timestep) {
    timestep = 1:n
    df = data.frame(timestep, x, y)
    names(df) = c("t", "x", "y")
  } else {
    df = data.frame(x,y)
    names(df) = c("x", "y")
  }

  if (plot & add.timestep) {
    p = ggplot2::ggplot() +
      ggplot2::geom_line(data = df, mapping = ggplot2::aes(x = t, y = x, col = "x")) +
      ggplot2::geom_line(data = df, mapping = ggplot2::aes(x = t, y = y, col = "y")) +
      ggplot2::theme_bw() +
      ggplot2::ylab("value") +
      ggplot2::ggtitle("Henon map")
    print(p)
  } else if (plot & !add.timestep) {
    warning("cannot plot without adding timestep.")
  }
  return(df)
}
