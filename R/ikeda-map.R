#' Ikeda map.
#'
#' K.Ikeda, Multiple-valued Stationary State and its Instability of the Transmitted Light by a Ring Cavity System, Opt. Commun. 30 257-261 (1979)
#'
#' The Ikeda map. From Ikeda, H. Daido and O. Akimoto, Optical Turbulence:
#' Chaotic Behavior of Transmitted Light from a Ring Cavity,
#' Phys. Rev. Lett. 45, 709–712 (1980).
#'
#' @param n Number of points in the time series.
#' @param xi The initial value for x
#' @param yi The initival value for y
#' @param u The value of the parameter u
#' @param add.timestep Add a column indicating the time step?
#' @param plot Plot the time series?
#' @return A n-by-2 data frame containing the values of the x and y components.
#' @export
ikeda_map <- function(n = 1000,
                     xi = -5,
                     yi = 0.1,
                     u = 0.6,
                     add.timestep = FALSE,
                     plot = FALSE) {
  x = vector(length = n)
  y = vector(length = n)
  t = vector(length = n)
  x[1] = xi
  y[1] = yi
  t[1] = 0.4 - (6 / (1 + x[1]^2 + y[1]^2))

  for (i in 2:n) {
    t[i] = 0.4 - (6 / (1 + x[i - 1]^2 + y[i - 1]^2))
    x[i] = 1 + u*(x[i - 1] * cos(t[i - 1] - y[i - 1] * sin(t[i - 1])))
    y[i] = u * (x[i - 1] * sin(t[i - 1] + y[i - 1] * cos(t[i - 1])))
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
      ggplot2::ggtitle("Ikeda map")
    print(p)
  } else if (plot & !add.timestep) {
    warning("cannot plot without adding timestep.")
  }
  return(df)
}
