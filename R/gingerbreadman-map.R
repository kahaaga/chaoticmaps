#' Gingerbreadman map.
#'
#' https://en.wikipedia.org/wiki/Gingerbreadman_map
#' Devaney, Robert L. (1988), "Fractal patterns arising in chaotic dynamical systems",
#' in Peitgen, Heinz-Otto; Saupe, Dietmar, The Science of Fractal Images, Springer-Verlag,
#' pp. 137–168, doi:10.1007/978-1-4612-3784-6_3. See in particular Fig. 3.3.
#'
#' @param n Number of points in the time series.
#' @param xi The initial value for x
#' @param yi The initival value for y
#' @param add.timestep Add a column indicating the time step?
#' @param plot Plot the time series?
#' @return A dataframe containing columns time series for x and y
#'     (and optionally "t", a timestep column).
#' @return A n-by-2 data frame containing the values of the x and y components.
#' @export
gingerbreadman_map <- function(n = 1000,
                              xi = -0.2,
                              yi = 0.18,
                              add.timestep = FALSE,
                              plot = FALSE) {
  x = vector(length = n)
  y = vector(length = n)
  x[1] = xi
  y[1] = yi

  for (i in 2:n) {
    x[i] = 1- y[i-1] + abs(x[i-1])
    y[i] = x[i-1]
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
    p = ggplot2::ggplot(df) +
      ggplot2::geom_line(data = df, mapping = ggplot2::aes(x = t, y = x, col = "x")) +
      ggplot2::geom_line(data = df, mapping = ggplot2::aes(x = t, y = y, col = "y")) +
      ggplot2::theme_bw() +
      ggplot2::ylab("value") +
      ggplot2::ggtitle("Gingerbreadman map")
    print(p)
  } else if (plot & !add.timestep) {
    warning("cannot plot without adding timestep.")
  }
  return(df)
}
