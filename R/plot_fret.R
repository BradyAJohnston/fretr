#' Create a FRET-Distance plot from a given R0
#'
#' @param R0 R0 For the dye-pair.
#' @param dis_range Range of distances in angstroms over which to create the
#'   plot.
#'
#' @return `ggplot2` plot.
#' @export
#'
#' @examples
#' library(fretr)
#' plot_fret(70, seq(150))
plot_fret <- function(R0 = 54, dis_range = 1:120) {
  df <- data.frame(
    dis = dis_range,
    fret = fretr::pred_fret(dis_range, R0 = R0)
  )

  ggplot2::ggplot(df, ggplot2::aes(dis, fret)) +
    ggplot2::geom_line() +
    ggplot2::theme_light() +
    ggplot2::geom_line(
      data = data.frame(
        x = c(min(df$dis), R0, R0),
        y = c(0.5, 0.5, 0)
      ),
      ggplot2::aes(x = x, y = y),
      linetype = "dashed",
      alpha = 0.8
    ) +
    ggplot2::annotate(
      "label",
      x = R0,
      y = 0.5,
      hjust = 1.1,
      vjust = 1.3,
      label = paste("R0 =", R0)
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(c(0.01, 0.01)),
      breaks = scales::pretty_breaks()
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(c(0.01, 0.05))
    ) +
    ggplot2::labs(x = "Distance",
                  y = "FRET") +
    ggplot2::theme(
      panel.grid = ggplot2::element_line(color = "gray80")
    )

}

