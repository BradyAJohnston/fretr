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


#' Create a Transition Density Plot
#'
#' @param data Data frame with relevant information.
#' @param from Column that contains from_state data.
#' @param to Column that contains to_state data.
#' @param nbins Number of bins for rasterisation of density.
#' @param contour Logical for the inclusion of contours.
#'
#' @return `ggplot` object.
#' @export
#'
plot_tdp <- function(data, from, to, nbins = 100, contour = FALSE) {
  data %>%
    ggplot2::ggplot(aes({{ from }}, {{ to }})) +
    ggplot2::stat_density_2d(
      ggplot2::aes(
        fill = ggplot2::after_stat(ndensity),
        alpha = ggplot2::after_stat(ndensity)
        ),
      geom = "raster",
      n = nbins,
      contour = contour,
      h = c(0.2, 0.2)
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 1, 0.2),
      expand = ggplot2::expansion()
      ) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 1, 0.2),
      expand = ggplot2::expansion()
      ) +
    ggplot2::scale_fill_viridis_c(option = "B", direction = 1) +
    ggplot2::scale_alpha_continuous(range = c(0.4, 1)) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::guides(alpha = "none", fill = "none") +
    ggplot2::labs(x = "FROM", y = "TO") +
    ggplot2::theme_light() +
    ggplot2::theme(
      aspect.ratio = 1,
      panel.grid = ggplot2::element_line(colour = "gray70"),
      strip.background = ggplot2::element_rect(fill = "gray30"),
      panel.grid.minor = ggplot2::element_blank()
    )
}

#' Plot Chromatograms of Raw don & acc and FRET Data
#'
#' @param data Data frame containing columns for time, don, acc, and fret
#'
#' @return `ggplot` object.
#' @export
#'
plot_fret_chrom <- function(data) {
  data <- data %>%
    tidyr::pivot_longer(c(fret, don, acc))

  fret_plot <- data %>%
    dplyr::filter(name == "fret") %>%
    ggplot2::ggplot(ggplot2::aes(time, value)) +
    ggplot2::geom_line(ggplot2::aes(colour = name)) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 1, 0.2),
      name = "FRET"
    ) +
    ggplot2::coord_cartesian(
      ylim = c(0, 1)
    ) +
    ggplot2::scale_x_continuous(
      name = "Time (s)",
      expand = ggplot2::expansion(c(0.02, 0.02))
    ) +
    ggplot2::theme_light() +
    ggplot2::scale_colour_manual(values = trace_colours) +
    ggplot2::guides(colour = "none") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(b = 0)
    )

  raw_plot <- data %>%
    dplyr::filter(name != "fret") %>%
    ggplot2::ggplot(ggplot2::aes(time, value)) +
    ggplot2::geom_line(ggplot2::aes(colour = name)) +
    ggplot2::scale_y_continuous(
      name = "Fluorescence"
    ) +
    ggplot2::coord_cartesian(
      ylim = c(0, NA)
    ) +
    ggplot2::scale_x_continuous(
      name = "Time (s)",
      expand = ggplot2::expansion(c(0.02, 0.02))
    ) +
    ggplot2::guides(colour = "none") +
    ggplot2::theme_light() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 0)
    ) +
    ggplot2::scale_colour_manual(values = trace_colours)


  patchwork::wrap_plots(
    fret_plot, raw_plot,
    ncol = 1
  )
}
