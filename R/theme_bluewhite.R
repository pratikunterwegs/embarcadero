#' @export

ggplot2::theme_bluewhite <- function(base_size = 11, base_family = "") {
  ggplot2::theme_classic() %+replace%
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "white"),
      panel.background = ggplot2::element_rect(fill = "lightblue"),
      panel.border = ggplot2::element_rect(color = "black", fill = NA),
      axis.line = ggplot2::element_line(color = "black"),
      axis.ticks = ggplot2::element_line(color = "black")
    )
}
