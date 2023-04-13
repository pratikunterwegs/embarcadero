#' @title A plot tool for riBART models
#'
#' @description
#'
#' Tiny code for tiny task.
#'
#' @param model Object of class rbart
#' @param temporal Is this yearly data (line plot) or categorical
#'
#' @return Returns a plot
#'
#' @export
#'
#'
#'
plot.ri <- function(model, temporal = TRUE) {
  if (temporal == TRUE) {
    df <- model$ranef %>%
      tibble::as_tibble() %>%
      gather() %>%
      group_by(key) %>%
      summarise(
        mean = mean(value),
        upper = quantile(value, 0.975, na.rm = TRUE),
        lower = quantile(value, 0.025, na.rm = TRUE)
      ) %>%
      dplyr::mutate(key = as.numeric(key))

    p <- ggplot2::ggplot(df, ggplot2::aes(x = key, y = mean, ymin = lower, ymax = upper)) +
      ggplot2::geom_ribbon(fill = "grey95") +
      ggplot2::geom_line(y = 0, lty = 2, col = "grey20") +
      ggplot2::geom_line(color = "#00AFDD", lwd = 1.35) +
      # ggplot2::geom_point(color="#00AFDD", cex=1.5) +
      xlab(NULL) +
      ylab("Intercept") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),
        axis.title.x = ggplot2::element_text(size = rel(1.3), vjust = -0.8),
        axis.text.y = ggplot2::element_text(size = rel(1.4)),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.grid.minor = ggplot2::element_blank()
      )
  } else {
    p <- model$ranef %>%
      data.frame() %>%
      gather() %>%
      group_by(key) %>%
      summarise(
        mean = mean(value),
        upper = quantile(value, 0.975, na.rm = TRUE),
        lower = quantile(value, 0.025, na.rm = TRUE)
      )

    # transform(key = reorder(key, mean)) %>%
    p <-
      ggplot2::ggplot(p, ggplot2::aes(x = key, y = mean)) +
      ggplot2::geom_pointrange(
        ggplot2::aes(y = mean, x = key, ymin = lower, ymax = upper),
        color = "#00AFDD"
      ) +
      xlab(NULL) +
      ylab("Intercept") +
      coord_flip() +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(angle = 90),
        axis.title.x = ggplot2::element_text(size = rel(1.3), vjust = -0.8),
        axis.text.y = ggplot2::element_text(size = rel(1.4)),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(
          color = "grey",
          linetype = "dashed"
        )
      )
  }
  print(p)
}
