#' @title Variable importance plot
#'
#' @description
#'
#' Variable importance, as measured in the proportion of total branches used
#' for a given variable. Error bars show standard deviation across iterations.
#'
#' Variables that are explicitly dropped in the model are included as "0" in
#' the variable importance table, but not plotted; variables that are included
#' but used zero times are shown in both.
#'
#' @param model The dbarts model object
#' @param plots Turn this on for a nice variable contribution plot
#'
#'
#' @export
#'
#'

varimp <- function(model, plots = FALSE) {
  if (!("fit" %in% names(model))) {
    stop("Please add \", keeptrees=TRUE\" to your dbarts model call")
  }
  if (inherits(model, "rbart")) {
    basenames <- unlist(attr(model$fit[[1]]$data@x, "drop"))
    names <- names(which(basenames == FALSE))
    varimps <- rowMeans(model$varcount / colSums(model$varcount))
    fitobj <- model$fit[[1]]
  }
  if (inherits(model, "bart")) {
    basenames <- unlist(attr(model$fit$data@x, "drop"))
    names <- names(which(basenames == FALSE))
    varimps <- colMeans(model$varcount / rowSums(model$varcount))
    fitobj <- model$fit
  }

  var.df <- data.frame(names, varimps)

  missing <- attr(fitobj$data@x, "term.labels")[
    !(attr(fitobj$data@x, "term.labels") %in%
      names(unlist(attr(fitobj$data@x, "drop"))))
  ]

  if (length(missing) > 0) {
    message(
      "dbarts auto-dropped this variable(s). ",
      " You will probably want to remove it"
    )
    message(paste(missing, collapse = " "), " \n")
  }

  if (length(missing) > 0) {
    missing.df <- data.frame(names = missing, varimps = 0)
    var.df <- rbind(var.df, missing.df)
  }

  var.df$names <- factor(var.df$names)
  var.df <- transform(var.df, names = reorder(
    names,
    -varimps
  ))

  if (plots == TRUE) {
    # g1 <- ggplot2::ggplot(var.df, ggplot2::aes(y=varimps, x=names)) +
    #  ggplot2::geom_bar(stat="identity", color="black") +
    #  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45)) +
    #  ylab("Relative importance") + ggplot2::theme_bluewhite()
    # print(g1)

    if (inherits(model, "rbart")) {
      rel <- t(model$varcount / colSums(model$varcount))
    } else {
      rel <- model$varcount / rowSums(model$varcount)
    }
    colnames(rel) <- names

    p <- rel %>%
      data.frame() %>%
      gather() %>%
      group_by(key) %>%
      summarise(
        mean = mean(value),
        sd = sd(value, na.rm = TRUE)
      ) %>%
      transform(Var = reorder(key, mean))

    p <- ggplot2::ggplot(p, ggplot2::aes(x = Var, y = mean)) +
      ggplot2::geom_pointrange(
        ggplot2::aes(y = mean, x = Var, ymin = mean - sd, ymax = mean + sd),
        color = "#00AFDD"
      ) +
      xlab(NULL) +
      ylab("Variable importance") +
      coord_flip() +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "none",
        axis.title.x = ggplot2::element_text(size = rel(1.3), vjust = -0.8),
        axis.text.y = ggplot2::element_text(size = rel(1.4)),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_line(
          color = "grey",
          linetype = "dashed"
        )
      )

    print(p)
  }

  return(var.df)
}
