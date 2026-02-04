#' Compute and Plot Pareto Front
#'
#' This function identifies the Pareto front in a dataset based on computation time and SPE,
#' and generates a plot to visualize the Pareto front.
#'
#' @param data A data frame with columns for time, SPE, nlambda, and thresh.
#' @param T_hope A numeric value representing the desired computation time threshold. Default is 20.
#' @param line logical(1). If TRUE, the Pareto-front points in the plot are　connected with a polyline (frontier line). If FALSE, only the points are shown. Default is TRUE.
#' @return A list containing:
#' \item{Pareto_front}{A ggplot object visualizing the Pareto front.}
#' \item{data}{A data frame containing the Pareto status of each configuration.}
#' \item{best_configuration}{A data frame with the best `nlambda` and `thresh`.}
#'
#' @import dplyr
#' @import ggplot2
#' @keywords internal
#' @export
#'
#'
cp_pareto_front <- function(data, T_hope=20, line=FALSE) {
  data <- as.data.frame(data)
  colnames(data) <- c("Time", "SPE", "params_nlambda", "params_thresh")

  df <- data %>%
    rowwise() %>%
    mutate(
      Type = if_else(
        any(SPE > data$SPE & Time > data$Time),
        "Not Pareto Front",
        "Pareto Front"
      )
    ) %>%
    ungroup()


  # Solve teh best configuration
  cand_idx <- which(df$Type == "Pareto Front" & df$Time <= T_hope)
  if (length(cand_idx) >= 1) {
    # Case 1: constrained candidates exist → choose best accuracy
    best_i <- cand_idx[ which.min(df$SPE[cand_idx]) ]
    df$Type[best_i] <- "Best Configuration"
  }else {
    # Case 2: no candidate meets Time <= T_hope → choose the fastest on the Pareto front
    cand_idx2 <- which(df$Type == "Pareto Front")
    if (length(cand_idx2) >= 1) {
      # order by Time (asc), then SPE (asc) as a tie-breaker
      ord <- order(df$Time[cand_idx2], df$SPE[cand_idx2])
      best_i <- cand_idx2[ ord[1] ]
      df$Type[best_i] <- "Best Configuration"
    } else {
      best_i <- integer(0)  # no Pareto points; keep as-is
    }
  }

  best_configuration <- data.frame(
    nlambda = df$params_nlambda[best_i],
    thresh = df$params_thresh[best_i]
  )

  df$Type <- factor(df$Type, levels = c("Not Pareto Front", "Pareto Front", "Best Configuration"))

  df_front <- df %>%
    dplyr::filter(Type %in% c("Pareto Front", "Best Configuration")) %>%
    dplyr::arrange(SPE)


  # Plot Pareto front
  p <- ggplot() +
    geom_hline(yintercept = T_hope, linetype = "dashed", color = "black", linewidth = 1) +
    geom_point(
      data = df %>% filter(Type != "Best Configuration"),
      aes(x = SPE, y = Time, fill = Type, shape = Type, alpha = Type, size = Type),
      color  = "black",
      stroke = 1
    ) +
    # Line of Pareto front
    {if (line) geom_path(
      data = df_front,
      aes(x = SPE, y = Time),
      linewidth = 1,
      color = "#2171B5",
      alpha = 0.9
    )} +
    # Best Configuration
    geom_point(
      data = df %>% filter(Type == "Best Configuration"),
      aes(x = SPE, y = Time, fill = Type, shape = Type, size = Type),
      color  = "black",
      stroke = 1,
      show.legend = TRUE
    ) +
    # Settings of scale
    scale_fill_manual(
      name = "Point Category",
      values = c(
        "Not Pareto Front"     = "#B0B0B0",
        "Pareto Front"         = "#2171B5",
        "Best Configuration" = "#D73027"
      )
    ) +
    scale_shape_manual(
      name ="Point Category",
      values = c(
        "Not Pareto Front"     = 21,
        "Pareto Front"         = 21,
        "Best Configuration" = 24
      )
    ) +
    scale_size_manual(
      values = c(
        "Not Pareto Front"     = 4,
        "Pareto Front"         = 4,
        "Best Configuration" = 8
      ),
      guide = "none"
    ) +
    scale_alpha_manual(
      values = c(
        "Not Pareto Front"     = 0.3,
        "Pareto Front"         = 1,
        "Best Configuration" = 1
      ),
      guide = "none"
    ) +
    guides(
      fill  = guide_legend(override.aes = list(size = 6)),
      shape = guide_legend(override.aes = list(size = 6))
    ) +
    # Labels of axis
    labs(x = "SPE", y = "Time", title = "") +
    theme(
      axis.title = element_text(size = 24),
      axis.text  = element_text(size = 20),

      legend.title = element_text(size = 20),
      legend.text  = element_text(size = 20),
      legend.position        = "inside",
      legend.position.inside = c(0.95, 0.95),
      legend.justification   = c("right", "top"),
      legend.background      = element_rect(fill = alpha("white", 0.6), color = "black", linewidth = 0.5),

      panel.background = element_rect(fill = "white", color = NA),
      panel.border     = element_rect(color = "black", fill = NA, linewidth = 1)
    ) +
    annotate(
      "text",
      x = max(df$SPE, na.rm = TRUE),
      y = T_hope,
      label = paste("T_hope =", T_hope),
      hjust = 1.1,
      vjust = -0.5,
      size  = 7
    )

  return(list(Pareto_front=p, data = df, best_configuration=best_configuration))
}

