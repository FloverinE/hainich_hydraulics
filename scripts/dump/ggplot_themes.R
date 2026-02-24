plot_theme <- theme_bw() +
  theme(
    legend.position = "bottom",
    legend.spacing.x = unit(0.7, 'cm'),
    axis.text = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 13),
    legend.key.width = unit(1.5, "cm", data = NULL),
    plot.tag = element_text(size = 18),
    # panel.grid = element_line(
    #   color = "grey",
    #   size = 0.25,
    #   linetype = 1
    # )
  )

presentation_theme <- 
  plot_theme +
  theme(plot.background = element_rect(fill = "transparent"),
        plot.title = element_text(size = 19),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 15, hjust = 0),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent"))

thesis_theme <- theme_bw() +
  theme(
    legend.position = "bottom",
    legend.spacing.x = unit(0.7, 'cm'),
    # aspect.ratio = 1,
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.key.width = unit(1, "cm", data = NULL),
    strip.background = element_blank(),
    plot.tag = element_text(size = 15)
    # panel.grid = element_line(
    #   color = "grey",
    #   size = 0.25,
    #   linetype = 1
    # )
  )


ft_theme <- function(ft) {
  ft |>
    hline(part = "header") |>
    # vline_left() |>
    # vline_right() |>
    fontsize(part = "header", size = 10) |>
    fontsize(part = "body", size = 9) |>
    font(fontname = "Times New Roman", part = "all") |>
    line_spacing(part = "body", space = .15) |>
    line_spacing(part = "header", space = .3) |>
    fix_border_issues() |>
    set_table_properties(layout = "autofit")
}

