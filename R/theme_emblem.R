#' ggplot color theme based on Willis Towers Watson's Emblem
#'
#' A theme that approximates the style of \emph{Emblem}
#'
#' \code{theme_emblem} implements the standard bluish-gray background and yellow
#' legend theme in \emph{Emblem}
#'
#' @inheritParams ggplot2::theme_grey
#'
#' @return An object of class \code{\link[ggplot2]{theme}()}
#'
#' @export
#' @family themes emblem
#'
#' @references \itemize{
#' \item \href{https://www.towerswatson.com/en-US/Services/Tools/emblem}{Emblem}
#' \item \url{https://www.willistowerswatson.com/en-US/about-us/overview}
#' }
#'
theme_emblem <- function (base_size = 10,
                          base_family = "sans",
                          base_line_size = 0.5,
                          base_rect_size = 0.5)
{
  emblem_blue <- "#F1F7FF"
  half_line <- base_size/2

  theme(line = element_line(colour = "black",
                            size = base_line_size,
                            linetype = 1,
                            lineend = "butt"),
        rect = element_rect(fill = emblem_blue,
                            colour = "black",
                            size = base_rect_size,
                            linetype = 1),
        text = element_text(family = base_family,
                            face = "plain",
                            colour = "black",
                            size = base_size,
                            lineheight = 0.9,
                            hjust = 0.5,
                            vjust = 0.5,
                            angle = 0,
                            margin = margin(),
                            debug = FALSE),

        axis.line = element_blank(),
        axis.line.x = element_line(colour = "grey30", size = base_line_size, linetype = 1, lineend = "butt"),
        axis.line.y = element_line(colour = "grey30", size = base_line_size, linetype = 1, lineend = "butt"),
        axis.text = element_text(size = rel(0.8), colour = "grey30"),
        axis.text.x = element_text(margin = margin(t = 0.8 * half_line/2), vjust = 1),
        axis.text.x.top = element_text(margin = margin(b = 0.8 * half_line/2), vjust = 0),
        axis.text.y = element_text(margin = margin(r = 0.8 * half_line/2), hjust = 1),
        axis.text.y.right = element_text(margin = margin(l = 0.8 * half_line/2), hjust = 0),
        axis.ticks = element_line(colour = "grey30"),
        axis.ticks.length = unit(half_line/2, "pt"),
        axis.title.x = element_text(size = rel(.8), margin = margin(t = half_line/2), vjust = 1),
        axis.title.x.top = element_text(margin = margin(b = half_line/2), vjust = 0),
        axis.title.y = element_text(angle = 90, margin = margin(r = half_line/2), vjust = 1),
        axis.title.y.right = element_text(angle = -90, margin = margin(l = half_line/2), vjust = 0),

        legend.background = element_rect(fill = "lightyellow", linetype=1, color = "black"),
        legend.spacing = unit(2 * half_line, "pt"),
        legend.spacing.x = NULL,
        legend.spacing.y = NULL,
        legend.margin = margin(half_line, half_line, half_line, half_line),
        legend.key = element_rect(fill = "lightyellow", colour = NA),
        legend.key.size = unit(1.2, "lines"),
        legend.key.height = NULL,
        legend.key.width = NULL,
        legend.text = NULL,
        legend.text.align = NULL,
        legend.title = element_blank(),
        legend.title.align = NULL,
        legend.position = "right",
        legend.direction = NULL,
        legend.justification = "center",
        legend.box = NULL,
        legend.box.margin = margin(0, 0, 0, 0, "cm"),
        legend.box.background = NULL,
        legend.box.spacing = unit(2 * half_line, "pt"),

        panel.background = element_rect(fill = "white", colour = NA),
        panel.border = element_rect(color="black", fill = NA),
        panel.grid = element_line(colour = "grey80", linetype = "dashed"),
        panel.grid.major = element_line(colour = "grey80", linetype = "dashed"),
        panel.grid.minor = element_line(colour = "grey80", linetype = "dashed"),
        panel.spacing = unit(half_line, "pt"),
        panel.spacing.x = NULL,
        panel.spacing.y = NULL,
        panel.ontop = FALSE,

        strip.background = element_rect(fill = "grey80", colour = NA),
        strip.text = element_text(colour = "grey10",
                                  size = rel(0.8),
                                  margin = margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)),
        strip.text.x = NULL,
        strip.text.y = element_text(angle = -90),
        strip.placement = "inside",
        strip.placement.x = NULL,
        strip.placement.y = NULL,
        strip.switch.pad.grid = unit(half_line/2, "pt"),
        strip.switch.pad.wrap = unit(half_line/2, "pt"),

        plot.background = element_rect(colour = "white"),
        plot.title = element_text(size = rel(1), hjust = 0.5, vjust = 1, margin = margin(b = half_line)),
        plot.subtitle = element_text(size = rel(0.8), hjust = 0.5, vjust = 1, margin = margin(b = half_line)),
        plot.caption = element_text(size = rel(0.8), hjust = 1, vjust = 1, margin = margin(t = half_line)),
        plot.tag = element_text(size = rel(1.2), hjust = 0.5, vjust = 0.5),
        plot.tag.position = "topleft",
        plot.margin = margin(half_line, half_line, half_line, half_line),
        complete = TRUE)
}
