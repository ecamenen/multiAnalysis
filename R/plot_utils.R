options(ggrepel.max.overlaps = Inf)

#' @export
theme_perso0 <- function(p) {
    axis <- element_text(
        face = "bold.italic",
        size = 12
    )
    p +
        theme(
            axis.title = axis,
            plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
            legend.title = element_text(face = "italic")
        )
}

#' @export
theme_histo <- function(p, angle = 90) {
    (p +
        theme_classic() +
        theme(axis.text.x = element_text(angle = angle, vjust = 1, hjust = 1)) +
        xlab("")) %>%
        theme_perso0()
}

#' @export
theme_perso_2D <- function(p) {
    theme_perso0(
        p +
            theme_classic() +
            geom_vline(
                xintercept = 0,
                col = "grey",
                linetype = "dashed",
                size = 0.5
            ) +
            geom_hline(
                yintercept = 0,
                col = "grey",
                linetype = "dashed",
                size = 0.5
            )
    )
}
