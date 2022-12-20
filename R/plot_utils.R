#' @export
theme_perso0 <- function(p, cex = 1, show_axis = TRUE) {
    axis <- element_text(
        face = "bold.italic",
        size = 18 * cex
    )
    p <- p +
        theme(
            axis.title = axis,
            axis.text = element_text(size = 10 * cex),
            axis.ticks = element_line(size = 1.2),
            plot.title = element_text(face = "bold", size = 22 * cex, hjust = 0.5),
            legend.title = element_text(face = "italic", size = 12 * cex),
            legend.text = element_text(colour = "black", size = 10 * cex)
        )
    if (show_axis) {
          p <- p + theme(axis.line = element_line(size = 1.2))
      }
    p
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


#' Histogram settings
#'
#' Default font for a vertical barplot.
#'
#' @param p A ggplot object.
#' @param df A dataframe with a column named "order"
#' @param title A character string giving a graphic title
#' @param color A vector of character giving the colors for the rows
#' @examples
#' df <- data.frame(x = runif(30), order = 30:1)
#' library("ggplot2")
#' p <- ggplot(df, aes(order, x))
#' plotHistogram(p, df, "This is my title", "red")
#' # Add colors per levels of a variable
#' df$color <- rep(c(1, 2, 3), each = 10)
#' p <- ggplot(df, aes(order, x, fill = color))
#' plotHistogram(p, df, "Histogram", as.character(df$color))
#' @export plotHistogram
plotHistogram <- function(p = NULL, df = NULL, hjust = 0, vjust = 0.5, n = 100, title = "", color = "black", color_gradient = c("#FFF5F0", "#99000D")) {
    if (is.null(p)) {
        df0 <- as.data.frame(df)
        colnames(df0)[1] <- "val"
        if (n < nrow(df0)) {
            n <- nrow(df0)
        }
        df <- (
            df0 %>%
                arrange(desc(val)) %>%
                mutate(order = rev(seq(nrow(df0))))
        )[seq(n), ]
        p <- ggplot(df, aes(order, val, fill = order)) +
            theme_classic()
    }
    p +
        # TODO: if NB_ROW > X, uncomment this
        # geom_hline(yintercept = c(-.5,.5), col="grey", linetype="dotted", size=1) +
        geom_hline(yintercept = 0, col = "grey", size = 1) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_x_continuous(breaks = df$order, labels = rownames(df)) +
        labs(
            title = title,
            x = "", y = "",
            fill = "Cluster"
        ) +
        # theme_classic() +
        # theme_perso() +
        theme(
            axis.text.y = element_text(size = 12, face = "italic", color = color),
            axis.text.x = element_text(size = 12, face = "italic", color = "darkgrey", angle = 90),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic")
        ) +
        geom_text(aes(label = round(..y.., 1)), hjust = hjust, vjust = vjust) +
        theme(legend.position = "none") +
        scale_fill_gradient(low = color_gradient[1], high = color_gradient[2])
}

#' Default font for plots
#' @export
theme_perso <- function() {
    theme(
        legend.text = element_text(size = 13),
        legend.title = element_text(face = "bold.italic", size = 16),
        plot.title = element_text(size = 25, face = "bold", hjust = 0.5, margin = margin(0, 0, 20, 0))
    )
}

#' @export
set_clusters <- function(Y) {
    path <- file.path(golem::get_golem_wd())
    cls <- read.csv2(file.path(path, "clusters_som2.temp.tsv"))
    k <- length(unique(cls[, 2]))
    if (!is.null(names(Y))) {
        cl <- left_join(data.frame(Y, X = as.double(names(Y))), cls)[, 3]
    } else {
        cl <- left_join(data.frame(X = as.double(rownames(blocks))), cls)[, 2]
    }
    colors_k <- get_colors()[seq(k) + 2]
    if (k > 2) {
        colors_k <- c(colors_k[seq(2)], "red", colors_k[3])
    }

    return(list(cl = cl, colors_k = colors_k, k = k))
}

#' @export
get_ctr <- function(x, i = "var") {
    x <- x[[i]]$contrib
    sapply(
        seq(ncol(x)),
        function(i) {
            which((x[, i] %>% sort(TRUE)) > (1 / nrow(x) * 100))
        }
    )
}
