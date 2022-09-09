#' @export
setGraphicBasic <- function() {
    par(cex.lab = 1.5, font.lab = 3, font.axis = 1, cex.axis = 0.8, cex.main = 2, cex = 1, lwd = 3)
}

#' @export
plot_dendrogram <- function(
    x,
    k,
    color = c("indianred1", "darkseagreen", "steelblue"),
    color_bar = color
) {
    x0 <- color_dendrogram(x, k = k, color = color) %>%
        set("labels_cex", 0.8)
    plot(
        x0,
        ylab = "Cophenetic distance",
        main = "Dendrogram",
        fg = "gray",
        cex.lab = 1.5,
        col.lab = "gray",
        font.lab = 3,
        cex.axis = 0.7,
        col.axis = "gray",
        cex.main = 2,
    )
    text(
        x,
        float = .02,
        col = c(au = "gray", bp = "gray", edge = NULL),
        cex = 0.75
    )
    # abline(
    #     h = mean(heights_per_k.dendrogram(x0)[k:(k + 1)]),
    #     col = "gray",
    #     lwd = 2
    # )
    rect.dendrogram(x0, k = k, border = "gray", lty = 3, lwd = 2)
    # pvrect(x, border = "gray", lty = 3, lwd = 2)
    colored_bars(colors = color_bar, dend = x0, rowLabels = "Disease")
}

#' @export
color_dendrogram <- function(
    x,
    k,
    colors = c("indianred1", "darkseagreen", "steelblue")
) {
    as.dendrogram(x) %>%
        set("branches_k_color", value = colors[seq(k)], k = k) %>%
        set("labels_col", value = colors[seq(k)], k = k) %>%
        set("branches_lwd", 2)
}

#' @export
plot_silhouette <- function(x, colors = c("indianred1", "darkseagreen", "steelblue")) {
    p <- fviz_silhouette(x, palette = colors, ylim = c(min(x[, 3]), (max(x[, 3]) + 0.1))) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90))
    p$layers[[2]]$aes_params$colour <- "gray"
    p
}

#' @export
get_clusters <- function(x, k) {
    res <- get_dist(x, stand = FALSE, method = "pearson") %>%
        hclust(method = "ward.D2") %>%
        cutree(k = k)
    list(cluster = res)
}

save_tiff <- function(f, filename = "violinplot_clin.tiff") {
    tiff(
        filename,
        units = "px",
        width = 4000,
        height = 2000,
        res = 300
    )
    f
    dev.off()
}

#' @export
calculate_test <- function(x) {
    df <- pivot_longer(x, !cl) %>%
        group_by(name) %>%
        wilcox_test(value ~ cl) %>%
        add_significance()
    return(df)
}

#' @export
plot_mean_test <- function(x, i, stats, color = colors_var[c(12, 14)]) {
    temp <- data.frame(
        var = as.data.frame(x[, i])[, 1],
        cl = factor(x$cl, labels = paste("Cluster", seq(2)))
    )
    stats <- stats %>%
        filter(name == i) %>%
        add_xy_position(x = "cl")
    stats$y.position <- max(temp[, 1], na.rm = TRUE) + 1
    stats$p <- format(stats$p, scientific = TRUE, digits = 2)
    ggbetweenstats(
        temp,
        cl,
        var,
        ggtheme = theme_classic(),
        xlab = "",
        ylab = ""
    ) +
        theme(
            axis.ticks.x = element_blank(),
            axis.line.x = element_line(color = "white")
        ) +
        scale_color_manual(values = color) +
        ggpubr::stat_pvalue_manual(stats, label = "1.4e-03 {p.signif}")
}
