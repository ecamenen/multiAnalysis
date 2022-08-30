#' @export
setGraphicBasic <- function() {
    par(cex.lab = 1.5, font.lab = 3, font.axis = 1, cex.axis = 0.8, cex.main = 2, cex = 1, lwd = 3)
}

#' @export
plot_dendrogram <- function(x, k) {
    x0 <- color_dendrogram(x, k = k) %>%
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
    colored_bars(colors = row_col0, dend = x0, rowLabels = "Disease")
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
    p <- fviz_silhouette(res_clus, palette = colors) +
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
