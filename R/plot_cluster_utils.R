#' @export
setGraphicBasic <- function() {
    par(cex.lab = 1.5, font.lab = 3, font.axis = 1, cex.axis = 0.8, cex.main = 2, cex = 1, lwd = 3)
}

#' @export
plot_dendrogram <- function(
    x,
    k,
    color = get_colors(),
    color_bar = color
) {
    x0 <- color_dendrogram(x, k = k, colors = color) %>%
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
    colors = get_colors()
) {
    as.dendrogram(x) %>%
        set("branches_k_color", value = colors[seq(k)], k = k) %>%
        set("labels_col", value = colors[seq(k)], k = k) %>%
        set("branches_lwd", 2)
}

#' @export
plot_silhouette <- function(x, colors = get_colors(), cex = 1) {
    p <- fviz_silhouette(x, palette = colors, ylim = c(min(x[, 3]), (max(x[, 3]) + 0.1))) +
        theme_classic() +
        theme(
            axis.title = element_text(size = cex * 20, face = "italic"),
            axis.ticks = element_blank(),
            axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.5, size = cex * 12, color = "gray"),
            axis.text.y = element_text(size = cex * 12, color = "gray"),
            plot.title = element_text(size = cex * 25, face = "bold", hjust = 0.5),
            legend.title = element_text(size = cex * 12, face = "italic")
        )
    p$layers[[2]]$aes_params$colour <- "gray"
    p$labels$title <- gsub(".+\n (.+)", "\\1", p$labels$title)
    p$labels$y <- gsub(" Si", "", p$labels$y)
    p$labels$colour <- str_to_title(p$labels$colour) -> p$labels$fill
    p
}

#' @export
get_clusters <- function(x, k, hc_method = "ward.D2", dist_method = "euclidean") {
    res <- get_dist(x, stand = FALSE, method = dist_method) %>%
        hclust(method = hc_method) %>%
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
calculate_test1 <- function(x, method = "anova") {
    df <- pivot_longer(x, !cl) %>%
        group_by(name) %>%
        # filter(!is.na(value)) %>%
        base::get(paste0(method, "_test"))(value ~ cl) %>%
        add_significance()
    return(df)
}

#' @export
calculate_test <- function(x, method = "anova") {
    df <- pivot_longer(x, !cl)
    stats <- group_by(df, name, cl) %>%
        summarise(m = mean(value)) %>%
        pivot_wider(names_from = cl, values_from = m) %>%
        mutate(fc2 = Yes / No, fc = Yes - No)
    df0 <- group_by(df, name)
    stats0 <- df0 %>% summarise(sd = sd(value))
    res <- df0 %>%
        # filter(!is.na(value)) %>%
        base::get(paste0(method, "_test"))(value ~ cl) %>%
        add_significance()
    Reduce(left_join, list(stats, stats0, res)) %>%
        mutate(D = (Yes - No) / sd)
}

calculate_test0 <- function(x, method = "lm") {
    df <- pivot_longer(x, !cl) %>%
        group_by(name) %>%
        # filter(!is.na(value)) %>%
        base::get(paste0(method, "_test"))(value ~ cl) %>%
        add_significance()
    return(df)
}

#' @export
plot_mean_test <- function(x, i, stats, color = c("red", "blue")) {
    temp <- data.frame(
        var = as.data.frame(x[, i])[, 1],
        cl = factor(x$cl, labels = paste("Cluster", seq(unique(x$cl))))
    )
    # stats <- stats %>%
    #     filter(name == i) %>%
    #     add_xy_position(x = "cl")
    # stats$y.position <- max(temp[, 1], na.rm = TRUE) + 1
    # stats$p <- format(stats$p, scientific = TRUE, digits = 2)
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
        scale_color_manual(values = color) #+
        # ggpubr::stat_pvalue_manual(stats, label = "2.3e-08***")
}

#' @export
plot_silhouette0 <- function(res_dist, cls, k,  cex = 1) {
    sils <- getSilhouettePerPart(cls, res_dist)
    sil_k <- sils[[k - 1]]
    colors_k <- get_colors()[seq(k) + 2]
    # plotSilhouette(sil_k)
    # abline(v = mean(sil_k[, 3]), col = "red", lwd = 2, lty = 1)
    plot_silhouette(sil_k, colors_k, cex = cex)
}

#' @export
get_summary <- function(res_dist, cls, MAX_CLUSTERS, row_dend0 = NULL, k = 2) {
    mean_sil <- getSilhouettePerPart(cls, res_dist) %>%
        getMeanSilhouettePerPart()
    # plotSilhouettePerPart(mean_sil)
    between <- getRelativeBetweenPerPart(MAX_CLUSTERS, res_dist, cls)
    between_diff <- getBetweenDifferences(between)
    # plotBetweenDiff(between_diff)
    summary <- tibble(
        `Number of clusters` = 2:MAX_CLUSTERS,
        `Between inertia (%)` = between,
        `Between difference` = between_diff,
        `Silhouette index` = mean_sil
    )
    if (!is.null(row_dend0)) {
        height <- rev(row_dend0$height)[seq(MAX_CLUSTERS)]
        height_diff <- abs(getBetweenDifferences(height))
        summary <- cbind(
            summary,
            tibble(
                `Dendrogram height` = height[-1],
                `Height difference` = height_diff[-1]
            )
        ) %>% tibble()
    }
    return(summary)
}

#' @export
scale0 <- function(x, method = "zscore") {
    stopifnot(method %in% c("minmax", "zscore"))
    if (method == "minmax") {
          heatmaply::normalize(x)
      } else {
          scale(x)
      }
}

#' @export
print_stats <- function(x, dec = 1) {
    paste(mean(x, na.rm = TRUE) %>% round(dec), "\u00b1", sd(x, na.rm = TRUE) %>% round(dec))
}

#' @export
chi2 <- function(cl, x) {
    temp0 <- data.frame(cls = as.character(cl), x) %>%
        filter(!is.na(cls)) %>%
        select(-cls) %>%
        pivot_longer(everything()) %>%
        group_by(name) %>%
        nest()
    lapply(seq(ncol(x)), function(i) chisq_test(cl, as.data.frame(temp0[i, ]$data)[, 1])) %>%
        Reduce(rbind, .) %>%
        cbind(colnames(x), .) %>%
        arrange(p) %>%
        adjust_pvalue(method = "BH") %>%
        add_significance0(p.col = p.adj) %>%
        select(-c("n", "df", "method", "p", "p.signif")) %>%
        set_colnames(c("Variables", "Chi²", "P-adjusted", "")) %>%
        kable0()
}
