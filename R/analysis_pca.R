block_name <- "blocks_clinic"
libs <- c(
    "FactoMineR",
    "factoextra",
    "rstatix"
)
source(file.path(golem::get_golem_wd(), "R", "set_analysis.R"))
colPers <- rev(colors_ind[3:2])
blocks <- blocks[[1]]

theme_perso0 <- function(p) {
    axis <- element_text(
        face = "bold.italic",
        size = 12
    )
    p +
    theme(
        axis.title =  axis,
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.title = element_text(face = "italic")
    )
}

theme_perso <- function(p) {
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

res_pca <- PCA(blocks, scale.unit = TRUE, ncp = 3, grap = FALSE) %>%
    suppressWarnings()

fviz_contrib(res_pca, choice = "ind", axes = 1, top = 50)
tibble(res_pca$eig)
fviz_eig(
    res_pca,
    addlabels = TRUE,
    geom = "bar",
    hjust = 0.5,
    ggtheme = theme_classic()
) %>% theme_perso0()

# Variables
fviz_contrib(res_pca, choice = "var", axes = 1, top = 50)
get_ctr <- function(x, i = "var") {
    x <- x[[i]]$contrib
    sapply(
        seq(ncol(x)),
        function(i)
            which((x[, i]  %>% sort(TRUE)) > (1 / nrow(x) * 100)))
}
(ctr <- get_ctr(res_pca))
vars <- dimdesc(res_pca, 1:2)
vars$Dim.1$quanti %>%
    as.data.frame() %>%
    adjust_pvalue("p.value") %>%
    add_significance("p.value.adj")
theme_perso(
    fviz_pca_var(
        res_pca,
        col.var = "contrib",
        repel = TRUE,
        gradient.cols = colPers,
        select.var = list(name = names(ctr[1:2]))
    )
)

# Individuals
theme_perso(
    fviz_pca_ind(
        res_pca,
        col.ind = "cos2",
        gradient.cols = colors_ind,
        repel = TRUE
    )
)

theme_perso(
    fviz_pca_ind(
        res_pca,
        geom.ind = "point",
        col.ind = disease,
        palette = colors_var,
        addEllipses = TRUE,
        legend.title = "Disease"
    )
)
res_pca$ind
fviz_contrib(res_pca, choice = "ind", axes = 1, top = 50)
corrplot(t(res_pca$ind$contrib), is.corr = FALSE, col = colorRampPalette(c(colPers, "black"))(100))
corrplot(t(res_pca$var$contrib), is.corr = FALSE, col = colorRampPalette(c(colPers, "black"))(100))

# Integrative
theme_perso(
    fviz_pca_biplot(
        res_pca,
        repel = TRUE,
        col.var = "gray",
        col.ind = disease,
        gradient.cols = colPers,
        # alpha.var = "cos2",
        palette = colors_var[10:11],
        legend.title = "Disease",
        # pointsize = "cos2",
        mean.point = FALSE
        # select.var = list(name = names(unlist(ctr[1:2])))
    )
)
