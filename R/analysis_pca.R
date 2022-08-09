block_name <- "blocks_clinic"
libs <- c(
    "FactoMineR",
    "factoextra"
)
source(file.path(golem::get_golem_wd(), "R", "set_analysis.R"))

res_pca <- PCA(blocks[[1]], scale.unit = TRUE, ncp = 3, grap = FALSE)
tibble(res_pca$eig)
fviz_eig(res_pca, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_var(
    res_pca,
    col.var = "contrib",
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
fviz_contrib(res_pca, choice = "var", axes = 1, top = 20)

fviz_pca_ind(
    res_pca,
    col.ind = "cos2",
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE
)

fviz_pca_ind(
    res_pca,
    geom.ind = "point",
    col.ind = disease,
    palette = c("#00AFBB", "#FC4E07", "#62bd50"),
    addEllipses = TRUE,
    legend.title = "Disease"
)
res_pca$ind
fviz_contrib(res_pca, choice = "ind", axes = 1, top = 20)
