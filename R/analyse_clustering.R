# Environment
libs <- c(
    "FactoMineR",
    "factoextra",
    "RColorBrewer",
    "cluster",
    "NbClust",
    "dendextend",
    "heatmaply",
    "pheatmap",
    "pvclust"
)
block_name <- "blocks_clinic"
hc_method <- "ward.D2"
hc_metric <- "pearson"
hc_index <- "silhouette"
k <- 2
# colors_var <- c("indianred1", "darkseagreen", "steelblue")
colors_var <- c(
    brewer.pal(n = 9, name = "Pastel1"),
    brewer.pal(n = 9, name = "Set1")
)
colors_ind <- c("blue", "white", "#cd5b45")
source(file.path(golem::get_golem_wd(), "R", "set_analysis.R"))
# names_levels <- c("Still", "Control")

to_remove <- c("1004", "1001", "1002")
i_row <- which(rownames(blocks[[1]]) %in% to_remove)
blocks[[1]] <- blocks[[1]][-i_row, ]
clinic_intersect <- clinic_intersect[-i_row, ]
disease <- clinic_intersect$disease %>%
    factor(., levels = unique(.))
names_levels <- str_replace(levels(disease), " \\(.+\\)", "")
row_annotation <- data.frame(
    Disease = factor(disease, labels = names_levels)
)

# Data preparation
res_scaled_na0 <- scale(blocks[[1]])
res_scaled0 <- res_scaled_na0
res_scaled0[is.na(res_scaled0)] <- 0

res_scaled_na <- normalize(blocks[[1]])
res_scaled <- res_scaled_na
res_scaled[is.na(res_scaled)] <- 0

get_clust_tendency(res_scaled, n = nrow(res_scaled) - 1)

# Optimal k

index <- c(
    "kl", "ch", "hartigan", "cindex", "db", "silhouette", "duda", "pseudot2",
    "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey",
    "mcclain", "gamma", "gplus", "tau", "dunn", "hubert",
    "sdindex", "dindex", "sdbw", "all", "alllong"
)

res.nbclust <- NbClust(
    data = res_scaled,
    # diss = res_dist,
    min.nc = 2, max.nc = 10,
    method = hc_method,
    index = hc_index
)

# fviz_nbclust(res.nbclust)

fviz_nbclust(
    res_scaled,
    hcut,
    method = hc_index
)

# Distance
res_dist <- get_dist(res_scaled, stand = FALSE, method = hc_metric)

fviz_dist(
    res_dist,
    gradient = list(
        low = colors_ind[1],
        mid = colors_ind[2],
        high = colors_ind[3]
    )
)

# Clustering

# res_clus <- HCPC(pca_res, nb.clust = k, graph = FALSE)
# res_clus$data.clust$clust
# res_clus$desc.var$test.chi2

# group <- hclust(res_dist, method = hc_method) %>%
#     cutree(k = k)
# table(group, disease)

res_clus <- eclust(
    res_scaled,
    "hclust",
    hc_method = hc_method,
    stand = FALSE,
    hc_metric = hc_metric,
    k = k
)
table(res_clus$cluster, t(row_annotation))

res_clus0 <- res_scaled0 %>%
    get_dist(stand = FALSE, method = hc_metric) %>%
    hclust(method = hc_method)
table(cutree(res_clus0, k = k), t(row_annotation))

res_clus_var <- res_scaled %>%
    t() %>%
    eclust(
        "hclust",
        hc_method = hc_method,
        stand = FALSE,
        hc_metric = hc_metric,
        k = k
    )

# Visualisation
rownames(row_annotation) <- rownames(res_scaled)
n <- seq(length(levels(disease)))
row_col <- colors_var[n]
names(row_col) <- names_levels
row_col0 <- as.character(factor(disease, labels = colors_var[seq(n)]))

fviz_dend(
    res_clus,
    k = k,
    rect = TRUE,
    # rect_fill = TRUE,
    k_colors = colors_var[seq(k)],
    color_labels_by_k = TRUE,
)

res0 <- pvclust(
    t(res_scaled),
    method.hclust = hc_method,
    method.dist = "correlation",
    use.cor = "pairwise.complete.obs",
    nboot = 1000,
    parallel = TRUE
) %>% suppressWarnings()
plot_dendrogram(res0, k = k)
# print(res0, digits = 3)
# pvpick(res0)

# plot(res_clus, choice = "3D.map")

fviz_cluster(
    res_clus,
    repel = TRUE,
    show.clust.cent = TRUE,
    palette = colors_var,
    ggtheme = theme_minimal(),
    main = "Factor map",
    ellipse.type = "norm"
) + theme_classic()

p <- fviz_silhouette(res_clus, palette = rev(colors_var[c(1, 3)])) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90))
p$layers[[2]]$aes_params$colour <- "gray"
p

# dl <- dendlist(
#     color_dendrogram(d1, 2),
#     color_dendrogram(d2, 2),
# )
# tanglegram(
#     dl,
#     common_subtrees_color_lines = FALSE,
#     highlight_branches_lwd = FALSE,
#     highlight_distinct_edges = FALSE,
#     # k_branches = 2,
#     # k_labels = 2,
#     lwd = 2
# )

# Heatmap
row_dend <- color_dendrogram(res_clus, k = k) %>% sort()
col_dend <- color_dendrogram(res_clus_var, k = k) %>% sort()

heatmaply(
    res_scaled_na,
    # k_col = k,
    # k_row = k,
    colors = colors_ind,
    na.value = "black",
    # distfun = hc_metric,
    # hclust_method = hc_method,
    Rowv = row_dend,
    Colv = col_dend,
    # row_dend_left = TRUE
    # seriate = "mean",
    # col_side_colors = group_code,
    # row_side_colors = row_annotation,
    # row_side_palette = row_col,
    # RowSideColors = factor(disease, labels = c("Still", "Control")),
    plot_method = "plotly",
    colorbar_xpos = 1.025,
    # na.rm = FALSE,
    # key.title = "Disease"
    # side_color_layers,
) %>% layout(xaxis = list(ticklen = 0), yaxis2 = list(ticklen = 0))

row_dend0 <- res_scaled %>%
    get_dist(stand = FALSE, method = hc_metric) %>%
    hclust(method = hc_method)

col_dend0 <- res_scaled %>%
    t() %>%
    get_dist(stand = FALSE, method = hc_metric) %>%
    hclust(method = hc_method)

pheatmap(
    res_scaled_na,
    color =  colorRampPalette(colors_ind)(100),
    angle_col = 315,
    na_col = "black",
    # annotation_row = row_annotation,
    border_color = NA,
    # annotation_colors = list(Disease = row_col),
    # annotation_col = my_sample_col,
    cluster_rows = row_dend0,
    cluster_cols = col_dend0,
    cutree_rows = k,
    cutree_cols = k
)

heatmap(
    as.matrix(res_scaled_na),
    col = colorRampPalette(colors_ind)(100),
    scale = "none",
    na.rm = FALSE,
    # na_col = "black",
    RowSideColors = row_col0,
    Rowv = row_dend,
    Colv = col_dend
)

# clinic_intersect %>% filter(str_detect(disease, "control")) %>%
#     arrange(immun_aid_identifier)
