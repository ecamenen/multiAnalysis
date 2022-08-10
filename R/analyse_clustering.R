# Environment
libs <- c(
    "cluster",
    "NbClust",
    "dendextend",
    "heatmaply"
)
block_name <- "blocks_clinic"
hc_method <- "ward.D2"
hc_metric <- "pearson"
hc_index <- "silhouette"
k <- 2
colors_var <- c("indianred1", "darkseagreen")
colors_ind <- c("blue", "white", "#cd5b45")
source(file.path(golem::get_golem_wd(), "R", "set_analysis.R"))

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
    gradient = list(low = colors_ind[1], mid = colors_ind[2], high = colors_ind[3])
)

# Clustering

# res_clus <- HCPC(pca_res, nb.clust = k, graph = FALSE)
# res_clus$data.clust$clust
# res_clus$desc.var$test.chi2

res_clus <- hclust(res_dist, method = hc_method)
group <- cutree(res_clus, k = k)
table(group, disease)

res_clus <- eclust(
    res_scaled,
    "hclust",
    hc_method = hc_method,
    stand = FALSE,
    hc_metric = hc_metric,
    k = k
)
table(res_clus$cluster, disease)

res_clus0 <- res_scaled0 %>%
    get_dist(stand = FALSE, method = hc_metric) %>%
    hclust(method = hc_method)
table(cutree(res_clus0, k = k), disease)

res_clus1 <-res_scaled %>%
    t() %>%
    get_dist(stand = FALSE, method = hc_metric) %>%
    hclust(method = hc_method)

# Visualisation

fviz_dend(
    res_clus,
    k = k,
    rect = TRUE,
    # rect_fill = TRUE,
    k_colors = colors_var[seq(k)],
    color_labels_by_k = TRUE,
)

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

fviz_silhouette(res_clus, palette = colors_var) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90))

# Heatmap
# gradient_col <- ggplot2::scale_color_gradientn(colours = colors, na.value = "black")

colors_var <- c("yellow", "red")
mycols <- c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07")
row_dend <- res_clus %>%
    as.dendrogram() %>%
    set("branches_lwd", .1) %>%
    set("branches_k_color", mycols[1:k], k = k)

col_dend <- res_clus1 %>%
    as.dendrogram() %>%
    set("branches_lwd", .1) %>%
    set("branches_k_color", mycols[1:k], k = k)

heatmaply(
    res_scaled,
    k_col = k,
    k_row = k,
    colors = colors_ind,
    distfun = hc_metric,
    hclust_method = hc_method,
    # Rowv = row_dend,
    # Colv = col_dend,
    # row_dend_left = TRUE
    seriate = "none",
    # col_side_colors = group_code,
    row_side_colors = data.frame("Disease" = factor(disease, labels = c("Still", "Control")), check.names = FALSE),
    row_side_palette = c("Still" = colors_var[1], "Control" = colors_var[2]),
    # RowSideColors = factor(disease, labels = c("Still", "Control")),
    plot_method = "plotly",
    # key.title = "Disease"
    # side_color_layers
)

# clinic_intersect %>% filter(str_detect(disease, "control")) %>%
#     arrange(immun_aid_identifier)
