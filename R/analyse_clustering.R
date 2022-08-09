# Environment
libs <- c(
    "cluster",
    "NbClust"
)
block_name <- "blocks_clinic"
hc_method <- "ward.D2"
hc_metric <- "pearson"
hc_index <- "silhouette"
k <- 2
source(file.path(golem::get_golem_wd(), "R", "set_analysis.R"))

# Data preparation

res_scaled <- scale(blocks[[1]])
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
    gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")
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

# Visualisation

fviz_dend(
    res_clus,
    k = 2,
    rect = TRUE,
    # rect_fill = TRUE,
    # k_colors = c("#2E9FDF", "#00AFBB", "#E7B800"),
    color_labels_by_k = TRUE,
)

# plot(res_clus, choice = "3D.map")

fviz_cluster(
    res_clus,
    repel = TRUE,
    show.clust.cent = TRUE,
    palette = c("#00AFBB", "#FC4E07", "#62bd50"),
    ggtheme = theme_minimal(),
    main = "Factor map"
)

fviz_silhouette(res_clus)
