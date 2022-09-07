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
n_boot <- 1000
source(file.path(golem::get_golem_wd(), "R", "set_analysis.R"))
source(file.path(golem::get_golem_wd(), "R", "plot_cluster_utils.R"))
source(file.path(golem::get_golem_wd(), "R", "cluster_utils.R"))
MAX_CLUSTERS <- 6
colPers <- function(x) colors_var[seq(x)]

names_levels <- str_replace(levels(disease), " \\(.+\\)", "")
# names_levels <- c("Still", "Control")
row_annotation <- data.frame(
    Disease = factor(disease, labels = names_levels)
)

# Data preparation
res_scaled_na0 <- scale(blocks[[1]])
res_scaled0 <- res_scaled_na0
res_scaled0[is.na(res_scaled0)] <- 0

res_scaled_na <- heatmaply::normalize(blocks[[1]])
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

# fviz_nbclust(
#     res_scaled,
#     get_clusters,
#     method = hc_index
# )
(gaps <- clusGap(
    res_scaled,
    FUN = get_clusters,
    K.max = 6,
    B = n_boot
))
fviz_gap_stat(gaps, maxSE = list(method = "Tibs2001SEmax", SE.factor = 1))

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
    nboot = n_boot,
    parallel = TRUE
) %>% suppressWarnings()
plot_dendrogram(res0, k = k, color = colors_var[-seq(k)])
# save_tiff(plot_dendrogram(res0, k = k), filename = "clust_tot.temp.tiff")

# print(res0, digits = 3)
# pvpick(res0)

# plot(res_clus, choice = "3D.map")

# plotDendrogram(2, row_dend0, res_scaled, MAX_CLUSTERS, cls[[k-1]])

fviz_cluster(
    res_clus,
    repel = TRUE,
    show.clust.cent = TRUE,
    palette = colors_var,
    ggtheme = theme_minimal(),
    main = "Factor map",
    ellipse.type = "norm"
) + theme_classic()

# plot_silhouette(res_clus, colors_var[k:1])


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
    row_side_colors = row_annotation,
    row_side_palette = row_col,
    RowSideColors = factor(disease, labels = names_levels),
    plot_method = "plotly",
    colorbar_xpos = 1.025,
    # na.rm = FALSE,
    key.title = "Disease"
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
    annotation_row = row_annotation,
    border_color = NA,
    annotation_colors = list(Disease = row_col),
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

# Quality
plotGapPerPart(gaps, MAX_CLUSTERS)
plotFusionLevels(MAX_CLUSTERS, row_dend0)
cls <- getClusterPerPart(MAX_CLUSTERS, row_dend0)
sils <- getSilhouettePerPart(res_scaled, cls, res_dist)
mean_sil <- getMeanSilhouettePerPart(sils)
plotSilhouettePerPart(mean_sil)
plotSilhouette(sils[[k-1]]); abline(v = 0.165, col = "red", lwd = 2, lty = 1)
plot_silhouette(sils[[k-1]], colors_var[c(3, 5)])
(between <- getRelativeBetweenPerPart(MAX_CLUSTERS, res_dist, cls))
between_diff <- getBetweenDifferences(between)
plotBetweenDiff(between_diff)
height <- rev(row_dend0$height)[seq(MAX_CLUSTERS)]
height_diff <- abs(getBetweenDifferences(height))
(summary <- tibble(
    `Number of clusters` = 2:MAX_CLUSTERS,
    `Dendrogram height` = height[-1],
    `Height difference` = height_diff[-1],
    `Between inertia (%)` = between,
    `Between difference` = between_diff,
    `Silhouette index` = mean_sil)
)

# Variable contribution
# 100 * getCtrVar(2, cls[[k-1]], res_scaled)
ctr <- getDiscriminantVariables(2, cls[[k-1]], res_scaled, ncol(blocks[[1]]))
plotHistogram(ggplot(ctr, aes(order, discr_var)), ctr)
round(ctr[, 1, drop = FALSE], 2)

# x = blocks[[1]]; res = sapply(seq(nrow(x)), function(i) round((length(which(is.na(x[i, ]))) / ncol(x))*100, 1))
# names(res) <- rownames(blocks[[1]])
# res <- sort(res)
# res <- data.frame(val = res, order = seq_along(res))
# plotHistogram(ggplot(res, aes(order, val)), res) + geom_hline(yintercept = c(35), col = "red")

cl <-  cls[[k-1]]
n <- 3
dat <- as.data.frame(clinic_intersect[, colnames(blocks[[1]])]) %>%
    cbind(cl = as.character(cl))
stats <- calculate_test(dat)
plot_mean_test(dat, "arthralgia_myalgia", stats)

(descr <- pivot_longer(dat, !cl) %>%
    group_by(name, cl) %>%
    summarise(
        mean = mean(value, na.rm = TRUE),
        sd = sd(value, na.rm = TRUE),
        n = length(which(!is.na(value)))
    ) %>%
    # filter(str_detect(name, paste(names(var0), collapse= "|")))  %>%
    pivot_wider(names_from = cl, values_from = c(mean, sd, n))
)

ctr2 <- tibble(name = rownames(ctr), ctr = ctr[, 1])
stats2 <- stats %>% dplyr::select(all_of(c("name", "p", "p.signif")))
tot <- Reduce(left_join, list(ctr2, descr, stats2)) %>% arrange(p)
tot$p <- format(tot$p * 6, scientific = TRUE, digits = 2)
arrange(tot, desc(ctr)) %>%
    # slice(seq(n)) %>%
    dplyr::select(-c(starts_with("n_"), contains("sd_")))

# Outputs
write.csv2(as.data.frame(cl), "clusters.temp.tsv")

# NHC
res_dist0 <- get_dist(res_scaled, stand = FALSE, method = "euclidian")
classif <- getClassif(1, MAX_CLUSTERS, res_scaled, res_dist)
# classif <- lapply(2:MAX_CLUSTERS, function(i) pam(res_dist0, i, diss = TRUE))
classif[[2]]$data <- res_scaled
cls <- getClusterPerPart(MAX_CLUSTERS, classif)

fviz_cluster(
    classif[[2]],
    data = res_scaled,
    repel = TRUE,
    show.clust.cent = TRUE,
    palette = colors_var,
    ggtheme = theme_minimal(),
    main = "Factor map",
    ellipse.type = "norm"
) + theme_classic()
