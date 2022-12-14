# Environment
block_name <- "clinic_still_processed"
hc_method <- "ward.D2"
hc_metric <- "pearson"
hc_index <- "silhouette"
k <- 2
n_boot <- 1000
to_remove <- c("1007", "1014")
source(file.path(golem::get_golem_wd(), "R", "set_analysis.R"))
source(file.path(golem::get_golem_wd(), "R", "plot_cluster_utils.R"))
source(file.path(golem::get_golem_wd(), "R", "cluster_utils.R"))
source(file.path(golem::get_golem_wd(), "R", "plot_utils.R"))

MAX_CLUSTERS <- 6
colors_k <- brewer.pal(n = 9, name = "Set1")[seq(k) + 2]

names_levels <- str_replace(levels(disease), " \\(.+\\)", "")
# names_levels <- c("Still", "Control", "SOJIA")
row_annotation <- data.frame(
    Disease = factor(disease, labels = names_levels)
)
blocks <- blocks[[1]] %>% select(-c("eye_manifestations", "cardiac_manifestations")) # %>% select(-c("gender", "BMI", "age_at_inclusion_time", "batch"))
# Data preparation
res_scaled_na0 <- scale(blocks)
res_scaled0 <- res_scaled_na0
res_scaled0[is.na(res_scaled0)] <- 0

res_scaled_na <- heatmaply::normalize(blocks)
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
    k_colors = colors_k,
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
plot_dendrogram(res0, k = k, color = colors_k, row_col0)
# save_tiff(plot_dendrogram(res0, k = k), filename = "clust_tot.temp.tiff")

# print(res0, digits = 3)
# pvpick(res0)

# plot(res_clus, choice = "3D.map")

# plotDendrogram(2, row_dend0, res_scaled, MAX_CLUSTERS, cls[[k-1]])

fviz_cluster(
    res_clus,
    repel = TRUE,
    show.clust.cent = TRUE,
    palette = colors_k,
    ggtheme = theme_minimal(),
    main = "Factor map",
    ellipse.type = "norm"
) + theme_classic()

# dl <- dendlist(
#     color_dendrogram(d1, 2, colors_var[c(3, 5) + 9]),
#     color_dendrogram(d2, 2, colors_var[c(3, 5) + 9])
# )
# tanglegram(
#     dl,
#     # common_subtrees_color_lines = FALSE,
#     highlight_branches_lwd = FALSE,
#     highlight_distinct_edges = FALSE,
#     # k_branches = 2,
#     # k_labels = 2,
#     lwd = 2
# )

# Heatmap
row_dend <- color_dendrogram(res_clus, k = k, colors = colors_k) %>% sort()
col_dend <- color_dendrogram(res_clus_var, k = 2, colors = c("#f03088", "#d9d52c")) %>% sort()

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
    fontsize_row = 20,
    fontsize_col = 20,
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
get_summary(res_scaled, res_dist, cls, MAX_CLUSTERS, row_dend0, k = k)

# Variable contribution
100 * getCtrVar(k, cls[[k - 1]], res_scaled)
ctr <- getDiscriminantVariables(k, cls[[k - 1]], res_scaled, ncol(blocks))
# rownames(ctr) <- str_sub(rownames(ctr), 1, 40) %>% snakecase::to_any_case(unique_sep = NULL, parsing_option = 3)
plotHistogram(df = ctr)
# round(ctr[, 1, drop = FALSE], 2)

# x = blocks; res = sapply(seq(nrow(x)), function(i) round((length(which(is.na(x[i, ]))) / ncol(x))*100, 1))
#  names(res) <- rownames(blocks[[1]])
# plotHistogram(df = res, hjust = -0.4) +
#     geom_hline(yintercept = c(35), col = "red")

cl <- cls[[k - 1]]
n <- 6
dat <- as.data.frame(clinic_intersect[, colnames(blocks)]) %>%
    cbind(cl = as.character(cl))
stats <- calculate_test(dat)
plot_mean_test(dat, "physician_global_assessment", as_tibble(stats), colors_k)

n <- 8
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

stats %>%
    as_tibble() %>%
    arrange(p) %>%
    filter(p < 0.05) %>%
    adjust_pvalue(method = "BH") %>%
    add_significance(p.col = "p.adj")

# for (i in as.data.frame(vars)[-c(7, 11, 12), 1]) group_by((data.frame(x = dat[, i], cl)), cl) %>% mutate(log_d = (x)) %>% shapiro_test(x) %>% print()

ctr2 <- tibble(name = rownames(ctr), ctr = ctr[, 1])
stats2 <- stats %>%
    as_tibble() %>%
    dplyr::select(all_of(c("name", "p", "p.signif")))
tot <- Reduce(left_join, list(ctr2, descr, stats2)) # %>% arrange(p)
tot <- tot %>%
    filter(p < 0.05) %>%
    adjust_pvalue(method = "BH") %>%
    add_significance(p.col = "p.adj")
tot$p <- format(tot$p, scientific = TRUE, digits = 2)
tot$p.adj <- format(tot$p.adj, scientific = TRUE, digits = 2)
arrange(tot, desc(ctr)) %>%
    # slice(seq(n)) %>%
    dplyr::select(-c(starts_with("n_"), contains("sd_"), "p", "p.signif"))

tab <- data.frame(batch = clinic_intersect$batch, group = as.numeric(cl))
fisher_test(table(tab))
pairwise_fisher_test(table(tab))
plotHistogram(df = ctr) +
    labs(subtitle = expression(paste("Fisher test for count data (2,32), ", italic("p"), " = ", "<0.0001"))) +
    theme(plot.subtitle = element_text(size = 15, hjust = 0.5, face = "italic", color = "black"))

# Outputs
# write.csv2(as.data.frame(cl), "clusters.temp.tsv")

# NHC
classif <- getClassif(2, MAX_CLUSTERS, res_scaled, res_dist)
# res_dist0 <- get_dist(res_scaled, stand = FALSE, method = "euclidian")
# classif <- lapply(2:MAX_CLUSTERS, function(i) pam(res_dist0, i, diss = TRUE))
classif[[k]]$data <- res_scaled
cls <- getClusterPerPart(MAX_CLUSTERS, classif)
get_summary(res_scaled, res_dist, cls, MAX_CLUSTERS, k = k)

fviz_cluster(
    classif[[k]],
    data = res_scaled,
    repel = TRUE,
    show.clust.cent = TRUE,
    palette = colors_k,
    ggtheme = theme_minimal(),
    main = "Factor map",
    ellipse.type = "norm"
) + theme_classic()
