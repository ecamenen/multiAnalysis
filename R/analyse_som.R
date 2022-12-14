block_name <- "clinic_still_processed"
hc_metric <- "pearson"
to_remove <- c("1007", "1014")
source(file.path(golem::get_golem_wd(), "R", "set_analysis.R"))
source(file.path(golem::get_golem_wd(), "R", "cluster_utils.R"))
source(file.path(golem::get_golem_wd(), "R", "plot_cluster_utils.R"))
blocks <- blocks[[1]]

library("kohonen")
n_run <- 100000
MAX_CLUSTERS <- 6

k <- 4
colors_k <- brewer.pal(n = 9, name = "Set1")[seq(k) + 2]
cols <- function(a = 1) {
      c(rgb(77 / 250, 175 / 250, 75 / 250, a), rgb(150 / 250, 80 / 250, 150 / 250, a), rgb(1, 40 / 250, 45 / 250, a), rgb(1, 0.5, 0, a), rgb(0 / 250, 120 / 250, 180 / 250, a))
  }
# c(rgb(77/250, 175/250, 75/250, a),rgb(77/250, 175/250, 75/250, a),rgb(150/250, 80/250, 150/250, a), rgb(150/250, 80/250, 150/250, a)) #, rgb(1, 40/250, 45/250, a), rgb(1, 0.5, 0, a), rgb(0/250, 120/250, 180/250, a))

col_grp <- function(x = som_clusters, a = 1) {
    x <- as.factor(x)
    levels(x) <- cols(a)
    return(as.vector(x))
}

# Data preparation
# res_scaled_na <- scale(blocks)
# res_scaled <- res_scaled_na
# res_scaled[is.na(res_scaled)] <- 0

res_scaled_na <- heatmaply::normalize(blocks)
res_scaled <- res_scaled_na
res_scaled[is.na(res_scaled)] <- 0

som_grid <- somgrid(
    xdim = 4,
    ydim = 4,
    topo = "hexagonal"
)
load("~/som_model.RData")
# som_model <- som(as.matrix(res_scaled), grid = som_grid, rlen = n_run)
plot(som_model, type = "changes")

# Clustering
som_codes <- som_model$codes[[1]]
som_codes_scaled <- scale(som_codes)
res_dist <- get_dist(som_codes_scaled, stand = FALSE, method = hc_metric)
n <- nrow(som_codes) - 2
if (MAX_CLUSTERS > n) {
      MAX_CLUSTERS <- n
  }
classif <- getClassif(2, MAX_CLUSTERS, som_codes_scaled, res_dist)
cls <- getClusterPerPart(MAX_CLUSTERS, classif)
get_summary(som_codes_scaled, res_dist, cls, MAX_CLUSTERS, k = k)
# res0 <- pvclust(
#   t(som_codes_scaled),
#   method.hclust = hc_method,
#   method.dist = "euclidian",
#   use.cor = "pairwise.complete.obs",
#   nboot = n_boot,
#   parallel = TRUE
# ) %>% suppressWarnings()
# plot_dendrogram(res0, k = k, color = colors_k)
# som_clusters <- cls[[k - 1]]
som_clusters <- read.csv2("clusters0.temp.tsv")$cl0
ind_clusters <- som_clusters[som_model$unit.classif]
names(ind_clusters) <- rownames(res_scaled_na)

# Code plots
par(mfrow = c(2, 2))
plot(som_model, type = "counts")
plot(som_model, type = "dist.neighbours")
add.cluster.boundaries(som_model, som_clusters)
plot(som_model, type = "quality")
plot(
    som_model,
    type = "mapping",
    pch = som_clusters,
    labels = rownames(res_scaled_na),
    col = col_grp(x = ind_clusters),
    bg = col_grp(a = 0.25)
)
add.cluster.boundaries(som_model, som_clusters)
par(mfrow = c(1, 1))

# plot(som_model, type = "codes", palette.name = rainbow, codeRendering = "segments")

# write.csv2(as.data.frame(ind_clusters), "clusters_som2.temp.tsv")
