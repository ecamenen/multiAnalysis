library("kohonen")
n_run <- 10000

cols <- function(a = 1)
  c(rgb(0, 0, 1, a), rgb(0, 0.5, 0, a), rgb(1, 0, 0, a))

col_grp <- function(x = som_clusters, a = 1) {
  x <- as.factor(x)
  levels(x) <- cols(a)
  return(as.vector(x))
}

som_grid <- somgrid(
  xdim = 3,
  ydim = 3,
  topo = "hexagonal")
som_model <- som(res_scaled, grid = som_grid, rlen = n_run)
plot(som_model, type = "changes")

# Clustering
som_codes <- som_model$codes[[1]]
som_codes_scaled <- scale(som_codes)
res_dist <- get_dist(som_codes_scaled, stand = FALSE, method = "euclidian")
classif <- getClassif(1, MAX_CLUSTERS, som_codes_scaled, res_dist)
cls <- getClusterPerPart(MAX_CLUSTERS, classif)
get_summary(som_codes_scaled, res_dist, cls, MAX_CLUSTERS)
# res0 <- pvclust(
#   t(som_codes_scaled),
#   method.hclust = hc_method,
#   method.dist = "euclidian",
#   use.cor = "pairwise.complete.obs",
#   nboot = n_boot,
#   parallel = TRUE
# ) %>% suppressWarnings()
# plot_dendrogram(res0, k = k, color = colors_k)
som_clusters <- cls[[k - 1]]
ind_clusters <- som_clusters[som_model$unit.classif]
names(ind_clusters) <- rownames(res_scaled)

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
  labels = rownames(res_scaled),
  col = col_grp(x = ind_clusters),
  bg = col_grp(a = 0.25))
add.cluster.boundaries(som_model, som_clusters)
par(mfrow = c(1, 1))

plot(som_model, type = "codes", palette.name = rainbow, codeRendering = "segments")

plot(
  som_model,
  type = "mapping",
  pch = som_clusters,
  labels = rownames(res_scaled),
  col = col_grp(x = ind_clusters),
  bg = col_grp(a = 0.25))
add.cluster.boundaries(som_model, som_clusters)
