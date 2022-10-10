block_name <- "clinic_still_processed"
source(file.path(golem::get_golem_wd(), "R", "set_analysis.R"))
source(file.path(golem::get_golem_wd(), "R", "plot_utils.R"))
blocks <- blocks[[1]] %>% select(-c("eye_manifestations"))
hc_metric <- "pearson"
ncomp <- 2

# Data preparation
res_scaled_na <- scale(blocks)
res_scaled <- res_scaled_na
res_scaled[is.na(res_scaled)] <- 0

res_dist <- get_dist(res_scaled, stand = FALSE, method = hc_metric)
mds <- cmdscale(res_dist, k = ncomp, eig = TRUE)
# mds <- isoMDS(res_dist)
colnames(mds$points) <- c("Dim.1", "Dim.2")
coord <- as_tibble(mds$points)
eig <- (mds$eig/sum(mds$eig[mds$eig > 0]) * 100)
data.frame(x = cumsum(round(eig, 2)))
plotHistogram(df = (eig[mds$eig > 0]))
axis <- paste(colnames(coord), paste(round(eig[seq(ncomp)], 1), "%"), sep = " - ")
# cls <- read.csv2("clusters.temp.tsv")
ggscatter(
  coord,
  x = colnames(coord)[1],
  y = colnames(coord)[2],
  # color = cls[, 2],
  # fill = cls[, 2],
  label = rownames(blocks),
  size = 1,
  repel = TRUE
) +
  xlab(axis[1]) +
  ylab(axis[2])

ctr <- sapply(seq(ncomp), function(i) cor(res_scaled, coord[, i], method = "spearman"))
rownames(ctr) <- colnames(res_scaled)

plotHistogram(df = abs(ctr[, 1]))

res_rgcca <- rgcca(list(CLINIC = res_scaled), ncomp = ncomp, method = "pca", scale = FALSE)
name_comp <- colnames(res_rgcca$Y[[1]])
res_rgcca$Y[[1]] <- mds$points
colnames(res_rgcca$Y[[1]]) <- name_comp
res_rgcca$AVE$AVE_X[[1]] <- eig[seq(ncomp)] / 100
names(res_rgcca$AVE$AVE_X[[1]]) <- name_comp
res_rgcca$a[[1]] <- ctr

plot_var_2D(res_rgcca, i_block = 1, cex_lab = 15, resp = ctr[, 1], n_mark = 50, remove_var = FALSE)
plot_ind(res_rgcca, i_block = 1, resp = disease, cex_lab = 15, colors = colors_var[seq(2) + 9])
cls <- read.csv2("clusters_som2.temp.tsv")

cls <- set_clusters(mds$points[, 1])
cl <- cls$cl
colors_k <- cls$colors_k

plot_ind(res_rgcca, i_block = 1, resp = cl, cex_lab = 15, colors = colors_k)

plot_var_1D(res_rgcca, i_block = 1, title = "") +
  geom_text(aes(label = round(..y.., 2)), hjust = -0.25) +
  ylim(-0.6, 0.65)

df <- RGCCA:::get_ctr2(
  rgcca_res = res_rgcca,
  compx = 1,
  compy = 2,
  i_block = 1,
  type = "weight",
  n_mark = 10,
  resp = ctr[, 1],
  remove_var = FALSE
)
p <- ggplot(df, aes(df[, 1], df[, 2], colour = resp))
RGCCA:::plot2D(
  res_rgcca,
  df,
  "",
  df$resp,
  "CTR",
  1,
  2,
  1,
  cex_lab = 13,
  p = p) +
  geom_path(
    aes(x, y),
    data = RGCCA:::plot_circle(),
    col = "grey",
    size =  0.5
  ) +
  geom_path(
    aes(x, y),
    data = RGCCA:::plot_circle() / 2,
    col = "grey",
    size = 0.5,
    lty = 2
  )

clinic_intersect %>%
  select(c("age_at_inclusion_time", "BMI", "gender", "batch")) %>%
  mutate(gender0 = as.numeric(as.character(factor(gender, labels = c(1, 0))))) %>%
  select(-gender)


