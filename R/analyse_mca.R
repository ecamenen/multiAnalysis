block_name <- "clinic_qual"
to_remove <- NULL
source(file.path(golem::get_golem_wd(), "R", "set_analysis.R"))
source(file.path(golem::get_golem_wd(), "R", "plot_utils.R"))
colPers <- rev(colors_ind[3:2])
blocks <- mutate_all(blocks, as.character) %>%
    select(-c(starts_with("treatment_group"), starts_with("consent_signed_by"), "country"))
blocks[blocks == 0] <- "No"
blocks[blocks == "1"] <- "Yes"
blocks$disease[blocks$disease == "Yes"] <- "AOSD"
blocks$disease[blocks$disease == "No"] <- "SOJIA"

res_mca <- MCA(blocks, ncp = 3, graph = FALSE)
res_mca0 <- dudi.acm(mutate_all(blocks, factor), nf = 3, scannf = FALSE)

fviz_screeplot(
    res_mca,
    addlabels = TRUE,
    geom = "bar",
    hjust = 0.5,
    ggtheme = theme_classic()
) %>% theme_perso0()

fviz_contrib(res_mca, choice = "ind", axes = 1, top = 50) %>% theme_histo()
fviz_mca_ind(
    res_mca,
    col.ind = "contrib",
    gradient.cols = colPers,
    repel = TRUE,
    ggtheme = theme_minimal()
)

fviz_contrib(res_mca0, choice = "var", axes = 1, top = 50) %>% theme_histo()
(ctr <- get_ctr(res_mca))
theme_perso_2D(
    fviz_mca_var(
        res_mca,
        col.var = "contrib",
        gradient.cols = colPers,
        repel = TRUE,
        select.var = list(contrib = 50)
    )
)

plotellipses(res_mca)
boxplot(res_mca0)
fviz_mca_var(
    res_mca,
    choice = "mca.cor",
    repel = TRUE,
    ggtheme = theme_minimal()
)


fviz_mca_var(
    res_mca,
    col.var = "cos2",
    ggtheme = theme_minimal()
)

fviz_mca_biplot(res_mca,
    select.var = list(contrib = 50),
    repel = TRUE,
    ggtheme = theme_minimal()
)


fviz_mca_ind(
    res_mca,
    label = "none",
    habillage = "disease",
    palette = colors_var[9:10],
    addEllipses = TRUE, ellipse.type = "confidence",
    ggtheme = theme_minimal()
)

# Integrative
theme_perso_2D(
    fviz_mca_biplot(
        res_mca,
        repel = TRUE,
        # col.var = "contrib",
        col.ind = "gray50",
        # col.ind = as.character(cls[-15, 2]),
        gradient.cols = colPers,
        # alpha.var = "cos2",
        # palette = c(colors_k, "red"),
        # legend.title = "Clusters",
        # pointsize = "cos2",
        mean.point = FALSE,
        select.var = list(contrib = 50)
    )
)

cls <- set_clusters(rownames(blocks))
cl <- cls$cl
colors_k <- cls$colors_k
# setdiff(rownames(blocks), names(cls))
# setdiff(cls[, 1], rownames(blocks))
# cls <- rbind(cls[-35, ], data.frame(X = c("1007", "1014"), ind_clusters = NA))
# cls <- cls %>% mutate(X = as.numeric(X)) %>% arrange(X)

# Integrative
theme_perso_2D(
    fviz_mca_biplot(
        res_mca0,
        repel = TRUE,
        axes = c(1, 2),
        # col.ind = as.numeric(disease),
        col.ind = cl,
        # col.ind = clinic_intersect$physician_global_assessment,
        col.var = "blue",
        # col.ind = "gray",
        gradient.cols = colors_k,
        # gradient.cols = colors_ind,
        # alpha.var = "cos2",
        # palette = c(colors_k, "red"),
        # legend.title = "Clusters",
        # pointsize = "cos2",
        mean.point = FALSE,
        title = "",
        select.var = list(contrib = 20)
    )
)

i <- 1
barplot(res_mca0$cr[, i], names.arg = row.names(res_mca0$cr), las = 2, main = paste("Axe", i))
plotHistogram(df = res_mca0$cr[, 1, drop = FALSE])
corrplot(t(res_mca0$l1), is.corr = FALSE, col = colorRampPalette(c(colPers, "black"))(100))

#
# sapply(colnames(blocks), function(i) length(which(is.na(as.data.frame(blocks[, i])[, 1])))) %>% sort()
Y <- res_mca$ind$coord[, 2]
Y <- res_mca0$l1[, 2]
stats <- cbind(Y, blocks) %>%
    pivot_longer(!Y) %>%
    group_by(name) %>%
    anova_test(Y ~ value)
stats0 <- cbind(Y, blocks) %>%
    pivot_longer(!Y) %>%
    group_by(name) %>%
    tukey_hsd(Y ~ value)
t_stats <- stats %>%
    as_tibble() %>%
    arrange(p) %>%
    filter(p < .05) %>%
    adjust_pvalue(method = "BH") %>%
    add_significance() %>%
    select(c("name", "p.adj", "p.adj.signif"))
t_stats0 <- stats0 %>%
    as_tibble() %>%
    arrange(p.adj) %>%
    filter(p.adj < .05) %>%
    select(c("name", "group1", group2, "p.adj", "p.adj.signif"))


ggbetweenstats(
    data.frame(
        var = res_mca0$l1[, 2],
        cl = factor(blocks$at_least_3_skin_rash)
    ),
    cl,
    var,
    ggtheme = theme_classic(),
    xlab = "",
    ylab = ""
) +
    theme(
        axis.ticks.x = element_blank(),
        axis.line.x = element_line(color = "white")
    ) +
    scale_color_manual(values = brewer.pal(n = 9, name = "Set1")[seq(2)]) +
    geom_hline(yintercept = 0, lwd = 0.25)

temp0 <- data.frame(cls = as.character(cl), blocks) %>%
    filter(!is.na(cls)) %>%
    select(-cls) %>%
    pivot_longer(everything()) %>%
    group_by(name) %>%
    nest()
lapply(seq_along(blocks), function(i) chisq_test(na.omit(cl), as.data.frame(temp0[i, ]$data)[, 1])) %>%
    Reduce(rbind, .) %>%
    cbind(colnames(blocks), .)
