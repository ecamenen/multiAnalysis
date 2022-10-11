block_name <- "clinic_still_processed"
libs <- c(
    "FactoMineR",
    "factoextra",
    "rstatix"
)
source(file.path(golem::get_golem_wd(), "R", "set_analysis.R"))
source(file.path(golem::get_golem_wd(), "R", "plot_utils.R"))
colPers <- rev(colors_ind[3:2])
blocks <- blocks[[1]]

res_pca <- PCA(blocks, scale.unit = TRUE, ncp = 3, grap = FALSE) %>%
    suppressWarnings()
var <- as.factor(clinic_intersect$gender)

Y <- res_pca$ind$coord[, 1]

dat <- data.frame(Y = Y - min(Y)) %>%
    mutate(group = var)
colnames(dat)[1] <- "Y"
(res_aov <- anova_test(dat, Y ~ group))
theme_perso_2D(
    fviz_pca_ind(
        res_pca,
        geom.ind = "text",
        col.ind = var,
        palette = colors_var[10:(10 + n)],
        addEllipses = TRUE,
        legend.title = "Batch",
        pointsize = 3
    )
) + scale_shape_manual(values = rep(19, n)) +
    labs(subtitle = get_test_label(res_aov, detailed = TRUE)) +
    theme(plot.subtitle = element_text(hjust = 0.5))


(fviz_contrib(res_pca, choice = "ind", axes = 1, top = 50)) %>% theme_histo()
tibble(res_pca$eig)
fviz_eig(
    res_pca,
    addlabels = TRUE,
    geom = "bar",
    hjust = 0.5,
    ggtheme = theme_classic()
) %>% theme_perso0()

# Variables
fviz_contrib(res_pca, choice = "var", axes = 1, top = 50) %>% theme_histo()
(ctr <- get_ctr(res_pca))
vars <- dimdesc(res_pca, 1:2)
temp0 <- vars$Dim.1$quanti %>%
    as.data.frame() %>%
    adjust_pvalue("p.value") %>%
    add_significance("p.value.adj")
theme_perso_2D(
    fviz_pca_var(
        res_pca,
        col.var = "contrib",
        repel = TRUE,
        gradient.cols = colPers,
        select.var = list(name = names(ctr[1:2]))
    )
)

# Individuals
theme_perso_2D(
    fviz_pca_ind(
        res_pca,
        col.ind = "contrib",
        gradient.cols = colors_ind,
        repel = TRUE
    )
)

theme_perso_2D(
    fviz_pca_ind(
        res_pca,
        geom.ind = "point",
        col.ind = disease,
        palette = colors_var[seq(2) + 9],
        addEllipses = TRUE,
        legend.title = "Disease"
    ) + scale_shape_manual(values = rep(19, 2))
)

var <- as.factor(clinic_intersect$gender)

Y <- res_pca$ind$coord[, 1]

dat <- data.frame(Y = Y - min(Y)) %>%
    mutate(group = var)
colnames(dat)[1] <- "Y"
(res_aov <- anova_test(dat, Y ~ group))
model <- lm(Y ~ group, data = dat)
ggqqplot(residuals(model))
group_by(dat, group) %>%
    shapiro_test(Y)
ggqqplot(dat, "Y", facet.by = "group")
levene_test(dat, Y ~ group)
(res_aov <- anova_test(dat, Y ~ group))
(pwc <- tukey_hsd(dat, Y ~ group))

n <- length(levels(var))
theme_perso_2D(
    fviz_pca_ind(
        res_pca,
        geom.ind = "point",
        col.ind = var,
        palette = colors_var[10:(10 + n)],
        addEllipses = TRUE,
        legend.title = "Gender",
        pointsize = 3
    )
) + scale_shape_manual(values = rep(19, n)) +
    labs(subtitle = get_test_label(res_aov, detailed = TRUE)) +
    theme(plot.subtitle = element_text(hjust = 0.5))

pwc <- pwc %>% add_xy_position(x = "group")
ggboxplot(dat, x = "group", y = "Y", color = "group", palette = colors_var[10:12]) +
    stat_pvalue_manual(pwc, hide.ns = TRUE) +
    labs(
        subtitle = get_test_label(res_aov, detailed = TRUE),
        caption = get_pwc_label(pwc)
    )

dat <- data.frame(Y = (Y)) %>%
    mutate(var = log1p(clinic_intersect$BMI))
colnames(dat)[1] <- "Y"
(model <- lm(Y ~ var, data = dat))
# dat <- data.frame(Y = Y ^(1/3)) %>%
#     mutate(var = (clinic_intersect$gender))
# (model  <- glm(var ~ Y, data = dat, family = binomial(link = logit)))
summary(model)
fs <- summary(model)$fstatistic
sub_title <- paste0(
    "Linear regression, ",
    "F",
    "(",
    paste(round(fs[2:3]), collapse = ","),
    ") = ",
    round(fs[1], 1),
    ", ",
    "p",
    " = ",
    round(summary(model)$coefficients[2, 4], 3)
)
ggplot(dat, aes(Y, var)) +
    geom_point() +
    stat_smooth(method = lm) +
    stat_regline_equation(label.x = 3, label.y = 7) +
    labs(subtitle = sub_title)
ggscatter(dat, x = "Y", y = "var", add = "reg.line") +
    stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~")))
shapiro.test(residuals(model))

cls <- set_clusters(Y)
cl <- cls$cl
colors_k <- cls$colors_k
k <- length(colors_k)

theme_perso_2D(
    fviz_pca_ind(
        res_pca,
        geom.ind = "point",
        col.ind = clinic_intersect$physician_global_assessment,
        addEllipses = FALSE,
        gradient.cols = colors_ind,
        legend.title = "Global assessment",
        pointsize = 3
    )
) +
    labs(subtitle = sub_title) +
    theme(plot.subtitle = element_text(hjust = 0.5))

theme_perso_2D(
    fviz_pca_ind(
        res_pca,
        axes = c(1, 2),
        geom.ind = "text",
        col.ind = as.character(cl),
        palette = c(colors_k, "red"),
        addEllipses = TRUE,
        legend.title = "Clusters"
    ) + scale_shape_manual(values = rep(19, k))
)

res_pca$ind
# fviz_contrib(res_pca, choice = "ind", axes = 1, top = 50)
corrplot(t(res_pca$ind$contrib), is.corr = FALSE, col = colorRampPalette(c(colPers, "black"))(100))
corrplot(t(res_pca$var$contrib), is.corr = FALSE, col = colorRampPalette(c(colPers, "black"))(100))

# Integrative
theme_perso_2D(
    fviz_pca_biplot(
        res_pca,
        repel = TRUE,
        col.var = "gray",
        col.ind = as.character(cl),
        gradient.cols = colPers,
        # alpha.var = "cos2",
        palette = c(colors_k, "red"),
        legend.title = "Clusters",
        # pointsize = "cos2",
        mean.point = FALSE,
        select.var = list(name = names(unlist(ctr[1:2])))
    ) + scale_shape_manual(values = rep(19, k))
)
