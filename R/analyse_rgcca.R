block_name <- "blocks"
libs <- c("RGCCA")
source(file.path(golem::get_golem_wd(), "R", "set_analysis.R"))

n <- length(blocks)
res <- rgcca(blocks, ncomp = 2, response = n) # tau = "optimal"
plot_ave(res)
RGCCA::plot_network2(res)
plot_var_2D(
    res,
    cex_main = 30,
    cex_lab = 20,
    no_overlap = TRUE,
    i_block = n
)
plot_var_2D(
    res,
    i_block = 1,
    cex_main = 30,
    cex_lab = 20,
    no_overlap = TRUE
)
plot_var_2D(
    res,
    i_block = 2,
    cex_main = 30,
    cex_lab = 20,
    no_overlap = TRUE
)

plot_ind0 <- function(
    x,
    colors = colors_var,
    no_overlap = TRUE,
    text = FALSE,
    i_block = n
) {
    if (text)
        no_overlap <- FALSE
    plot_ind(
        res,
        i_block = i_block,
        cex_main = 30,
        cex_lab = 20,
        no_overlap = no_overlap,
        resp = as.data.frame(clinic_intersect[, x])[[1]],
        response_name = str_to_title(x),
        text = text,
        colors = colors
    )
}

plot_ind0("disease", text = FALSE)

table_occ <- function(x, i, y) {
    sapply(
        y,
        function(j) {
            nrow(
                filter(
                    x,
                    str_detect(
                        as.data.frame(x[, i])[, 1], replace_parenthesis(j)
                    )
                ) %>%
                    select(all_of(i))
            )
        }
    )
}

replace_parenthesis <- function(x) {
    str_replace_all(x, "\\(", "\\\\(") %>%
        str_replace_all("\\)", "\\\\)")
}

clinic_intersect$disease <- as.character(clinic_intersect$disease)

tab_diseases <- table_occ(
    clinic_intersect,
    "disease",
    unique(sort((
        clinic_intersect$disease
    )))
)
levels <- tab_diseases[as.numeric(tab_diseases) >= 5]

tab_gender <- table_occ(
    clinic_intersect,
    "gender",
    unique(sort((
        clinic_intersect$gender
    )))
)

clinic_intersect <- mutate(
    clinic_intersect,
    diseases = case_when(
        !str_detect(
            disease,
            paste(replace_parenthesis(names(levels)), collapse = "|")
        ) ~ "Others",
        TRUE ~ disease
    )
)

colors <- c(
    rgb(255 / 255, 0 / 255, 0 / 255, 0.5),
    rgb(255 / 255, 204 / 255, 0 / 255, 0.5),
    rgb(0 / 255, 255 / 255, 0 / 255, 0.5),
    rgb(102 / 255, 153 / 255, 255 / 255, 0.5),
    rgb(204 / 255, 51 / 255, 255 / 255, 0.5),
    rgb(153 / 255, 153 / 255, 30 / 255, 0.5),
    rgb(0, 0, 0, 0.5)
)
# toRGB(c(pal_ucscgb()(7)[-2], "black"), 0.5)

plot_ind0("diseases", colors = colors)


# eval <- rgcca_cv_k(res, validation = "loo", detectCores())
