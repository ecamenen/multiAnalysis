# path <- "~/bin/MultiOmics4ImmunAID/data/"
path <- file.path(golem::get_golem_wd(), "data")
load(file.path(path, paste0("blocks", ".rda")))
RGCCA:::load_libraries(
    c(
        "RColorBrewer",
        "RGCCA",
        "tidyverse",
        "vapoRwave"
    )
)

blocks <- blocks
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
    colors = c(
       brewer.pal(n = 12, name = "Paired"),
       vapoRwave:::vapoRwave_palette
    ),
    no_overlap = TRUE,
    text = FALSE,
    i_block = n
) {
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

load(file.path(path, paste0("clinic", ".rda")))
clinic_intersect <- filter(
    clinic,
    str_detect(
        immun_aid_identifier,
        paste0("^", rownames(blocks[[1]]), "$", collapse = "|")
    )
)

disease <- clinic_intersect$disease
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

clinic_intersect$disease <- as.character(clinic_intersect$disease)
clinic_intersect <- mutate(clinic_intersect,
    diseases = case_when(
        !str_detect(
            disease, paste(replace_parenthesis(names(levels)), collapse = "|")
        ) ~ "Others",
        TRUE ~ disease
    )
)

plot_ind0("diseases", colors = colors)
# c(pal_ucscgb()(7)[-2])
colors <- c(
    rgb(255 / 255, 0 / 255, 0 / 255, 0.5),
    rgb(255 / 255, 204 / 255, 0 / 255, 0.5),
    rgb(0 / 255, 255 / 255, 0 / 255, 0.5),
    rgb(102 / 255, 153 / 255, 255 / 255, 0.5),
    rgb(204 / 255, 51 / 255, 255 / 255, 0.5),
    rgb(153 / 255, 153 / 255, 30 / 255, 0.5),
    rgb(0, 0, 0, 0.5)
)

# eval <- rgcca_cv_k(res, validation = "loo", detectCores())
