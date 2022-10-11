#' @export
set_analysis <- function(block_name = "blocks_clinic", to_remove = NULL) {
    options(ggrepel.max.overlaps = Inf)
    path <- file.path(golem::get_golem_wd(), "data")

    RGCCA:::load_libraries(
        c(
            "ade4",
            "cluster",
            "corrplot",
            "dplyr",
            "factoextra",
            "FactoMineR",
            "ggpubr",
            "ggstatsplot",
            "heatmaply",
            "kohonen",
            "NbClust",
            "pheatmap",
            "pvclust",
            "RColorBrewer",
            "RGCCA",
            "rstatix",
            "tidyverse"
        )
    )

    load(file.path(path, paste0(block_name, ".rda")))
    blocks <- get(block_name)

    if (!is.null(to_remove)) {
        i_row <- which(rownames(blocks[[1]]) %in% to_remove)
        blocks <- lapply(blocks, function(i) i[-i_row, ])
    }
    blocks <<- blocks
    # colnames(blocks[[1]])[which(colnames(blocks[[1]]) == "neutrophils_5")] <- "neutrophils_%"

    load(file.path(path, "clinic_transf.rda"))
    clinic_intersect <<- filter(
        clinic_transf,
        str_detect(
            immun_aid_identifier,
            paste0("^", rownames(blocks[[1]]), "$", collapse = "|")
        )
    ) %>% arrange(immun_aid_identifier)
    disease <<- clinic_intersect$disease %>%
        factor(., levels = unique(.))

    colors_var <<- c(
        brewer.pal(n = 9, name = "Pastel1"),
        brewer.pal(n = 9, name = "Set1")
    )
    colors_ind <<- c("blue", "white", "#cd5b45")
}
