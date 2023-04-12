#' @export
set_analysis <- function(block_name = "blocks_clinic", to_remove = NULL, type = "quant") {
    options(ggrepel.max.overlaps = Inf)
    libs <- desc::desc_get_deps()$package
    load_libraries(libs[libs != "R"])
    library(tidyverse)
    library(dplyr)
    path1 <<- file.path(golem::get_golem_wd(), "data")
    path0 <- file.path(golem::get_golem_wd(), "inst", "results")
    path_img <<- file.path(path0, "img")
    path_out <<- file.path(path0, "tsv")

    load(file.path(path1, paste0(block_name, ".rda")))
    blocks <- get(block_name)

    if (!is.null(to_remove)) {
        i_row <- which(rownames(blocks[[1]]) %in% to_remove)
        row_names <- rownames(blocks[[1]])[-i_row]
        blocks <- lapply(blocks, function(i) i[-i_row, ])
        for (i in seq(length(blocks))) {
              rownames(blocks[[i]]) <- row_names
          }
    }
    if (type != "qual") {
        colnames(blocks[[1]])[which(colnames(blocks[[1]]) == "neutrophils_5")] <- "neutrophils_%"
        x <- blocks[[1]]
        # res <- sapply(rownames(x), function(i) round((length(which(is.na(x[i, ]))) / ncol(x)) * 100, 1))
        # x <- x[-which(res > 50), ]
    } else {
        x <- blocks
    }

    load(file.path(path1, "clinic_transf.rda"))
    clinic_intersect <<- filter(
        clinic_transf,
        str_detect(
            immun_aid_identifier,
            paste0("^", rownames(x), "$", collapse = "|")
        )
    ) %>% arrange(immun_aid_identifier)
    if (type == "list") {
        blocks[[1]] <- x
        blocks <<- blocks
    } else {
        blocks <<- x
    }
    disease <<- clinic_intersect$disease %>%
        fct_drop() %>%
        fct_infreq() %>%
        fct_relabel(~ str_replace(.x, " \\(.+\\)", ""))
    colors_ind <<- c("blue", "gray", "#cd5b45")
}

#' @export
get_colors <- function() {
    c(
        brewer.pal(n = 9, name = "Set1"),
        brewer.pal(n = 9, name = "Pastel1")
    )
}
