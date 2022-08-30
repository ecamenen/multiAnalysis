if (!exists("libs")) {
    libs <- NULL
}
if (!exists("block_name")) {
    block_name <- "blocks_clinic"
}

# path <- "~/bin/MultiOmics4ImmunAID/data/"
path <- file.path(golem::get_golem_wd(), "data")

RGCCA:::load_libraries(
    c(
        "RColorBrewer",
        libs,
        "tidyverse",
        "dplyr"# ,
        # "vapoRwave"
    )
)

load(file.path(path, paste0(block_name, ".rda")))
blocks <- get(block_name)

# to_remove <- c("1004", "1007", "1002", "25012", "1001", "1008", "1010")
# i_row <- which(rownames(blocks[[1]]) %in% to_remove)
# blocks <- lapply(blocks, function(i) i[-i_row, ])

load(file.path(path, "clinic_transf.rda"))
clinic_intersect <- filter(
    clinic_transf,
    str_detect(
        immun_aid_identifier,
        paste0("^", rownames(blocks[[1]]), "$", collapse = "|")
    )
) %>% arrange(immun_aid_identifier)
disease <- clinic_intersect$disease %>%
    factor(., levels = unique(.))

# colors_var <- c("indianred1", "darkseagreen", "steelblue")
colors_var <- c(
    brewer.pal(n = 9, name = "Pastel1"),
    brewer.pal(n = 9, name = "Set1")
)
colors_ind <- c("blue", "white", "#cd5b45")
