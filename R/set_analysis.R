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
        "tidyverse" # ,
        # "vapoRwave"
    )
)

load(file.path(path, paste0(block_name, ".rda")))
blocks <- get(block_name)

load(file.path(path, "clinic.rda"))
clinic_intersect <- filter(
    clinic,
    str_detect(
        immun_aid_identifier,
        paste0("^", rownames(blocks[[1]]), "$", collapse = "|")
    )
)
disease <- clinic_intersect$disease
