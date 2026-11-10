project_selection <- read.csv("inst/extdata/project_selection.csv", stringsAsFactors = FALSE)
save(project_selection, file = "data/project_selection.rda", version = 2, compress = "xz")
