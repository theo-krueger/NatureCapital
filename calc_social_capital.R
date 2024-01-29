library(optparse)

parser = OptionParser()

parser <- add_option(parser,
                     c("-i", "--input-file"),
                     default = NULL,
                     help = "Social capital file in csv format")
parser <- add_option(parser,
                     c("-s", "--shapes"),
                     default = NULL,
                     help = "Shape file in csv")
parser <- add_option(parser,
                     c("-d", "--id-variable"),
                     type = "character",
                     default = NULL,
                     help = "ID Column of the input file")
parser <- add_option(parser,
                     c("-o", "--output-file"),
                     type = "character",
                     default = NULL,
                     help = "Output file, input + scores")


args <- parse_args(parser)

if (is.null(args$`input-file`)) {
  print_help(parser)
  stop("At least one argument must be supplied (input file)", call. = FALSE)
}

# packages
suppressPackageStartupMessages(library(tidyverse))

# paths
path_input <- args$`input-file`[1]
path_shapes <- args$`shapes`[1]

if (is.null(args$`output-file`)) {
  path_output <- paste0(tools::file_path_sans_ext(path_shapes), "_social_capital.csv")
} else {
  path_output <- args$`output-file`[1]
}

# variables
id_var <- args$`id-variable`

# read data
social_capital <- read_csv(
  path_input,
  col_types = c(zip = "character"),
  show_col_types = FALSE
)
sh_areas <- read_csv(
  path_shapes,
  show_col_types = FALSE
)

# Calculation
if (id_var == "fbcz_id") {
  relevant_statistics <- social_capital %>%
    group_by_at(id_var) %>%
    reframe(clustering_zip_median = median(clustering_zip, na.rm = TRUE),
            clustering_zip_min = min(clustering_zip, na.rm = TRUE),
            clustering_zip_max = max(clustering_zip, na.rm = TRUE),
            support_ratio_zip_median = median(support_ratio_zip, na.rm = TRUE),
            support_ratio_zip_min = min(support_ratio_zip, na.rm = TRUE),
            support_ratio_zip_max = max(support_ratio_zip, na.rm = TRUE),
            ec_zip_median = median(ec_zip, na.rm = TRUE),
            ec_zip_min = min(ec_zip, na.rm = TRUE),
            ec_zip_max = max(ec_zip, na.rm = TRUE)
    ) %>%
    ungroup()
} else if (id_var == "ZIP_CODE") {
  print("Analysis on ZIP level")
  relevant_statistics <- social_capital %>%
  select(zip, clustering_zip, support_ratio_zip, ec_zip) %>%
  mutate(
    zip = as.character(zip),
    zip = case_when(
      nchar(zip) < 5 ~ sprintf("%05s", zip),
      nchar(zip) == 5 ~ zip,
      TRUE ~ NA
    )
  ) %>%
  rename({{id_var}} := zip)
} else {
  stop("ID var not found")
}

sh_areas_withsc <- sh_areas %>%
  left_join(relevant_statistics, by = join_by({{id_var}}))

write_csv(sh_areas_withsc, file = path_output)
