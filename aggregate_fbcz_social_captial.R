library(optparse)

parser = OptionParser()

parser <- add_option(parser,
                     c("-i", "--input-file"),
                     default = NULL,
                     help = "Social capital file in csv format")
parser <- add_option(parser,
                     c("-t", "--types"),
                     default = NULL,
                     help = "ZIP codes and aggregated groups in csv format")
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
suppressPackageStartupMessages(library(sf))

# paths
path_input <- args$`input-file`[1]
path_types <- args$`types`[1]

if (is.null(args$`output-file`)) {
  path_output <- paste0(tools::file_path_sans_ext(path_input), "_agg.csv")
} else {
  path_output <- args$`output-file`[1]
}

# read files 
social_capital <- read_csv(path_input, col_types = c(zip="character"))
zip_to_cz_us <- read.csv(path_types)
zip_to_cz_us <- zip_to_cz_us %>%
  mutate(
    zipcode = as.character(zipcode),
    zipcode = case_when(
      nchar(zipcode) < 5 ~ sprintf("%05s", zipcode),
      nchar(zipcode) == 5 ~ zipcode, 
      TRUE ~ NA
    )
  )

# compute
social_capital_withcz <- social_capital %>%
  mutate(
    zip = case_when(
      nchar(zip) < 5 ~ sprintf("%05s", zip),
      nchar(zip) == 5 ~ zip, 
      TRUE ~ NA
    )
  ) %>%
  left_join(zip_to_cz_us, by=join_by(zip == zipcode))

# save
write_csv(social_capital_withcz, path_output)