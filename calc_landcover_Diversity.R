library(optparse)

parser = OptionParser()

parser <- add_option(parser,
                     c("-i", "--input-file"),
                     default = NULL,
                     help = "Landcover counts in csv format")
parser <- add_option(parser,
                     c("-t", "--types"),
                     default = NULL,
                     help = "Landcover types as list in txt file")
parser <- add_option(parser,
                     c("-o", "--output-file"),
                     type = "character",
                     default = NULL,
                     help = "Output file, input + scores")
parser <- add_option(parser,
                     c("-d", "--id-variable"),
                     type = "character",
                     default = NULL,
                     help = "ID Column of the input file")

args <- parse_args(parser)

if (is.null(args$`input-file`)) {
  print_help(parser)
  stop("At least one argument must be supplied (input file)", call. = FALSE)
}

# packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(vegan))
suppressPackageStartupMessages(library(mcpdiversity))

# paths
path_input <- args$`input-file`[1]
path_land_covers <- args$`types`[1]

if (is.null(args$`output-file`)) {
  path_output <- paste0(tools::file_path_sans_ext(path_input), "_diversity.csv")
} else {
  path_output <- args$`output-file`[1]
}

# variables
id_var <- args$`id-variable`

# read files
df_counts <- read_csv(path_input, show_col_types = FALSE)
land_covers <- readLines(path_land_covers)

# select matrix from df
df_counts_matrix <- df_counts %>%
  column_to_rownames(id_col <- id_var) %>%
  select(any_of(land_covers)) %>%
  replace(is.na(.), 0)

# claculate indices
shannon <- diversity(df_counts_matrix, index = "shannon")
df_shannon <- data.frame(names=names(shannon), shannon = as.numeric(shannon))

simpson <- diversity(df_counts_matrix, index = "simpson")
df_simpson <- data.frame(names=names(simpson), simpson = as.numeric(simpson))

richness <- specnumber(df_counts_matrix)
pielou_evenness <- shannon / log(richness)
df_pielou <- data.frame(names=names(pielou_evenness), pielou_evenness = as.numeric(pielou_evenness))


richness_raretyWeighted <- compute_rwr(df_counts_matrix, nselect = nrow(df_counts_matrix))
df_richness_raretyWeighted <- data.frame(names = richness_raretyWeighted$site.names, richness_raretyWeighted = as.numeric(richness_raretyWeighted$rwr.scores))


# add to output df
df_counts <- df_counts %>%
  select(!any_of(land_covers)) %>%
  left_join(df_shannon, by = join_by({{id_var}} == names)) %>%
  left_join(df_simpson, by = join_by({{id_var}} == names)) %>%
  left_join(df_pielou, by = join_by({{id_var}} == names)) %>%
  left_join(df_richness_raretyWeighted, by = join_by({{id_var}} == names))


write_csv(df_counts,
          file = path_output
          )