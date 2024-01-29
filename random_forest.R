###########
### random forest 
############

### init
# parser
library(optparse)

parser = OptionParser()

parser <- add_option(parser,
                     c("-i", "--input-file"),
                     default = NULL,
                     help = "CSV file with all necessary data")
parser <- add_option(parser,
                     c("-t", "--output-tree"),
                     type = "character",
                     default = NULL,
                     help = "Output file path for tree")
parser <- add_option(parser,
                     c("-d", "--output-data"),
                     type = "character",
                     default = NULL,
                     help = "Output file path for data")

args <- parse_args(parser)


# # set file paths
data_in <- args$`input-file`[1]
data_out_tree <- args$`output-tree`[1]
data_out_table <- args$`output-data`[1]


# general packages and functions
library(tidyverse)

print_timestamp <- function(...) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%OS")
  m <- paste("[", ts, "]", paste(..., collapse = " "))
  print(m)
}

# # check input
if (!file.exists(data_in)){ 
  stop(paste("Could not find input file:"), data_in)
}

### reading and preparing the file
df <- read.csv(data_in, header = TRUE) %>%
  column_to_rownames(var = "ZIP_CODE")
df_na <- na.omit(df)

rm(df)

### First tree iteration
library(randomForest)
library(igraph)

print_timestamp("Creating first forest...")
forest <- randomForest::randomForest(x = df_na, ntree = 1000, proximity = TRUE)

### Getting info
print_timestamp("Finished. Getting info for improvement.")


df_dist <- forest$proximity

print_timestamp("Building graph.")
x <- graph_from_adjacency_matrix(
  df_dist,
  weighted = TRUE,
  diag = FALSE,
  mode = "undirected")


clusters <- cluster_leading_eigen(
  x,
  weights = E(x)$weight
)
# 4 clusters modularity coefficient 0.536!!!
# and cluster membership quite balanced. nice.

df_na$label <- factor(clusters$membership)

rm(x, clusters, df_dist)

### Second improved tree
print_timestamp("Creating second improved forest...")
forest_final <- randomForest::randomForest(
  label ~ .,
  data = df_na,
  importance = TRUE,
  localImp = TRUE,
  ntree = 1000
  )


# save files
print_timestamp("Finished. Saving improved forest...")

save(forest_final, file = data_out_tree)

library(tibble)
df_na <- tibble::rownames_to_column(df_na, "ZIP_CODE")
write.csv(df_na, file = data_out_table, row.names = FALSE)
