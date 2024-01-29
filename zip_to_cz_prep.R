suppressPackageStartupMessages(library(tidyverse))

filename_ziptocz <- "/Users/tkrueger/Desktop/largefiles/zip_to_cz.rda"

load(filename_ziptocz)

zip_to_cz_us <- zip_to_cz %>%
filter(country_iso3 == "USA") %>%
select(fbcz_id, zipcode)

write_csv(zip_to_cz_us, file = paste0(paste0(tools::file_path_sans_ext(filename_ziptocz), "_us.csv")))
