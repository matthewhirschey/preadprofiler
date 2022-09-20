library(tidyverse)
library(here)
library(cowplot)
library(qs)

#import
csv_files <- list.files(here::here("data"), full.names = TRUE)
csv_names <- list.files(here::here("data"), full.names = FALSE) %>%
  stringr::str_remove_all(., pattern = "\\.csv")

for(i in 1:length(csv_files)) {
  assign(paste(csv_names[i]),
         read_tsv(csv_files[i]))
}

#join to make protein df and rna df
add_meta_data <- function(df = "01_inguinal_male_protein"){
  name_list <- stringr::str_split(df, pattern = "_")
  df_new <-
    get(df) %>% #df is character, so get() gets object of same name
    dplyr::rename(id = 1,
                  name = 2) %>%
    tidyr::pivot_longer(cols = !c(id, name, LogFC, adj.p.value), names_to = "key", values_to = "value") %>%
    dplyr::mutate(id = as.character(id),
                  name = stringr::str_to_upper(name),
                  key = map_chr(key, ~ stringr::str_remove_all(., pattern = "^[[:upper:]][[:punct:]]")),
                  key = map_chr(key, ~ stringr::str_remove_all(., pattern = "[[:punct:]][[:digit:]]$|[[:digit:]]$")),
                  key = map_chr(key, ~ stringr::str_replace_all(., pattern = "_neg$|_Neg$", replacement = "-")),
                  key = map_chr(key, ~ stringr::str_replace_all(., pattern = "_pos$|_Pos$", replacement = "+")),
                  tissue = name_list[[1]][2],
                  sex = name_list[[1]][3],
                  sample_type = name_list[[1]][4])
  return(df_new)
}
# tmp <- add_meta_data()
# tmp2 <- add_meta_data(df = "02_inguinal_male_rna")

#combine
tissue_data <-
  csv_names[1:8] %>%
  purrr::map_dfr(~ add_meta_data(.x)) %>%
  tidyr::nest(data = !c("id", "name"))

#repeat for others?
qsave(tissue_data, "tissue_data.qs")
