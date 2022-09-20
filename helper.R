#test to get data back out
get_data <- function(dataframe,
                     fav_gene){
  query <- stringr::str_to_upper(fav_gene)
  fav_data <-
    dataframe %>%
    filter(name %in% query) %>%
    unnest(data)

  return(fav_data)
}
# df <- get_data(dataframe = tissue_data, fav_gene = "CUL4B")
# get_data("Cul4b")

make_boxplot <- function(dataframe,
                         fav_gene){
  df <- get_data(dataframe = dataframe,
                 fav_gene = fav_gene)
  #df$key <- factor(df$key, levels = c("DPP4+", "DPP4-", "APC", "FIP")) #forcats::fct_reorder(key, value, .desc = TRUE)
  ggplot(df) +
    geom_boxplot(aes(x = key, y = value, fill = name)) +
    facet_wrap(vars(str_to_title(tissue), sample_type), scales = "free") +
    theme_cowplot(rel_small = 9/14)
}
# make_boxplot(dataframe = tissue_data,
#              fav_gene = "CUL4B")
# make_boxplot(dataframe = tissue_data, fav_gene = c("CUL4A", "CUL4B"))
# make_boxplot(dataframe = tissue_data, fav_gene = "CUL4B")


