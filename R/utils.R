years <- data.frame(year = 1992:2016,
                    smallyear = c(92:99, paste0(0, 0:9), 10:16),
                    stringsAsFactors = FALSE)
years <- years[!years$year %in% 2012:2014, ] # Only avaiable on 
years$smallyear[years$smallyear %in% 15:16] <- 2015:2016


fix_col_names <- function(cols) {
  cols <- tolower(cols)
  cols <- trimws(cols)
  cols <- gsub(" \\(1\\)", "", cols)
  cols <- gsub("[[:punct:]]|\\s", "_", cols)
  cols <- gsub("_+", "_", cols)
  cols <- gsub("_$", "", cols)
  cols <- gsub("population_thousands.*", "population_thousands", cols)
  cols <- gsub("store_", "stores_", cols)
  cols <- gsub("charge$*", "charges", cols)
  cols <- gsub("expenditure_", "expenditures_", cols)
  cols <- gsub("governmental", "government", cols)
  cols <- gsub("x_thousands_of_dollars|item", "state", cols)
  
  return(cols)
}
