library(readr)
library(dplyr)
library(stringr)
library(tidyverse)
setwd("data/raw")


files <- list.files(path = ".")
csv_files <- files[grepl("\\.csv$", files)]
xls_files <- files[grepl("\\.xls", files)]

final <- data.frame()
for (file in xls_files) {
  source('C:/Users/user/Dropbox/R_project/government_finances/R/utils.R')
  # Gets the file year
  file_year <- substr(file, 1, 2)
  if (file_year == "20") {
    file_year <- as.numeric(substr(file,1, 4))
  } else {
    file_year <- years$year[years$smallyear == file_year]
  }
  
  if (!file_year %in% 1992:1997) {
    message(file)
    if (file_year == 1998) {
      temp <- data.frame(read_excel(file, sheet = 2))
      temp[3, 1] <- "item"
    } else {
      temp <- data.frame(read_excel(file))
    }
    if (file_year %in% 2015:2016) temp <- rbind(names(temp), temp)
    temp <- temp[grep("united|US", temp[,2], ignore.case = TRUE)[1]:nrow(temp), ]
    temp <- temp[!is.na(temp[,1]), ]
    temp <- temp[, !is.na(temp[1, ])]
    temp[] <- sapply(temp, as.character)
    
    # Removes footnote
    temp <- temp[1:grep("cash and security holdings", temp[, 1],
                        ignore.case = TRUE)[1], ]
    
    # Transpose columns and rows
    temp <- data.frame(t(temp), stringsAsFactors = FALSE)
    names(temp) <- unname(unlist(temp[1,]))
    temp <- temp[-1, ]
    rownames(temp) <- 1:nrow(temp)
    names(temp) <- fix_col_names(names(temp))
    
    
    # Changes state name to state abb.
    temp$state <- gsub("\\.", " ", temp$state)
    temp$state <- tolower(temp$state)
    states <- data.frame(state.name, state.abb)
    states <- rbind(states, data.frame(state.name = c("United States",
                                                      "District of Columbia",
                                                      "Pennsylvannia"),
                                       state.abb  = c("US", "DC", "PA")))
    states$state.name <- tolower(states$state.name)
    if (any((grepl("^us$", temp$state, ignore.case = TRUE))))  {
      temp$state <- toupper(temp$state)
      temp$state <- states$state.name[match(temp$state, states$state.abb)]
      temp$state <- as.character(temp$state)
    }
    
    temp$year <- file_year
    
    
    # Remove some columns that are only in a few years
    temp <- temp[, !duplicated(names(temp))]
    temp <- temp[, -grep("personal|by_function|population", names(temp))]
    
    temp[2:ncol(temp)] <- sapply(temp[2:ncol(temp)], as.numeric)    
    final <- bind_rows(final, temp)
    
  }
}
final$state_abb <- as.character(states$state.abb[match(final$state, states$state.name)])
