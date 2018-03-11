library(readr)
library(dplyr)
library(stringr)
library(zoo)
library(tidyverse)
setwd("data/raw")


files <- list.files(path = ".")
csv_files <- files[grepl("\\.csv$", files)]
xls_files <- files[grepl("\\.xls", files)]

final_xls <- data.frame()
for (file in xls_files) {
  source('C:/Users/user/Dropbox/R_project/government_finances/R/utils.R')
  # Gets the file year
  file_year <- get_year(file, years)
  
  if (!file_year %in% 1992:1997) {
    message(file)
    if (file_year == 1998) {
      temp <- data.frame(read_excel(file, sheet = 2))
      temp[3, 1] <- "item"
    } else if (file_year == 1999) {
      temp <- data.frame(read_excel(file))
      temp2 <- data.frame(read_excel(file, sheet = 2))
      temp <- cbind(temp, temp2)
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
    temp <- temp[-grep("by function", temp[, 1], ignore.case = TRUE), ]
    
    # Adds type of budget
    temp$budget_type <- NA
    temp$budget_type[grepl("^total revenue|^total expen|^general exp|^debt|^cash",
                           temp[,1], ignore.case = TRUE)] <-
      temp[,1][grepl("^total revenue|^total expen|^general exp|^debt|^cash",
                     temp[,1], ignore.case = TRUE)]
    temp$budget_type[1:(grep(".", temp$budget_type)[1] - 1)] <- ""
    temp$budget_type <- gsub(",.*", "", temp$budget_type)
    temp$budget_type <- gsub("^debt.*", "Total debt", temp$budget_type, 
                             ignore.case = TRUE)
    #temp$budget_type <- gsub("general", "", temp$budget_type, 
    #                         ignore.case = TRUE)
    temp$budget_type <- gsub("expenditures", "expenditure", temp$budget_type, 
                             ignore.case = TRUE)
    temp$budget_type <- gsub("^cash.*", "Total cash", temp$budget_type,
                             ignore.case = TRUE)
    temp$budget_type <- zoo::na.locf(temp$budget_type)
    temp[,1] <- paste(temp$budget_type, temp[,1])
    temp$budget_type <- NULL
    
    
    # Transpose columns and rows
    temp <- data.frame(t(temp), stringsAsFactors = FALSE)
    names(temp) <- unname(unlist(temp[1,]))
    temp <- temp[-1, ]
    rownames(temp) <- 1:nrow(temp)
    names(temp) <- gsub("General expenditure Direct expenditure",
                        "expenditure_direct_general_expenditure",
                        names(temp), ignore.case = TRUE)
    names(temp) <- gsub("General expenditure Intergovernmental expenditure",
                        "expenditure_intergovernmental_general_expenditure",
                        names(temp), ignore.case = TRUE)
    
    names(temp) <- fix_col_names(names(temp))
    
    
    temp <- fix_states(temp)
    temp$year <- file_year
    
    
    # Remove some columns that are only in a few years
    temp <- temp[, !duplicated(names(temp))]
    if (any(grepl("personal|population", names(temp)))) {
      temp <- temp[, -grep("personal|population", names(temp))]
    }
    
    num_cols <- names(temp)[!names(temp) %in% c("state", "state_abb")]
    temp <- temp[temp$state != "item",]
    temp[num_cols] <- sapply(temp[num_cols], as.numeric)    
    final_xls <- bind_rows(final_xls, temp)
    
  }
}

final_csv <- data.frame()
for (file in csv_files) {
  source('C:/Users/user/Dropbox/R_project/government_finances/R/utils.R')
  # Gets the file year
  file_year <- get_year(file, years)
  temp <- data.frame(read_csv(file, skip = 1))
  names(temp) <- gsub("Geographic.area.name", "state", names(temp))
  temp <- temp[, -grep("^id|geograph|^finance", names(temp), ignore.case = TRUE)]

  if (file_year != 2012) {
    temp$type <- paste(temp$Meaning.of.Finance.Type, 
                       temp$Meaning.of.Finance.Source,
                       sep = "_")
    temp <- temp[, -grep("Finance|expenditure_intergovernment_general_expenditure",
                         names(temp))]
    temp <- spread(temp, key = type, value = Amount...1.000.)
  }
  names(temp) <- fix_col_names(names(temp))
  temp$year <- file_year
  temp <- fix_states(temp)
  final_csv <- bind_rows(final_csv, temp)
}

table(duplicated(names(final_csv)))
table(duplicated(names(final_xls)))
table(names(final_csv) %in% names(final_xls))
table(names(final_xls) %in% names(final_csv))
names(final_xls)[!names(final_xls) %in% names(final_csv)]
names(final_csv)[!names(final_csv) %in% names(final_xls)]

finances <- bind_rows(final_csv, final_xls)
finances <- finances %>% plyr::arrange(state, desc(year))
oth_cols <- names(finances)[!names(finances) %in% c("state", "state_abb", "year")]
finances <- finances[, c("state", "state_abb", "year", oth_cols)]
