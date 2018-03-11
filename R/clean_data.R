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
  
    message(file)
    if (file_year <= 1997) {
      temp <- data.frame(read_excel(file))
      temp <- temp[, colSums(is.na(temp)) < nrow(temp)]
      temp <- temp[, 1:2]
      names(temp) <- c("item", "amount")
      temp$state <- NA
      temp$state[toupper(temp$item) %in% c(toupper(state.name), "UNITED STATES")] <- 
        temp$item[toupper(temp$item) %in% c(toupper(state.name), "UNITED STATES")]
      temp <- temp[grep(".", temp$state)[1]:nrow(temp), ]
      temp$state <- zoo::na.locf(temp$state)
      temp <- temp[temp$item != temp$state, ]
      temp <- temp[, c(1, 3, 2)]
    } else if (file_year == 1998) {
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
    if (file_year > 1997) {
    temp <- temp[1:grep("cash and security holdings", temp[, 1],
                        ignore.case = TRUE)[1], ]
    temp <- temp[-grep("by function|popula", temp[, 1], ignore.case = TRUE), ]
    } else {
      temp <- temp[-grep("population|by function", temp$item, ignore.case = TRUE), ]
    }

    
    # Adds type of budget
    temp <- add_budget_type(temp)
    
    
    # Transpose columns and rows
    if (file_year > 1997) {
    temp <- data.frame(t(temp), stringsAsFactors = FALSE)
    names(temp) <- unname(unlist(temp[1,]))
    temp <- temp[-1, ]
    } else {
      temp <- spread(temp, key = item, value = amount)
    }
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

final_csv <- final_csv[, names(final_csv) %in% names(final_xls)]
finances <- bind_rows(final_csv, final_xls)
finances <- finances %>% plyr::arrange(state, desc(year))
oth_cols <- names(finances)[!names(finances) %in% c("state", "state_abb", "year")]
finances <- finances[, c("state", "state_abb", "year", oth_cols)]
setwd("..")
write_csv(finances, "government_finances.csv")

temp <- data.frame(read_excel(file))
temp <- temp[, 1:2]
names(temp) <- c("item", "amount")
temp$state <- NA
temp$state[toupper(temp$item) %in% c(toupper(state.name), "UNITED STATES")] <- 
  temp$item[toupper(temp$item) %in% c(toupper(state.name), "UNITED STATES")]
temp <- temp[grep(".", temp$state)[1]:nrow(temp), ]
temp$state <- zoo::na.locf(temp$state)
temp <- temp[temp$item != temp$state, ]
temp <- temp[!is.na(temp$item), ]
temp <- temp[-grep("population|by function", temp$item, ignore.case = TRUE), ]
temp <- add_budget_type(temp)
temp <- spread(temp, key = item, value = amount)
names(temp) <- gsub("General expenditure Direct expenditure",
                    "expenditure_direct_general_expenditure",
                    names(temp), ignore.case = TRUE)
names(temp) <- gsub("General expenditure Intergovernmental expenditure",
                    "expenditure_intergovernmental_general_expenditure",
                    names(temp), ignore.case = TRUE)


names(temp) <- fix_col_names(names(temp))
table(duplicated(names(temp)))
names(temp)[duplicated(names(temp))]
names(temp)[!names(temp) %in% names(final_csv)]
names(final_csv)[!names(final_csv) %in% names(temp)]
