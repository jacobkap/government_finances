years <- data.frame(year = 1992:2016,
                    smallyear = c(92:99, paste0(0, 0:9), 10:16),
                    stringsAsFactors = FALSE)
years <- years[!years$year %in% 2012:2014, ] # Only avaiable on 
years$smallyear[years$smallyear %in% 15:16] <- 2015:2016


fix_col_names <- function(cols) {
  cols <- tolower(cols)
  cols <- trimws(cols)

  
  col_replace <- c(" \\(1\\)"                               = "",
                   "\\.\\.1\\."                             = "",
                   "[[:punct:]]|\\s"                        = "_",
                   "store_"                                 = "stores_",
                   "charge$"                                = "charges",
                   "expenditures_"                          = "expenditure_",
                   "expenditure_general_expenditure_"       = "expenditure_",
                   "insurance_trust_expenditure_"           = "expenditure_",
                   "governmental"                           = "government",
                   "item"                                   = "state",
                   "x_thousands_of_dollars"                 = "state",
                   "1"                                      = "",
                   "by_function"                            = "",
                   "total_revenue_general_revenue_"         = "revenue_",
                   "general_expenditure_by_function_"       = "expenditue",
                   "revenue_general_revenue_"               = "revenue",
                   "revenue_revenue_taxes_"                 = "revenue_",
                   "expenditure_general_expenditure_"       = "expenditure_",
                   "revenue.*in.*revenue_"                  = "revenue_",
                   "geographic_area_name"                   = "state",
                   "expenditure_expenditure"                = "expenditure_",
                   "revenue_general_revenue_"               = "revenue_",
                   "secuirty"                               = "security",
                   "_+"                                     = "_",
                   "_$|^_"                                  = "",
                   "corporation"                            = "corporate"
                   ) 
  
  col_replace2 <- c("expenditure_total_expenditure_"        = "expenditure_",
                    "expenditure_direct_expenditure_"       = "expenditure_",
                    "expenditure_general_expenditure_"      = "expenditure_",
                    "_net"                                  = "",
                    "tax$"                                  = "taxes",
                    "operation$"                            = "operations",
                    ".*cash.*"                              = "cash_and_security_holdings",
                    "^debt.*|^total_debt.*"                 = "debt_at_end_of_fiscal_year",
                    "total_expenditure"                     = "expenditure",
                    "all_other"                             = "other",
                    "^revenue_taxes$"                       = "revenue_total_taxes",
                    "general_expenditure_general_expenditure" = "total_general_expenditure",
                    "^general_expenditure_"                 = "expenditure_",
                    "^total_revenue"                        = "revenue",
                    "^revenue_revenue$"                     = "total_revenue",
                    "^expenditure_expenditure$"             = "total_expenditure",
                    "^revenue_taxes$"                       = "revenue_total_taxes",
                    "^expenditure_general_expenditure$"     = "total_general_expenditure",
                    "_and_gross_receipts_taxes"             = "",
                    "revenue_general_revenue_"              = "revenue_",
                    "^utility_expenditure$"                 = "expenditure_utility_expenditure",
                    "^revenue$"                             = "total_revenue",
                    "^expenditure$"                         = "total_expenditure",
                    "revenue_total_taxes_"                  = "revenue_",
                    "^liquor_stores_expenditure"             = "expenditure_liquor_stores",
                    "expenditure_liquor_stores_expenditure" = "expenditure_liquor_stores",
                    "^insurance_trust_expenditure" = "expenditure_insurance_trust",
                    "expenditure_insurance_trust_expenditure" = "expenditure_insurance_trust",
                    "^revenue_total_revenue$"               = "total_revenue",
                    "^revenue_individual_income$"               = "revenue_individual_income_taxes",
                    "^revenue_corporate_income$"               = "revenue_corporate_income_taxes",
                    "_$|^_"                                 = "")
  cols <- stringr::str_replace_all(cols, col_replace)
  cols <- stringr::str_replace_all(cols, col_replace2)
  
  return(cols)
}

get_year <- function(file, years) {
  file_year <- substr(file, 1, 2)
  if (file_year == "20") {
    file_year <- as.numeric(substr(file, 1, 4))
  } else if (file_year == "SG") {
    file_year <- as.numeric(substr(file, 5, 8))
  } else {
    file_year <- years$year[years$smallyear == file_year]
  }
  return(file_year)
}


fix_states <- function(temp) {
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
  temp$state_abb <- as.character(states$state.abb[match(temp$state,
                                                         states$state.name)])  
  return(temp)
}


add_budget_type <- function(temp) {
  temp$budget_type <- NA
  temp$budget_type[grepl("^total revenue|^total expen|^general exp|^debt|^cash",
                         temp[,1], ignore.case = TRUE)] <-
    temp[,1][grepl("^total revenue|^total expen|^general exp|^debt|^cash",
                   temp[,1], ignore.case = TRUE)]
  if (grep(".", temp$budget_type)[1] != 1) {
  temp$budget_type[1:(grep(".", temp$budget_type)[1] - 1)] <- ""
  }
  temp$budget_type <- gsub(",.*", "", temp$budget_type)
  temp$budget_type <- gsub("^debt.*", "Total debt", temp$budget_type, 
                           ignore.case = TRUE)
  temp$budget_type <- gsub("expenditures", "expenditure", temp$budget_type, 
                           ignore.case = TRUE)
  temp$budget_type <- gsub("^cash.*", "Total cash", temp$budget_type,
                           ignore.case = TRUE)
  temp$budget_type <- zoo::na.locf(temp$budget_type)
  temp[,1] <- paste(temp$budget_type, temp[,1])
  temp$budget_type <- NULL
  return(temp)
}
