setwd("data")
source('C:/Users/user/Dropbox/R_project/government_finances/R/utils.R')
# https://www.census.gov/programs-surveys/state/data/tables.All.html

# American Fact Finder - 
# https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk

for (i in 1:nrow(years)) {
  setwd("raw")
  
  file_name <- "states"
  if (years$year[i] > 1998) file_name <- "statess"
  if (years$year[i] > 2014) file_name <- "_summary_table"
  type = ".xls"
  if (years$year[i] > 2015) type <- ".xlsx"
  # State
  download.file(paste0("https://www2.census.gov/programs-surveys/state/tables/",
                       years$year[i],
                       "/historical-tables/",
                       years$smallyear[i],
                       file_name,
                       type),
                destfile = paste0(years$smallyear[i],
                                  "_assgf", type),
                mode = "wb")
  setwd("..")
  
}
