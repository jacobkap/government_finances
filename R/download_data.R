setwd("data")
# https://www.census.gov/programs-surveys/apes/data/tables.html
years <- data.frame(year = 1992:2016,
                    smallyear = c(92:99, paste0(0, 0:9), 10:16),
                    stringsAsFactors = FALSE)
years <- years[!years$year %in% 2012:2014, ] # Only avaiable on 
years$smallyear[years$smallyear %in% 15:16] <- 2015:2016
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
                                  "_assgf.xls"),
                mode = "wb")
  setwd("..")
  
}
