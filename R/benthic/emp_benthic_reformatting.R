library(purrr)
library(dplyr)
library(lubridate)

#' Reformat EMP benthic data from wide to long format
#' @param data_url (char) Download URL to the csv file
#' @return (data.frame) A data frame of benthic data in long format
reformat_benthic <- function(data_url){
  
  d <- read.csv(data_url, skip = 6, header = T)
  
  # find columns to not pivot over
  fixed_cols <- grep("X", colnames(d), value = TRUE, invert = TRUE)
  
  # read in just the first 6 rows with taxa info
  h <- read.csv(data_url, nrows = 6, header = F)
  
  # paste all taxa together
  col_n <- map_chr(h, paste, collapse = "|")
  
  # replace appropriate column names in main data frame with pasted taxa info
  col_n[grep("NA|Phylum", col_n)] <- NA
  col_d <- colnames(d)
  col_d[grep("X", col_d)] <- col_n[!is.na(col_n)]
  colnames(d) <- col_d
  

  # move from wide to long and split taxa info back into columns
  d_long <- d %>% 
    pivot_longer(cols = -all_of(fixed_cols),
                 names_to = c("phylum", "class", "order", "family","genus", "species"),
                 values_to = "count", 
                 names_sep = "\\|") %>% 
    mutate_all(as.character)
}

urls <- c("https://emp.baydeltalive.com/assets/e106ca2a359a122e74e33ef183a0fb4a/text/csv/2011-2019_Benthic_CPUE.csv",
          "http://emp.baydeltalive.com/assets/06942155460a79991fdf1b57f641b1b4/text/csv/2007-2010CPUE.csv",
          "http://emp.baydeltalive.com/assets/06942155460a79991fdf1b57f641b1b4/text/csv/2001-2006CPUE.csv",
          "http://emp.baydeltalive.com/assets/06942155460a79991fdf1b57f641b1b4/text/csv/1996-2000CPUE.csv",
          "http://emp.baydeltalive.com/assets/06942155460a79991fdf1b57f641b1b4/text/csv/1991-1995CPUE.csv",
          "http://emp.baydeltalive.com/assets/06942155460a79991fdf1b57f641b1b4/text/csv/1981-1990CPUE.csv",
          "http://emp.baydeltalive.com/assets/06942155460a79991fdf1b57f641b1b4/text/csv/1975-1980CPUE.csv")

d_list <- lapply(urls, reformat_benthic)

d_merge <- do.call(bind_rows, d_list, ) %>% 
  mutate(count = as.numeric(count),
         Date = mdy(Date)) %>% 
  filter(Site != "Grand Total" & Month != "(blank)") %>% # filter out grand totals and blank cells with NA count values
  mutate(Month = match(substr(Month, 1, 3), month.abb)) %>% # convert month word to number
  rename(year = Year, date = Date, site = Site, month = Month) %>% 
  filter(count != 0) # 0 counts may or may not be relevant



