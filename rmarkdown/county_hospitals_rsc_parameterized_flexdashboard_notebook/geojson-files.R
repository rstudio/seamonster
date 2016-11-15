

library(tidyverse)
library(stringr)
library(jsonlite)

county_summary <- read_csv("./data/county_summary.csv") %>%
  mutate(original_county=str_replace_all(original_county," Parish",""),
         original_county=str_replace_all(original_county,"\\'",""),
         original_county=str_replace_all(original_county," County",""),
         original_county=str_replace_all(original_county," city","")) %>%
  mutate(file_name=paste("map-data/", state, "/", original_county, ".geo.json",sep="")) %>%
  left_join(data.frame(file_name=list.files(recursive=TRUE), matched=1), by="file_name")

unmatched <- county_summary %>%
  filter(is.na(matched)==TRUE)

county_summary <- filter(county_summary, matched==1)

all_files_index <- county_summary %>%
  select(state, county, file_name) %>%
  mutate(index=row_number())

all_files <- lapply(county_summary$file_name, function(x){prettify(read_lines(x))})

rm(county_summary)


#all_test <- all_files[all_files_index$index[all_files_index$state=="CA"]]

save.image("counties.RData")