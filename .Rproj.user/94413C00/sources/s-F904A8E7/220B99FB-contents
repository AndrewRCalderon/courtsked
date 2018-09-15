library(tidyverse)
library(pdftools)
library(lubridate)

#reading the court schedule from a PDF
courtsked <- pdf_text("raw_data/CRTR9342.pdf")

#breaking the schedule up at line breaks
courtsked <- courtsked %>% 
  str_split("\n") %>% 
#removing the header and last (blank) row from each page
  lapply(function(x) {
            x <- tail(x, -6)
            x <- head(x, -1)
            x <- enframe(x)
            x <- select(x, -name)
                  })

#converting the list into one long dataframe
courtsked <- do.call(rbind, courtsked)

#removing attorney names for easier parsing
courtsked <- courtsked %>% 
  filter(str_detect(value, " CR |/|Event")==TRUE) %>% 
#removing a specific event type that messes with results
  filter(str_detect(value, "IN LIEU/CONVICTION")!=TRUE) %>% 
#creating an key by which to spread the data
  mutate(legend=case_when(str_detect(value, (" CR "))==TRUE ~ "header", 
                        str_detect(value, ("/"))==TRUE ~ "date",
                        str_detect(value, ("Event"))==TRUE ~ "event_type")) 

#each court event has three unique rows, so I'm creating unique IDs for each event 
  #and spreading the dataframe so each row is one court event
courtsked <- courtsked %>% 
  group_by(legend) %>% 
  mutate(id=row_number()) %>% 
  spread(legend, value) %>% 
  select(-id) 

#cleaning up the date and event_type columns
courtsked$date <- mdy(courtsked$date)
courtsked$event_type <- str_trim(str_sub(courtsked$event_type, 1, -19))

#pulling info from the header line
cr_loc <- str_locate(courtsked$header, " CR ")
vs_loc <- str_locate(courtsked$header, "vs")

courtsked$time <- str_trim(str_sub(courtsked$header, 1, 8))
courtsked$judge <- str_sub(courtsked$header, -3, -1)
courtsked$case_no <- str_sub(courtsked$header, cr_loc[ ,1]-4, cr_loc[ ,2]+6)
courtsked$deft_name <- str_trim(str_sub(courtsked$header, vs_loc[ ,2]+2, -3))

courtsked$datetime <- ymd_hm(paste(courtsked$date, courtsked$time, sep=""))

#removing the header and organizing things
courtsked <- courtsked %>% 
  select(-header, -date, -time) %>% 
  select(datetime, event_type, case_no, deft_name, judge)
  

#removing drug court status hearings, which we don't cover
courtsked <- filter(courtsked, event_type!="DRUG COURT STATUS")

#writing it as a csv file to share with another report who covers the courts
date <- today()
filename <- paste0("output_data/", date, "courtsked.csv")
write_csv(courtsked, filename)

#join with a list of cases I'm covering to find out what hearings I need to cover this week
caselist <- read_csv("raw_data/caselist.csv")
weeksked <- left_join(caselist, courtsked, by=case_no)
filename2 <- paste0("output_data/", date, "caselist.csv")
write_csv(weeksked, filename)