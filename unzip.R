library(tidyverse)
library(data.table)
library(lubridate)
library(stringr)
library(rstudioapi)

wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

#Process new data export downloads if needed
downloaddate <- as_date("2024-01-11")
zips <- file.info(list.files(here::here(), full.names = TRUE, pattern="*.zip"))
zips <- subset(zips, date(zips$mtime) == downloaddate)

# Set folder name to place unzipped files into, folder is created if it doesn't already exist
folder_name <- "SEACARdata"
if(!dir.exists(folder_name)){dir.create(folder_name)}

for(z in row.names(zips)){
  unzip(z, exdir = here::here(folder_name), junkpaths = TRUE)
  
  while(TRUE %in% str_detect(list.files(here::here(folder_name)), ".zip$")){
    for(zz in list.files(here::here(folder_name), full.names = TRUE, pattern = ".zip$")){
      unzip(zz, exdir = here::here(folder_name), junkpaths = TRUE)
      file.remove(zz)
      print(paste0(zz, " unzipped"))
    }
  }
  file.remove(z)
}