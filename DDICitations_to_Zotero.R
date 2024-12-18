#Grab "suggested citation" information from each program page in the SEACAR DDI and use it to build 
#a .bib file that can be imported into reference management software (e.g., Zotero)
#
#Author: Stephen R. Durham, PhD*
#Date: 12/9/2024
#Affiliation: Office of Resilience and Coastal Protection, Florida Department of Environmental Protection
#Email: stephen.durham@floridadep.gov
#
#*The web-scraping section of this script was almost entirely produced by ChatGPT 4o, except for adapting
#the scraping to use parallel processing.


#Setup
library(tidyverse)
library(data.table)
library(rvest)
library(dplyr)
library(doFuture)
library(future.apply)
library(progressr)

#Obtain the suggested citations from the program pages in the SEACAR DDI-------------------------------------
#Start with the main page
main_url <- "https://data.florida-seacar.org/programs"
main_page <- read_html(main_url)

#Extract all program page links
links <- main_page %>%
  html_nodes("a") %>%  # Adjust the selector to target links
  html_attr("href") %>%
  na.omit() %>%  # Remove NA values
  unique()

View(links)
links <- links[15:length(links)] #restrict to just program page links

#Ensure full URLs
links <- ifelse(grepl("^http", links), links, paste0("https://data.florida-seacar.org", links))

#Function to scrape the "SEACAR citation"
get_seacar_citation <- function(url) {
  page <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(page)) return(NA)
  citation <- page %>%
    html_nodes(xpath = "///html/body/div[3]/div[2]/div[1]/div[1]/div/div[2]/div/div[1]/dl/dd[16]") %>%  # Adjust XPath/CSS selector
    html_text(trim = TRUE)
  return(citation)
}

# #Apply the function to each link
# citations <- data.frame(
#   Link = links,
#   Citation = sapply(links, get_seacar_citation)
# )

#Faster, parallel version of above
plan(multisession, workers = availableCores() - 2)
handlers(global = TRUE) # Enable global handling of progress bars
handlers("txtprogressbar") # or "progress" for a more dynamic progress bar

with_progress({
  
  p <- progressor(along = seq(1, length(links)))
  
  citations <- future_lapply(X = links, 
                             future.seed = T, 
                             FUN = function(x){
                               citation_x <- get_seacar_citation(x)
                               
                               p(message = sprintf("Processing %s", x)) # Update progress bar
                               return(citation_x)
                               
                             })
})

#Get ProgramIDs from the links
links_pid <- str_sub(links, max(str_locate_all(links[1], "\\/")[[1]]) + 1, -1)
#Create data.table of the citation for each ProgramID
citations2 <- data.table(ProgramID = links_pid,
                         Citation = unlist(citations))
#Remove ProgramIDs with missing "suggested citation" info
citations3 <- citations2[!is.na(Citation) & Citation != "", ]



#Format citations in "Better BibLaTeX" format interpretable by Zotero and other ref management software--------------------------------
#Function to extract author info
author <- function(c){
  auth_c <- str_sub(c, 1, str_locate(c, "\\. \\(\\d\\d\\d\\d\\)\\.")[1])
  auth_c <- str_replace_all(auth_c, "\\;", "\\{\\;\\}")
  auth_c <- str_replace_all(auth_c, "\\,", "\\{\\,\\}")
  auth_c <- str_replace_all(auth_c, "\\-", "\\{\\-\\}")
  auth_c <- str_replace_all(auth_c, "(?<!\\)) and ", " \\{and\\} ")
  auth_c <- str_replace_all(auth_c, "\\) and ", "\\)\\} and \\{")
  auth_c <- str_replace_all(auth_c, "\\\r|\\\n", "")
  auth_c <- paste0("{", auth_c, "}")
  
  return(auth_c)
}

#Function to extract title info (the additional curly brackets are needed to prevent Zotero from converting all titles to sentence case)
title <- function(c){
  t <- str_sub(c, str_locate(c, "\\. \\(\\d\\d\\d\\d\\)\\. .")[2], str_locate(c, ".\\. Updated")[1])
  allwords <- head(str_locate_all(t, "\\b")[[1]][, 1], -1)
  sentwords <- str_locate_all(t, "\\b[:lower:]")[[1]][, 1]
  if(length(sentwords) > 0){
    for(s in seq_along(sentwords)){
      cat("\rStarting ", s, "\n")
      if(s == 1 & sentwords[s] != allwords[1]){
        str_sub(t, 1, 1) <- paste0("{{", str_sub(t, 1, 1))
        allwords <- head(str_locate_all(t, "\\b")[[1]][, 1], -1)
        sentwords <- str_locate_all(t, "\\b[:lower:]")[[1]][, 1]
      }
      if(!(allwords[which(allwords == sentwords[s]) - 2] %in% sentwords)){
        str_sub(t, sentwords[s] - 1, sentwords[s] - 1) <- "}} "
        allwords <- head(str_locate_all(t, "\\b")[[1]][, 1], -1)
        sentwords <- str_locate_all(t, "\\b[:lower:]")[[1]][, 1]
      }
      if(sentwords[s] != max(allwords)){
        if(!(allwords[which(allwords == sentwords[s]) + 2] %in% sentwords)){
          str_sub(t, allwords[which(allwords == sentwords[s]) + 2] - 1, allwords[which(allwords == sentwords[s]) + 2] - 1) <- " {{"
          allwords <- head(str_locate_all(t, "\\b")[[1]][, 1], -1)
          sentwords <- str_locate_all(t, "\\b[:lower:]")[[1]][, 1]
          if(sentwords[s] == max(sentwords)){
            str_sub(t, -1, -1) <- paste0(str_sub(t, -1, -1), "}}")
          }
        }
      }
    }
  } else{
    # str_sub(t, allwords[3] - 1, allwords[3] - 1) <- " {{"
    str_sub(t, 1, 1) <- paste0("{{", str_sub(t, 1, 1))
    str_sub(t, -1, -1) <- paste0(str_sub(t, -1, -1), "}}")
  }
  return(t)
}

#Function to extract the citation date
date <- function(c){
  str_sub(c, str_locate(c, "\\. \\(\\d\\d\\d\\d\\)\\.")[1] + 3, str_locate(c, "\\. \\(\\d\\d\\d\\d\\)\\.")[2] - 2)
}

#Function to extract the ProgramID
number <- function(c){
  str_sub(c, str_locate(c, "https\\:\\/\\/data\\.florida\\-seacar\\.org\\/programs\\/details\\/.")[2], -1)
}

#Function to extract the publisher info
publisher <- function(c){
  str_sub(c, str_locate(c, "\\. Distributed by")[1] + 2, str_locate(c, ".\\. https")[1])
}

#Function to extract the program page URL
url <- function(c){
  str_sub(c, str_locate(c, "\\. https")[1] + 2, -1)
}

#Function to extract the dataset version date
version <- function(c){
  str_sub(c, str_locate(c, "\\. Updated ")[1] + 2, str_locate(c, ".\\. Distributed by")[1])
}

#Build a "Better BibLaTeX"-formatted version of each citation
citations3[, bib := {
  paste0("@dataset{SEACARID", number(Citation), ",\n  ",
         "author = {{", author(Citation), "}},\n  ",
         "date = {", date(Citation), "},\n  ",
         "title = {", title(Citation), "},\n  ",
         "number = {Program ID ", number(Citation), "},\n  ",
         "publisher = {", publisher(Citation), "},\n  ",
         "url = {", url(Citation), "},\n  ",
         "urldate = {", Sys.Date(), "},\n  ",
         "langid = {english},\n  ",
         "version = {", version(Citation), "}\n}") #str_replace_all(str_sub(title(Citation), 1, max(str_locate_all(title(Citation), "\\b")[[1]][, 1][which(str_locate_all(title(Citation), "\\b")[[1]][, 1] <= 30) - 1])), " ", "")
}, by = Citation]


# Write to a .bib file that can be imported into Zotero
bibs <- paste(citations3$bib, collapse = "\n")
writeLines(bibs, here::here(paste0("seacar_bibs_", Sys.Date(), ".bib")))

