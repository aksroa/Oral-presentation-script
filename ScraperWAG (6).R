rm(list = ls())

library(tidyverse)
library(dplyr)
library(rvest)
library(httr)

path <- "https://dk.trustpilot.com/review/www.whiteaway.com"
master_df <- data.frame(matrix(NA,0,3)) %>% `colnames<-`(c("title","text","stars"))
ua <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36")
prop_table <- data.frame(
  perc = c(0.76,0.11,0.04,0.03,0.05)
)


for (i in 1:5) {
  timeslot <- rnorm(1,1)
  Sys.sleep(ifelse(timeslot < 0, 1, timeslot))
  n_path <- paste0("https://dk.trustpilot.com/review/www.whiteaway.com?page=1&stars=",i)
  
  n_max <- prop_table$perc[i]*html_session(n_path, ua) %>% html_nodes("span.headline__review-count") %>% html_text() %>% gsub(pattern = "\\.",replacement = "") %>% as.numeric()
  
  n <- ceiling(n_max/20)
  for (j in 1:n) {
    timeslot <- rnorm(1,1)
    Sys.sleep(ifelse(timeslot < 0, 1, timeslot))
    path_read <- paste0("https://dk.trustpilot.com/review/www.whiteaway.com?page=",j,"&stars=",i)
    
    rev_url <- paste0(
      "https://dk.trustpilot.com/",
      html_session(path_read,ua) %>% html_nodes("a.link.link--large.link--dark") %>% html_attr("href")
    )
    
    for (x in rev_url) {
      timeslot <- rnorm(1,1)
      Sys.sleep(ifelse(timeslot < 0, 1, timeslot))
      title <- html_session(x,ua) %>% html_nodes("a.link.link--large.link--dark") %>% html_text()
      if (length(title) == 0) {title = 0}
      text <- html_session(x,ua) %>% html_nodes("p.review-content__text") %>% html_text()
      if (length(text) == 0) {text = NA}
      
      df <- data.frame(title = title,
                       text = text,
                       stars = i)
      master_df <- rbind(master_df,df)
      
    }
    
  }
}

