blogdown::build_dir("static")

library(dplyr)
slides <-
  list.files(
    path = "static",
    recursive = T,
    pattern = ".Rmd",
    full.names = TRUE
  )

message(slides)

gettitle <- function(file) {
  lines <- readLines(file)
  title = grep("title:(.*)$", lines, value = TRUE)[1] %>% gsub("title:", "", .) %>% stringr::str_trim()
  return(title)
}
getdate <- function(file){
  lines <- readLines(file)
  date = grep("date:", lines, value = TRUE)[1] %>% gsub("date:", "", .) %>% stringr::str_trim()
  return(date)
}

titles <- sapply(slides, gettitle)
dates <- sapply(slides, getdate)
dates <- '18-01-31'
# dates <- lubridate::parse_date_time(dates,orders = c("b! d!, Y!","ymd", "y-m-d", "y.m.d", "Ymd","Y/m/d"))

message(dates)

slides2 <- gsub("static/", "", slides) %>%
  gsub("Rmd","html",.)
indxheader <- yaml::as.yaml(list(title = "SLIDES",
                                 nocomment = 'true'))
contents <-
  paste0("- ",format.Date(dates,"%Y-%m-%d") ," [", titles, "](/", slides2, ")")
message(contents)
text <- c("---", indxheader, "---", "{{< now >}}\n", contents)

fileConn <- file("content/myslides.md")
writeLines(text, fileConn, useBytes = TRUE)
close(fileConn)
