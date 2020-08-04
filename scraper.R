library(RSelenium)

scrapper <- function(keyword,num_jobs,slp_time,verbose = False){
  # Initialize the driver
  driver <- remoteDriver(
    remoteServerAddr <- "localhost",
    port <- 4444,
    browserName <- "chrome"
  )
  # Uncomment the line below if you'd like to scrape without a new Chrome window every time.
}


