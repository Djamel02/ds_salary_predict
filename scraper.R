library(RSelenium)
library(wdman)

scrapper <- function(keyword, num_jobs, slp_time, verbose = False){
  # Initialize the driver
  driver <- remoteDriver(
    remoteServerAddr = "localhost",
    browserName = 'chrome',
    port = 4444,
  )
  #Open the browser and wait until it will be ready
  #driver$setWindowSize('1120', '1000', winHand = "current")
  driver$open()
  Sys.sleep(slp_time)
  
  # Glassdor url
  url <- paste('https://www.glassdoor.com/Job/jobs.htm?suggestCount=0&suggestChosen=false&clickSource=searchBtn&typedKeyword=' ,keyword,'&locT=&locId=&jobType=&context=Jobs&sc.keyword=',keyword,'&dropdown=0')
  # Uncomment the line below if you'd like to scrape without a new Chrome window every time.
  
  #navigate to the url
  driver$navigate(url)
  jobs <- list()
  #Start looping
  while(length(jobs) < num_jobs){
    #Let the page load. Change this number based on your internet speed.
    #Or, wait until the webpage is loaded, instead of hardcoding it.
    Sys.sleep(3)
    
    try(
      driver$findElement(using = 'class name', value = 'selected')$clickElement()
      )
    
    # Clicking on close modal
    Sys.sleep(.1)
    tryCatch(
      expr = {
      driver$findElement(using = 'css selector', value = '[alt= "Close"]')$clickElement()
      print('X out works') 
    },
    error = function(NoSuchElement){
      message('X out failed')
    },
    finally = {
    }
    )
      
    #going through each job in this page
    job_buttons <- driver$findElements(using = 'class name', value = 'jl') #jl for job listing
    
    for (job_button in job_buttons) {
      print(paste('Progress : ', length(jobs),'/',num_jobs))
      if (length(jobs)>=num_jobs) {
        break
      }
      
      job_button$clickElement() # clicking on buttons
      Sys.sleep(1)
      
      collected_successfully <- FALSE
      while(!collected_successfully){
        tryCatch(expr = {
          company_name <- driver$findElement(using = 'xpath', value = './/div[@class="employerName"]')['text']
          location <- driver$findElement(using = 'xpath', value = './/div[@class="location"]')
          job_title <- driver$findElement(using = 'xpath', value = './/div[contains(@class, "title")]')['text']
          job_description <- driver$findElement(using = 'xpath', value = './/div[@class="jobDescriptionContent desc"]')
          collected_successfully <- TRUE
        },
          error = function(err){
            Sys.sleep(5)
          }
        )
      }
      
      #Salary Estimate
      tryCatch(expr = {
        salary_estimate <- driver$findElement(using = 'xpath', value = './/span[@class="gray salary"]')
      },
      error = function(err){
        salary_estimate <- -1
      },
      finally = {})
      
      #Rating
      tryCatch(expr = {
        rating <- driver$findElement(using = 'xpath', value = './/span[@class="rating"]')
      },
      error = function(err){
        rating <- -1
      },
      finally = {})
      
      #printing for debbuging
      if (verbose) {
        print(job_title)
       # print(paste('Salary Estimate : ', salary_estimate))
        #print(paste('Job Description : ', job_description))
        #print(paste('Rating : ', rating))
        #print(paste('Company name : ',company_name))
        #print(paste('Location : ',location))
      }
      
    }
  }
  
  return(jobs)
}


