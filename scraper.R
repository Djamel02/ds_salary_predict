library(RSelenium)
library(wdman)

scrapper <- function(keyword, num_jobs, slp_time, verbose = F){
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
  #initialize the matrix
  jobs <- matrix(, ncol = 12)
  colnames(jobs) <- c('Job Title','Salary Estimate','Job Description','Rating','Company Name','Location','Size','Founded','Type Of Ownership','Industry','Sector','Revenue')
  print(dim(jobs)[1])
  
  #Start looping
  while(dim(jobs)[1] < num_jobs){
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
      print(paste('Progress : ', dim(jobs)[1],'/',num_jobs))
      if (dim(jobs)[1]>num_jobs) {
        break
      }
      
      job_button$clickElement() # clicking on buttons
      Sys.sleep(1)
      
      collected_successfully <- FALSE
      while(!collected_successfully){
        tryCatch(expr = {
          company_name <- driver$findElement(using = 'xpath', value = './/div[@class="employerName"]')$getElementText()
        },
          error = function(err){
            company_name <- NA
          }
        )
        tryCatch(expr = {
          location <- driver$findElement(using = 'xpath', value = './/div[@class="location"]')$getElementText()
          },
          error = function(err){
            location <- NA
          }
        )
        tryCatch(expr = {
            job_title <- driver$findElement(using = 'xpath', value = './/div[contains(@class, "title")]')$getElementText()
          },
          error = function(err){
            job_title <- NA
          }
        )
        
        tryCatch(expr = {
            job_description <- driver$findElement(using = 'xpath', value = './/div[@class="jobDescriptionContent desc"]')$getElementText()
          },
          error = function(err){
            job_description <- NA
          }
        )

        collected_successfully <- TRUE
      }
      
      #Salary Estimate
      tryCatch(expr = {
        salary_estimate <- driver$findElement(using = 'xpath', value = './/span[@class="gray salary"]')$getElementText()
      },
      error = function(err){
        salary_estimate <- -1
      },
      finally = {})
      
      #Rating
      tryCatch(expr = {
        rating <- driver$findElement(using = 'xpath', value = './/span[@class="rating"]')$getElementText()
      },
      error = function(err){
        rating <- -1
      },
      finally = {})
      
      #printing for debbuging
      if (verbose) {
        print(paste('Job Title',job_title))
        print(paste('Salary Estimate : ', salary_estimate))
        print(paste('Job Description : ', job_description))
        print(paste('Rating : ', rating))
        print(paste('Company name : ',company_name))
        print(paste('Location : ',location))
        print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
      }
      
      #Going to the Company tab...
      #clicking on this:
      #<div class="tab" data-tab-type="overview"><span>Company</span></div>
      tryCatch(
        expr = {
          driver$findElement(using = 'xpath', value = './/div[@class="tab" and @data-tab-type="overview"]')$clickElement()
          Sys.sleep(1)
          
          tryCatch(
            expr = {
              size <- driver$findElement(using = 'xpath', value = './/div[@class="infoEntity"]//label[text()="Size"]//following-sibling::*')$getElementText()
            },
            error = function(NoSuchElement){
              size <- -1
            },
            finally = {}
          )
          
          tryCatch(
            expr = {
              founded <- driver$findElement('xpath','.//div[@class="infoEntity"]//label[text()="Founded"]//following-sibling::*')$getElementText()
            },
            error = function(NoSuchElement){
              founded <- -1
            },
            finally = {}
          )
          
          tryCatch(
            expr = {
              type_of_ownership <- driver$findElement('xpath','.//div[@class="infoEntity"]//label[text()="Type"]//following-sibling::*')$getElementText()
            },
            error = function(NoSuchElement){
              type_of_ownership <- -1
            },
            finally = {}
          )
          
          tryCatch(
            expr = {
              industry <- driver$findElement('xpath','.//div[@class="infoEntity"]//label[text()="Industry"]//following-sibling::*')$getElementText()
            },
            error = function(NoSuchElement){
              industry <- -1
            },
            finally = {}
          )
          
          tryCatch(
            expr = {
              sector <- driver$findElement('xpath','.//div[@class="infoEntity"]//label[text()="Sector"]//following-sibling::*')$getElementText()
            },
            error = function(NoSuchElement){
              sector <- -1
            },
            finally = {}
          )
          
          tryCatch(
            expr = {
              revenue <- driver$findElement('xpath','.//div[@class="infoEntity"]//label[text()="Revenue"]//following-sibling::*')$getElementText()
            },
            error = function(NoSuchElement){
              revenue <- -1
            },
            finally = {}
          )
          
        },
        error = function(NoSuchElement){
          #Rarely, some job postings do not have the "Company" tab.
         # headquarters <- -1
          size <- -1
          founded <- -1
          type_of_ownership <- -1
          industry <- -1
          sector <- -1
          revenue <- -1
          competitors <- -1
        },
        finally = {
          # for debugging purposes
          if(verbose){
           # print(paste('Headquarter',headquarters))
            print(paste('Size : ', size))
            print(paste('Founded : ', founded))
            print(paste('Type Of Ownership : ', type_of_ownership))
            print(paste('Industry : ',industry))
            print(paste('Sector : ',sector))
            print(paste('Revenue : ',revenue))
            #print(paste('Competitors : ',competitors))
            print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
          }
          
          job <- c(job_title,salary_estimate,job_description,rating,company_name,location,size,founded,type_of_ownership,industry,sector,revenue)
          jobs <- rbind(jobs,job)
          print(paste('number of collected jobs is :', dim(jobs)[1]-1))
        }
      )
      #Clicking on the "next page" button
      if(dim(jobs)[1]+1 %% 30 == 0){
        tryCatch(
          expr = {
            driver$find('xpath','.//li[@class="next"]//a')$clickElement()
          },
          error = function(NoSuchElement){
            print(paste('Scraping terminated before reaching target number of jobs. Needed ',num_jobs, ' got ', dim(jobs)[1],'.'))
            break
          },
          finally = {}
        )    
      }
        
    }
    

  }
  driver$close()
  return(as.data.frame.array(jobs,row.names = 1:dim(jobs)[1]))
}


