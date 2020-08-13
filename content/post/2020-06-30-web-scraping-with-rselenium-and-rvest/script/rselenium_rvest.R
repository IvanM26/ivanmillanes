# Load Libraries ####
library(tidyverse)
library(RSelenium)
library(rvest)

# Setup Selenium Driver ####
# Start Selenium server and browser
rD <- RSelenium::rsDriver(browser = "chrome",
                          chromever =
                            system2(command = "wmic",
                                    args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
                                    stdout = TRUE,
                                    stderr = TRUE) %>%
                            stringr::str_extract(pattern = "(?<=Version=)\\d+\\.\\d+\\.\\d+\\.") %>%
                            magrittr::extract(!is.na(.)) %>%
                            stringr::str_replace_all(pattern = "\\.",
                                                     replacement = "\\\\.") %>%
                            paste0("^",  .) %>%
                            stringr::str_subset(string =
                                                  binman::list_versions(appname = "chromedriver") %>%
                                                  dplyr::last()) %>%
                            as.numeric_version() %>%
                            max() %>%
                            as.character())
# Assign the client to an object
remDr <- rD[["client"]]

# Basic Usage ####
# Use method without () to get a description of what it does
remDr$navigate
remDr$findElement

# Some functions
remDr$navigate("https://www.google.com/")
remDr$navigate("https://www.nytimes.com/")

remDr$goBack()
remDr$goForward()
remDr$refresh()
remDr$getCurrentUrl()
remDr$maxWindowSize()
remDr$getPageSource()[[1]]

remDr$close()
remDr$open()

# Working with elements

# Some knowledge of HTML, CSS and/or xpath is needed to find elements
# I use CSS, sometimes copy-paste xpath from stackoverflow posts
# SelectorGadget / Inspect source code (F12)

# Navigate to Google
remDr$navigate("https://www.google.com/")

# Find search box
webElem <- remDr$findElement(using = "css selector", value = ".gLFyf.gsfi")

# Highlight to check that was correctly selected
webElem$highlightElement()

# Send search and press enter
# Option 1
webElem$sendKeysToElement(list("the new york times"))
webElem$sendKeysToElement(list(key = "enter"))
# Option 2
webElem$sendKeysToElement(list("the new york times", key = "enter"))

# Go back to Google
remDr$goBack()

# Find search box
webElem <- remDr$findElement(using = "css selector", value = ".gLFyf.gsfi")

# Search something else
webElem$sendKeysToElement(list("finantial times"))

# Clear element
webElem$clearElement()

# Search and click
webElem <- remDr$findElement(using = "css selector", value = ".gLFyf.gsfi")
webElem$sendKeysToElement(list("the new york times", key = "enter"))
webElem <- remDr$findElement(using = "css selector", value = ".LC20lb.DKV0Md")
webElem$clickElement()

# Other functions
remDr$getStatus()
remDr$getTitle()
remDr$screenshot()

remDr$getWindowSize()
remDr$setWindowSize(1000,800)
remDr$getWindowPosition()
remDr$setWindowPosition(100, 100)

webElem$getElementLocation()


# Premier League Example ####
# Navigate to the website
remDr$navigate("https://www.premierleague.com/stats/top/players/goals?se=274")
# Give some time to load
Sys.sleep(4)

# Increase window size to find elements
remDr$maxWindowSize()

# Accept cookies
acceptCookies <- remDr$findElement(using = "css selector",
                                   value = "div[class='btn-primary cookies-notice-accept']")
acceptCookies$clickElement()

# > Get values to iterate over ####
# Read page source
source <- remDr$getPageSource()[[1]]

# Get topStats
list_topStats <- read_html(source) %>% 
  html_nodes(".topStatsLink") %>% 
  html_text() %>% 
  str_trim() %>% 
  str_to_title() # In this particular case

# To make example simple
list_topStats <- list_topStats[c(1, 2, 5, 8, 10, 15)]



# Get seasons
list_seasons <- read_html(source) %>% 
  html_nodes("ul[data-dropdown-list=FOOTBALL_COMPSEASON] > li") %>% 
  html_attr("data-option-name") %>% 
  .[-1]

# To make example simple
list_seasons <- list_seasons[c(2,3)]



# Get positions
list_positions <- read_html(source) %>% 
  html_nodes("ul[data-dropdown-list=Position] > li") %>% 
  html_attr("data-option-id") %>% 
  .[-1]


# > Web scraping ####

# Preallocate vector
data_topStats <- vector("list", length(list_topStats))

# Iterate over topStat
for (i in seq_along(list_topStats)){
  # Open topStat dropdown list
  DDLtopStat <- remDr$findElement(using = "css selector", 
                    value = ".dropDown.noLabel.topStatsFilterDropdown")
  DDLtopStat$clickElement()
  Sys.sleep(2)
  
  # Click corresponding topStat
  ELEMtopStat <- remDr$findElement(using = "link text", 
                                   value = list_topStats[[i]])
  ELEMtopStat$clickElement()
  Sys.sleep(2)
  
  # Preallocate vector
  data_seasons <- vector("list", length(list_seasons))
  
  # Iterate over seasons
  for (j in seq_along(list_seasons)){
    # Open seasons dropdown list
    DDLseason <- remDr$findElement(using = "css selector", 
                                   value = ".current[data-dropdown-current=FOOTBALL_COMPSEASON]")
    DDLseason$clickElement()
    Sys.sleep(2)
    
    # Click corresponding season
    ELEMseason <- remDr$findElement(using = "css selector",
                                    value = str_c("ul[data-dropdown-list=FOOTBALL_COMPSEASON] > li[data-option-name='", list_seasons[[j]], "']"))
    ELEMseason$clickElement()
    Sys.sleep(2)
    
    # Preallocate vector
    data_positions <- vector("list", length(list_positions))
    
    # Iterate over position
    for (k in seq_along(list_positions)){
      # Open positions dropdown list
      DDLposition <- remDr$findElement(using = "css selector", 
                                       value = ".current[data-dropdown-current=Position]")
      DDLposition$clickElement()
      Sys.sleep(2)
      
      # Click corresponding position
      ELEMposition <- remDr$findElement(using = "css selector", 
                                        value = str_c("ul[data-dropdown-list=Position] > li[data-option-id='", list_positions[[k]], "']"))
      ELEMposition$clickElement()
      Sys.sleep(2)
      
      # Check that there is a table to scrape. If there isn't, go to next position
      check_table <- remDr$getPageSource()[[1]] %>% 
        read_html() %>% 
        html_node(".statsTableContainer") %>% 
        html_text()
      
      if(check_table == "No stats are available for your search") next
      
      # Populate element of corresponding position (first page)
      data_positions[[k]] <- remDr$getPageSource()[[1]] %>% 
        read_html() %>% 
        html_table() %>% 
        .[[1]] %>% 
        as_tibble() %>% 
        # Thousands are character ("1,000"), problem when binding with integer (1:999)
        mutate(Stat = as.character(Stat) %>% 
                 parse_number())
      
      # Get tables from every page
      btnNextExists <- remDr$getPageSource()[[1]] %>% 
        read_html() %>% 
        html_node(".paginationNextContainer.inactive") %>% 
        html_text() %>% 
        is.na()
      
      # While there is a Next button to click
      while (btnNextExists){
        # Click "Next"
        btnNext <- remDr$findElement(using = "css selector",
                                     value = ".paginationNextContainer")
        btnNext$clickElement()
        Sys.sleep(2)
        
        # Get table from new page
        table_n <- remDr$getPageSource()[[1]] %>% 
          read_html() %>% 
          html_table() %>% 
          .[[1]] %>% 
          as_tibble() %>% 
          mutate(Stat = as.character(Stat) %>% 
                   parse_number())  
        
        #Rowbind existing table and new table
        data_positions[[k]] <- bind_rows(data_positions[[k]], table_n)
        
        # Update Check for Next Button
        btnNextExists <- remDr$getPageSource()[[1]] %>% 
          read_html() %>% 
          html_node(".paginationNextContainer.inactive") %>% 
          html_text() %>% 
          is.na()
        
        Sys.sleep(1)
      }
      
      # Data wrangling
      data_positions[[k]] <- data_positions[[k]] %>% 
        rename(!!list_topStats[[i]] := Stat) %>% 
        mutate(Position = list_positions[[k]])
      
      # Go to top of the page to select next position
      goTop <- remDr$findElement("css", "body")
      goTop$sendKeysToElement(list(key = "home"))
      Sys.sleep(3)
    }
    
    # Rowbind positions dataset
    data_positions <- reduce(data_positions, bind_rows)
    
    # Populate corresponding season
    data_seasons[[j]] <- data_positions %>% 
      mutate(Season = list_seasons[[j]])
    
  }
  
  # Rowbind seasons dataset
  data_seasons <- reduce(data_seasons, bind_rows)
  
  # Populate corresponding topStat
  data_topStats[[i]] <- data_seasons
    
}

# > Data wrangling ####
dataset <- data_topStats %>% 
  # Remove Rank column
  map(function(x) select(x, -Rank)) %>% 
  # Reorder columns
  map(function(x) select(x, Season, Position, Club, Player, Nationality, everything())) %>%
  # Full join (because not necessarily all players have all stats)
  reduce(full_join, by = c("Season", "Position", "Club", "Player", "Nationality")) %>% 
  # Replace NA with 0 in numeric columns
  mutate(across(where(is.numeric), replace_na, replace = 0))


# Comments ####

#   Caution when adding topStats to the loop. 
# Position filter is hidden for some topStats (Clean Sheets) 


# parallel Framework ####
library(tidyverse)
library(parallel)

# Define function to stop Selenium on each core
close_rselenium <- function(){
  clusterEvalQ(clust, {
    remDr$close()
    rD$server$stop()
  })
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
}

# Determine cores
detectCores()
clust <- makeCluster(4)

# List ports
ports = list(4567L, 4444L, 4445L, 5555L)

# Open Selenium on each core, using one port per core
clusterApply(clust, ports, function(x){
  library(tidyverse)
  library(RSelenium)
  library(rvest)
  rD <<- RSelenium::rsDriver(browser = "chrome",
                             chromever =
                               system2(command = "wmic",
                                       args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
                                       stdout = TRUE,
                                       stderr = TRUE) %>%
                               stringr::str_extract(pattern = "(?<=Version=)\\d+\\.\\d+\\.\\d+\\.") %>%
                               magrittr::extract(!is.na(.)) %>%
                               stringr::str_replace_all(pattern = "\\.",
                                                        replacement = "\\\\.") %>%
                               paste0("^",  .) %>%
                               stringr::str_subset(string =
                                                     binman::list_versions(appname = "chromedriver") %>%
                                                     dplyr::last()) %>%
                               as.numeric_version() %>%
                               max() %>%
                               as.character(),
                             port = x)
  remDr <<- rD[["client"]]
})

# List element to iterate with parallel processing
pgs <- list("https://www.google.com",
            "https://www.nytimes.com",
            "https://www.ft.com")

# Define iteration
parLapply(clust, pgs, function(x) {
  remDr$navigate(x)
})

# Close Selenium on each core
close_rselenium()

# Stop the cluster
stopCluster(clust)

