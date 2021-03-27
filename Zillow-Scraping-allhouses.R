#--------------------------------------------------------------
#-------------SCRAPING FUNCTIONS FOR ZILLOW
#--------------------------------------------------------------
# Libraries:
library(tidyverse)
library(rvest)
library(stringr)
#Here we scrape some important feature and the links to the houses
#We write to a csv file to save the information.
#The links will be used in the data cleaning file 
# Function to get the total number of posts on the current page:


get_post_total = function(page_html){
  str_amount = page_html %>% 
    html_node(".total-text") %>%
    html_text() 
  results_amount = as.integer(gsub("[, ]+","", str_amount))
  return(results_amount)
}

# Function to extract home specific info:
# Add site cookie***
get_home_info = function(num_pages, search_url, area_name){
  
  
  links = sprintf(paste0(search_url,"%d_p"), 1:num_pages) 
  
  results = map(links, ~ {
    Sys.sleep(30)
    # select houses
    houses = read_html(.x, cookie = ".") %>%
      #Nodes for all listings, photo-cards refers to section attribute,
      #li is the list node, article refers to each listing
      html_nodes(".photo-cards li article") 
    z_id = houses %>%
      html_attr("id")
    
    #Links for houses
    house_links = houses %>%
      html_node(".list-card-info a") %>%
      html_attr("href")
    
    # address 
    address = houses %>%
      html_node(".list-card-addr") %>%
      html_text()
    
    # price
    price = houses %>%
      html_node(".list-card-price") %>%
      html_text() %>%
      readr::parse_number()
    
    # info
    params = houses %>%
      html_node(".list-card-info") %>%
      html_text2()
    
    # number of bedrooms
    beds = params %>%
      str_extract("\\d+(?=\\s*bds)") %>%
      as.numeric()
    
    # number of bathrooms
    baths = params %>%
      str_extract("\\d+(?=\\s*ba)") %>%
      as.numeric()
    
    # total square footage
    house_a = params %>%
      str_extract("[0-9,]+(?=\\s*sqft)") %>%
      str_replace(",", "") %>%
      as.numeric()
    
    tibble(location_name = area_name, address = address,
           price = price,  beds= beds, baths=baths, 
           house_area = house_a, house_links = house_links)
    
  }
  ) %>% 
    bind_rows(.id = 'page_no')
  return(results)
}

write_to_csv = function(home_data, area_name){
  write_csv2(home_data,
             paste0("/Users/joseph_gonzalez/Desktop/STA 220/",
                    "FINAL Project/Zillow datasets ventura county/",
            area_name, "-home_data.csv"), col_names = TRUE)
}

# Function to get listings:
# Add site cookie***
get_listings = function(area_name, area_url){
  Sys.sleep(120) #Change this for faster scrape
  first_page_URL = paste0(zillow_url, area_url)
  first_page_html = read_html(first_page_URL, cookie = ".")
  post_total = get_post_total(first_page_html)
  if(post_total>=240){
    num_pages = 6
  }else if(post_total > 0 & post_total < 240){
    num_pages = ceiling(post_total/40)
  }else{
    results = tibble(location_name = area_name, price = NA, 
                     beds = NA, baths= NA, house_area = NA, 
                     house_links = NA)
    write_to_csv(results, area_name)
    return(results)
  }
  home_data = get_home_info(num_pages, first_page_URL, area_name)
  write_to_csv(home_data, area_name)
  return(home_data)
}

#---------------------------------------------------------------------------------------------------------------------------
#-------------END OF MAIN FUNCTIONS
#---------------------------------------------------------------------------------------------------------------------------
#Zillow URL
zillow_url = "https://www.zillow.com/"

### EXPLANATION FOR FOLLOWING CODE TO SCRAPE:
# At first, the full code intended to scrape for real estate in all California
# counties. Due to limitations with zillow, I decided to only scrape for real 
# estate data in cities with Ventura County(My current county). The code can be
#altered to do all california counties.

# County Names
#county_data = read.csv("California_Counties.csv", header = TRUE)
#county_half_url = gsub(" ", "-", county_data[,"Name"]) 
#county_half_url = paste0(county_half_url,"-county-CA/")

#--------------------------------------------------------------------------------------------------------------------------
# Import the city data:
#City Names:
ventura_county_data = read.csv("Ventura_Cities_Data.csv", 
                               header = TRUE, sep= ";")
area_names = ventura_county_data$city_names
areas_half_url =ventura_county_data$zillow_part_urls
house_data = mapply(get_listings, area_names, areas_half_url)

