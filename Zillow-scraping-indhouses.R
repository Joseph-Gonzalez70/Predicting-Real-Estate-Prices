#------------------------------------------------------------------------------
### Scraping & Cleaning Zillow data:
#------------------------------------------------------------------------------
# This files scraps individual home data and cleans some features
library(tidyverse)
library(rvest)
library(stringr)
library(lubridate)

# ****Working Directory should be the file with all data files*****
data_files = list.files(getwd()) #File with the zilow datasets only
real_estate_data = lapply(data_files, 
                          function(x) read.csv(x, header = TRUE,sep = ";"))
real_estate_data = bind_rows(real_estate_data)
real_estate_data = real_estate_data[which(
  !is.na(real_estate_data$beds) & !is.na(real_estate_data$baths)),]
html_list = list()


##-----------------------------------------------------------------------
## FUNCTIONS TO OBTAIN HOUSE SPECIFIC DATA
##-----------------------------------------------------------------------
# This function grabs the values we want to form variables
# These variables include:
          ## Family type, year built, lot sqft, parking, heating, cooling
          # Number of appliances included
          # flooring, new construction, levels, parking spaces etc
# Set house keys to keys that we want variables for:
house_keys = c("flooring", "heating", "cooling", 
               "appliances", "home type", "parking features",
               "total spaces", "(levels)|(stories)", 
               #year built", "Foundation", "Fire Place", "lot size"
               )


get_num_appliances = function(house_details){
  appliance_string = house_details[grepl("appliances?", 
                                         house_details, ignore.case = TRUE)]
  # On Zillow, the appliances seem to be separated by either a comma
  # or a space. Therefore, we can split on both and get max.
  appliances = str_extract(appliance_string, "[^:]+$") %>%
               trimws("both")
  appliances =  appliances[which.max(nchar(appliances))]
  if(nchar(appliances) == 1 |
     nchar(appliances)==0 |
     is.na(appliances)){
    return(NA)
  }
  appliances_1 = str_split(appliances, ",")
  return(length(appliances_1[[1]]))
}

get_years_old = function(house_details){
  year_built = house_details[grepl("year[ ]?built", 
                                   house_details, ignore.case = TRUE)]
  house_year = str_extract(year_built, "[^:]+$") 
  house_year =  house_year[1]
  if(length(house_year) == 0 |
     is.na(house_year) |
     nchar(house_year) != 4){
    return(NA)
  }
  house_age = year(Sys.time())-as.integer(house_year)
  return(house_age)
}

## The Function below finds the key in the house details and
## extracts the information in a convenient form. For the appliances data, 
## I aim to get the number of appliances that come with the house. 
## For year built, I aim to convert this value to house age.
check_house_keys = function(house_key, house_details){ 
  if(house_key == "appliances" & 
     any(grepl("appliances?", house_details, ignore.case = TRUE))){
      return(get_num_appliances(house_details)) 
  }else if(house_key == "year built" & 
           any(grepl("year[ ]?built", house_key, ignore.case = TRUE))){
      return(get_years_old(house_details))
  }else{
      index = grepl(house_key, house_details,
                     ignore.case = TRUE)
      if(all(index == FALSE)){
        return(NA)
      }
      house_detail = house_details[index]
      house_feature = str_extract(house_detail, "[^:]+$") %>%
                      trimws("both")
      house_feature = paste(house_feature, collapse =", ")
      return(house_feature)
    }
}

#This function reads the html file and obtains the data unorganized
#ENTER WEBSITE COOKIE BELOW!!! ***********************************************
web_cookie_new = paste0("")
get_features = function(house_link){
  Sys.sleep(6)
  home_html = read_html(house_link, 
                        cookie = web_cookie_new)
  html_list <<- c(html_list, as.character(home_html))
  various_feat_html = home_html %>%
    html_nodes('.bjTesh') #This does change(CSS one above property details)****
  #Below gives full list of house characteristics
  #Only takes CSS values
  house_details = various_feat_html %>%
    html_nodes(css_values) %>%
    html_text()
  house_characteristics = sapply(house_keys, check_house_keys, house_details)
  return(house_characteristics)
}

#These css values seem to change daily:
## **May need to update everytime we run or use regex
css_values = ".foiYRz"

first_half_links = real_estate_data$house_links #retrunto 1:500
home_features_data_1 = sapply(first_half_links, get_features)

#bFix the data set to be in a form where we will combine it with the other data:
colnames(home_features_data_1) = NULL
home_features_transposed = t(home_features_data_1)
home_features_transposed = as.data.frame(home_features_transposed)

# I would suggest saving the html list.
# If more data is needed, It can be referred to easily.

# We stopped half way, now we finish the rest:
second_half_links = real_estate_data$house_links[501:length(
                                                real_estate_data$house_links)]
second_half_data = sapply(second_half_links, get_features)


# Fix the data set to be in a form where we will combine it with the other data:
colnames(second_half_data ) = NULL
second_half_data_transposed = t(second_half_data)
second_half_data_transposed = as.data.frame(second_half_data_transposed)

write_csv(second_half_data_transposed, 
          paste0("/Users/joseph_gonzalez/Desktop/STA 220/FINAL Project/",
                 "Zillow datasets ventura county/",
                 "vc_home_dataset_part2_organized.csv"), 
          col_names = TRUE)

#I would suggest saving the html list. 
# If more data is needed, It can be referred to easily:
#html_files = unlist(html_list)
#write_csv(as.data.frame(html_files), 
# "/Users/joseph_gonzalez/Desktop/STA 220/FINAL Project/
# Zillow datasets ventura county/house_html_files_2.csv")
#write.table(as.data.frame(html_files),
            #"/Users/joseph_gonzalez/Desktop/STA 220/FINAL Project/
            #Zillow datasets ventura county/house_html_files_2.text",
            #sep ="--xx--")



