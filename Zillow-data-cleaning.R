# ------------------------------------------------------------------------------
# CLEANING THE DATA:
#-------------------------------------------------------------------------------
# Libraries: 
library(tidyverse)
library(rvest)
library(stringr)

# In this file, I cleaned the data and form the final dataset:
data_folder = paste0("/Users/joseph_gonzalez/Desktop/",
                  "STA 220/FINAL Project/Zillow datasets ventura county")
house_data_files = list.files(data_folder) #File with the zillow datasets only
real_estate_data = lapply(house_data_files, function(x)
                      read.csv(paste0(data_folder,"/", x),
                               header = TRUE,sep = ";"))
real_estate_data = bind_rows(real_estate_data)
real_estate_data = real_estate_data %>%
                 filter(!is.na(real_estate_data$beds) & 
                        !is.na(real_estate_data$baths))

# Import the scraped data and combine the data frames:
house_data_1 = read.csv("vc_home_dataset_part1_organized.csv", header = TRUE)
house_data_2 = read.csv("vc_home_dataset_part2_organized.csv", header = TRUE)
ind_home_data = rbind(house_data_1, house_data_2)

# read in appliances data:
# Scraping file fixed this code can be deleted afte writing final file:
appliances_data = read.csv("home_appliances.csv", header = TRUE)
all_homes_data = data.frame(real_estate_data, ind_home_data, appliances_data) 
#Save this as full dataset

#-------------------------------------------------------------------------------
### FIX VARIABLES
#-------------------------------------------------------------------------------
## First, we can decide which variables can be removed:
colnames(all_homes_data)

# We can remove address, links, and page number.
full_homes_data = all_homes_data[, -c(1, 8, 12)] #Remove 12 later
colnames(full_homes_data) = c("City", "Address", "Price", "Beds", 
                              "Baths", "House_Area", "Flooring", 
                              "Heating", "Cooling", "Home_Type",
                              "Garage_Type", "Total_spaces", "Levels",
                              "Age", "Foundation", "Fire_Place",
                              "Lot_Size", "Appliances")
sapply(full_homes_data, class)
address_tables = table(full_homes_data$Address)
remove_index = names(address_tables[which(address_tables > 1)])
for (i in 1:length(remove_index)){
  index = which(full_homes_data$Address == remove_index[i])
  full_homes_data = full_homes_data[-index[1],]
}

# City, Price, Beds, Baths, House_Area, age, total spaces, and 
# appliances are in the correct format

# Format Flooring variable:----------------------------------------------------
table(full_homes_data$Flooring)

# Categorical variable, we can make cost levels for this variable
# High: house has costly materials - Stone, Tile, Hardwood
# Medium: house has moderately costly materials - wood, bamboo
# Low: house has low cost materials - vinyl, carpet, laminate, concrete
#Start with high cost materials first:

full_homes_data = full_homes_data %>%
      mutate(Flooring = replace(Flooring,
                        grepl("(Stone)|(Hardwood)|(Tile)", Flooring), "High"))

full_homes_data = full_homes_data %>%
  mutate(Flooring = replace(Flooring,
                            grepl("(Wood)|(Bamboo)", Flooring), "Medium"))

#We can assume that if flooring material isn't provided, 
#it is most likely low cost
full_homes_data = full_homes_data %>%
  mutate(Flooring = replace(Flooring, 
                      grepl("(Carpet)|(Vinyl)|(Laminate)", Flooring), "Low"),
         Flooring = replace(Flooring,
                      grepl("(Concrete)|(Linoleum)|(Other)|(See Remarks)",
                                                              Flooring), "Low"),
         Flooring = replace(Flooring, is.na(Flooring), "Unspecified"))

table(full_homes_data$Flooring)

# Format Heating variable:-----------------------------------------------------
# We want to capture that the house has a fireplace -- Removed

#full_homes_data = full_homes_data %>%
   #mutate(Fire_Place = replace(Fire_Place, grepl("Fireplace", Heating), TRUE))

#Now, we investigate the options:
table(full_homes_data$Heating)
any(grepl("Space", full_homes_data$Heating))
which(grepl("Pump", full_homes_data$Heating))

# There appears to be many options, 
# One: We can try to organize by most expensive to least expensive
# Heat Pump, Gas, Electric, Natural Gas, Zoned etc.
# Two: We can indicate whether the estate has heating
# For this project, I decided to go with option two

#Option 1 code:
#full_homes_data = full_homes_data %>%
  #mutate(Heating = replace(Heating, grepl("Heat Pump",Heating), "Heat Pump"),
  #Heating = replace(Heating, grepl("(^Gas)|([^A-z]+ Gas)|(Propane)", 
                                                            #Heating), "Gas"),
  #Heating = replace(Heating, grepl("Electric",Heating), "Electric"),
  #Heating = replace(Heating, grepl("Natural Gas",Heating), "Natural Gas"),
  #Heating = replace(Heating, grepl("Forced Air",Heating), "Forced Air"),
  #Heating = replace(Heating, grepl("(^Furnace)|([^A-z]+ Furnace)", 
                                                        # Heating), "Furnace"),
  #Heating = replace(Heating, grepl("Zoned",Heating), "Zoned"),
  #Heating = replace(Heating, grepl("(Central)|(Other)|(Yes)", Heating), 
                                                      #"Unspecified Central"),
  #Heating = replace(Heating, grepl("See Remarks",
                                    #Heating) | is.na(Heating), "None"))

# option two
full_homes_data = full_homes_data %>%
  mutate(Heating = replace(Heating, is.na(Heating) | 
                    grepl("(^None$)|(^See Remarks$)|(^Fire Place\\(?s?\\)?$)", 
                          Heating), FALSE), 
         Heating = replace(Heating, !(Heating == FALSE),TRUE)
         )
full_homes_data$Heating = as.logical(full_homes_data$Heating)
table(full_homes_data$Heating)

# Format Cooling variable:-----------------------------------------------------
# Similar to heating, we have two options
# For this project, I decided option two:
table(full_homes_data$Cooling)

# The "15" and "c" options are cooling systems, scraping code adjusted
# "None, C" is also a cooling system. 
# I don't count ceiling fans as a cooling system

full_homes_data = full_homes_data %>%
  mutate(Cooling = replace(Cooling, is.na(Cooling) | 
                  grepl("(^See Remarks$)|(^None$)|(^Ceiling Fan\\(?s?\\)?$)",
                                                Cooling), FALSE), 
         Cooling = replace(Cooling,  !(Cooling == FALSE), TRUE))

full_homes_data$Cooling = as.logical(full_homes_data$Cooling)
table(full_homes_data$Cooling)

# Check the home types:--------------------------------------------------------
table(full_homes_data$Home_Type) 


# We will check other, MobileManufactured triple, unknown, 
# and MobileManufactured Double 
# If there is info I can add, I will adjust the data entry
# other:
full_homes_data %>%
  filter(Home_Type == "Other")

# Not much info on this house, I suggest not including in the 
# modeling or possibly removing these observations.
# Check Na home types:
all_homes_data %>%
  filter(is.na(home.type))
# There are only 2 NA home types. Most variables are missing and, 
# therefore, these should not be considered in the modeling part.

# MobileManufactured, triple:
full_homes_data %>%
  filter(Home_Type == "MobileManufactured, Triple")

# The estate is single level and does have a fire place

full_homes_data = full_homes_data %>%
  mutate(Levels = replace(Levels, 
                          Home_Type == "MobileManufactured, Triple", "One"))

# MobileManufactured Double:
full_homes_data %>%
  filter(Home_Type == "MobileManufactured, Double")

#Single Level and Raised
full_homes_data = full_homes_data %>%
  mutate(Levels = replace(Levels, 
                          Home_Type == "MobileManufactured, Double", "One"),
         Foundation = replace(Foundation, 
                          Home_Type == "MobileManufactured, Double", "Raised"))

#Unknown:
full_homes_data %>%
  filter(Home_Type == "Unknown")

all_homes_data %>%
  filter(home.type == "Unknown")
# all unknowns apear to be single level, except last one
# most seem to be a singlefamily homes
#Unknowns seem to be singlefamily
full_homes_data = full_homes_data %>%
  mutate(Levels = replace(Levels, 
                          is.na(Levels) & Home_Type =="Unknown", "One"),
         Foundation = replace(Foundation,
                        (is.na(Foundation) | grepl("See Remarks", Foundation)) 
                                          & Home_Type == "Unknown" , "Raised"),
          Home_Type = replace(Home_Type,
                              Home_Type == "Unknown" , "SingleFamily"))


# Edit Parking features:-------------------------------------------------------
#Most concerned with garage types
table(full_homes_data$Garage_Type)
# Three Door, Two Door garage, Single door|garage|attached,
# Covered|carport, Assigned, Unspecified
# Few issues with scraping. This will be adjusted later
# Notes:
# "1" is covered
# "2" garage, "3" garage
# "6", "2, Inside Entrance",  two door
# "3, Inside Entrance" three door
# blanks will be entered manually

all_homes_data %>%
  filter(parking.features == "")

full_homes_data = full_homes_data %>%
  mutate(Garage_Type = replace(Garage_Type, 
                            grepl("(Three Door)|(3, Inside Entrance)", 
                                    Garage_Type), "Three Door"),
         Garage_Type = replace(Garage_Type,
                            grepl("(Two Door)|(6)|(2, Inside Entrance)", 
                                     Garage_Type), "Two Door"),
         Garage_Type = replace(Garage_Type, 
                              grepl("(Single Door)|(Garage)|(Attached)|(2)|(3)",
                                    Garage_Type), "Single Door"),
         Garage_Type = replace(Garage_Type, 
                               grepl("(Covered)|(Carport)|(Cochere)|(1)",
                                     Garage_Type), "Covered")
         )
# Blanks:
blanks_corrected = rep("Covered", 27)
blanks_corrected[c(1,9,20,21,25:27)] = "None"
blanks_corrected[c(18,22,23)] = "Single Door"

full_homes_data$Garage_Type[
  which(full_homes_data$Garage_Type == "")] = blanks_corrected

#Check NAs:
all_homes_data %>%
  filter(is.na(parking.features))

# Fix the rest and NAs
#Most NAs do not have garages

full_homes_data = full_homes_data %>%
  mutate(Garage_Type = replace(Garage_Type, is.na(Garage_Type) | 
         !(grepl("(Three Door)|(Two Door)|(Single Door)|(Covered)", 
                 Garage_Type)), "None")) 
table(full_homes_data$Garage_Type)

# Edit Total Spaces:-----------------------------------------------------------
class(full_homes_data$Total_spaces) #Appears to be in correct form
all_homes_data %>%
  filter(is.na(total.spaces))
# all seem to have at least two spaces
full_homes_data = full_homes_data %>% 
          mutate(Total_spaces = replace(Total_spaces, is.na(Total_spaces), 2))

# Edit Levels-------------------------------------------------------------------
table(full_homes_data$Levels)

all_homes_data %>%
  filter(levels == "Split")

# "Level" has 3 stories
# "Split" is for multi/split
# For the Missing values, we can assume that they are one story.
full_homes_data = full_homes_data %>% 
  mutate(Levels = replace(Levels, grepl("Three|(Levels?)", Levels), "3+"),
         Levels = replace(Levels, grepl("Two|(Split)", Levels), "2"),
         Levels = replace(Levels, grepl("One", Levels), "1"),
         )

# After further consideration, the missing values vary too much on level.
# Therefore, we may need to remove level from the regression analysis

# Age:-------------------------------------------------------------------------
class(full_homes_data$Age)

all_homes_data %>%
  filter(is.na(year.built))
#No futher details on age

# Edit Foundation--------------------------------------------------------------
table(full_homes_data$Foundation)
#Too many missing values, removing foundation from analysis
colnames(full_homes_data)
full_homes_data = full_homes_data[, -15]

#Edit Fireplace:---------------------------------------------------------------
sum(is.na(full_homes_data$Fire_Place))
# Too m any missing values from fire place
# Remove from analysis
full_homes_data = full_homes_data[, -15]


# Next, we can turn lot size into an integer: ---------------------------------
#Make sure all are in sqft first:
full_homes_data %>%
  filter(!grepl("sqft",Lot_Size))
# It appears some entries are in acres
# When I convert to integers, I will keep in mind the positions for acre values
acre_index = which(grepl("Acres", full_homes_data$Lot_Size))

convert_to_integers = function(size_string){
  num_string = str_extract(size_string, "[\\d,]+")
  comma_removed_values = gsub(",", "", num_string)
  return(as.integer(comma_removed_values))
}

sq_feet_values = sapply(full_homes_data$Lot_Size, convert_to_integers)

#Convert the acres to square feet:
sq_feet_values[acre_index] = sq_feet_values[acre_index] * 43560
names(sq_feet_values) = NULL
full_homes_data$Lot_Size = sq_feet_values

# Check Appliances:---------------------------------------------
sum(is.na(full_homes_data$Appliances)) #Too many missing to check
class(full_homes_data$Appliances)

# Add population: ------------------------------
population_data = read.csv("Ventura_Cities_Data.csv",
                           header = TRUE, sep = ";")
population_data$populations = gsub(",", "", population_data$populations) %>%
                                      as.integer()

full_homes_data = full_homes_data %>%
                    mutate(population =  NA)
# Function to make a population variable.
# City populations:

get_population = function(city_name){
  full_homes_data <<- full_homes_data %>%
                      mutate(population = replace(population, City == city_name, 
                      population_data[population_data$city_names == city_name, 
                                      "populations"]))
}

sapply(population_data$city_names, get_population)

# check the datatypes one more time:
sapply(full_homes_data, class)

#Write final dataset to a CSV
write_csv2(full_homes_data, paste0("/Users/joseph_gonzalez/Desktop/STA 220/",
                    "FINAL Project/final_zillow_datset.csv"), col_names = TRUE)






















