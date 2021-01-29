# Preamble ##################################################################################################################################################################################################################

# ---
# title: 'Freezing Fires: Toronto Firefighters were More Likely to be Injured or Killed while Battling Rapidly Spreading House Fires in the Winter between 2011 to 2019'
# author: "Laura Cline"
# email: laura.cline@mail.utoronto.ca
# date: "29/01/2021"
# abstract: "This paper uses exploratory data analysis to discover the most likely causes of Toronto Fire Service (TFS) firefighter casualties between 2011 to 2019. Using Open Data Toronto's fire incident dataset, 
# the paper exposes that the majority of firefighter casualties occured at residential fires, during the winter months, 
# and when the fire had already spread beyond of object of origin when the first unit arrived on scene. Although the TFS is one of the few Canadian fire departments with declining firefighter causality rates, the results 
# demonstrate where the TFS can take proactive steps and develop timely interventions to create safer operational 
# environments and further reduce firefighter casualties." 
# output:
#   bookdown::pdf_document2:
#   toc: FALSE
# bibliography: references.bib 
# ---

# Setup Environment #########################################################################################################################################################################################################

#Install packages and open libraries 

# install.packages("tidyverse")
# install.packages("here")
# install.packages("opendatatoronto")
# install.packages("lubridate")
# install.packages("tinytex")
# install.packages("kableExtra")
library(tidyverse)
library(here)
library(opendatatoronto)
library(lubridate)
library(tinytex)


# Load the Data #############################################################################################################################################################################################################

# Get Fire Incident Packages
fire_incidents_packages <- search_packages("fire incidents")  
fire_incidents_packages 

# Get all the resources for this package 
fire_incidents_resources <- fire_incidents_packages %>%
  list_package_resources()

# Download the resource (i.e., the actual data) directly into R using resource():
fire_incidents_data <- fire_incidents_resources %>%
  get_resource()
fire_incidents_data

# Write raw data to a csv
write_csv(fire_incidents_data, "inputs/raw_fire_incidents_data.csv")


# Clean Data ################################################################################################################################################################################################################

# Create a dataset for extent_of_fire analysis 
# Remove all "NA" character values from the Extent_of_Fire column 
fire_incidents_data_clean_eof <- read_csv("inputs/raw_fire_incidents_data.csv") %>% 
  filter(Extent_Of_Fire != "NA")

# Create a datadet for alarm_time analysis 
#Convert character vector to datetime 
fire_incidents_data_clean_times <- fire_incidents_data 
fire_incidents_data_clean_times$TFS_Alarm_Time <- as_datetime(fire_incidents_data_clean_times$TFS_Alarm_Time)

# Double checking that the TFS_Alarm_Type column was converted to datetime
class(fire_incidents_data_clean_times$TFS_Alarm_Time)


# Descriptive Data Analysis #################################################################################################################################################################################################

## Total Fire Incidents, Firefighter Casualties, and Average Casualties per Incident ========================================================================================================================================

firefightercasulties_table <-
  fire_incidents_data %>% # Load dataset
  summarise( # Summarise for total number of fire incidents, sum firefighter causalities, and calculate the average number of casualties per incident
    Total_Fire_Incidents = n(),
    Total_Firefighter_Casulties = sum(TFS_Firefighter_Casualties, na.rm = TRUE), # Remove possible missing values 
    Average_Casulties_per_Incident = Total_Firefighter_Casulties / Total_Fire_Incidents) %>% 
  rename("Total Fire Incidents" = Total_Fire_Incidents, "Total Firefighter Casualties" = Total_Firefighter_Casulties,
         "Average Casualties per Incident" = Average_Casulties_per_Incident) # Rename the columns so they are more human readable and clean 

firefightercasulties_table %>% # Create a table 
  slice(1:5) %>% 
  knitr::kable(caption = "Total Fire Incidents, Total Firefighter Casualties, and Averate Firefighter Casualties per Fire Incident (2011-2019)") %>%
  kableExtra::kable_styling() # Create a cleaner table 


## Top Ten CAD Event Types with the Highest Number Firefighter Casualties ===================================================================================================================================================

# Create a table showing all the total number of casualties per CAD event type between 2011 to 2019 
firefightercasulties_fireextent_table <-
  fire_incidents_data %>%
  group_by(Initial_CAD_Event_Type) %>% # Grouping by the CAD Event Time 
  summarise(
    Sum = sum(TFS_Firefighter_Casualties, na.rm = TRUE) # Sums all firefighter casualties in the column and removes all missing values from the calculation 
  ) %>%
  arrange(desc(Sum)) %>% # Sorts the table so CAD event types with the highest firefighter casualties appear at the top  
  filter(Sum != 0) %>% # Shorten table by filtering out CAD event types that do not have any firefighter casualties 
  rename("Number of Firefighter Casualties" = Sum, "Initial CAD Event Type" = Initial_CAD_Event_Type)
firefightercasulties_fireextent_table %>% # Creates table 
  slice(1:10) %>% # Only show the top two rows 
  knitr::kable(caption = "Top Ten CAD Event Types with the Highest Number Firefighter Casualties (2011-2019)") %>%
  kableExtra::kable_styling() # Create a cleaner table 


## Total Fire Incidents per Month ===========================================================================================================================================================================================

# Create a Line Graph that shows the counts the number of fire incidents in each month between 
# 2011 to 2018. I am curious which months have more fire incident activity 
fire_incidents_data_clean_times %>% 
  mutate(Month = month(TFS_Alarm_Time)) %>% # Pulls the month value out of then TFS_Alarm_Time datetime column and I am duplicating the value into a column called "Month" 
  group_by(Month) %>% # Performs operations on the new Month column 
  summarise( # Counts all fire incidents per month 
    n = n()
  ) %>% 
  ggplot(mapping = aes(x = Month, y = n)) + # Creates plot with Month on the x-axis and the number of fire incidents on the y-axis 
  geom_line(aes(color = "red")) + # Creates line graph
  scale_y_continuous(limits = c(0,NA)) + # Makes y-axis start at 0 
  scale_x_continuous(breaks = seq(1, 12, by = 1)) + # Removes decimals from the x-axis scale so it is more clear which month the number of calls belongs to 
  labs( # Add labels 
    title = "Number of TFS Fire Incidents Peaked in May Between 2011 and 2019",
    subtitle = "The number of fire incidents in Toronto started increasing in February, peaked in May, \nand plateaued by August.",
    caption = "(data from 'opendatatoronto' package)",
    x = "Month",
    y = "# of Fire Incidents"
  ) +
  theme_bw() + # Cleaner theme 
  guides(color = FALSE) # Removes legend 


## Total Firefighter Casualties per Month ===================================================================================================================================================================================

fire_incidents_data_clean_times %>% 
  mutate(Month = month(TFS_Alarm_Time)) %>% # Pulls the month value out of then TFS_Alarm_Time datetime column and I am duplicating the value into a column called "Month" 
  group_by(Month) %>% # Performs operations on the new Month column 
  summarise(
    Sum_of_firefighter_casulties = sum(TFS_Firefighter_Casualties, na.rm = TRUE) # Sums all firefighter casualties in the column and removes all missing values from the calculation 
  ) %>%
  ggplot(mapping = aes(x = Month, y = Sum_of_firefighter_casulties)) + # Creates plot with Month on the x-axis and the number of firefighter casualties on the y-axis 
  geom_line(color = "red") + # Creates line graph 
  scale_y_continuous(limits = c(0,NA)) + # Makes y-axis start at 0 
  scale_x_continuous(breaks = seq(1, 12, by = 1)) + # Removes decimals from the x-axis scale so it is more clear which month the number of calls belongs to  
  labs( # Add labels 
    title = "Number of TFS Firefighter Casulties Peaked in January Between \n2011 and 2019",
    subtitle = "The number of firefighter casulties in Toronto has an unusual pattern where there are \nmultiple sharp fluctuations in casulties per month.",
    caption = "(data from 'opendatatoronto' package)",
    x = "Month",
    y = "# of TFS Firefighter Casulties") +
  theme_bw() + # Cleaner theme 
  guides(color = FALSE) # Removes legend 


## Total Fire Incidents per Extent of Fire ==================================================================================================================================================================================

# Create bar plot that shows  number of fire incidents for each of "extent of fire" when the first unit arrived on the scene  
fire_incidents_data_clean_eof %>%
  ggplot(mapping = aes(x = Extent_Of_Fire)) + # Creates plot with Extent_of_Fire Month on the x-axis
  geom_bar(aes(color = "orange")) + # Creates bar plot 
  coord_flip() + # Flips our x-axis to the horizontal axis for easier readability 
  labs( # Add labels 
    title = "Majority of TFS Fire \nIncidents were Confined \nto Object of Origin \n(2011-2019)",
    caption = "(data from 'opendatatoronto' package)",
    x = "Extent of Fire",
    y = "# of TFS Fire Incidents") +
  theme_bw() + # Cleaner theme 
  guides(color = FALSE) # Removes legend


## Total Firefighter Casualties per Extent of Fire ==========================================================================================================================================================================

second_barchart <- fire_incidents_data_clean_eof %>%
  group_by(Extent_Of_Fire) %>% 
  summarise(sum_casulties = sum(TFS_Firefighter_Casualties)) %>% 
  arrange(desc(sum_casulties))
second_barchart %>% 
  ggplot(mapping = aes(x = reorder(Extent_Of_Fire, sum_casulties), y = sum_casulties)) +
  geom_bar(aes(color = "red"), stat = "identity") +
  coord_flip() +
  labs( # Add labels 
    title = "Firefighter Casualties \nwere Higher when Fire \nSpread Beyond \nObject of Origin \n(2011-2019)",
    caption = "(data from 'opendatatoronto' package)",
    x = "Extent of Fire",
    y = "# of TFS Firefighter Casulties"
  ) +
  theme_bw() + # Cleaner theme 
  guides(color = FALSE) # Removes legend 


## Total Firefighter Casualties per Year ====================================================================================================================================================================================

# Make a line graph for firefighter casualties per year 
fire_incidents_data_clean_times %>% 
  mutate(Year = year(TFS_Alarm_Time)) %>% # Pulls the month value out of then TFS_Alarm_Time datetime column and I am duplicating the value into a column called "Month" 
  group_by(Year) %>% # Performs operations on the new Month column 
  summarise(
    Sum_of_firefighter_casulties = sum(TFS_Firefighter_Casualties, na.rm = TRUE) # Sums all firefighter casualties in the column and removes all missing values from the calculation 
  ) %>%
  ggplot(mapping = aes(x = Year, y = Sum_of_firefighter_casulties)) + # Creates plot with Month on the x-axis and the number of firefighter casualties on the y-axis 
  geom_line(color = "blue", linetype = "dashed") + # Creates line graph 
  scale_y_continuous(limits = c(0,NA)) + # Makes y-axis start at 0 
  scale_x_continuous(breaks = seq(2011, 2019, by = 1)) + # Removes decimals from the x-axis scale so it is more clear which month the number of calls belongs to
  labs( # Add labels 
    title = "Number of TFS Firefighter Casulties Decreased Between 2011 and 2019",
    subtitle = "Despite the large spike 2013, the number of firefighter casualties has gradually decreased between 2011 to 2019.",
    caption = "(data from 'opendatatoronto' package)",
    x = "Year",
    y = "# of TFS Firefighter Casulties") +
  scale_color_brewer(palette = "YlOrRd") +
  theme_bw() + # Cleaner theme 
  guides(color = FALSE) # Removes legend 





















































