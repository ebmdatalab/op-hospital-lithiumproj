##SECONDARY ANALYSIS:
 
# Installing packages needed:
install.packages("tidyverse")
library(tidyverse)
 
install.packages("dplyr")
library(dplyr)
 
install.packages("ggplot2")
library(ggplot2)
 
install.packages("scales")
library(scales)
 
install.packages("sf")
library(sf)
 
install.packages("here")
library(here)
 
install.packages("readr")
library(readr)
 
install.packages("data.table")
library(data.table)
 
# NEW DATASET TO BE WORKING FROM,
# Originally they were 26 organisations in the unlabelled region column, and 1 NA.
# Now using the new dataset on mergers there are 4, on thex-axis labelled region they come up as 'NA'kingdirectory = setwd("C:/Users/K22007671/OneDrive - King's College London")
getwd()
install.packages("readxl")
library(readxl)
Lithium_SCMD= read_excel('scmd_lithium-2.xlsx')
 
New_Missingregions<-Lithium_SCMD %>%
  filter(is.na(region)) %>%
  group_by(ods_name) %>%
  summarise(N=n())
 
print(New_Missingregions)
# Originally they were 26 organisations in the unlabelled region column, and 1 NA.
# Now using the new dataset on mergers there are 4, on thex-axis labelled region they come up as 'NA'
 
# Cleaning up dataset
# Manually adding the regions to the organisations (4) that come up in the NA region column
Lithium_SCMD <- Lithium_SCMD %>%
  mutate(region = ifelse(ods_name == "Black Country Healthcare NHS Foundation Trust" & is.na(region),
                         "West Midlands",
                         region))
 
Lithium_SCMD <- Lithium_SCMD %>%
  mutate(region = ifelse(ods_name == "Bradford District Care NHS Foundation Trust" & is.na(region),
                         "Yorkshire and The Humber",
                         region))
 
Lithium_SCMD <- Lithium_SCMD %>%
  mutate(region = ifelse(ods_name == "Camden and Islington NHS Foundation Trust" & is.na(region),
                         "London",
                         region))
 
Lithium_SCMD <- Lithium_SCMD %>%
  mutate(region = ifelse(ods_name == "Sheffield Health & Social Care NHS Foundation Trust" & is.na(region),
                         "Yorkshire and The Humber",
                         region))
 
 
unique(Lithium_SCMD$region) # ALL organisations are now attributed to a region
 
#converting from date-time to date, will make plotting easier
class(Lithium_SCMD$year_month) #[1] "POSIXct" "POSIXt"
 
Lithium_SCMD <- Lithium_SCMD %>%
  mutate(year_month = as.Date(year_month)) # converting to date from date-time
 
Lithium_SCMD <- Lithium_SCMD %>%
  filter(indicative_cost >= 0)
 
# changing from 9 to 7 regions, beause the primary care dataset has 7 regions:
# Step 1: Merging specific regions: North East and Yorkshire and the Humber and the east and west midlands
# Replace specific regions with broader region names
Lithium_SCMD$region_grouped <- with(Lithium_SCMD,
                                    ifelse(region %in% c("East Midlands", "West Midlands"), "Midlands",
                                           ifelse(region %in% c("North East", "Yorkshire and The Humber"), "North East & Yorkshire",
                                                  region)))
View(Lithium_SCMD)
unique(Lithium_SCMD$region_grouped)
 
# Removing ampoule from Lithium_SCMD$unit_dose_uom because lithium injectable is not used in secondary care Orla said
unique(Lithium_SCMD$unit_dose_uom)
 
Lithium_SCMD <- Lithium_SCMD %>%
  filter(unit_dose_uom != "ampoule")
 
# Creating a quantity_mg column for regional analysis
quantity_mg = Lithium_SCMD$strnt_nmrtr_val * Lithium_SCMD$quantity
 
Lithium_SCMD <- Lithium_SCMD %>%
  mutate(quantity_mg = quantity_mg)
 
# Secondary care regional plots:
# Regional plot, vtm_name: Lithium carbonate vs Citrate
ggplot(Lithium_SCMD, aes(x = reorder(region_grouped, -quantity_mg), y = quantity_mg, fill = vtm_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Secondary care Regional breakdown of Lithium usage",
       x = "Region",
       y = "Lithium usage (quantity_mg)",
       fill = "VTM Name") +
  scale_y_continuous(labels = label_comma()) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
 
# Regional plot, vmo_name: Lithium carbonate vs Citrate products
ggplot(Lithium_SCMD, aes(x = reorder(region_grouped, quantity_mg), y = quantity_mg, fill = vmp_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Secondary care Regional breakdown of Lithium usage",
       x = "Region",
       y = "Lithium usage (quantity_mg)",
       fill = "VMP Name") +
  scale_y_continuous(labels = label_comma()) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
 
# Organisation breakdown in MGs for Lithium Carbonate and Citrate:
# PLOTTING top 50 organisations to help with readability:
top_50_data <- Lithium_SCMD %>%
  group_by(ods_name) %>%
  summarise(total_quantity = sum(quantity_mg)) %>%
  arrange(desc(total_quantity)) %>%
  slice(1:50)
 
top_50_data <- top_50_data %>%
  inner_join(Lithium_SCMD, by = "ods_name")
 
# Plotting top 50 organisations: # Organisation breakdown in MGs for Lithium Carbonate and Citrate:
ggplot(top_50_data, aes(x = reorder(ods_name, total_quantity), y = quantity_mg, fill = vtm_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 50 Organisations' Lithium Usage",
       x = "Organisation (ods_name)",
       y = "Lithium usage (quantity_mg)",
       fill = "VTM Name") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 0.25),
        axis.text.x = element_text(angle = 90, hjust = 1))
 
# Plotting top 50: Organisational breakdown in MGs for the products of Lithium citrate and carbonate:
ggplot(top_50_data, aes(x = reorder(ods_name, total_quantity), y = quantity_mg, fill = vmp_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 50 Organisations' Lithium Usage, looking at vmp_name",
       x = "Organisation (ods_name)",
       y = "Lithium usage (quantity_mg)",
       fill = "VMP Name") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 0.25),
        axis.text.x = element_text(angle = 90, hjust = 1))
 
# Secondary care Mapping plot, 2023 mg / population
################################################
# NEW SHAPEFILE - it was in a python format so had to make RDS
library(sf)
 
# Read the GeoJSON file
geojson_file <- "/Users/K22007671/OneDrive - King's College London/Desktop/Research methods 1 2022/Represent/new_nuts_shp.txt"
geojson_data <- st_read(geojson_file)
 
# Save the object as an RDS file
saveRDS(geojson_data, file = "/Users/K22007671/OneDrive - King's College London/Desktop/Research methods 1 2022/Represent/new_nuts_shp.rds")
 
# Load the RDS file
nuts_shp <- readRDS( "/Users/K22007671/OneDrive - King's College London/Desktop/Research methods 1 2022/Represent/new_nuts_shp.rds")
View(nuts_shp)
 
# Ensuring geometries are valid
nuts_shp <- st_make_valid(nuts_shp)
View(nuts_shp)
########################################################
 
# Secondary care Mapping plot, focusing on all of 2023 mg / population
# getting the sum of mg per region
unique(Lithium_SCMD$region_grouped)
 
lithium_df<-Lithium_SCMD %>%
  # getting the sum of mg per region
  group_by(region_grouped) %>%
  summarise(quantity_mg =sum(quantity_mg)) %>%
  mutate(name = case_when(
    region_grouped =="East of England"~"UKH",
    region_grouped=="North West"~"UKD",
    region_grouped=="North East & Yorkshire"~"UKE",
    region_grouped=="London"~"UKF",
    region_grouped=="Midlands"~"UKG",
    region_grouped=="South East"~"UKJ",
    region_grouped=="South West"~"UKK"),
         region = as.factor(region_grouped)) %>%
  filter(!is.na(region_grouped))
 
View(lithium_df)
 
twenty_three <- Lithium_SCMD %>%
  filter(year_month %in% as.Date(c("2023-01-01",
                                   "2023-02-01",
                                   "2023-03-01",
                                   "2023-04-01",
                                   "2023-05-01",
                                   "2023-06-01",
                                   "2023-07-01",
                                   "2023-08-01",
                                   "2023-09-01",
                                   "2023-10-01",
                                   "2023-11-01",
                                   "2023-12-01")))
 
 
View(twenty_three)
 
twenty_three <- twenty_three %>%
  mutate(twentythree_quantity_mg = strnt_nmrtr_val * quantity)
 
# Calculate the total mg of Lithium for each region in 22023
total_mg_by_region <- twenty_three%>%
  group_by(region_grouped) %>%
  summarize(total_mg2023 = sum(twentythree_quantity_mg, na.rm = TRUE))
 
View(total_mg_by_region)
 
secondary_lithium_df <- lithium_df %>%
  mutate(total_mg_by_region)
 
 
# Now adding population data to this dataset, that I got from population_data, estimates from ONS
NorthEast_Yorkt_pop = 8224302
NorthWest_pop = 7516113
midlands_pop = 10956592
east_pop = 6398497
london_pop = 8866180
SouthEast_pop = 9379833
SouthWest_pop = 5764881
 
 
secondary_lithium_df <- secondary_lithium_df %>%
  mutate(population = case_when(
    region == "North East & Yorkshire" ~ NorthEast_Yorkt_pop,
    region == "North West" ~ NorthWest_pop,
    region == "Midlands" ~ midlands_pop,
    region == "East of England" ~ east_pop,
    region == "London" ~ london_pop,
    region == "South East" ~ SouthEast_pop,
    region == "South West" ~ SouthWest_pop,
  ))
 
View(secondary_lithium_df)
 
secondary_lithium_df <- secondary_lithium_df %>%
  mutate(`mg/population(2023)` = total_mg2023 / population)
 
 
# the names in the shape file and my regions in my lithium_df are different, so fixing this
mapping_table <- tibble(
  name = c(
    "LONDON COMMISSIONING REGION",
    "SOUTH WEST COMMISSIONING REGION",
    "SOUTH EAST COMMISSIONING REGION",
    "MIDLANDS COMMISSIONING REGION",
    "EAST OF ENGLAND COMMISSIONING REGION",
    "NORTH WEST COMMISSIONING REGION",
    "NORTH EAST AND YORKSHIRE COMMISSIONING REGION"
  ),
  region = c(
    "London",
    "South West",
    "South East",
    "Midlands",
    "East of England",
    "North West",
    "North East & Yorkshire"
  )
)
 
library(dplyr)
 
# Join nuts_shp with the mapping table
nuts_shp <- nuts_shp %>%
  left_join(mapping_table, by = "name")
 
# Join the datasets ~
coverage_data <- nuts_shp %>%
  left_join(secondary_lithium_df, by = "region")  # Join using the correct column names
 
View(coverage_data)
 
 
# Extract unique values for breaks and labels
unique_values <- sort(unique(coverage_data$`mg/population(2023)`))
breaks <- c(0, unique_values, max(unique_values, na.rm = TRUE) + 10)
labels <- scales::label_number(breaks)
 
# Plot with specified color scale
legend_labels <- c( "0" = "0",paste0(sort(round(secondary_lithium_df$`mg/population(2023)`, 1)), " mg/population"), "120" = "120")
 
coverage_plot <- coverage_data %>%
  ggplot(aes(geometry = geometry, fill = `mg/population(2023)`)) +
  geom_sf(lwd = 0.8, colour = 'black') +
  geom_sf_label(aes(label = region),  # Use `region` for labels
                colour = "white",
                label.size = 0.1,
                label.r = unit(0.5, "lines"),
                fun.geometry = st_centroid,
                show.legend = FALSE) +
  scale_fill_gradientn(
    colors = c("#ffd13a", "#ff7c00", "#f20c51"),
    breaks = breaks,
    labels = labels
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.2, 0.5),
    legend.text = element_text(hjust = 1),
    panel.background = element_rect(fill = "white")
  ) +
  ggtitle("Secondary care: Total mg of lithium used in 2023 regionally/ population estimates") +
  guides(fill = guide_legend(title = "mg Lithium/ population")) +
  xlab("") +
  ylab("")
 
# Display the plot
plot(coverage_plot)
 
# Acconpanying barplot: 2023/pop
view(secondary_lithium_df)
# ammended hist
hist = ggplot(secondary_lithium_df, aes(x = region_grouped, y = `mg/population(2023)`)) +
  geom_col(fill = "#FF0000"   , color = "#FF6f6f")+
  # Use geom_col() for bar plots where the height of bars is defined by y
  geom_text(aes(label = round(`mg/population(2023)`, 1)), vjust = -0.3, size = 3.5) +  # Add data labels
  theme_minimal() +
  xlab("Region") +
  ylab("Lithium usage (mg) / population") +
  ggtitle("Secondary Care: Total Regional Lithium Usage (mg) for 2023 / Population Estimates") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  #
    axis.text.y = element_text(size = 10), 
    plot.title = element_text(size = 12, face = "bold"), 
    plot.margin = margin(10, 10, 10, 10)  # Adjusting the plot margins for better spacing
  )
 
plot(hist)
 
# #  ######plot that is more focused on the last 6 months of 2023 for secondary care
# Filter for the last 6 months of 2023 in secondary care dataset
secondary_six_months <- Lithium_SCMD %>%
  filter(year_month %in% as.Date(c("2023-07-01",
                                   "2023-08-01",
                                   "2023-09-01",
                                   "2023-10-01",
                                   "2023-11-01",
                                   "2023-12-01")))
 
# Calculate the total mg of Lithium for each region for the last 6 months
secondary_total_mg_six_months <- secondary_six_months %>%
  mutate(twentythree_quantity_mg = strnt_nmrtr_val * quantity) %>%
  group_by(region_grouped) %>%
  summarize(total_mg_six_months = sum(twentythree_quantity_mg, na.rm = TRUE))
 
# Add population data to the secondary care dataset
secondary_lithium_df_six_months <- secondary_total_mg_six_months %>%
  left_join(secondary_lithium_df, by = "region_grouped") %>%
  mutate(`mg/population(2023)` = total_mg_six_months / population)
 
 
# Create the bar plot for secondary care using purple colors
hist_secondary <- ggplot(secondary_lithium_df_six_months, aes(x = region_grouped, y = `mg/population(2023)`)) +
  geom_col(fill = "#9b59b6", color = "#d1a3ff") +  # Purple fill with light purple border
  geom_text(aes(label = round(`mg/population(2023)`, 1)), vjust = -0.3, size = 3.5) +  # Add data labels
  theme_minimal() +
  xlab("Region") +
  ylab("Lithium usage (mg) /population") +
  ggtitle("Secondary Care: Total Regional Lithium Usage (mg) for the Last 6 Months of 2023 / Population Estimates") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Adjust angle and size of x-axis labels
    axis.text.y = element_text(size = 10),  # Adjust size of y-axis labels
    plot.title = element_text(size = 12, face = "bold"),  # Title style
    plot.margin = margin(10, 10, 10, 10)  # Adjust plot margins
  )
 
plot(hist_secondary)
 
##PRIMARY ANALYSIS:
workingdirectory = setwd("C:/Users/K22007671/OneDrive - King's College London")
PrimaryCare_Lithium= read.csv('df_primarycare.csv')
unique(PrimaryCare_Lithium$month) # STARTS FROM APRIL 2019 LIKE THE scmd DATASET
 
# I need to link practice codes to regions
Practise_codes= read.csv('practice_codes.csv')
 
# I NEED TO MERGE: PRACTICE of primarycare_lihtium to the column 'code' of practice codes
# Creating a merged dataset so now I have practice codes and their associated region
merged_data <- PrimaryCare_Lithium %>%
  left_join(Practise_codes, by = c("practice" = "code"))
 
View(merged_data)
head(merged_data)
 
#strnt_nmrtr_val
df_primarycare2= read.csv('df_primarycare2.csv')
View(df_primarycare2)
 
PRIMARYCARE_dataset <- merge(merged_data,
                             df_primarycare2[, c("bnf_code", "strnt_nmrtr_val")],
                             by = "bnf_code",
                             all.x = TRUE)
 
View(PRIMARYCARE_dataset)
 
unique(PRIMARYCARE_dataset$setting)
PRIMARYCARE_dataset <- subset(PRIMARYCARE_dataset, setting == 4) # So it just includes GPS
View(PRIMARYCARE_dataset)
 
 
# NOW I CAN CACULATE Quantity in mg of Lithium: because I have strnt_nmrtr_val and quantity
# HOW i calculated it last time:
quantity_mg = PRIMARYCARE_dataset$strnt_nmrtr_val *  PRIMARYCARE_dataset$quantity
 
PRIMARYCARE_dataset <- PRIMARYCARE_dataset %>%
  mutate(quantity_mg = quantity_mg)
 
View(PRIMARYCARE_dataset)
 
# ADDING to the dataset/ cleaning up:
# adding a chemical column, so I know whether it is, carbonate or citrate
# Assuming 'bnf_name' is the column with the product names in your dataset
library(dplyr)
 
PRIMARYCARE_dataset <- PRIMARYCARE_dataset %>%
  mutate(chemical = case_when(
    bnf_name %in% c("Camcolit 250 tablets",
                    "Camcolit 400 modified-release tablets",
                    "Liskonum 450mg modified-release tablets",
                    "Lithium carbonate 200mg modified-release tablets",
                    "Lithium carbonate 200mg/5ml oral suspension",
                    "Lithium carbonate 250mg tablets",
                    "Lithium carbonate 400mg modified-release tablets",
                    "Lithium carbonate 450mg modified-release tablets",
                    "Lithonate 400mg modified-release tablets",
                    "Priadel 200mg modified-release tablets",
                    "Priadel 400mg modified-release tablets") ~ "Lithium Carbonate",
    bnf_name %in% c("Li-Liquid 1.018g/5ml oral solution",
                    "Li-Liquid 509mg/5ml oral solution",
                    "Lithium citrate 1.018g/5ml oral solution",
                    "Lithium citrate 509mg/5ml oral solution",
                    "Lithium citrate 520mg/5ml oral solution sugar free",
                    "Priadel 520mg/5ml liquid") ~ "Lithium Citrate",
    TRUE ~ "Other"  # TO see if anything isn't attributed to the two groups
  ))
 
unique(PRIMARYCARE_dataset$chemical)
 
# Primary care, regional PLOTS, using regional_team
unique(PRIMARYCARE_dataset$regional_team)
 
# adding a regions column:
# Assuming your dataset is stored in a data frame called df
 
# Create a named vector for the mapping of regional_team to regions
region_mapping <- c(
  "Y60" = "Midlands",
  "Y63" = "North Eastern Yorkshire",
  "Y59" = "South East",
  "Y61" = "East of England",
  "Y62" = "North West",
  "Y56" = "London",
  "Y58" = "South West"
)
 
# Add the new 'regions' column by mapping the 'regional_team' column
PRIMARYCARE_dataset$Region <- region_mapping[PRIMARYCARE_dataset$regional_team]
 
View(PRIMARYCARE_dataset)
 
unique(PRIMARYCARE_dataset$Region)
 
# Lithium carbonate VS citrate use regionally in primary care plot
citrate_color <- "#1f77b4"    # Blue for Citrate
carbonate_color <- "#ff7f0e"  # Orange for Carbonate
 
ggplot(PRIMARYCARE_dataset, aes(x = reorder(Region, quantity_mg), y = quantity_mg, fill = chemical)) +
  geom_bar(stat = "identity") +
  labs(title = "Primary care Regional breakdown of Lithium usage",
       x = "Region",
       y = "Lithium usage (quantity_mg)",
       fill = "Carbonate | Citrate") +
  scale_y_continuous(labels = scales::label_comma()) +  # Format y-axis labels with commas
  scale_fill_manual(values = c("Lithium Citrate" = citrate_color, "Lithium Carbonate" = carbonate_color)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
 
#  Plotting Lithium carbonate and citrate products used regionally
ggplot(PRIMARYCARE_dataset, aes(x = reorder(Region, -quantity_mg), y = quantity_mg, fill = bnf_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Primary care Regional breakdown of Lithium usage",
       x = "Region",
       y = "Lithium usage (quantity_mg)",
       fill = "BNF Name") +
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
                               "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5",
                               "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F",
                               "#B3DE69", "#BC80BD")) +  # Added two more colors
  scale_y_continuous(labels = scales::label_comma()) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
 
#  Plotting Lithium carbonate and citrate products used regionally - generric vs product
#Classify BNF names into Generic or Branded
PRIMARYCARE_dataset <- PRIMARYCARE_dataset %>%
  mutate(bnf_type = case_when( bnf_name %in% c("Lithium carbonate 250mg tablets", "Lithium carbonate 400mg modified-release tablets",
                                               "Lithium carbonate 450mg modified-release tablets",
                                               "Lithium carbonate 200mg modified-release tablets",
                                               "Lithium carbonate 200mg/5ml oral suspension",
                                               "Lithium citrate 520mg/5ml oral solution sugar free",
                                               "Lithium citrate 1.018g/5ml oral solution",
                                               "Lithium citrate 509mg/5ml oral solution") ~ "Generic",
                               bnf_name %in%c("Camcolit 250 tablets",
                                              "Camcolit 400 modified-release tablets",
                                              "Liskonum 450mg modified-release tablets",
                                              "Priadel 400mg modified-release tablets",
                                              "Priadel 200mg modified-release tablets",
                                              "Priadel 520mg/5ml liquid",
                                              "Li-Liquid 509mg/5ml oral solution",
                                              "Li-Liquid 1.018g/5ml oral solution") ~ "Branded", TRUE ~ "Unknown" ))
# PLOT
ggplot(PRIMARYCARE_dataset, aes(x = reorder(Region, -quantity_mg), y = quantity_mg, fill = bnf_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Primary Care Regional Breakdown of Lithium Usage", x = "Region", y = "Lithium Usage (quantity_mg)", fill = "BNF Type") +
  scale_fill_manual(values =c("Generic" = "#00A651", "Branded" = "#0072B2")) +
  scale_y_continuous(labels =scales::label_comma()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
 
############################## NEW PCD mapping code for all of  2023 #######################
# Mapping plot, primary care all of 2023 - I need tge mg for 2023 and the population data
primary_lithium_df<-PRIMARYCARE_dataset %>%
  # getting the sum of mg per region
  group_by(Region) %>%
  summarise(quantity_mg =sum(quantity_mg)) %>%
  ###
  mutate(nuts118cd = case_when(Region =="East of England"~"UKH",
                               Region=="North West"~"UKD",
                               Region=="North Eastern Yorkshire"~"UKE",
                               Region=="London"~"UKF",
                               Region=="Midlands"~"UKG",
                               Region=="South East"~"UKJ",
                               Region=="South West"~"UKK"),
         region = as.factor(Region)) %>%
  filter(!is.na(Region))
 
PRIMARYCARE_dataset <- PRIMARYCARE_dataset %>%
  mutate(month = as.Date(month))
 
PRIMARY_twenty_three <- PRIMARYCARE_dataset %>%
  filter(month %in% as.Date(c("2023-01-01",
                              "2023-02-01",
                              "2023-03-01",
                              "2023-04-01",
                              "2023-05-01",
                              "2023-06-01",
                              "2023-07-01",
                              "2023-08-01",
                              "2023-09-01",
                              "2023-10-01",
                              "2023-11-01",
                              "2023-12-01")))
 
View(PRIMARY_twenty_three)
 
 
PRIMARY_twenty_three <- PRIMARY_twenty_three %>%
  mutate(twenty_three_quantity_mg = strnt_nmrtr_val * quantity)
 
# Calculate the total mg of Lithium for each region in primary care 2023
primary_total_mg_by_region <- PRIMARY_twenty_three %>%
  group_by(Region) %>%
  summarize(total_mg_2023 = sum(twenty_three_quantity_mg, na.rm = TRUE))
 
View(primary_total_mg_by_region)
 
View(primary_lithium_df) # THIS IS the dataset i have been using for the mapping code,
 
#adding this column to of total_mg for 2023 for each region to lithium_df
primary_lithium_df <- primary_total_mg_by_region %>%
  mutate(primary_total_mg_by_region)
 
View(primary_lithium_df)
 
# Now adding population data to this dataset, that I got from ONS statistics
NorthEast_Yorkt_pop = 8224302
NorthWest_pop = 7516113
midlands_pop = 10956592
east_pop = 6398497
london_pop = 8866180
SouthEast_pop = 9379833
SouthWest_pop = 5764881
 
 
primary_lithium_df <- primary_lithium_df %>%
  mutate(population = case_when(
    Region == "North Eastern Yorkshire" ~ NorthEast_Yorkt_pop,
    Region == "North West" ~ NorthWest_pop,
    Region == "Midlands" ~ midlands_pop,
    Region == "East of England" ~ east_pop,
    Region == "London" ~ london_pop,
    Region == "South East" ~ SouthEast_pop,
    Region == "South West" ~ SouthWest_pop,
  ))
 
View(primary_lithium_df)
 
 
# this is what I'll be plotting - NOW calculating the total_mg_lithium(mg)/ population for the first 6 months of 2022. (Making is more specific as it was in the last mapping code the 5 years of lihtium stock)
primary_lithium_df <- primary_lithium_df %>%
  mutate(`mg/population(2023)` = total_mg_2023 / population)
 
View(primary_lithium_df)
 
# there are differences in the  region names I need to fix, specifically in "North East & Yorkshire"/ "North Eastern Yorkshire"
View(nuts_shp)
View(primary_lithium_df)
 
mapping <- data.frame(
  lithium_region = c( "North Eastern Yorkshire", "London", "Midlands", "East of England", "South East", "South West", "North West"),
  nuts_shp_region = c( "North East & Yorkshire", "London", "Midlands", "East of England", "South East", "South West", "North West"),
  stringsAsFactors = FALSE
)
 
# Join on the correct column names
primary_lithium_df <- primary_lithium_df %>%
  left_join(mapping, by = c("Region" = "lithium_region"))
 
 
# Inspect the joined dataset
primary_coverage_data <- nuts_shp %>%
  left_join(primary_lithium_df, by = c("region" = "nuts_shp_region"))
 
View(primary_coverage_data)
 
unique_values <- sort(unique(primary_lithium_df$`mg/population(2023)`))
 
# Define labels for the legend
legend_labels <- scales::label_number()(unique_values)
 
# Plot with specified color scale
coverage_plot <- primary_coverage_data %>%
  ggplot(aes(geometry = geometry, fill = `mg/population(2023)`)) +
  geom_sf(lwd = 0.8, colour = 'black') +
  geom_sf_label(aes(label = region),  # Use `region` for labels
                colour = "white",
                label.size = 0.1,
                label.r = unit(0.5, "lines"),
                fun.geometry = st_centroid,
                show.legend = FALSE) +
  scale_fill_gradientn(
    colors = c("#00A651", "#0072B2", "#CC79A7"),
    breaks = unique_values,  # Use unique values for breaks
    labels = legend_labels  # Use labels based on unique values
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.2, 0.5),
    legend.text = element_text(hjust = 1),
    panel.background = element_rect(fill = "white")
  ) +
  ggtitle("Primary Care: Total mg of lithium used in 2023 regionally/ population estimates") +
  guides(fill = guide_legend(title = "mg Lithium/ population")) +
  xlab("") +
  ylab("")
 
# Display the plot
plot(coverage_plot)
 
# ammended hist
hist = ggplot(primary_coverage_data, aes(x = region, y = `mg/population(2023)`)) +
  geom_col(fill = "#008080"   , color = "#E0FFFF")+  # Add border color and size
  # Use geom_col() for bar plots where the height of bars is defined by y
  geom_text(aes(label = round(`mg/population(2023)`, 1)), vjust = -0.3, size = 3.5) +  # Add data labels
  theme_minimal() +
  xlab("Region") +
  ylab("Lithium usage (mg) / population") +
  ggtitle("Primary Care: Total Regional Lithium Usage (mg) for 2023 / Population Estimates") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Adjust angle and size of x-axis labels
    axis.text.y = element_text(size = 10),  # Adjust size of y-axis labels
    plot.title = element_text(size = 12, face = "bold"),  # Adjust title size and style
    plot.margin = margin(10, 10, 10, 10)  # Adjust plot margins for better spacing
  )
 
plot(hist)
 
# plot that is more focused on the last 6 months of 2023 for primary care, fix this
# Subset for the last 6 months of 2023
PRIMARY_six_months <- PRIMARYCARE_dataset %>%
  filter(month %in% as.Date(c("2023-07-01",
                              "2023-08-01",
                              "2023-09-01",
                              "2023-10-01",
                              "2023-11-01",
                              "2023-12-01")))
 
# Calculate the total mg of Lithium for each region in the last 6 months
primary_total_mg_six_months <- PRIMARY_six_months %>%
  mutate(twenty_three_quantity_mg = strnt_nmrtr_val * quantity) %>%
  group_by(Region) %>%
  summarize(total_mg_six_months = sum(twenty_three_quantity_mg, na.rm = TRUE))
 
# Add population data to the dataset focusing on the last 6 months
primary_lithium_df_six_months <- primary_total_mg_six_months %>%
  left_join(primary_lithium_df, by = "Region") %>%
  mutate(`mg/population(2023)` = total_mg_six_months / population)
 
 
# View updated dataset
View(primary_lithium_df_six_months)
 
# Plotting the total lithium usage per population for the last 6 months
hist2 = ggplot(primary_lithium_df_six_months, aes(x = Region, y = `mg/population(2023)`)) +
  geom_col(fill = "#17d7e6", color = "#1f77b4") +  # Add border color
  geom_text(aes(label = round(`mg/population(2023)`, 1)), vjust = -0.3, size = 3.5) +  # Add data labels
  theme_minimal() +
  xlab("Region") +
  ylab("Lithium usage (mg)/ population") +
  ggtitle("Primary Care: Total Regional Lithium Usage (mg) for the Last 6 Months of 2023 / Population Estimates") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Adjust angle and size of x-axis labels
    axis.text.y = element_text(size = 10),  # Adjust size of y-axis labels
    plot.title = element_text(size = 12, face = "bold"),  # Title style
    plot.margin = margin(10, 10, 10, 10)  # Adjust plot margins
  )
plot(hist2)
