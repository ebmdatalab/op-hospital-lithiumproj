## ANALYSIS:

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

install.packages("patchwork")
library(patchwork)

install.packages("paletteer")
library(paletteer)

install.packages("RColorBrewer")
library(RColorBrewer)

install.packages("gridExtra")
library(gridExtra)

install.packages("readxl")
library(readxl)

## SECONDARY ANALYSIS:
# Secondary care Dataset
# NEW DATASET TO BE WORKING FROM, 
# Originally they were 26 organisations in the unlabelled region column, and 1 NA.
# Now using the new dataset on mergers there are 4, on thex-axis labelled region they come up as 'NA'kingdirectory = setwd("C:/Users/K22007671/OneDrive - King's College London")
setwd("C:/Users/hayleyschiffer/Desktop")
getwd() 

Lithium_SCMD <- read_excel("scmd_lithium-2.xlsx")


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

# regional breakdown 
# SPECIFYING - REGION ORDERS ON THE X-AXIS FOR MY PLOTS:
# Specify region orders
secondary_region_order <- c("London", "Midlands", "North West", "North East & Yorkshire", "South West", "South East", "East of England")

# Regional plot, vtm_name: Lithium carbonate vs Citrate for secondary care
# Secondary Care Plot
vtm_regional_secondary_plot <- ggplot(Lithium_SCMD, aes(x = factor(region_grouped, levels = secondary_region_order), y = quantity_mg / 1e6, fill = vtm_name)) +
  geom_bar(stat = "identity", width = 0.7) +  # Adjust bar width for thinner bars
  labs(title = "Secondary Care: Regional Breakdown of Lithium Usage",
       subtitle = "Analysis of Lithium (mg) usage by region for secondary care settings 2023",
       x = "Region",
       y = "Lithium usage (millions of mg)",
       fill = "Lithium Carbonate | Lithium Citrate") +
  scale_y_continuous(labels = label_comma(), expand = c(0, 0)) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12),  # Larger x-axis labels
        axis.title = element_text(size = 14, face = "bold"),  # Bold axis titles
        plot.title = element_text(size = 16, face = "bold"),  # Larger and bold title
        plot.subtitle = element_text(size = 12),  # Subtitle
        legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
        legend.text = element_text(size = 10))


plot(vtm_regional_secondary_plot)

# Regional plot, vmp_name: Lithium carbonate vs Citrate products
vmp_regional_secondary_plot = ggplot(Lithium_SCMD, aes(x = factor(region_grouped, levels = secondary_region_order), y = quantity_mg / 1e6, fill = vmp_name)) +
  geom_bar(stat = "identity", width = 0.7) +  # Thinner bars
  labs(title = "Secondary Care: Regional Breakdown of Lithium Usage",
       x = "Region",
       y = "Lithium usage (millions of mg)",  # Y-axis label unchanged
       fill = "VMP Name") +
  scale_y_continuous(labels = scales::label_comma()) +  # Ensure y-axis labels are formatted with commas
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12),  # Larger x-axis labels
        axis.title = element_text(size = 14, face = "bold"),  # Bold axis titles
        plot.title = element_text(size = 16, face = "bold"),  # Larger and bold title
        plot.subtitle = element_text(size = 12),  # Subtitle
        legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
        legend.text = element_text(size = 10))  # Legend text size


plot(vmp_regional_secondary_plot)

# organisational breakdown 
# Organisation breakdown in MGs for Lithium Carbonate and Citrate:
# PLOTTING top 50 organisations to help with readability:
top_50_data <- Lithium_SCMD %>%
  group_by(ods_name) %>%
  summarise(total_quantity = sum(quantity_mg)) %>%
  arrange(desc(total_quantity)) %>%
  slice(1:50)

top_50_data <- top_50_data %>%
  inner_join(Lithium_SCMD, by = "ods_name")


# Plotting top 50 organisations: Organisation breakdown in MGs for Lithium Carbonate and Citrate - vtm 
ggplot(top_50_data, aes(x = reorder(ods_name, total_quantity), y = quantity_mg, fill = vtm_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 50 Organisations' Lithium Usage 2023",
       x = "Organisation (ods_name)",
       y = "Lithium usage (mg)",
       fill = "VTM Name") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),  # Hide y-axis labels
        axis.text.x = element_text(angle = 90, hjust = 1, size = 12, face = "bold"),  # Bold and larger x-axis labels
        axis.title.x = element_text(size = 14, face = "bold"),  # Bold x-axis title
        axis.title.y = element_text(size = 14, face = "bold"),  # Bold y-axis title
        plot.title = element_text(size = 16, face = "bold"),  # Bold title
        plot.subtitle = element_text(size = 12, face = "bold"),  # Bold subtitle (if needed)
        legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
        legend.text = element_text(size = 10))  # Legend text size

# Plotting top 50: Organisational breakdown in MGs for the products of Lithium citrate and carbonate - vmp
ggplot(top_50_data, aes(x = reorder(ods_name, total_quantity), y = quantity_mg, fill = vmp_name)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 50 Organisations' Lithium Usage (products), 2023",
       x = "Organisation (ods_name)",
       y = "Lithium usage (mg)",
       fill = "VMP Name") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),  # Hide y-axis labels
        axis.text.x = element_text(angle = 90, hjust = 1, size = 12, face = "bold"),  # Bold and larger x-axis labels
        axis.title.x = element_text(size = 14, face = "bold"),  # Bold x-axis title
        axis.title.y = element_text(size = 14, face = "bold"),  # Bold y-axis title
        plot.title = element_text(size = 16, face = "bold"),  # Bold title
        plot.subtitle = element_text(size = 12, face = "bold"),  # Bold subtitle (if needed)
        legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
        legend.text = element_text(size = 10))  # Legend text size



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
barplot_mg_2023_secondary = ggplot(secondary_lithium_df, aes(x = region_grouped, y = `mg/population(2023)`)) +
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

plot(barplot_mg_2023_secondary)


### primary care analysis ###
##PRI##PRI##PRIMARY ANALYSIS:
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
df_primarycare2 <- read_csv(file.choose())

View(df_primarycare2)

PRIMARYCARE_dataset <- merge(merged_data, 
                             df_primarycare2[, c("bnf_code", "strnt_nmrtr_val")], 
                             by = "bnf_code", 
                             all.x = TRUE)

View(PRIMARYCARE_dataset)

unique(PRIMARYCARE_dataset$setting)
PRIMARYCARE_dataset <- subset(PRIMARYCARE_dataset, setting == 4) # So it just includes GPS
View(PRIMARYCARE_dataset)

PRIMARYCARE_dataset <- PRIMARYCARE_dataset %>%
  mutate(month = as.Date(month))

PRIMARYCARE_dataset <- PRIMARYCARE_dataset %>%
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
unique(PRIMARYCARE_dataset$Region)

View(PRIMARYCARE_dataset) 


# vmp analysis later on I will need vmp_names, using the bnf names - TO ALLOCATE CORRECT VMP_NAMES:
bnf_to_vmp <- data.frame(
  bnf_name = c(
    "Camcolit 250 tablets",                              
    "Camcolit 400 modified-release tablets",             
    "Li-Liquid 1.018g/5ml oral solution",                 
    "Li-Liquid 509mg/5ml oral solution",                 
    "Liskonum 450mg modified-release tablets",            
    "Lithium carbonate 200mg modified-release tablets",  
    "Lithium carbonate 200mg/5ml oral suspension",        
    "Lithium carbonate 250mg tablets",                   
    "Lithium carbonate 400mg modified-release tablets",   
    "Lithium carbonate 450mg modified-release tablets",  
    "Lithium citrate 1.018g/5ml oral solution",           
    "Lithium citrate 509mg/5ml oral solution",           
    "Lithium citrate 520mg/5ml oral solution sugar free", 
    "Lithonate 400mg modified-release tablets",          
    "Priadel 200mg modified-release tablets",             
    "Priadel 400mg modified-release tablets",            
    "Priadel 520mg/5ml liquid"
  ),
  VMP_name = c(
    "Lithium carbonate 250mg tablets (Camcolit brand)", 
    "Lithium carbonate 400mg modified-release tablets (Camcolit brand)", 
    "Lithium citrate 1.018g/5ml oral solution (Li-Liquid brand)", 
    "Lithium citrate 509mg/5ml oral solution (Li-Liquid brand)", 
    "Lithium carbonate 450mg modified-release tablets (Liskonum brand)", 
    "Lithium carbonate 200mg modified-release tablets", 
    "Lithium carbonate 200mg/5ml oral suspension", 
    "Lithium carbonate 250mg tablets", 
    "Lithium carbonate 400mg modified-release tablets", 
    "Lithium carbonate 450mg modified-release tablets", 
    "Lithium citrate 1.018g/5ml oral solution", 
    "Lithium citrate 509mg/5ml oral solution", 
    "Lithium citrate 520mg/5ml oral solution sugar free", 
    "Lithium carbonate 400mg modified-release tablets (Lithonate brand)", 
    "Lithium carbonate 200mg modified-release tablets (Priadel brand)", 
    "Lithium carbonate 400mg modified-release tablets (Priadel brand)", 
    "Lithium citrate 520mg/5ml oral solution (Priadel brand)"
  )
)


# merge function
PRIMARYCARE_dataset <- merge(PRIMARYCARE_dataset, bnf_to_vmp, by = "bnf_name", all.x = TRUE)

View(PRIMARYCARE_dataset) 

# Regional plot, vtm_name: Lithium carbonate vs Citrate for Primary care
# Lithium carbonate VS citrate use regionally in primary care plot
citrate_color <- "#E69F00"
carbonate_color <- "#56B4E9"

primary_region_order <- c("London", "Midlands", "North West", "North Eastern Yorkshire", "South West", "South East", "East of England")

vtm_regional_primary_plot <- ggplot(PRIMARYCARE_dataset, aes(x = factor(Region, levels = primary_region_order), y = quantity_mg / 1e6, fill = chemical)) + # chemical = vtm in this dataset
  geom_bar(stat = "identity", width = 0.7) +  # Adjust bar width for thinner bars
  labs(title = "Primary Care: Regional Breakdown of Lithium Usage",
       subtitle = "Analysis of Lithium (mg) usage by region for primary care settings 2023",
       x = "Region",
       y = "Lithium usage (millions of mg)",
       fill = "Lithium Carbonate | Lithium Citrate") +
  scale_y_continuous(labels = scales::label_comma(), expand = c(0, 0)) +  
  scale_fill_manual(values = c("Lithium Citrate" = citrate_color, "Lithium Carbonate" = carbonate_color)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12),  # Larger x-axis labels
        axis.title = element_text(size = 14, face = "bold"),  # Bold axis titles
        plot.title = element_text(size = 16, face = "bold"),  # Larger and bold title
        plot.subtitle = element_text(size = 12),  # Subtitle
        legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
        legend.text = element_text(size = 10))


# Now, I have both regional plots for vtm for primary and secondary 2023
combined_plot <- vtm_regional_primary_plot / vtm_regional_secondary_plot 

plot(combined_plot)

# Regional plot, vmp_name: Lithium carbonate vs Citrate products in primary care
# Use a palette with 17 distinct colours - because in primary I have  17 VMPs
colors1 <- brewer.pal(12, "Set3")  # 12 colors from Set3
colors2 <- brewer.pal(9, "Set1")    # 9 colors from Set1
color_palette <- c(colors1, colors2)  # Combine to create a palette of 21 colors
color_palette <- color_palette[1:17]


vmp_regional_primary_plot <- ggplot(PRIMARYCARE_dataset, aes(x = factor(Region, levels = primary_region_order), y = quantity_mg / 1e6, fill = VMP_name)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(title = "Primary Care: Regional Breakdown of Lithium Usage",
       subtitle = "Detailed analysis of Lithium usage (mg) by VMP Name across different regions in primary care and secondary care 2023.",
       x = "Region",
       y = "Lithium usage (millions of mg)",
       fill = "VMP Name") +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = color_palette) +  # Apply the 17-color palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        legend.key.height = unit(1, "cm"),  # Adjust the height of the legend keys
        legend.key.width = unit(0.5, "cm")) +  # Adjust the width of the legend keys for more squishing
  guides(fill = guide_legend(ncol = 2, byrow = TRUE, label.position = "right", title.position = "top"))

# Print the primary plot
print(vmp_regional_primary_plot)


# depeding on preference:
combined_plot <- vmp_regional_primary_plot | vmp_regional_secondary_plot  
combined_plot <- vmp_regional_primary_plot / vmp_regional_secondary_plot  

# Now, I have both regional plots for vmp for primary and secondary 2023

# Print the combined plot
plot(combined_plot)


#  Plotting Lithium carbonate and citrate products used regionally - generic vs product
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
ggplot(PRIMARYCARE_dataset, aes(x = reorder(Region, -quantity_mg), y = quantity_mg / 1e6, fill = bnf_type)) + 
  geom_bar(stat = "identity", width = 0.6) +  # Adjust bar width for smaller body
  labs(title = "Primary Care Regional Breakdown of Lithium Usage",
       subtitle = "Analysis of lithium usage in Primary care: Branded vs Generic for 2023",
       x = "Region", 
       y = "Lithium Usage (mg in millions)",  # Updated y-axis label
       fill = "BNF Type") + 
  scale_fill_manual(values = c("Generic" = "#00A651", "Branded" = "#0072B2")) + 
  scale_y_continuous(labels = scales::label_comma()) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 12),  # Larger x-axis labels
        axis.title = element_text(size = 14, face = "bold"),  # Bold axis titles
        plot.title = element_text(size = 16, face = "bold"),  # Bold title
        plot.subtitle = element_text(size = 12, face = "italic"),  # Italic subtitle
        legend.title = element_text(size = 12, face = "bold"),  # Bold legend title
        legend.text = element_text(size = 10))  # Legend text size

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

unique(PRIMARYCARE_dataset$month)

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


barplot_mg_2023_primary = ggplot(primary_coverage_data, aes(x = region, y = `mg/population(2023)`)) +
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

plot(barplot_mg_2023_primary)

# now I have bar plots for 2023 for both primary and secondary

grid.arrange(barplot_mg_2023_primary, barplot_mg_2023_secondary, ncol = 1)
