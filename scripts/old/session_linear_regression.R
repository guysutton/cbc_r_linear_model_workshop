



###########################################################################
# Data import and cleaning ------------------------------------------------
###########################################################################

# Import dataset (example using .xlsx)
insect_data <- readxl::read_excel("./data_raw/cochineal_fitness_data.xlsx") 

# Import dataset (example using .csv)
# - Depending on how your PC works, some PC's encode .csv's with 
#   actual commas (as you would expect), but my PC uses a semi-colon 
#   (go figure!).

# If your .csv uses commas 
insect_data <- readr::read_csv("./data_raw/cochineal_fitness_data.csv") 

# If your .csv uses semi-colons 
insect_data <- readr::read_csv2("./data_raw/cochineal_fitness_data.csv") 

# Check to make sure data imported properly
head(insect_data)
str(insect_data)

# Clean up the data frame 
insect_data <- insect_data %>%
  mutate(lineage = as.factor(lineage),
         crawlers_total = crawlers_start + crawlers_final)
