# Simulate data for workshop example

# Load packages
library(tidyverse)
library(DHARMa)

# Let's simulate some data 
set.seed(2019)
reps <- 1000
b0 <- 0.2
b1 <- 0.5
n <- 1000
x <- runif(n, 1, 5)
y <- rpois(n, exp(b0 + b1*x))
y
data <- cbind(x, y)
data <- as.data.frame(data)
head(data)

data <- data %>%
  dplyr::rename(insect_abundance = y,
                nitrogen_conc = x) %>%
  dplyr::mutate(nitrogen_conc = round(nitrogen_conc, 2))

# Export data to .csv
write_excel_csv(x = data,
                path = "./data_raw/exercise_data.csv")

# Check model
mod.pois <- glm(y ~ x, 
                family = poisson(link = "log"))
summary(mod.pois)
mod.res <- simulateResiduals(mod.pois)
plot(mod.res)

# Plot graph of data 
data %>%
  ggplot(data = ., aes(x = nitrogen_conc,
                       y = insect_abundance)) +
  geom_point() + 
  labs(x = "Nitrogen concentration",
       y = "Insect abundance")
