######################################################################
######################################################################
######################################################################
# - R workshop on linear models
# - Model #4: Poisson GLM
# - Centre for Biological Control, Rhodes Universtiy
# - Script written: 27/07/2020
# - By: Guy F. Sutton
######################################################################
######################################################################
######################################################################

# In this session, we will be analysing some data from an experiment which 
# aimed to determine if genetic diversity has an effect on insect fecundity. 
# - The experiment took female cochineal insects from three different treatments,
#   inoculated the insects onto their host-plant, and counted how many offspring
#   each female produced. 
# - The treatments were:
#   (1) Single - all insects used originate from one female (low genetic diversity)
#   (2) Multiple - all insects used originate from 20 females (medium diversity)
#   (3) Outcrossed - all insects used originate from field-collected females 
#                    (high genetic diversity). 
# - At the start of the experiment, all females were weighed (in mg). 
# - The number of offspring produced by each female was counted and summed, 
#   as our measure of interest. 

######################################################################
# Step 1: Load packages ----------------------------------------------
######################################################################

# We need a need package today: emmeans 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, 
               tidyr,
               emmeans,
               DHARMa) 

# Change ggplot theme
# - This makes the graphs we will eventually make look pretty. 
# - Don't worry about this for now. 
theme_set(theme_classic() +
            theme(panel.border = element_rect(colour = "black", 
                                              fill = NA),
                  axis.text = element_text(colour = "black"),
                  axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), 
                                                            "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), 
                                                            "mm")),
                  legend.position = "none"))

###########################################################################
# Step 2: Import data -----------------------------------------------------
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

###########################################################################
# Step 3: Tidy and transform raw data -------------------------------------
###########################################################################

# Check to make sure data imported properly
# - Check the first 6 rows of the data 
head(insect_data)

# Have a look at the structure of the data 
# - Check that the numbers are numeric (not characters), and vice-versa, 
#   and that there are as many columns and rows as expected from your .xlsx
#   or .csv file 
glimpse(insect_data)

# Clean up the data frame 
insect_data <- insect_data %>%
  # Make the lineage column a factor - R doesn't play well with character columns
  # and then make a column summing crawlers_start + crawlers_final 
  # - total_crawlers will be our response variable 
  dplyr::mutate(lineage = as.factor(lineage), 
                crawlers_total = crawlers_start + crawlers_final)

# Check to see if our changes worked
glimpse(insect_data)

######################################################################
# Step 4: Exploratory data analysis ----------------------------------
######################################################################

# We have already done EDA for this dataset in the linear regression 
# and ANOVA tut. 
# - So far, we have analysed whether lineage and initial female body mass
#   had an effect on fecundity (number of crawlers produced).
# - We have found no statistically significant relationships yet, 
#   and that is cool... 
# - We then did ANCOVA, but our model didn't quite meet the statistical
#   distributional requirements.
# - As such, we need a different model. 

insect_data %>%
  ggplot(data = ., aes(x = mass_start,
                       y = crawlers_total,
                      colour = lineage)) +
  geom_point() +
  theme(legend.position = "right") +
  labs(x = "Female mass at start of experiment",
       y = "No. of crawlers produced")

# Frequency distribution of no. of crawlers 
ggplot(data = insect_data, aes(x = crawlers_total)) +
  geom_histogram()

######################################################################
# Step 5: State your aims/hypotheses/predictions ---------------------
######################################################################

# In this session, we are going to ask:
# (1) Does the number of crawlers produced differ across lineages?
# (2) Is there a relationship between initial body mass and 
#     the number of crawlers produced?
# (3) Does the body mass and lineage interact to determine crawler output? 

######################################################################
# Step 6: Perform the analysis ---------------------------------------
######################################################################

# Performing an Poisson in R is not as simple as linear regression or ANOVA. 
# We will use the 'glm' function.
# First, we specify our response variable (the thing we are trying to predict),
# Second, we specify our predictor variables (including an interaction term)
#  - An interaction is specified using '*' instead of '+'
# Then like usualy, we have to tell R where to look for those variables. 
# and, we store the ANCOVA in an object called 'mod1'. 
# The big difference now is that we have to tell R which type of GLM we want.
# - We use the family = <model_name> argument 

mod1 <- glm(crawlers_total ~ lineage * mass_start, 
            data = insect_data,
            # Tell R we want Poisson GLM
            # Other model families are: gaussian, binomial and Gamma
            family = poisson)
summary(mod1)

# Type III SOS tables 
car::Anova(mod1, type = "III")

######################################################################
# Step 7: Model diagnostics ------------------------------------------
######################################################################

# Model diagnostics for GLM's are a little different in look and code, 
# but the interpretation is much the same. 

# Standard GLM diagnostics using the 'DHARMa' package
mod_diag <- DHARMa::simulateResiduals(mod1)
plot(mod_diag)

# - QQ plot clearly shows that a Poisson was okay. 
# - The non-significant KS test indicates there our data do not deviate 
#   significantly from what would be expected under the Poisson
#   distribution. 

# - The residuals vs predicted plot provides an overall test of equal variance.
#   - We can see that it was fine 'no significant problems detected'
#   - All the black lines are approximately on the dashed line. 
#   - If there were issues, those lines would be red and they would be 
#     wiggly/far away from the dashed lines. 

# Are there outliers? 
testOutliers(mod1)

# No. 

# What about zero inflation?
testZeroInflation(mod1)

# No. 

###########################################################################
# Step 8: Interpret model output ------------------------------------------
###########################################################################

# Here comes the tricky part. 
# - The expected values for a Poisson model are on the log-scale, 
#   so we need to back-transform to get actual counts and make the model easier
#   to interpret.
# - Thankfully this is easy to do in R using the exp(...) function

# Exponentiate model co-efficients 
exp(coef(mod1))

# What does this mean? 
# (1) Intercept - This effectively means what is the expected no. of 
#     crawlers produced by a female of body mass = 0, from the
#     'single' lineage (notice single isn't in the table!!!).
#     - We can't get a female with body mass = 0, so this 
#       co-efficient has no meaning for this model (it is often
#       very useful though!!!)
exp(1.52)

# - Expect a female of body_mass 0 on the Single lineage to 
#   make 4.57 crawlers 

# (2) lineage_x - these coefficients indicate the factor by which 
#     the number of crawlers (Y) will change relative to the 
#     baseline level (i.e. the missing level) for a female of body mass = 0. 
#     - e.g. how many more/fewer crawlers would a female with body mass = 0 
#            make on the Multiple on Outcrossed lineage relative to the 
#            Single lineage? 

# Example #1: lineageMultiple
# The expected number of crawlers for a female from the 'multiple' lineage, 
# with body mass = 0 is: 3.56 times fewer crawlers than Single lineage
exp(1.52 + (-0.25))

# Or, we can think of this as percentage change:
# - A female of body mass = 0,
#   would be expected to make about 22% fewer crawlers on 'multiple' 
#   lineage versus 'single' lineage.
exp(-0.25) - 1

# (3) mass_start - this coefficient indicates the factor by which
#     the expected change in the number of crawlers produced by a female 
#     from the 'single' treatment,  with a 1 unit increase in body mass. 
#     - A female on the Single lineage would be expected to 
#       produce 1.03x more, or 3% more crawlers for each 1mg 
#       increase in body mass. 
exp(0.032)

# (4) Interactions:

# (a) lineageMultiple:mass_start
# - As you increase body mass by one unit, the expected number of crawlers 
#   produced by a female on the Multiple lineage increases by a factor of 
#   exp(0.032 + 0.059) = 1.095 or about 9.5%, relative to Single lineage.  
exp(0.032 + 0.059)

# (a) lineageOutcrossed:mass_start
# - As you increase body mass by one unit, the expected number of crawlers 
#   produced by a female on the Outcrossed lineage decreases by a factor of 
#   exp(0.032 + (-0.02)) = 1.012 or about 1%, relative to Single lineage. 
exp(0.032 + (-0.02))

# Tricky, right? Most of these interpretations are made really difficult to
# understand because body mass cannot = 0, but when 0
# makes sense (e.g. temperature), these interpretations make much more sense. 
# - Much easier to plot the predictions and interactions and visualise 
#   the relationships between variables. 

###########################################################################
# Step 9: Plot model predictions ------------------------------------------
###########################################################################

# Get the link function for the Poisson model
# - We need this to back-transform the predictions 
ilink <- family(mod1)$linkinv

## some data to predict at: 100 values over the range of adult_mass
ndata <- expand.grid(lineage = levels(insect_data$lineage), 
                     # Range of female mass to predict over 
                     mass_start = seq(min(insect_data$mass_start), 
                                      max(insect_data$mass_start),
                             # How many points?
                             l = 100))

# Add the fitted values by predicting from the model for the new data
ndata <- add_column(ndata, 
                    fit = predict(mod1, 
                                  newdata = ndata,
                                  type = 'response'))

# Add predictions from the model to a new df
ndata <- bind_cols(ndata, setNames(as_tibble(predict(mod1, 
                                                     ndata,
                                                     se.fit = TRUE)[1:2]),
                                   c("fit_link", "se_link")))
head(ndata)

# Create the confidence interval and back-transform
ndata <- ndata %>%
  mutate(fit_resp = ilink(fit_link),
         right_upr = ilink(fit_link + (2 * se_link)),
         right_lwr = ilink(fit_link - (2 * se_link)))
head(ndata)

# Make the plot
ggplot(data = ndata) +
  geom_line(aes(x = mass_start,
                y = fit,
                colour = lineage)) +
  geom_point(data = insect_data, aes(x = mass_start,
                                     y = crawlers_total,
                                     colour = lineage)) + 
  scale_y_continuous(breaks = seq(0, 1000, 100), 
                     limits = c(0, 1000)) +
  geom_ribbon(aes(x = mass_start,
                  ymin = right_lwr,
                  ymax = right_upr,
                  fill = lineage),
              alpha = 0.2) +
  labs(x = "Adult mass (g)",
       y = "No. of larvae produced",
       colour = "Lineage") +
  theme(legend.position = "right") +
  guides(fill = FALSE) +
  facet_wrap(~lineage, ncol = 3)

# Take note about how the Outcrossed lineage is so wildly unreasonable. 
# - This is a clear indication that something is wrong. 
# - Just look at the raw data - this is a case of only having a few 
#   samples and extrapolating from these samples!!! 

###########################################################################
# Step 11: Posterior predictive check -------------------------------------
###########################################################################

# Posterior predictive check
# - Simulate 1000 response variables from the fitted models and
#   compare with how well the predicted values link up to 
#   the raw data

# I re-fit the models, just to make sure I know what I am comparing
# Mod1: Poisson GLM
mod1 <- glm(crawlers_total ~ lineage * mass_start, 
            data = insect_data,
            # Tell R we want Poisson GLM
            family = poisson(link = "log"))

# Mod2: Negative binomial GLM
mod2 <- MASS::glm.nb(crawlers_total ~ lineage * mass_start, 
                     data = insect_data)
mod2 <- glm(crawlers_total ~ lineage * mass_start, 
            data = insect_data)
# Define range of response variables
range(insect_data$crawlers_total)  

# Define maximum value of response var
max_y <- max(insect_data$crawlers_total)

# Simulate response variable for each candidate model x 1000 times
# - Set a seed to make it reproducible
set.seed(2019)
glmer.sim1 <- simulate(mod1, nsim = 1000)
glmer.sim2 <- simulate(mod2, nsim = 1000)

# Extract the simulated frequencies of each Y value across models 
out <- matrix(NA, ncol = 2, nrow = max_y)
cnt <- 0:49 # max_y-1
for (i in 1:length(cnt)) {
  for (j in 1:2) {
    eval(parse(text = paste("out[i,", j, "] <- 
          mean(sapply(glmer.sim", j,",\nFUN = function(x) {\nsum(x == cnt  
      [i]) }))", sep = "")))
  }
}
head(cnt)
head(out)

# Plot raw data vs predicted (simulation)
plot(table(insect_data$crawlers_total), 
     ylab = "Frequency", 
     xlab = "Y", 
     lwd = 2, 
     col="darkgrey")
lines(x = 0:49, y = out[, 1], lwd = 2, lty = 2, col = "red")    
lines(x = 0:49, y = out[, 2], lwd = 2, lty = 2, col = "blue")    

# tidy-style approach to plotting comparisons  
sim_data <- as_tibble(out)
sim_data$x <- cnt
head(sim_data)

# Manually specify colours
colors <- c("Poisson" = "blue", 
            "Neg. Bin." = "red")

# Plot the graph
ggplot() +
  # Add frequency of raw data 
  geom_histogram(data = insect_data, aes(x = crawlers_total),
                 binwidth = 1) +
  # Add model 1 simulated frequency
  geom_line(data = sim_data, aes(x = x,
                                 y = V1,
                                 colour = "Poisson"),
            size = 1.2) +
  # Add model 2 simulated frequency
  geom_line(data = sim_data, aes(x = x,
                                 y = V2,
                                 colour = "Neg. Bin."),
            size = 1.2) +
  scale_color_manual(values = colors) +
  labs(x = "Response variable",
       y = "Count",
       color = "Model") +
  theme(legend.position = "right")
