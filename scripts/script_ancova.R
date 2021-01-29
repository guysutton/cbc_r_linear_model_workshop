######################################################################
######################################################################
######################################################################
# - R workshop on linear models
# - Model #3: Analysis of co-variance (ANCOVA)
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
               emmeans) 

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

# But, we may have missed something: what if the females used for the different
# lineages were slightly bigger or smaller in some lineages than others? 
# - This would confound any analysis we have done so far...

# Let's look: did initial female body mass differ amongst the treatments? 
ggplot(data = insect_data, aes(x = lineage,
                               y = mass_start)) +
  geom_boxplot() +
  geom_point() +
  labs(x = "Lineage (treatment)",
       y = "Initial female body mass (g)")

# With such a small sample size (n = 5), it is probably better to add some 
# more replicates before analysing this dataset. 
# Anyway, I think that it is clearly that one of the females used 
# in the 'single' treatment was about 2-4x larger than most of the other 
# females used across all treatments, and that another two females 
# (the two that weighed ~ 50g), were about double the size of the other 
# females used. 

# We should be asking: 
# - Did the number of crawlers produced differ amongst lineage treatments, 
#   after accounting for different sized females being used in the 
#   different lineage treatments? 
#   - i.e. after accounting for different sized females, does lineage still
#     have an impact on number of crawlers produced? 
#      - Time to rip out the analysis of covariance (ancova)
#      - Here, we are going to test whether lineage had an effect on 
#        the number of crawlers produced, after controlling for 
#        female body size at the start of the experiment (mass_start)

######################################################################
# Step 5: State your aims/hypotheses/predictions ---------------------
######################################################################

# Null hypothesis: There was no difference in the number of crawlers
#                  produced across lineages, after accounting for 
#                  potential differences in female body mass between
#                  lineage treatments. 
# Alternative hyp: The number of crawlers produced differs across lineages,
#                  even after accounting for potential differences in 
#                  female body mass between lineage treatments.

######################################################################
# Step 6: Perform the analysis ---------------------------------------
######################################################################

# Performing an ANCOVA in R is not as simple as linear regression or ANOVA. 
# We will use the 'aov' function.
# First, we specify our response variable (the thing we are trying to predict),
# The order and structure of the predictor variables is really 
# important for ANCOVA:
# - First, we must specify the categorical predictor (i.e. the thing we 
#   are most interested in),
# - Second, we must then specify the numeric predictor (i.e. the thing
#   we are trying to control for)
# - We need to specify an interaction between these two variables:
#   - I.e. we allow the effect of body mass to differ between lineages.
#   - An interaction is specificed using '*' instead of '+'
# Then like usualy, we have to tell R where to look for those variables. 
# and, we store the ANCOVA in an object called 'mod1' 

# + = independent effects of both variables 
# * = interactive effects between variables 

# General formula for an ANCOVA
mod1 <- aov(<response_variable> ~ <categorical_variable> * <numeric_covariate>,
            data = <name_of_data>)

# Actually run our ANCOVA 
mod1 <- aov(crawlers_total ~ lineage * mass_start, 
               data = insect_data)

# Get Type III errors (NOT TYPE II LIKE ANOVA)
# - Always type III when you have an interaction term
# - Details of type I, II and III errors beyond scope here. 
# - Need to be very careful about choice of errors!!! 
car::Anova(mod1, type="III")

# Look at the difference in output when type II errors used like ANOVA
# and type I used in default R output
car::Anova(mod1, type="II")
summary(mod1)

# - When we use type III (correct choice) - lineage is not significant, 
#   after controlling for starting mass.
# - BUT, When we use type II or type I (default R output):
#   lineage is significant, after controlling
#   for starting mass. 

# Here, 
# (1) We see there was a significant interaction between lineage and 
#     mass_start - meaning that the mass of females was different between
#     the different lineages (look at the lineage:mass_start line P < 0.05). 
# (2) Look at the lineage line, we can see that after controlling for the differences
#     in female body mass at the start of the experiment (mass_start),
#     there is no significant difference in crawler output produced
#     on the different lineages. 

# Take home: ANCOVA clearly demonstrates that not accounting for difference 
#            in female body mass between treatments leads us to incorrectly
#            infer that lineage had a statistically significant effect 
#            on crawler output. 

# We can calculate much variation was accounted for by the main predictor (lineage)?
COVmodelError <- aov(crawlers_total ~ lineage + Error(mass_start), 
                     data = insect_data)
summary(COVmodelError)
car::Anova(COVmodelError$Within, type = 2)
sjstats::anova_stats(car::Anova(COVmodelError$Within, 
                                type = 3)) %>% 
  dplyr::select(1:7)

# etasq = proportion of total variation accounted for by a given factor
# So, 24.6% (0.246) of the variation in the number of crawlers 
# being produced was accounted for by lineage, despite not being
# statistically signicant... Hmmmmm.  

######################################################################
# Step 7: Model diagnostics ------------------------------------------
######################################################################

# Before looking at the outcome of our model,
# we should first check whether the model we specified is 
# a good fit to the data or not. 
# We do this by performing model diagnostics. 

# Basically, here we are going to check whether our 
# data and model meet the assumptions required to run a linear model.

# 1. Independence of data points
#    - This cannot be checked per se. 
#    - This is a property of your experimental design
#    - Make sure you design your experiment properly (pseudo-replication!!!). 

# 2. Are the residuals normally distributed?
#    - Residuals represent the difference between the observed data and the predicted 
#      value (obs - exp).
#    - This assumption is here to determine if the statistical distribution
#      we are using for our model is appropriate for the data. 
#    - If the model is a good fit, then the residuals should be normally 
#      distributed. 
#    - We don't want to see any systematic patterns in residuals, so
#      the points should fall approximately on the dashed line. 
plot(mod1, which = 2)

# Here, we can see that two of the points fall quite far from the 
# dashed line (points labelled 11 and 5), but this is not terrible.
# - As such, our data appears to fit the expectation under the
#   normal (Gaussian) distribution okay-ish.

# 2. Linearity
#   - We must look at the residuals vs fitted plot.
#   - Here, we want to see the mean of the residuals for any x-value 
#     should be approximately = 0. that red line lie on the y = 0 line.
plot(mod1, which = 1)

#   - Linearity assumption does not appear valid here. 
#   - The red line is not on the dashed line, at smaller x-values. 

# 3. Homogeneity of variance
#   - Testing the assumption of equality of variances. 
#   - We must look at the residuals vs fitted plot, again.
#     - We don't want to see any pattern.
#   - You really don't want to see your white circles have any pattern 
#     (i.e. funnel, U-shape)
plot(mod1, which = 1)

#   - Here, we can see the spread in y-values is much bigger at larger x-values
#     than lower x-values. 
#   - Clearly, the variances are NOT EQUAL. 

#  - An ANCOVA DOES NOT appear to be 100% appropriate for this data. 

# Let's assume that our model met the assumptions for ANCOVA, 
# and proceed with how we would finish this analysis.

###########################################################################
# Step 9: Plot model predictions ------------------------------------------
###########################################################################

# Check data
head(insect_data)

# Refit as an 'lm'
mod.lm <- lm(crawlers_total ~ lineage * mass_start, 
             data = insect_data)

# Make new predictions for our model 
newdata <- expand.grid(lineage = levels(insect_data$lineage), 
                       # Range of female mass to predict over 
                       mass_start = seq(min(insect_data$mass_start), 
                                        max(insect_data$mass_start),
                                        # How many points?
                                        l = 100))

# Extract confidence interval for the predictions
fit <- predict(mod.lm, 
               newdata = newdata, 
               interval = "confidence")

# Make fit into a data frame and then add predictions back to the 
# original data 
fit <- data.frame(newdata, fit)
part.obs <- cbind(insect_data, 
                  part.obs = fitted(mod.lm) + resid(mod.lm))

# Reorder lineage 
fit <- fit %>%
  mutate(lineage = fct_relevel(lineage, 
                               "Single", 
                               "Multiple",
                               "Outcrossed"))
part.obs <- part.obs %>%
  mutate(lineage = fct_relevel(lineage, 
                               "Single", 
                               "Multiple",
                               "Outcrossed"))

# Plot the final graph
ggplot(data = fit, aes(x = mass_start, 
                       y = fit)) + 
  geom_point(data = part.obs, aes(y = part.obs, 
                                  group = lineage,
                                  colour = lineage)) + 
  geom_line(aes(colour = lineage)) + 
  theme(legend.position = "right") + 
  # Plot 95% confidence ribbons
  geom_ribbon(aes(ymin = lwr, 
                  ymax = upr, 
                  fill = lineage), 
              alpha = 0.2) + 
  # Manually specificy colours
  scale_fill_manual(values = c("grey70", "grey60", "grey40")) +
  # Manually specificy colours
  scale_colour_manual(values = c("grey70", "grey60", "grey40")) +
  scale_y_continuous("No. of crawlers", 
                     breaks = seq(0, 250, 50),
                     limits = c(0, 250)) +
  scale_x_continuous("Initial female body mass (mg)") + 
  theme(legend.position = "right") +
  labs(colour = "Lineage") +
  guides(fill = FALSE) 

# A cleaner way to look at this would be to have a separate panel 
# for each lineage 
ggplot(data = fit, aes(x = mass_start, 
                       y = fit, 
                       group = lineage)) + 
  geom_point(data = part.obs, aes(y = part.obs, 
                                  group = lineage,
                                  colour = lineage)) + 
  geom_line(aes(colour = lineage)) + 
  geom_ribbon(aes(ymin = lwr, 
                  ymax = upr, 
                  fill = lineage), 
              alpha = 0.2) + 
  # Manually specificy colours
  scale_fill_manual(values = c("grey70", "grey60", "grey40")) +
  # Manually specificy colours
  scale_colour_manual(values = c("grey70", "grey60", "grey40")) +
  scale_y_continuous("No. of crawlers", 
                     breaks = seq(0, 250, 50),
                     limits = c(0, 250)) +
  scale_x_continuous("Initial female body mass (mg)") + 
  theme(legend.position = "right") +
  labs(fill = "Lineage") +
  guides(colour = FALSE) + 
  # Each lineage gets its own panel
  facet_wrap(~lineage, ncol = 3)


###########################################################################
# Post-hoc tests for ANCOVA -----------------------------------------------
###########################################################################

library(multcomp)

# Does number of crawlers produced differ across lineages (which lineages)? 
postHocs<-glht(mod1, 
               linfct = mcp(lineage = "Tukey"))
summary(postHocs)
confint(postHocs)



