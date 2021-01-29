######################################################################
######################################################################
######################################################################
# - R workshop on linear models
# - Model #2: Analysis of variance (ANOVA)
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

# We have already done EDA for this dataset in the linear regression tut. 
# - Let's just re-look at one particular bit of data.
# - Remember, anova requires a continuous response variable, and 
#   a categorical predictor variable. 
# - In this session, we are going to apply an ANOVA to this dataset
#   to evaluate:
#   - Does lineage (treatment) effect female fecundity (no. of babies produced)?

# Let's graph this 

# Take your data
insect_data %>%
  # Make a plot, put lineage (treatment) on the x-axis, 
  # and crawlers_total on the y-axis
  ggplot(data = ., aes(x = lineage,
                       y = crawlers_total)) +
  # Make it a boxplot
  geom_boxplot()

# - Not clear if there are mean differences between groups.
# - Probably will fail assumption of homogeneity of variances. 

######################################################################
# Step 5: State your aims/hypotheses/predictions ---------------------
######################################################################

# Null hypothesis: There was no difference in the number of crawlers
#                  produced across lineages. 
# Alternative hyp: The number of crawlers produced differs across lineages.

######################################################################
# Step 6: Perform the analysis ---------------------------------------
######################################################################

# Performing an ANOVA in R is very simple. 
# We will use the 'aov' function - which stands for analysis of variance.
# We have to specify a model formula (this style is consistent across 
# most models in R, so get used to it).
# First, we specify our response variable (the thing we are trying to predict),
# and then our explanatory variable(s). 
# Second, we have to tell R where to look for those variables. 
# Lastly, we store the linear regression in an object called 'mod1' 

# General formula for an ANOVA
mod1 <- aov(<response_variable> ~ <predictor_variable(s)>,
            data = <name_of_data>)

# Let's write our first anova
# Response variable: crawlers_total
# Predictor variable(s): lineage
# Where is our data: insect_data 
mod1 <- aov(crawlers_total ~ lineage, 
            data = insect_data)
  
# Nothing happened. 
  
# Our anova was stored in mod1, remember? 
# We have to tell R we want to see mod1 to get the results. 
summary(mod1)

# See how little time and code it took to actually run the model? 

# Before, we start looking at our results, it is always a good idea
# to see how good the model fit was to your data.

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

# Here, we can see that at one of the points falls quite far from the 
# dashed line (point labelled 3), but this is okay. 
# - As such, our data appears to fit the expectation under the
#   normal (Gaussian) distribution quite well, so for now, 
#   an ANOVA seems appropriate. 

# 2. Linearity
#   - We must look at the residuals vs fitted plot.
#   - Here, we want to see the mean of the residuals for any x-value 
#     should be approximately = 0. that red line lie on the y = 0 line.
plot(mod1, which = 1)

#   - Linearity assumption appears valid here. 
#   - The red line is pretty much on the dashed line, showing
#     the mean residuals for each lineage are roughly = 0. 

# 3. Homogeneity of variance
#   - Testing the assumption of equality of variances. 
#   - We must look at the residuals vs fitted plot, again.
#     - We don't want to see any pattern.
#   - You really don't want to see your white circles have any pattern 
#     (i.e. funnel, U-shape)
plot(mod1, which = 1)

#   - Here, we can see the spread in y-values is much bigger at largest x-values
#     than lower x-values. 
#   - Clearly, the variances amongst lineage treatments is NOT EQUAL. 

# In summary:
# (1) Normality of residuals - Assumption was met. Normal distribution appears
#     appear valid.
# (2) Linearity - Assumption was met. 
# (3) Equality of variances - Assumption not met. There appeared to be 
#     more variation for some lineage treatments than others. 
#  - An ANOVA is NOT APPROPRIATE for this data. 

# Let's assume that our model met the assumptions for ANOVA, 
# and proceed with how we would finish this analysis.
# - Note: ANOVA is often unsuitable for messy ecological data. 

######################################################################
# Step 8: Interpret model output -------------------------------------
######################################################################

# Assuming that our above model was a good fit (which it IS NOT),
# we can now look at the stuff we are really interested in. 

# Print summary of results 
# - Remember, our ANOVA is stored in the 'mod1' object.
# - Anyone who has run models in R before is probably familiar with 
#   extracting the model output by summary(<model_name>). 
# - I encourage you not to get too familiar with this output.
# - More times than not, these are NOT the results you want to look at. 
#   - This is due to the way in which R calculates the p-values. 
#   - The summary(...) output produces type I sum of squares, which calculates
#     p-values based on the order in which terms are added to the model.
#   - This is not great (but makes no difference when you only have
#     one predictor variable in the model).
summary(mod1)

# We need to specify how we want R to calculate p-values. 
# - When you have 1 term in the model, type I SS is fine. 
# - When you have 2 terms in the model (but no interaction),
#   type II SS is fine. 
# - When you have 2 terms in the model, and an interaction term, 
#   you must use type III SS. 
# - We won't go into more details now, but get into the habit of 
#   manually specifying the SS required. 

# Here, we only have 1 term in the model (lineage), so type I would 
# have been fine. 
# - Let's just use type II SS to get into a good habit
car::Anova(mod1, type="II")

# There are 5 things that we should be looking at here to understand
# the results from our model:

# - (1) Is my treatment/factor significant?
#       Look at the row for your predictor variable in SOS table (here: lineage)
#       Look at the P-value: 
#       - P < 0.05 means this variable is significant,
#       - P > 0.05 means that there is no evidence for a significant effect 
#         - NB: This DOES NOT mean that there is no effect. 
#       - Here, lineage P-val > 0.05, 
#         so we conclude there is no evidence for a statistically significant  
#         difference in female fecundity across different plant lineages. 

# - (2) Which levels within your factor differ from each other? 
#       - e.g. are the number of crawlers produced different
#         between single, multiple and outcrossed lineages?
#         - There shouldn't be as we had P > 0.05 above. 
#       - We can use post-hoc tests (e.g. Tukey tests)
#       - Two options shown below

#   (2.1) Standard Tukey test 
tukey_res <- TukeyHSD(mod1)
tukey_res

#   (2.2) emmeans Tukey test 
#         - My preference, provides nice output and confidence intervals 
emm1 <- emmeans(mod1, 
                specs = pairwise ~ lineage,
                adjust = "tukey")
emm1

# Get 95% confidence intervals 
emm1$contrasts %>%
  confint()

# Combine both tables 
emm1$contrasts %>%
  summary(infer = TRUE) 

# Interpretation:
# - Estimate: Mean difference between groups
# - Lower and Upper CL: 95% CI's of mean difference between groups

###########################################################################
# Step 9: Plot model predictions ------------------------------------------
###########################################################################

# Above, we have performed a hypothesis test (i.e. is there a statistically
# significant relationship between female body mass and fecunity).
# - With ANOVA, we can't necessarily make predictions because our 
#   predictor variable(s) are categorical, so our graph 
#   is usually just the raw data with significant differences between groups. 

# Much easier than plotting the 'lm' results 
ggplot(data = insect_data, aes(x = lineage, 
                               y = crawlers_total)) +
  geom_boxplot() +
  labs(y = "No. of larvae produced") 

# The order of the x-axis is a little weird. 
# - It makes more sense to me to have single, then multiple, then outcrossed.
# - Let's remake the graph with this new order 
insect_data <- insect_data %>%
  # Manually specify order of x-axis categories
  dplyr::mutate(lineage = forcats::fct_relevel(lineage, 
                                               "Single", 
                                               "Multiple",
                                               "Outcrossed"))


# Graph
plot_2 <- ggplot(data = insect_data, aes(x = lineage, 
                               y = crawlers_total,
                               # Add different colours for different lineages
                               fill = lineage)) +
  # Tell R we want a boxplot
  geom_boxplot() +
  # Manually specificy colours
  scale_fill_manual(values = c("grey80", "grey60", "grey40")) +
  # Manually specify x-axis 
  scale_x_discrete("Insect lineage",
                   labels = c("Single \n (low diversity)", 
                              "Multiple \n (medium diversity)", 
                              "Outcrossed \n (high diversity)")) +
  # Add y-axis label
  labs(y = "No. of larvae produced",
       fill = "Lineage",
       subtitle = "(b) Does insect genetic diversity affect fecundity?") +
  scale_y_continuous(limits = c(0, 55),
                     breaks = seq(0, 55, 10)) +
  theme(legend.position = "right") +
  theme(axis.text = element_text(colour = "black")) +
  # Add significance stars (use the emmeans output/tukey)
  # Use x = "..." to indicate which group
  # x = 1 means first group,
  # x = 2 means second group... 
  # Manually play around with y-values
  annotate("text", x = 1, y = 32, label = "a") +
  annotate("text", x = 2, y = 37, label = "a") +
  annotate("text", x = 3, y = 53, label = "a") 
plot_2

# Saving the figure to your PC
# - Can change file format to .svg, .jpg or whatever you want
# - If SVG, requires package: svglite
ggsave("./figures/anova_figure_example.png",
       height = 6,
       width = 8,
       dpi = 600)

# 

# First graph we want 
plot_1 <- insect_data %>%
  # Make a plot, put lineage (treatment) on the x-axis, 
  # and crawlers_total on the y-axis
  ggplot(data = ., aes(x = lineage,
                       y = crawlers_total,
                       fill = lineage)) +
  # Make it a boxplot
  geom_boxplot() +
  labs(subtitle = "(a)",
       fill = "Lineage") +
  theme(legend.position = "right") +
  scale_fill_manual(values = c("grey80", "grey60", "grey40")) 
plot_1

# Put the two graphs together 
library(cowplot)
plot_grid(plot_1, plot_2,
          ncol = 1)
ggsave("./figures/two_plot_example.png",
       height = 6,
       width = 6,
       dpi = 600)
# And that is your first ANOVA, done and dusted... :) 

# How to write up your ANOVA results? 

# Lineage treatment did not have a significant effect on insect fecundity
# (F = 0.548, d.f. = 2, P = 0.592). 

###########################################################################
# Make half boxplot / half raw points -------------------------------------
###########################################################################

# Need a need package 
library(ggpol)

plot_2 <- ggplot(data = insect_data, aes(x = lineage, 
                                         y = crawlers_total,
                                         # Add different colours for different lineages
                                         fill = lineage)) +
  # Tell R we want a half boxplot / half raw data points
  geom_boxjitter(errorbar.draw = T,
                 jitter.width = 0.05) +
  # Manually specificy colours
  scale_fill_manual(values = c("grey80", "grey60", "grey40")) +
  # Manually specify x-axis 
  scale_x_discrete("Insect lineage",
                   labels = c("Single \n (low diversity)", 
                              "Multiple \n (medium diversity)", 
                              "Outcrossed \n (high diversity)")) +
  # Add y-axis label
  labs(y = "No. of larvae produced",
       fill = "Lineage",
       subtitle = "(b) Does insect genetic diversity affect fecundity?") +
  scale_y_continuous(limits = c(0, 55),
                     breaks = seq(0, 55, 10)) +
  theme(legend.position = "right") +
  theme(axis.text = element_text(colour = "black")) +
  # Add significance stars (use the emmeans output/tukey)
  # Use x = "..." to indicate which group
  # x = 1 means first group,
  # x = 2 means second group... 
  # Manually play around with y-values
  annotate("text", x = 1, y = 32, label = "a") +
  annotate("text", x = 2, y = 37, label = "a") +
  annotate("text", x = 3, y = 53, label = "a") 
plot_2


