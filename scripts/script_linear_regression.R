######################################################################
######################################################################
######################################################################
# - R workshop on linear models
# - Model #1: Linear regression
# - Centre for Biological Control, Rhodes Universtiy
# - Script written: 27/07/2020
# - By: Guy F. Sutton
######################################################################
######################################################################
######################################################################

######################################################################
# Aims:
# - 1. Import .csv and .xlsx files
# - 2. Make sure data imported correctly
# - 3. Tidy and visualise raw data
# - 4. Specify a linear regression
# - 5. Evaluate the fit of your model
# - 6. Interpret output from model
# - 7. Plot predictions from model
# - 8. Write-up your model results 
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

# For this first session, we only need the tidyverse and tidyr packages. 
# The code will check to see if the packages in the p_load brackets
# (here: tidyverse, tidyr) are already installed on your PC. 
# - If they aren't installed on your PC, it will install and load them.
# - If they are installed on your PC, it will just load them. 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, 
               tidyr) 

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

# Once we have confirmed our data imported correctly and the data is in 
# the correct format, the next step is to explore your data.
# - This step is often called 'exploratory data analysis' (EDA).
# - I have two aims during this step:
#   (1) Understand the structure of your data (e.g. how many data points are there?,
#       are there different sample sizes between groups, ect...). 
#   (2) Start to generate predictions of what I expect to find during 
#       my statistical analyses.
#       - e.g. looking at my data, do I expect a high correlation or a significant
#              p-value... 

# How many distinct lineages are there?
insect_data %>%
  dplyr::distinct(lineage)

# And how many samples do we have for each lineage?
insect_data %>%
  dplyr::count(lineage)

# Are larger insects more fecund (i.e. do they make more offspring)?
# Take your data
insect_data %>%
  # Make a plot, put mass_start on the x-axis, and crawlers_total on the y-axis
  ggplot(data = ., aes(x = mass_start,
                       y = crawlers_total)) +
  # Make it a scatterplot
  geom_point()

# - Yes, larger females appear to produce more offspring. 

# Does treatment (single, multiple, outcrossed) affect female fecundity?
# Take your data
insect_data %>%
  # Make a plot, put lineage (treatment) on the x-axis, 
  # and crawlers_total on the y-axis
  ggplot(data = ., aes(x = lineage,
                       y = crawlers_total)) +
  # Make it a boxplot
  geom_boxplot()

# - Hmmmm. Difficult to say. 

######################################################################
# Step 5: State your aims/hypotheses/predictions ---------------------
######################################################################

# A big mistake I see people make is not stating a quantifiable hypothesis 
# prior to their analysis.
# - You need to make sure that you are asking the correct question during 
#   your analysis. 
# - You should state your hypothesis PRIOR to running your experiment,
#   but it is a good idea to explicitly state them before running your analysis,
#   prior to looking at the data.

# That being said, it is a good idea to state your predictions and first
# impressions from looking at your data. 
# Let's state our first impressions:

# 1. Do bigger females make more offspring?
# - Yes. 

# 2. Does treatment (lineage) affect female fecundity?
# - I'm not too sure. 

# 3. Anything else? (NB - equal variance amongst groups or across range of x values)
# - 

# Let's say that our hypothesis was: 
# - Do bigger insects produce more offspring?
#  - What is our response variable?
#    - No. offspring (crawlers_total)
#  - What is our predictor variable?
#    - Mass (mass_start)
#  - Which type of analysis should we be using? 
#    - Linear regression.

######################################################################
# Step 6: Perform the analysis ---------------------------------------
######################################################################

# Performing a linear regression in R is very simple. 
# We will use the 'lm' function - which stands for linear model.
# We have to specify a model formula (this style is consistent across 
# most models in R, so get used to it).
# First, we specify our response variable (the thing we are trying to predict),
# and then our explanatory variable(s). 
# Second, we have to tell R where to look for those variables. 
# Lastly, we store the linear regression in an object called 'mod1' 

# General formula for a linear model:
mod1 <- lm(<response_variable> ~ <predictor_variable(s)>,
           data = <name_of_data>)

# Let's write our first linear model
# Response variable: crawlers_total
# Predictor variable(s): mass_start
# Where is our data: insect_data 
mod1 <- lm(crawlers_total ~ mass_start, 
           data = insect_data)

# Nothing happened. 

# Our linear model was stored in mod1, remember? 
# We have to tell R we want to see mod1 to get the results. 
summary(mod1)

# See how little time and code it took to actually run the model? 

# Wait. Before, we start looking at our results, it is always a good idea
# to see how good the model fit was to your data.
# - You don't want to look at the results and find a really cool result, and
#   then later check and find that the model was not a good fit. 

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

# Here, we can see that at least two of the points fall quite far from the 
# dashed line (points labelled 4 and 5). 
# - As such, our data does not appear to fit the expectation under the
#   normal (Gaussian) distribution well, so a linear regression is 
#   probably not a great option. 

# 2. Linearity
#   - We must look at the residuals vs fitted plot.
#   - Here, we want to see the mean of the residuals for any x-value 
#     should be approximately = 0. that red line lie on the y = 0 line.
plot(mod1, which = 1)

#   - Linearity assumption DOES NOT appear valid here. 

# 3. Homogeneity of variance
#   - Testing the assumption of equality of variances. 
#   - We must look at the residuals vs fitted plot, again.
#     - We don't want to see any pattern.
#   - Here, we want to see that red line lie approximately on the y = 0 line.
#   - You really don't want to see your white circles have any pattern 
#     (i.e. funnel, U-shape)
plot(mod1, which = 1)

#   - Here, the red line varies a lot, indicating that the variance varies 
#     across the range of x-values. 
#     - Biologically, we can infer that there appears to be greater variation
#       in the no. of crawlers produced by bigger females.
#     - This is a very common pattern, particularly in ecology (more on this later).

# In summary:
# (1) Normality of residuals - Assumption not met. Normal distribution did not
#     appear valid.
# (2) Linearity - Assumption not met. There did not appear to be a linear 
#     relationship between female body mass and the no. of offspring she 
#     produced. 
# (3) Equality of variances - Assumption not met. There appeared to be 
#     more variation as females got bigger. 

# Let's assume that our model met the assumptions for linear regression, 
# and proceed with how we would finish this analysis.
# - Note: Linear regression is often unsuitable for messy ecological data. 

######################################################################
# Step 8: Interpret model output -------------------------------------
######################################################################

# Assuming that our above model was a good fit (which it IS NOT),
# we can now look at the stuff we are really interested in. 

# Print summary of results 
# - Remember, our linear regression is stored in the 'mod1' object.
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

# Here, we only have 1 term in the model (mass_start), so type I would 
# have been fine. 
# - We will see how to get type II and III SS tomorrow. 
summary(mod1)

# There are 5 things that we should be looking at here to understand
# the results from our model:

# - (1) Intercept estimate - value of y-value (no. of offspring) 
#       when X (female body mass) = 0
coef(mod1)[[1]]

#       Intercept = 10.08,
#       In this example, female body mass cannot = 0,  
#       Intercept has no meaning in this example.
#       It often can have a meaning (whenever x = 0 is reasonable, e.g. temperature),
#       and be very important. 

# - (2) Is my treatment/factor significant?
#       Look at the row for your predictor variable in SOS table (here: mass_start)
#       Look at the P-value: 
#       - P < 0.05 means this variable is significant,
#       - P > 0.05 means that there is no evidence for a significant effect 
#         - NB: This DOES NOT mean that there is no effect. 
#       - Here, mass_start P-val > 0.05, 
#         so we conclude there is no evidence for a statistically significant  
#         association between female body mass and fecundity. 

# - (3) How does my treatment effect response?
#       Look at the estimate value in your predictor variable row
#       > 0 estimate = response increases with greater values of predictor
#       < 0 estimate = response decreases with greater values of predictor
coef(mod1)[2]

#       Here, our estimate = 0.24 (> 0, positive relationship).
#       - The value is positive (> 0) which means that bigger females produce 
#         more offspring. 
#       - The actual value (0.24) means that for every 1 unit increase in
#         female body mass (i.e. if adult mass increases by 1mg), 
#         fecundity increases by 0.24 offspring. 

# - (4) Extract parameter uncertainty 
#       It is always good to report the uncertainty of your model estimates.
#       To do this, we report a 95% confidence interval.
#       - The interval DOES NOT say that we can be 95% certain that 
#         our estimate falls somewhere within this range (lots of papers do this 
#         incorrectly.)
#       - The interval shows that if we had to repeat this experiment 100 times,
#         we would expect our parameter to fall within the 95% CI, 95 times.
#       - A subtle but very important distinction - outside the scope of this course.
#       - We will now extract the 95% CI for our estimate
confint(mod1)

# Here, our 95% CI for adult body mass is -0.02 to 0.51
# This means that there is some uncertaintly in our parameter estimate from (3).
# Interpretation: For every 1 unit increase in adult mass (i.e. as adults 
# become 1mg heavier), we can be quite confident that they will produce 
# somewhere between 0.01 fewer to 0.51 more offspring than a female 
# that is 1mg lighter. 

# - (5) How much variation is accounted for?
#       - We must extract the adjusted R-squared value
#         - This shows how much variation in your response var (e.g. larvae)
#           is explained by your predictors (e.g. adult_mass)
summary(mod1)$adj.r.squared

#       Here, an R-squared = 0.17
#       Therefore, 17% of the variation in offspring produced 
#       is explained by female body mass. 

###########################################################################
# Step 9: Plot model predictions ------------------------------------------
###########################################################################

# Above, we have performed a hypothesis test (i.e. is there a statistically
# significant relationship between female body mass and fecunity).
# - We may also be interested in making predictions from our model. 

# (1) Define the range of x-values you want to predict over 
#     - Here, we predict over body mass of 1 to 150 mg
x_axis_range <- seq(1, 150, 1)

# (2) Predict the number of larvae from our model
mod_predict <- predict(mod1, 
                       # List(<name_of_predictor> = <range_of_values_to_predict_over>)
                       list(mass_start = x_axis_range), 
                       interval = "confidence")

# (3) Convert (2) into a data frame 
mod_predict <- as.data.frame(mod_predict)
head(mod_predict)

# (4) Add the range of x values back to the model predictions
mod_predict <- bind_cols(mod_predict, as.data.frame(x_axis_range))
head(mod_predict)

# Plot your model predictions
ggplot() + 
  #Add a ribbon of 95% confidence intervals
  geom_ribbon(data = mod_predict, aes(x = x_axis_range, 
                                      ymin = lwr,
                                      ymax = upr),
              fill = "grey80") + 
  # Add line of model prediction
  geom_line(data = mod_predict, aes(x = x_axis_range, 
                                    y = fit)) +
  # Write x and y axis labels
  labs(x = "Adult body mass (mg)",
       y = "No. of offspring produced")

# Here, we can see the black line indicating the line of best fit. 
# - The grey shading indicates the upper and lower 95% CI.
#   - Notice how the CI becomes wider and wider at higher x-values.

###########################################################################
# Step 10: Communicate your results  --------------------------------------
###########################################################################

# Everyone has a different style of writing. 
# Below I give some examples. 
# Any text within [] is me explaining, and not actually included in the text. 
# This is my approach, it does not have to be yours. 

# (1) The standard example is:
# Insect body mass did not have a significant influence on the number of 
# offspring produced (P > 0.05). 

# - In my opinion, this is pretty poor. 
# - It really tells us nothing. 

# (2) Improvement on (1):
# Insect body mass did not have a significant influence on the number of 
# offspring produced (beta = 0.24; P < 0.05). [where beta is estimate from 3]. 

# - Adding the parameter estimate at least tells the reader the sign
#   and magnitude of the effect mass_start has on no. offspring produced. 

# (3) More improvements:
# Insect body mass did not have a significant influence on the number of 
# offspring produced (beta = 0.24; 95% CI = -0.01 - 0.051; P > 0.05).

# - Adding the CI shows the uncertainty in our estimate.
# - Much better. 
# - We can still do better. 

# (4) Actually explain your results.
#     - This is my preferred approach. 

# We found no statistical support for larger females being more fecund than 
# smaller females (beta = 0.24; P > 0.05), albeit approximately 17% of the variation in
# female fecundity was explained body mass (Adj. R-squared = 0.17). 
# For every 1mg increase in body mass, females were predicted to produce
# 0.01 fewer to 0.51 more offspring than a female weighing 1mg less 
# (95% CI: -0.01 - 0.51) (Fig. x) [refer the reader to your plot
# of the model prediction]. 

# - We have told the reader exactly how the predictor effects response,
#   we have reported uncertainty, and referred the reader to our awesome graph!!! 

# Your results for your paper are now ready to go!!!







