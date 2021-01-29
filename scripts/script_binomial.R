######################################################################
######################################################################
######################################################################
# - R workshop on linear models
# - Model #4: Binomial (logistic) GLM
# - Centre for Biological Control, Rhodes Universtiy
# - Script written: 27/07/2020
# - By: Guy F. Sutton
######################################################################
######################################################################
######################################################################

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
insect_data <- readr::read_csv2("https://raw.githubusercontent.com/guysutton/CBC_coding_club/master/data_raw/binomial_data.csv")

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

######################################################################
# Step 4: Exploratory data analysis ----------------------------------
######################################################################

# We have data on the occurence of an insect, across an elevation gradient.

# How many sites do we have? 
insect_data %>%
  dplyr::count(altitude)

# - We have one survey per altitude and 18 different altitudes. 

# Naturally, we should plot to see if there is a relationship
# between elevation and presence/absence of our insect. 
insect_data %>%
  ggplot(data = ., aes(x = altitude,
                       y = occ)) +
  geom_point() +
  labs(x = "Altitude (m)",
       y = "Insect present (1) or absent (0)") 
  # Add linear regression line
  #geom_smooth(method = "lm")

# At low altitudes (0 - 500m), insect is always present
# At medium altitudes (600 - 1000m), insect is sometimes present, sometimes not.
# At high altitude (> 1100m), insect is always present. 

# Already we should be expecting that our analyses will find a statisically
# significant relationship between altitude and insect presence/absence. 

######################################################################
# Step 5: State your aims/hypotheses/predictions ---------------------
######################################################################

# In this session, we are going to ask:
# (1) Does altitude have an effect on insect presence/absence?

######################################################################
# Step 6: Perform the analysis ---------------------------------------
######################################################################

# Performing a binomial GLM is quite straighforward. 
# We will use the 'glm' function.
# First, we specify our response variable (the thing we are trying to predict),
# Second, we specify our predictor variable. 
# Then like usualy, we have to tell R where to look for those variables. 
# and, we store the ANCOVA in an object called 'mod1'. 
# The big difference now is that we have to tell R which type of GLM we want.
# - We use the family = <model_name> argument 

mod1 <- glm(occ ~ altitude, 
            data = insect_data,
            # Tell R we want binomial GLM
            family = binomial)
summary(mod1)
car::Anova(mod1)
#car::Anova(mod1, type = "III",
#           contrasts = list(topic = contr.sum, 
#                            sys=contr.sum))

######################################################################
# Step 7: Model diagnostics ------------------------------------------
######################################################################

# Model diagnostics for GLM's are a little different in look and code, 
# but the interpretation is much the same. 

# Standard GLM diagnostics using the 'DHARMa' package
mod_diag <- simulateResiduals(mod1)
plot(mod_diag)

# - QQ plot clearly shows that a binomial GLM was okay. 
# - The non-significant KS test indicates there our data do not deviate 
#   significantly from what would be expected under the binomial
#   distribution. 

# - The residuals vs predicted plot provides an overall test of equal variance.
#   - We can see that this assumption was viotated, a bit. 
#   - Bold lines always above our expected values of 0.25, 0.50, 0.75
#   - Our model is displaying some heterogeneity in variance,
#     and will systematically underpredict probability of recording focal species.

# We would need to try a different model for this data as it fails the 
# assumptions tests above, if this was for a paper/thesis. 
# - For now, let's proceed with how we would finish this analysis if 
#   the assumptions tests were okay. 

###########################################################################
# Step 8: Interpret model output ------------------------------------------
###########################################################################

# Here comes the tricky part. 
# - The expected values for a binomial model are on the log-odds scale.
#   - These numbers are quite difficult to interpret.
#   - We will go through them below, but we will make the intepretation
#     much easier just now by transforming log-odds into 
#     predicted probabilities. 

# - (1) Is my treatment/factor significant?
#       Look at the row for your predictor variable (here: altitude)
#       Look at the P-value: P <0.05 means this variable is significant 
#       - Some type of association between altitude and occurence 
#         of our focal species. 

# - (2) How does my treatment effect response?
#       Look at the estimate value in your predictor variable row
#       > 1 estimate = response increases with greater values of predictor
#       < 1 estimate = response decreases with greater values of predictor
#       NB - This value is on log-odds scale - need to back-transform
#          - Coefficients indicate the change in log-odds of our response
#            with a 1 unit increase in the predictor variable. 
exp(coef(mod1)[2])

# - Now we can say that for every unit increase in altitude,
#   the odds of us recording our focal species (i.e. recording a 1/presence)
#   decreases by a factor of 1.01 (0.99 means a decrease) or 1%. 
#   - Another example would be easier to interpret:
#     Say our OR for altitude = 1.20, this would mean that for every 1 unit
#     increase in altitude, the odds of recording our species increases by a factor 
#     of 1.2x or 20%. 
#   - Again, OR for altitude = 0.9, this would mean that for every 1 unit
#     increase in altitude, the odds of recording our species decreases by a factor 
#     of 1.1x or 10%.

# This is still pretty confusing though, at least to me. 

# Tricky, right? 
# - Much easier to plot the predicted probabilities of recording
#   the focal species and visualise the relationships between variables. 

###########################################################################
# Step 9: Plot model predictions ------------------------------------------
###########################################################################

# Get the link function for the binomial model
# - We need this to back-transform the predictions 
ilink <- family(mod1)$linkinv

## some data to predict at: 100 values over the range of altitude
ndata <- expand.grid(# Range of altitudes to predict over 
                     altitude = seq(min(insect_data$altitude), 
                                      max(insect_data$altitude) + 500,
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
  geom_line(aes(x = altitude,
                y = fit)) +
  geom_ribbon(aes(x = altitude,
                  ymin = right_lwr,
                  ymax = right_upr),
              alpha = 0.2) +
  labs(x = "Altitude (m)",
       y = "Probability of recording species") +
  scale_y_continuous(breaks = seq(0, 1, 0.2), 
                     limits = c(0, 1)) +
  theme(legend.position = "right") 

# That makes a lot more sense to me. 
# - Between 0 and 600m altitude, we are highly likely to record the species.
#   - Any site within this altitude would be >80% confident to record species. 
# - Above 600m, probability of finding species rapidly declines.
# - Above 1200m, probability approaches 0,
# - But, the CI's (gray bands), are quite wide, indicating a lot of uncertainty. 
# - With a small sample size like this study, not surprising.
