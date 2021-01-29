######################################################################
######################################################################
######################################################################
# - R workshop on linear models
# - Class exercise: Model Answer
# - Centre for Biological Control, Rhodes Universtiy
# - Script written: 26/08/2020
# - By: Guy F. Sutton
######################################################################
######################################################################
######################################################################

######################################################################
# Step 1: Load packages ----------------------------------------------
######################################################################

# Load the packages we will require
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, 
               tidyr,
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
class_data <- readr::read_csv("./data_raw/exercise_data.csv") 

###########################################################################
# Step 3: Tidy and transform raw data -------------------------------------
###########################################################################

# Check import
head(class_data)
glimpse(class_data)

# Looks good. Both variables should be numeric. 

###########################################################################
# Step 4: Visualise data --------------------------------------------------
###########################################################################

# We only have two variables, so let's just plot them. 
ggplot(data = class_data, aes(x = nitrogen_conc,
                              y = insect_abundance)) +
  geom_point()

# First impressions:
# - There is a relationship between the variables.
# - A positive relationship. As nitrogen increases, so does 
#   insect abundance. 
# - Is it linear? Hard to tell, isnt it? 
# - Equal variances? Hmmmmm. 

######################################################################
# Step 5: State your aims/hypotheses/predictions ---------------------
######################################################################

# In this session, we are going to ask:
# (1) Is there a statistical relationship between nitrogen concentration
# and insect abundance? 

######################################################################
# Step 6: Run statistical model - linear regression ------------------
######################################################################

# The data is two numeric variables, so our first thought could be 
# that this should be a linear regression. 

# Let's try it:
mod.lm <- lm(insect_abundance ~ nitrogen_conc, 
             data = class_data)
summary(mod.lm)

# Was a linear regression appropriate? 
# 1. Normality of residuals
plot(mod.lm, which = 2)

# - It doesn't appear so. 
# - There is a systematic pattern of too much variance at the top-right 
#   of the panel. 
# - A normal distribution is probably not appropriate, but this is not terrible. 

# 2. We could check linearity and equality of variances assumption, 
#    just for practice. 
plot(mod.lm, which = 1)

# - Linearity assumption is not quite right. Almost. 
# - Equality of variance assumption is not met.
#   - There us much more variation at higher fitted values. 

# Unsurprisingly, a linear regression doesn't appear valid. 
# - Why is this unsurprising? 
#   - Well, we are clearly analysing count/integer data,
#     so we could have probably just started with a Poisson GLM.
#   - I think it is a good idea to check an LM though. 
#   - Start simple and build to more complex models. 

######################################################################
# Step 6b: Run statistical model - Gaussian GLM ----------------------
######################################################################

# The data is two numeric variables, so it is possible that a 
# Gaussian (normally distributed) GLM came to mind. 

# Let's try it:
mod.glm <- glm(insect_abundance ~ nitrogen_conc, 
               data = class_data,
               family = gaussian(link = "identity"))
summary(mod.glm)

# Was a Gaussian GLM appropriate? 
res.glm <- simulateResiduals(mod.glm)
plot(res.glm)

# 1. QQplot - KS test is NS and points fall approximately on the expectation line.
#           - This plots tells us that it is at least possible our data is 
#           - normally distributed.

# 2. Res vs fitted - Linear relationship does not appear suitable 
#                    - Red bold lines are not flat and or black (black = no issues,
#                      red = issues)
#                  - Strong evidence for heterogeneity of variances

# While there is some evidence that the data could be normally distributed,
# the rest of the model diagnostics were not good (non-linear relationships 
# an heterogeneity of variances). 
# - Because our response variable is an integer (count, no decimal places),
#   we could have probably just started with a Poisson GLM (remember, the 
#   Poisson distribution is specifically designed for count data).
# - Let's see whether a Poisson GLM was a better fit. 

######################################################################
# Step 6c: Run statistical model - poisson GLM -----------------------
######################################################################

# Let's now try a Poisson GLM

# Let's try it:
mod.pois <- glm(insect_abundance ~ nitrogen_conc, 
                data = class_data,
                family = poisson(link = "log"))

# Was a Poisson GLM appropriate? 
res.pois <- simulateResiduals(mod.pois)
plot(res.pois)

# 1. QQplot - KS test is NS and points fall perfectly on the expectation line.
#           - Our data appears to come from a Poisson distribution. 

# 2. Res vs fitted - Linear relationship appears suitable (black bold lines)
#                  - No evidence for heterogeneity of variances. 

# 3. Zero infation - With a count model, we should check for zero-inflation.
testZeroInflation(mod.pois)

# - P > 0.05, t.f. no evidence for zero inflation. 

# (6) Which model was most appropriate (e.g. anova, ancova, 
#     linear regression, poisson or binomial glm)?

#     - Binomial requires 0/1 data. 
#       - Our response variable is an integer. 
#     - ANCOVA requires a categorical AND numeric predictor. 
#       - We only have a numeric predictor. 
#     - ANOVA requires a categorical predictor. 
#       - We have a numeric predictor. 

#     - Linear regression, Gaussian GLM and Poisson GLM were all potential options.
#       - Response variable is an integer, so Poisson is likely a better 
#         option (it is specifically designed for count data).
#       - We ran a linear regression and Gaussian GLM anyway, and could clearly see 
#         that the data although the data may follow a normal distribution (QQplot),
#         the linearity and equality of variances assumptions were failed.
#       - Also note that the lm and gaussian GLM are exactly the same model
#         (exactly same parameter estimates and diagnostics issues)/ 

#     - The correct model choice was a Poisson GLM. 
  
######################################################################
# Step 7: Model inference ------------------------
######################################################################

# Default summary table 
summary(mod.pois)

# Type 1 SOS table
anova(mod.pois, test = "Chisq")

# Our usual approach is to use the code below
# - Will default to type I SOS because only one term in the model. 
car::Anova(mod.pois, type = "II")

# Answer some Q's: 
# (7a) Does nitrogen concentration influence insect abundance?
#     - Yes. The Wald Chisq test shows a significant effect of 
#       nitrogen_conc on insect abundance (X2 = 1776.3, df = 1, P < 0.001). 

# (7b)  What effect does it have (e.g. positive/negative)? 
#     - Look at the parameter estimate / look at a graph. 
#       - Estimate = 0.47, so positive relationship.
#       - As nitrogen_conc increases, so does insect abundance. 

# (7c) How many more insects would you expect to find, on average, if 
#      nitrogen increases by 1 unit?
exp(coef(mod.pois))

# - In a Poisson GLM, this means that a 1 unit increase in nitrogen_conc,
#   will lead to an increase in insect abundance by a factor of 1.61x. 
# - To get absolute numbers, we would need to extract predictions. 

# (8) Produce a 95% confidence interval of the effect of nitrogen conc. 
#     on insect abundance
exp(confint(mod.pois))

# - The confidence interval is 1.57 - 1.61.   
# - This means that a 1 unit increase in nitrogen_conc,
#   will lead to an increase in insect abundance by a factor of 1.57 - 1.65x. 

###########################################################################
# Step 8: Plot model predictions ------------------------------------------
###########################################################################

# Get the link function for the Poisson model
# - We need this to back-transform the predictions 
ilink <- family(mod.pois)$linkinv

## some data to predict at: 100 values over the range of nitrogen_conc vals
ndata <- expand.grid( 
                     # Range of nitrogen_conc to predict over 
                     nitrogen_conc = seq(0, # Minimum x-value
                                         10, # Maximum x-value
                                      # How many points?
                                      l = 100))

# Option #2: 
ndata <- expand.grid( 
  # Range of nitrogen_conc to predict over 
  nitrogen_conc = seq(min(class_data$nitrogen_conc), # Minimum x-value
                      max(class_data$nitrogen_conc) + 5, # Maximum x-value
                      # How many points?
                      l = 100))

# Add the fitted values by predicting from the model for the new data
ndata <- add_column(ndata, 
                    fit = predict(mod.pois, 
                                  newdata = ndata,
                                  type = 'response'))

# Add predictions from the model to a new df
ndata <- bind_cols(ndata, setNames(as_tibble(predict(mod.pois, 
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

# (9a) Make the plot
ggplot(data = ndata) +
  geom_line(aes(x = nitrogen_conc,
                y = fit)) +
  geom_ribbon(aes(x = nitrogen_conc,
                  ymin = right_lwr,
                  ymax = right_upr),
              alpha = 0.2) +
  #scale_y_continuous(limits = c(0, 18),
  #                   breaks = seq(0, 18, 3)) +
  labs(x = "Nitrogen concentration",
       y = "Insect abundance") +
  theme(legend.position = "right") +
  guides(fill = FALSE) 

# (9b) How many insects would you expect to find at 
#      a nitrogen concentration = 10?
ndata %>%
  dplyr::filter(nitrogen_conc == 10)

# - At nitrogen_conc = 10, we would expect to find 158 (95% CI: 135 - 184)
#   insects. 
