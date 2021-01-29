# Script R basics 

###########################################################################
# Some R basics -----------------------------------------------------------
###########################################################################

# Add 
5 + 2

# Subtract
5 - 2

# Multiply
5 * 2

# Divide
5 / 2

# Store something for later use 
# - Add 5 + 2, and store into an object called my_sum
# - '<-' means assign, so below, we will assign the sum of 5 and 2
#    to an object called my_sum
my_sum <- 5 + 2

# Nothing seems to have happened. 
# - We need to explicitly tell R we want to see what was stored in our object. 
my_sum

###########################################################################
# Loading R packages ------------------------------------------------------
###########################################################################

# R has a lot of built-in code, but many people can submit code they 
# have written to perform other analyses. 
# - This code is available in R 'packages'.
# - We will need different packages for different analyses.
# - To use this code, we need to install it on our PC, and then 
#   load the package of code into our R session.

# Installing R packages (only do this once, not every session)

# The general approach is:
install.packages(<insert_package_name>)

# So, two very common packages we will use for our workshop
# are called tidyverse and tidyr. 
# - To install these packages, we would say:

install.packages("tidyverse")
install.packages("tidyr")

# or in one step:
# - We use the c(...) to tell R we want to install multiple packages... 
install.packages(c("tidyverse", "tidyr"))

# Loading R packages (must do this for each new R session)

# Remember, the above code just downloads the packages onto your PC,
# it does not load them into your R session. 
# - Each time you open a new R session, you need to load the packages 
#   you need. 

# The general approach is:
library(<package_name>)

# To load these two packages, we would say:
library(tidyverse)
library(tidyr)





library(tidyverse)
library(raster)








