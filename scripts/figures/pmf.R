devtools::load_all()
library(data.table) # for general data manipulation
library(ggplot2) # for plotting
library(patchwork) # for combining plots
library(stringr) # for string manipulation


# Define a continuous PMF (could consider more than one delay but not likely
# we want to)
# Define a range of growth rates
# Show the implied prior for the primary event for each growth rate alongside
# the various discrete approximations priors (or impled priors). 
# Show the PMF for each growth rate alongside the various discrete approximations.
# Show the mean and standard deviation of the PMF for each growth rate and the 
# various discrete approximations.




sim <- simulate_double_censored_pmf(0.6, 0.5, 10, 1000)
