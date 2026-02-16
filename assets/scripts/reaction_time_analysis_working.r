## This is the solution script for the first practical


#######################################################
## first line of code is to clear R's memory
rm(list=ls())
#######################################################


#######################################################
## First we load some required add-on package
## (you need to install these if you haven't already)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
#######################################################

## You need to download the dataset. A link is in the practical description.


#######################################################
## Now read in the data, using the read_csv() function.
## Use the read_csv function to read in the data
## Note that you will need to put the data in the same folder as this script, then
## open RStudio by double clicking on the script file.
class_RTs <- read_csv("datasets/FS26 Reaction time form (Responses) - Form Responses 1.csv")
#######################################################

## You do not have to use read.csv above!!!

## --->>> Once more, and even if a TA tells you to, do not use read.csv on line 25.
## --->>> You should keep the underscore (_) and not replace it with a dot (.)

#######################################################
## Have a look at the data in R, does it look OK?
class_RTs
#######################################################


#######################################################
## Now we need to do some data wrangling (cleaning and tidying)
## Clean up the column / variable names:
## Must be very careful to get the next line right!!! Really important!!!
## Otherwise columns will have the wrong names, which would be very confusing
names(class_RTs) <- c("Timestamp",
                      "RT1", "RT2", "RT3", "RT4", "RT5",
                      "Random_number",
                      "Sex_at_birth")
## check the variable names are what we just tried to set them to be
names(class_RTs)
#######################################################

## View the data
View(class_RTs)

#######################################################
## Check the variable types are correct
## (they should be in this case, but checking is a good habit.)
## Timestamp should be a character
## ID should be a character
## Sex at birth should be a character
## Handed should be character
## The remaining variables should be numeric (<dbl>)
str(class_RTs)
#######################################################

#######################################################
## We need to add an identifier variable for each participant
class_RTs <- class_RTs |>
  mutate(ID = paste0("ID-", row_number()))


#######################################################
## Change the data from wide to long format
RTs_long <- class_RTs %>%
  pivot_longer(cols = starts_with("RT"),
               names_to = "Trial",
               values_to = "RT_value")

#######################################################
## Find the number of observations of each sex at birth
class_RTs %>%
  group_by(Sex_at_birth) %>%
  summarise(number = n())
#######################################################

#######################################################
## calculate the mean reaction time for each participant
RTs <- RTs_long %>%
  group_by(ID, Sex_at_birth) %>%
  summarise(mean_RT = mean(RT_value))
#######################################################



#######################################################
## Now make a figure containing the histogram of reaction times
ggplot(data=RTs, aes(x=mean_RT)) +
  geom_histogram()

## Now make a figure containing two histograms histograms (i.e. two "facets"), one for each sex at birth
ggplot(data=RTs, aes(x=mean_RT)) +
  geom_histogram() +
  facet_grid(~ Sex_at_birth)

## And a box and whisker plot
ggplot(data=RTs, aes(x=Sex_at_birth, y=mean_RT)) +
  geom_boxplot()

## Or just the data points (with some jitter, to separate overlapping points):
ggplot(data=RTs, aes(x=Sex_at_birth, y=mean_RT)) +
  geom_jitter(width=0.05)
#######################################################

#######################################################
## Perhaps filter out some extreme values
RTs_filtered <- RTs %>%
  filter(mean_RT > 50,
         mean_RT < 500)
RTs_filtered %>%
ggplot() +
  geom_jitter(mapping = aes(x=Sex_at_birth, y=mean_RT),
              width=0.05)
#######################################################


#######################################################
## Do you think there is a difference in reaction times between females and males?
## What is the effect size (i.e. the magnitude of the difference?)
## Is this likely to be of practical significance?
## Look at your graphs and assess assumptions:
## - Do you think the residuals will be normally distributed?
## - Do the two groups have similar variance?
## - Do there seem to be any outliers?
## - Are data points independent? (You don't get this from the graph, but rather from knowing how the data were collected.)
#######################################################


#######################################################
## Do a t-test and assign the outcome to an object:
my_ttest <- t.test(mean_RT ~ Sex_at_birth,
                   data=RTs_filtered,
                   var.equal=TRUE)
## look at the result of the t-test
my_ttest
#######################################################



#######################################################
## Critical thinking
# How might the work be flawed?
# How might the analysis be flawed (assumptions violated)?
# Is the difference (i.e. effect size) small, medium, large, relative to differences caused by other factors?
# How general might be the finding?
# How do the qualitative and quantitative findings compare to those in previous studies?
# What could have been done better?
# What are the implications of the findings?
#######################################################


#######################################################
## Report and communicate the results
## Write a sentence that gives the direction and extent of difference,
## and a measure of certainty / uncertainty in that finding.
## Make a beautiful graph that very clearly communicates the findings!
ggplot(data=???, aes(x=???, y=???)) +
  geom_boxplot() +
  geom_jitter(width= 0.1) +
  ylab("Reaction time (milliseconds)")
