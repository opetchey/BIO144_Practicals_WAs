## BIO144 Unit 1 practical: analysis of the class reaction time data
## VERSION WITH ERRORS. This script contains about ten deliberate errors.
## Your task is to find and fix them, so that the script runs from top to bottom.
## Most errors cause an error message. At least one does not: the code runs,
## but gives a wrong result. So check that each step does what the comment says.
## The errors are the kinds of mistakes everyone makes when writing code
## (typos, missing commas and brackets, and so on); you do not need to know
## dplyr or ggplot2 to fix them.
##
## The question: do the reaction times of male and female students differ?
##
## Before you start:
## - Open your BIO144 RStudio project (File > Open Project...), and save this
##   script in the project folder.
## - Lines starting with ## are comments: R ignores them. Read them!
## - Some of the code below uses functions from the dplyr, tidyr and ggplot2
##   packages. You will learn how they work in Unit 2. For now, read the
##   comments to see what each step does. You do not need to change these lines,
##   except to fix the errors.
## - Run the script one line (or one command) at a time, from the top, and check
##   what happens after each one.


## Load the add-on packages we need ------------------------------------------
## (If you get an error here, install the package first, e.g. with
## install.packages("tidyverse"), which installs all four. Then run the line again.)
library(readr)
library(dplyr)
library(ggplot2)


## Get the data ----------------------------------------------------------------
## Download the class data into your project folder.
## (You only need to run this line once.)
download.file("https://raw.githubusercontent.com/opetchey/BIO144_Practicals_WAs/refs/heads/main/assets/datasets/reaction_times_2027.csv",
              destfile = "reaction_times_2027.csv")

## Read the data file into R
class_RTs <- read_cvs("reaction_time_2027.csv")

## Have a look at the data. Does it look OK?
class_RT


## Tidy up the data ------------------------------------------------------------
## Give the variables short, simple names (be careful to get this right!)
names(class_RTs) <- c("RT1", "RT2", "RT3", "RT4", "RT5"
                      "Random_number",
                      "Sex_at_birth")

## Check the variable names are now what we set them to be
names(class-RTs)

## Check the variable types: RT1 to RT5 and Random_number should be numbers
## (<dbl>), and Sex_at_birth should be text (<chr>)
str(class_RTs)

## Add an identifier for each participant (ID-1, ID-2, ...)
class_RTs <- class_RTs |>
  mutate(ID = paste0("ID-", row_number))

## How many participants are there of each sex at birth?
class_RTs |>
  group_by(sex_at_birth) |>
  summarise(number = n())

## Rearrange the data so that each reaction time is on its own row
## ("long" format: one row per participant per trial)
RTs_long <- class_RTs |>
  pivot_longer(cols = starts_with("RT"),
               names_to = "Trial",
               values_to = "RT_value")

## Calculate the mean of the five reaction times of each participant
mean_RTs <- RTs_long |>
  group_by(ID, Sex_at_birth) |>
  summarise(mean_RT = mean(RT_value), .groups = "drop")


## Look at the data in graphs ----------------------------------------------------
## Histogram of the mean reaction times
ggplot(data = mean_RTs, aes(x = mean_RT)) +
  geom_histogram()

## One histogram for each sex at birth
ggplot(data = mean_RTs, aes(x = mean_RT)) +
  geom_histogram() +
  facet_grid(~ Sex_at_birth)

## Box plot with the data points on top
ggplot(data = mean_RTs, aes(x = Sex_at_birth, y = mean_RT)) +
  geom_boxplot() +
  geom_jitter(width = 0.05)


## Remove implausible values ----------------------------------------------------
## Keep only mean reaction times greater than 50 ms and less than 500 ms
## (faster than 50 ms or slower than 500 ms is probably a mistake in measuring
## or recording)
mean_RTs_filtered <- mean_RTs |>
  filter(mean_RT < 50, mean_RT > 500)

## How many participants are left? (It should be most of them!)
nrow(mean_RTs_filtered


## The statistical test -----------------------------------------------------------
## A two-sample t-test of mean reaction time between the sexes at birth.
## (You met t-tests in previous courses; the lecture used the same test.)
RT_ttest <- t.test(mean_RT ~ Sex_at_birth,
                   data = mean_RTs_filtered,
                   var.equal = TRUE)

## Look at the result of the t-test
t.test


## Last step: check the script is reproducible ------------------------------------
## In RStudio: Session > Restart R. Then run the whole script from the top
## (Code > Run Region > Run All). If it runs without errors from top to bottom,
## it will give the same results every time, for you and for anyone else.
