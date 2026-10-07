# =============================================================================
# BIO144 Data Analysis in Biology
# Unit 2 practical: example solution
#
# This is ONE way to solve the practical. It is not the only way: other code
# that gives the same answers is just as good. You will learn much more if you
# solve the practical yourself first (with other students, if you can), and
# only then compare your solution with this one.
#
# How to use this script
# - Save it in your BIO144 RStudio project folder and open it in RStudio.
# - Run it one line (or one command) at a time, from the top.
# - Lines starting with # are comments. Read them: they explain each step and
#   point out what to look at in the output.
# - The datasets are read directly from the internet, so you need to be online.
#
# The practical asks you to make a separate script for Part 2 and Part 3.
# Here both are in one script, in two sections.
# The R code used here is explained in Chapter 2 (R and RStudio) of the
# course book, and in the "Practical toolbox" section of its walkthrough script.
# =============================================================================


# Load the packages ----
# tidyverse loads readr, dplyr, tidyr, ggplot2 and more.
library(tidyverse)
library(GGally)   # for ggpairs() (install it once: install.packages("GGally"))


# Practical part 1: questions about dplyr and ggplot2 ----
# Part 1 has no R code: it checks what you learned in the course book and
# lecture. The answers:
# - Add a new column of transformed data: mutate().
# - Get specific columns: select().
# - Get particular rows by position (e.g. rows 1 to 10): slice().
# - Get rows that meet a condition (e.g. values less than 10): filter().
# - Sort the rows into an order: arrange().
# - First argument of group_by(): the data frame. (With a pipe |> you do not
#   see it, but it is still there.)
# - Second argument of group_by(): the variable with the grouping information.
# - Which summarise() works: summarise(my_grouped_data, mean = mean(height)).
#   Data frame first, then new_name = summary_function(variable), with ONE =.
# - Function inside ggplot() that maps variables to x, y, colour: aes().
# - First argument of ggplot(): the data frame.
# - Facets: multiple graphs (panels) in one figure, one for each group.


# Practical part 2: the body fat dataset ----

# Part 2, preliminaries: import the data ----
# bodyfat.txt is tab-separated (not comma-separated), so we use read_delim()
# with delim = "\t" (tab), not read_csv().
bodyfat <- read_delim("https://raw.githubusercontent.com/opetchey/BIO144_Practicals_WAs/refs/heads/main/assets/datasets/bodyfat.txt",
                      delim = "\t")

# Check the import: 252 rows (people) and 19 columns (variables)?
dim(bodyfat)
glimpse(bodyfat)
# Look at: 252 rows and 19 columns, and all variables are numbers <dbl>.
# (If you get only ONE column, the delimiter is wrong.)
# Note: the data frame and its response variable are both called bodyfat.
# That works, but be careful which one you mean.


# Part 2, step 1: Distribution of the response variable (bodyfat) ----
ggplot(bodyfat, aes(x = bodyfat)) +
  geom_histogram(bins = 30) +
  labs(x = "Body fat (%)", y = "Number of people")
# Answer (normally distributed?): "Its quite normally distributed, but there
# are a couple of rather high values." The histogram is roughly symmetric and
# bell-shaped, with a few values on the right (around 40 to 48).
# Look also at the left: one person has body fat 0, which is not possible for
# a living human. This could be a mistake in the data, worth checking.

# Answer (mean and median?): "The mean and median are very similar", because
# the distribution is roughly symmetric. We can check this (after answering):
bodyfat |>
  summarise(mean_bodyfat = mean(bodyfat),
            median_bodyfat = median(bodyfat))


# Part 2, step 2: Individuals with bodyfat greater than 35 ----
bodyfat |>
  filter(bodyfat > 35)
# Count them:
bodyfat |>
  filter(bodyfat > 35) |>
  nrow()
# Answer: 4 individuals have body fat greater than 35.
# Remember: R is case sensitive. filter(bodyfat, Bodyfat > 35) gives the
# error "object 'Bodyfat' not found". Check names with names(bodyfat).
# And in ggplot2 the layers are added with +, not with |>.


# Part 2, step 3: bmi of the individual with the highest bodyfat ----
# Sort by bodyfat from highest to lowest, and keep the first row.
bodyfat |>
  arrange(desc(bodyfat)) |>
  slice(1) |>
  select(Nr, bodyfat, bmi)
# The same, with filter():
bodyfat |>
  filter(bodyfat == max(bodyfat)) |>
  select(Nr, bodyfat, bmi)
# Answer: 37.62 (person Nr 216, body fat 47.5).
# (You can also use View(bodyfat) and click a column name to sort it.)


# Part 2, step 4: Distributions of all the other variables ----
# Quick way: make the data "long" (one row per person per variable), then
# make one histogram per variable with facet_wrap().
# Nr is just an ID number, so we leave it out.
bodyfat |>
  select(-Nr) |>
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value") |>
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ variable, scales = "free")
# Answer (at least one rather extreme value): hip, ankle and height.
# - hip: one value far above the others (about 148).
# - ankle: two values far above the others (about 34).
# - height: one value far below the others (29.5, in inches: about 75 cm!).
# wrist, forearm and density have no value far away from the rest.
# (weight, bmi, and others such as neck and abdomen also have extreme values.)


# Part 2, step 5: Is it the same individuals that have the extreme values? ----
# The "old fashioned" way: for each variable, look at the most extreme rows
# and write down the Nr. slice_max() gives the rows with the largest values,
# slice_min() the rows with the smallest values.
bodyfat |> slice_max(hip, n = 3) |> select(Nr, hip)
bodyfat |> slice_max(ankle, n = 3) |> select(Nr, ankle)
bodyfat |> slice_min(height, n = 3) |> select(Nr, height)
bodyfat |> slice_max(weight, n = 3) |> select(Nr, weight)
bodyfat |> slice_max(bmi, n = 3) |> select(Nr, bmi)
# Look at:
# - Nr 39 has the highest hip AND the highest weight (363 pounds).
# - Nr 31 and Nr 86 have the two very large ankle values.
# - Nr 42 has the very small height, and so a very large bmi (166!), because
#   bmi is calculated from height.
# So it is partly the same individual (39) and partly different ones.

# Optional: a coded way (see "Finding observations with extreme values" in
# Chapter 2 of the course book). Calculate a z-score for every value
# (how many standard deviations from the mean of that variable), and find the
# people with very large z-scores. Here we use |z| > 4. With |z| > 3 (as in the
# book) you get a few more people, with less extreme values.
bodyfat |>
  pivot_longer(cols = !Nr,
               names_to = "var_name",
               values_to = "var_value") |>
  group_by(var_name) |>
  mutate(z_val = (var_value - mean(var_value)) / sd(var_value)) |>
  filter(z_val < -4 | z_val > 4) |>
  pull(Nr) |>
  unique()


# Part 2, step 6: Duplicated variables ----
# weight (pounds) and gewicht (German for weight, in kg), and
# height (inches) and hoehe (German for height, in cm) measure the same things.
# If they are duplicates, the ratio is the same for every person:
bodyfat |>
  mutate(kg_per_pound = gewicht / weight,
         cm_per_inch = hoehe / height) |>
  summarise(min_kg_per_pound = min(kg_per_pound),
            max_kg_per_pound = max(kg_per_pound),
            min_cm_per_inch = min(cm_per_inch),
            max_cm_per_inch = max(cm_per_inch))
# Look at: about 0.454 kg per pound and 2.54 cm per inch, for everyone.
# A graph shows it too: all points lie exactly on a straight line.
ggplot(bodyfat, aes(x = weight, y = gewicht)) +
  geom_point() +
  labs(x = "Weight (pounds)", y = "Gewicht (kg)")
# Answer: 2 pairs of duplicates (weight and gewicht; height and hoehe).
# (bodyfat and density are also very strongly related, because body fat was
# calculated from body density. But they are not the same measure in
# different units, so they are not duplicates.)


# Part 2, step 7: Calculate bmi ourselves ----
# Formula: bmi = weight in kg / (height in m)^2.
# The units matter! weight and height are in pounds and inches, so we use
# gewicht (kg) and hoehe (cm, divided by 100 to get m).
bodyfat <- bodyfat |>
  mutate(my_bmi = gewicht / (hoehe / 100)^2)
# Compare with the bmi in the dataset:
bodyfat |>
  select(Nr, gewicht, hoehe, bmi, my_bmi) |>
  head()
ggplot(bodyfat, aes(x = bmi, y = my_bmi)) +
  geom_point() +
  labs(x = "bmi in the dataset", y = "bmi calculated in R")
# Look at: the values are the same (except for small rounding differences),
# so the bmi in the dataset was calculated from kg and m.
# (With pounds and inches the formula would be 703 * weight / height^2.)

# Answer (calculate in R or elsewhere?): do such calculations in R. Then they
# are documented in the script, can be checked by others, and are easy to redo.


# Part 2, step 8: Remove individuals with extreme values ----
# Answer (which individuals?): 31, 39, 42, 86 (from step 5).
bodyfat_c <- bodyfat |>
  filter(!(Nr %in% c(31, 39, 42, 86)))
# Check: 4 fewer rows than before (252 - 4 = 248).
nrow(bodyfat_c)


# Part 2, step 9: Relationships among all the variables ----
# ggpairs() makes a scatterplot for every pair of variables (lower triangle),
# the distribution of each variable (diagonal) and the correlation
# coefficients (upper triangle). It can take a while! Look at it on a big
# screen (click "Zoom" in the Plots pane).
# We leave out Nr (an ID) and my_bmi (the same as bmi).
bodyfat_c |>
  select(-Nr, -my_bmi) |>
  ggpairs()
# Focus on the column of graphs with bodyfat at the top.
# The correlations with bodyfat as numbers, from strongest to weakest:
bodyfat_c |>
  select(-Nr, -my_bmi) |>
  cor() |>
  as_tibble(rownames = "variable") |>
  select(variable, bodyfat) |>
  arrange(desc(abs(bodyfat)))
# Look at: density (very strong, but bodyfat was calculated from it, and it is
# hard to measure), then abdomen, bmi, chest, hip, weight/gewicht.
# height, age and ankle are only weakly related to bodyfat.

# Answer (good predictors of body fat): abdomen and weight. Both are easy to
# measure, and are strongly positively related to body fat. height and age are
# only weakly related to body fat.


# Practical part 3: the healthcare financing dataset ----

# Part 3, preliminaries: import the data ----
# read_csv() guesses the type of each variable from the first 1000 rows. In
# this dataset the first 1000 rows of health_insurance are all NA, so
# read_csv() guesses the wrong type (logical) and gives a warning about
# "parsing issues". guess_max = 40000 tells it to look at all rows first.
healthcare <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Practicals_WAs/refs/heads/main/assets/datasets/financing_healthcare.csv",
                       guess_max = 40000)
# The message "New names: `` -> `...1`" means the first column had no name.
# It is just the row number from when the file was saved.

# Check: 36873 rows and 18 variables (including ...1)?
dim(healthcare)
glimpse(healthcare)


# Part 3, step 1: How many countries and years? ----
# n_distinct() counts the different values; length(unique()) does the same.
healthcare |>
  summarise(n_countries = n_distinct(country),
            n_countries_again = length(unique(country)),
            n_years = n_distinct(year))
# Answer: 319 countries (some are regions or territories, not countries).
# Answer: 255 years (from 1761 to 2015, but not every year in between).


# Part 3, step 2: Rows with values for both health_exp_total and child_mort ----
healthcare |>
  filter(!is.na(health_exp_total) & !is.na(child_mort)) |>
  nrow()
# Answer: 3510 rows. Most rows have missing values (NA) for these variables.


# Part 3, step 3: Make a smaller dataset for 2013 ----
child_mort_2013 <- healthcare |>
  filter(year == 2013) |>
  select(year, country, continent, health_exp_total, child_mort, life_expectancy) |>
  drop_na()
# Check: 178 rows and 6 variables?
dim(child_mort_2013)


# Part 3, step 4: Mean and standard deviation of child mortality per continent ----
child_mort_by_continent <- child_mort_2013 |>
  group_by(continent) |>
  summarise(mean_child_mort = mean(child_mort),
            sd_child_mort = sd(child_mort),
            n_countries = n())
child_mort_by_continent
# A tibble prints only a few digits. To see more, before rounding:
child_mort_by_continent |>
  filter(continent == "Africa") |>
  pull(mean_child_mort)
# Answer: 75.5 deaths before the age of five per 1000 births in Africa in 2013
# (75.469..., rounded to one decimal place).


# Part 3, step 5: Box and whisker plot of child mortality per continent ----
# Use the 178 countries, not the means.
ggplot(child_mort_2013, aes(x = continent, y = child_mort)) +
  geom_boxplot() +
  labs(x = "Continent", y = "Child mortality (deaths before age 5 per 1000 births)")
# Answer: two statements are correct:
# - The values are rather non-normally distributed, with some rare and extreme
#   HIGH values (the points above the boxes, and long upper whiskers).
# - The variability among countries is larger in some continents than in
#   others (e.g. large in Africa, small in Europe).


# Part 3, step 6: Scatterplots of child mortality against health expenditure ----
# Points coloured by continent. ggplot adds the key (legend) automatically.
ggplot(child_mort_2013, aes(x = health_exp_total, y = child_mort, colour = continent)) +
  geom_point() +
  labs(x = "Total health expenditure per person",
       y = "Child mortality (per 1000 births)",
       colour = "Continent")

# One panel (facet) per continent, all with the same axis limits (the default).
ggplot(child_mort_2013, aes(x = health_exp_total, y = child_mort)) +
  geom_point() +
  facet_wrap(~ continent) +
  labs(x = "Total health expenditure per person",
       y = "Child mortality (per 1000 births)")

# The same, with the axis limits free to vary among panels (see ?facet_wrap:
# the argument scales = "free").
ggplot(child_mort_2013, aes(x = health_exp_total, y = child_mort)) +
  geom_point() +
  facet_wrap(~ continent, scales = "free") +
  labs(x = "Total health expenditure per person",
       y = "Child mortality (per 1000 births)")
# Answer: two statements are the best:
# - In Europe child mortality is very low in all countries (2 to 15 per 1000),
#   so there is little relationship with health spending, compared with the
#   other continents. (Careful: with free axes the Europe panel is zoomed in,
#   so a small decrease can look large. Always read the axis values.)
# - In Africa child mortality is negatively related to health spending, but
#   with a lot of scatter.
# In Asia there is a clear negative relationship, and in the Americas it is
# negative, not positive.


# Check that the script is reproducible ----
# (This is also the last task of the practical.)
# Session > Restart R, then run the whole script from the top
# (Code > Run Region > Run All). It should run without errors and give the
# same results as before. If not, perhaps a line was run in the console but
# is missing from the script, or lines are in the wrong order.
# Also check the object names: names such as bodyfat_c and child_mort_2013
# say what the object contains; x2 or data_new do not.
