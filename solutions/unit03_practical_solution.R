# =============================================================================
# BIO144 Data Analysis in Biology
# Unit 3 practical: example solution
# Course book chapter: Regression Part 1
# https://opetchey.github.io/BIO144_Course_Book/3.1-regression-part1.html
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
# =============================================================================


# Load the packages ----
library(tidyverse)  # readr, dplyr, tidyr, ggplot2 and more
library(ggfortify)  # autoplot() for model checking plots
library(patchwork)  # put several ggplots side by side


# Part 1, step 1: Read the data into R ----
fh_data <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Practicals_WAs/refs/heads/main/assets/datasets/financing_healthcare.csv")
# You will see a message about a new column name "...1" (the first column has
# no name in the file) and a warning about "parsing issues". The parsing
# issues are in the column health_insurance, which we do not use, so we can
# ignore them here. (Look with problems(fh_data) if you are curious.)

# Look at the data.
fh_data
glimpse(fh_data)
# Check: one row per country per year, with many variables and many NAs.


# Part 1, step 2: Wrangle and quality control the data ----
# Our question: in 2013, is there a relationship between health care spending
# of a country and child mortality in that country?
# So we keep only 2013, keep only the columns we need, and THEN remove rows
# with missing values. If we removed rows with NA before select(), we would
# also lose countries with NAs in variables we do not need.
fh_2013 <- fh_data |>
  filter(year == 2013) |>
  select(country, health_exp_total, child_mort) |>
  drop_na()

# How many countries (data points) do we have?
nrow(fh_2013)
# Answer: 186 countries. (If you also keep life_expectancy, drop_na() removes
# 8 more countries and you get 178: that is why the hint says "not 178".)

# Quality control: are the values sensible?
summary(fh_2013)
# Check: spending is between about 19 and 6900 USD per person per year, and
# child mortality between about 2 and 170 per 1000 live births. No negative
# or impossible values. Each country appears only once:
n_distinct(fh_2013$country)


# Part 1, step 3: Visualise the data, with the question in mind ----
# The response (child mortality) goes on the y-axis.
ggplot(fh_2013, aes(x = health_exp_total, y = child_mort)) +
  geom_point() +
  labs(x = "Health care spending (total, per capita, USD)",
       y = "Child mortality (per 1000 live births)")
# Answer (biggest problem): the relationship does not seem to be linear. It
# is strongly curved: child mortality falls steeply at low spending, then
# levels off. A straight line would fit badly.

# Look at the distribution of each variable.
ggplot(fh_2013, aes(x = health_exp_total)) +
  geom_histogram(bins = 30)
ggplot(fh_2013, aes(x = child_mort)) +
  geom_histogram(bins = 30)
# Both are right-skewed: many small values and a few large ones.
# Answer: a log transformation spreads out small values and compresses large
# ones, so it can make right-skewed variables more nearly normal.


# Part 1, step 4: Log10-transform both variables ----
fh_2013 <- fh_2013 |>
  mutate(log_health_exp_total = log10(health_exp_total),
         log_child_mort = log10(child_mort))

ggplot(fh_2013, aes(x = log_health_exp_total, y = log_child_mort)) +
  geom_point() +
  labs(x = "log10(Health care spending, USD per capita)",
       y = "log10(Child mortality, per 1000 live births)")
# Answer: Yes, the relationship now looks much more linear.

# A log10 value of 2.0 corresponds to a raw value of 10^2:
10^2
# Answer: 100. (log10 values are easy to translate: 1 = 10, 2 = 100, 3 = 1000.)


# Part 1, step 5: Guess the intercept and slope ----
# Extend the axes so the x-axis reaches zero, to see where a line would cut
# the y-axis.
ggplot(fh_2013, aes(x = log_health_exp_total, y = log_child_mort)) +
  geom_point() +
  xlim(0, 5) +
  ylim(0, 4) +
  labs(x = "log10(Health care spending, USD per capita)",
       y = "log10(Child mortality, per 1000 live births)")
# Answer (guessed intercept): about 3.25 (anything from about 2.5 to 4 is a
# reasonable guess). Place a straight edge on the screen through the middle of
# the points and read off where it crosses x = 0.
# Answer (guessed slope): about -0.75. For example, the line goes from about
# y = 2.0 at x = 1.7 to about y = 0.9 at x = 3.2: (0.9 - 2.0) / (3.2 - 1.7)
# = -1.1 / 1.5 = about -0.73. The slope is negative: more spending, lower
# child mortality.


# Part 1, step 6: Degrees of freedom for error ----
# Number of data points minus number of estimated coefficients (intercept and
# slope, so 2).
nrow(fh_2013) - 2
# Answer: 186 - 2 = 184 degrees of freedom for error.


# Part 1, step 7: Fit the regression model ----
# lm stands for "linear model". The formula is response ~ explanatory, and
# data = the data frame: lm(??1?? ~ ??2??, data = ??3??) with ??1?? = B
# (response), ??2?? = C (explanatory), ??3?? = A (data frame).
# The practical calls the model my_model. We give it a more meaningful name,
# m_child_mort, which is also the name used in the Unit 4 practical.
m_child_mort <- lm(log_child_mort ~ log_health_exp_total, data = fh_2013)
# We look at the results (summary) next week, in Unit 4. First we check the
# assumptions.


# Part 1, step 8: Check the assumptions: residuals and fitted values ----
# residuals() gives the residuals, fitted() gives the fitted values.
fh_2013 <- fh_2013 |>
  mutate(residuals = residuals(m_child_mort),
         fitted = fitted(m_child_mort))

# Histogram of the residuals.
ggplot(fh_2013, aes(x = residuals)) +
  geom_histogram(bins = 20)
# Answer: Yes, the residuals look reasonably symmetric and bell-shaped, so we
# can assume they are (approximately) normally distributed.

# Residuals against fitted values.
ggplot(fh_2013, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed")
# Answer: No, there is no clear pattern (no curve, no funnel shape). The
# points are scattered evenly around zero, so a straight line is a good
# description of the relationship on the log-log scale.

# All four model checking plots at once, with autoplot() (ggfortify):
autoplot(m_child_mort)
# Check: QQ-plot points close to the line; scale-location plot without a
# strong trend (roughly constant variance).


# Part 1, step 9: Influential observations ----
# Residuals vs leverage plot, which also shows Cook's distance.
autoplot(m_child_mort, which = 5)
# The five largest Cook's distances:
sort(cooks.distance(m_child_mort), decreasing = TRUE)[1:5]
# Which countries are these, and which country has the highest leverage?
fh_2013 |>
  mutate(cooks_d = cooks.distance(m_child_mort),
         leverage = hatvalues(m_child_mort)) |>
  arrange(desc(cooks_d)) |>
  select(country, health_exp_total, child_mort, cooks_d, leverage) |>
  head(5)
fh_2013$country[which.max(hatvalues(m_child_mort))]
# Answer: the largest Cook's distance is about 0.06 (Eritrea). The highest
# leverage is the Central African Republic (lowest spending). Correct
# statements: (1) leverage measures how unusual the x value is; (2) Cook's
# distance measures how much the fitted line changes if that point is
# removed, and combines leverage and residual size; (3) no observation is
# highly influential here, as all Cook's distances are far below 0.5 or 1.
# We should NOT remove the Central African Republic just because it has high
# leverage.

# Check how much the line changes without the most influential point:
m_child_mort_no_eritrea <- lm(log_child_mort ~ log_health_exp_total,
                              data = filter(fh_2013, country != "Eritrea"))
coef(m_child_mort)
coef(m_child_mort_no_eritrea)
# Look at: the slope changes only from -0.757 to -0.767. Very little.


# Part 1, step 10: Try the model on the untransformed data ----
# The practical asks you to try this, to see what badly violated assumptions
# look like.
m_child_mort_raw <- lm(child_mort ~ health_exp_total, data = fh_2013)

# Residuals vs fitted values, and QQ-plot, side by side (patchwork).
p_raw_resid <- ggplot(mapping = aes(x = fitted(m_child_mort_raw),
                                    y = residuals(m_child_mort_raw))) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals",
       title = "Residuals vs Fitted\n(Untransformed Data)")
p_raw_qq <- ggplot(mapping = aes(sample = residuals(m_child_mort_raw))) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ-Plot of Residuals\n(Untransformed Data)")
p_raw_resid + p_raw_qq
# Look at: a strong curved pattern in the residuals vs fitted values, and
# points far from the line in the QQ-plot (right-skewed residuals). The
# assumptions are badly violated, so the log-log model is much better.

# The histogram of these residuals is also clearly not normal:
ggplot(mapping = aes(x = residuals(m_child_mort_raw))) +
  geom_histogram(bins = 20)


# Part 1, step 11: Potential sources of non-independence ----
# There is no code for this. Model answer:
# Each data point is a country. Countries may not be independent because
# (1) neighbouring countries share climate, diseases, trade and history;
# (2) countries with the same political or economic system may be similar;
# (3) data for some groups of countries may have been collected in a
# different way. Such things can make the residuals of similar countries
# similar. We ignore this for now; mixed models (Unit 11) are one way to deal
# with non-independence. You could check, for example, whether the residuals
# differ among continents.


# Part 2, step 1: Normally distributed "residuals" ----
# Fun with QQ-plots. We make synthetic data, so we know the true distribution.
# set.seed() makes the random numbers the same every time you run the script.
# Remove it (or change the number) to see how much the plots vary by chance.
set.seed(144)
resids <- rnorm(30, 0, 1)

# Distribution of these residuals, with ggplot.
ggplot(mapping = aes(x = resids)) +
  geom_histogram(bins = 10)

# The QQ-plot, and the line where the points should lie if the data are
# normally distributed.
qqnorm(resids)
qqline(resids)
# Look at: with only 30 values, the histogram may not look very normal, but
# the QQ-plot points are close to the line.


# Part 2, step 2: Histograms and QQ-plots for many distributions ----
# We make 100 random numbers from each distribution. The parameter values are
# our choice; try others.
n_values <- 100
sim_data <- tibble(
  normal = rnorm(n_values, mean = 0, sd = 1),
  uniform = runif(n_values, min = 0, max = 1),
  left_skewed = rbeta(n_values, 5, 2),
  right_skewed = rbeta(n_values, 2, 5),
  log_normal = rlnorm(n_values, meanlog = 0, sdlog = 1),
  poisson = rpois(n_values, lambda = 3),
  binomial = rbinom(n_values, size = 10, prob = 0.5)
)

# A small function that makes the pair of plots for one distribution:
# histogram on the left, QQ-plot on the right. (This saves us writing the same
# code seven times. You can also just copy and paste the code instead.)
plot_hist_qq <- function(values, name) {
  p_hist <- ggplot(mapping = aes(x = values)) +
    geom_histogram(bins = 20) +
    labs(x = "Value", title = paste("Histogram:", name))
  p_qq <- ggplot(mapping = aes(sample = values)) +
    stat_qq() +
    stat_qq_line() +
    labs(title = paste("QQ-plot:", name))
  p_hist + p_qq
}

plot_hist_qq(sim_data$normal, "normal")
# Points close to the line.
plot_hist_qq(sim_data$uniform, "uniform")
# S-shape: points above the line at the low end and below it at the high end
# (the tails are too short: no extreme values).
plot_hist_qq(sim_data$left_skewed, "left-skewed, rbeta(100, 5, 2)")
# Curve bending downwards: points below the line at both ends (a long tail
# of small values).
plot_hist_qq(sim_data$right_skewed, "right-skewed, rbeta(100, 2, 5)")
# Curve bending upwards: points above the line at both ends (a long tail of
# large values).
plot_hist_qq(sim_data$log_normal, "log-normal")
# A strongly right-skewed distribution: a strong upward curve.
plot_hist_qq(sim_data$poisson, "Poisson, lambda = 3")
plot_hist_qq(sim_data$binomial, "binomial, size = 10, prob = 0.5")
# Poisson and binomial values are whole numbers (discrete), so the QQ-plot
# shows horizontal steps.

# To put the pairs side by side in a Word document, you can save each pair
# with ggsave(), e.g.
# ggsave("qq_uniform.png", plot_hist_qq(sim_data$uniform, "uniform"),
#        width = 8, height = 4)


# Part 2, step 3: The QQ-plot quiz ----
# Here is the code that made the six QQ-plots in the practical (with a seed,
# so they are the same each time).
set.seed(3)
resids_poisson <- rpois(1000, lambda = 3)
qqnorm(resids_poisson, main = "QQ-Plot 1")
qqline(resids_poisson)
resids_binomial <- rbinom(1000, size = 10, prob = 0.5)
qqnorm(resids_binomial, main = "QQ-Plot 2")
qqline(resids_binomial)
resids_left_skewed <- rbeta(1000, 5, 2)
qqnorm(resids_left_skewed, main = "QQ-Plot 3")
qqline(resids_left_skewed)
resids_right_skewed <- rbeta(1000, 2, 5)
qqnorm(resids_right_skewed, main = "QQ-Plot 4")
qqline(resids_right_skewed)
resids_normal <- rnorm(1000, 0, 1)
qqnorm(resids_normal, main = "QQ-Plot 5")
qqline(resids_normal)
resids_uniform <- runif(1000, min = 0, max = 1)
qqnorm(resids_uniform, main = "QQ-Plot 6")
qqline(resids_uniform)
# Answer (uniform): 6. A symmetric S-shape: flat in the middle, with the ends
# bending back towards the line, because there are no extreme values.
# Answer (left-skewed): 3. Points below the line at both ends.
# Answer (Poisson or binomial): 1 or 2. Discrete values give horizontal steps.
# Answer (right-skewed): 4. Points above the line at both ends.
# (Plot 5 is the normal distribution: points on the line.)


# Check that the script is reproducible ----
# When you have finished: click Session > Restart R, then run the whole
# script again from the top (Ctrl+Shift+Enter, or Cmd+Shift+Enter on a Mac).
# It should run without errors and give the same answers. If it does not,
# something in your script depends on something you did outside the script.
