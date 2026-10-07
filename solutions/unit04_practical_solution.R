# =============================================================================
# BIO144 Data Analysis in Biology
# Unit 4 practical: example solution
# Course book chapter: Regression Part 2
# https://opetchey.github.io/BIO144_Course_Book/4.1-regression-part2.html
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


# Part 1, step 1: Last week's script: read, wrangle, fit, check ----
# This repeats the Unit 3 practical, in short. See the Unit 3 solution for
# the explanations.
fh_data <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Practicals_WAs/refs/heads/main/assets/datasets/financing_healthcare.csv")
# (Ignore the warning about parsing issues: it is about the column
# health_insurance, which we do not use.)

# Keep 2013, the variables we need, and remove rows with missing values.
# We also keep continent, for the graph later. This does not change the
# number of countries: all 186 have a continent.
fh_2013 <- fh_data |>
  filter(year == 2013) |>
  select(country, continent, health_exp_total, child_mort) |>
  drop_na() |>
  mutate(log_health_exp_total = log10(health_exp_total),
         log_child_mort = log10(child_mort))
nrow(fh_2013)
# Check: 186 countries.

# Fit the model on the log10-log10 scale, and check the assumptions.
m_child_mort <- lm(log_child_mort ~ log_health_exp_total, data = fh_2013)
autoplot(m_child_mort)
# Check: no worrying patterns (as in Unit 3).


# Part 1, step 2: Write down your guesses ----
# Before looking at the results, write down guesses. Ours (from the graph in
# Unit 3): intercept about 3.25, slope about -0.75, the slope is very clearly
# different from zero (so p will be very small), and 186 - 2 = 184 degrees
# of freedom for error.


# Part 1, step 3: Interpreting regression results with summary() ----
summary(m_child_mort)
# Look at the "Coefficients" table:
#  - "(Intercept)" row, "Estimate" column.
# Answer (intercept): 3.29.
#  - "log_health_exp_total" row, "Estimate" column.
# Answer (slope): -0.76.
#  - "Multiple R-squared" near the bottom.
# Answer (fraction of variation explained): 0.76 (76%).
#  - The slope's p-value is "< 2e-16": tiny. And the slope is 24 standard
#    errors away from zero.
# Answer (is a slope of zero not compatible with the data?): yes. A slope of
# zero is not compatible with the data.
#  - "Residual standard error: 0.2443 on 184 degrees of freedom".
# Answer (degrees of freedom for error): 184.
# Checking back: these are all very close to our guesses. Good: the cross
# check found no mistakes (for example, response and explanatory variables
# swapped in the lm() formula).

# You can also get the numbers directly, rounded:
round(coef(m_child_mort), 2)
round(summary(m_child_mort)$r.squared, 2)
df.residual(m_child_mort)


# Part 1, step 4: Where does the t-value come from? ----
# t = estimate / standard error. Take both from the coefficients table.
coef_table <- summary(m_child_mort)$coefficients
coef_table
slope_estimate <- coef_table["log_health_exp_total", "Estimate"]
slope_se <- coef_table["log_health_exp_total", "Std. Error"]
slope_estimate / slope_se
# Answer: -24.09 (the t value R reports). The minus sign only shows that the
# slope is negative. (With the rounded values, -0.7567 / 0.0314 = -24.10.)


# Part 1, step 5: Confidence interval for the slope ----
# Critical t-value for a 95% confidence interval with 184 degrees of freedom:
t_crit <- qt(0.975, df = 184)
t_crit
# Answer: 1.97 (close to 1.96, the value for the normal distribution).

# The 95% CI by hand: estimate +/- critical t-value * standard error.
slope_estimate - t_crit * slope_se
slope_estimate + t_crit * slope_se

# Easier and safer: let R do it.
confint(m_child_mort)
# Answer (lower bound of the 95% CI of the slope): -0.82. The CI is -0.82 to
# -0.69. Zero is far outside it.

# What if the data were from only 10 countries (8 degrees of freedom)?
qt(0.975, df = 8)
# Answer: the CI would be wider, because the critical t-value with 8 degrees
# of freedom (2.31) is larger than with 184 (1.97).


# Part 1, step 6: Correlation versus R squared ----
r_spend_mort <- cor(fh_2013$log_health_exp_total, fh_2013$log_child_mort)
r_spend_mort
# Answer: r = -0.87.
r_spend_mort^2
# Look at: r squared = 0.76 = the R-squared of the model.
# Answer (correct statements): R squared equals r squared in simple linear
# regression; r gives the direction (negative here) and R squared does not;
# R squared is the proportion of variation in log child mortality explained
# by log spending. A large R squared does NOT show causation, and r is NOT
# the slope (r = -0.87, slope = -0.76).


# Part 1, step 7: Predictions with confidence and prediction intervals ----
# The model works on the log10 scale, so we give it log10 values and then
# back-transform the predictions with 10^.
new_spending <- data.frame(log_health_exp_total = log10(c(100, 5000)))
pred_conf <- predict(m_child_mort, newdata = new_spending, interval = "confidence")
pred_pred <- predict(m_child_mort, newdata = new_spending, interval = "prediction")
10^pred_conf
10^pred_pred
# Rows: 1 = USD 100, 2 = USD 5000. Columns: fit, lower and upper bound.
# Answer (predicted child mortality at USD 100): 59.7 deaths per 1000 live
# births.
# Answer (lower bound of the 95% prediction interval at USD 100): 19.5.
# The 95% prediction interval (19.5 to 182.2) is much wider than the 95%
# confidence interval (52.7 to 67.5). The confidence interval is about the
# AVERAGE child mortality of countries spending USD 100; the prediction
# interval is about ONE new country, so it also includes the scatter of
# countries around the line.
# At USD 5000 the prediction is 3.1 per 1000 (95% PI 1.0 to 9.5).


# Part 1, step 8: Extrapolation ----
# The range of spending in the data, and the country with the highest value:
range(fh_2013$health_exp_total)
fh_2013 |>
  slice_max(health_exp_total, n = 1)
# The highest spending is about USD 6900 (United States).

# Predict for USD 20,000:
new_spending_20k <- data.frame(log_health_exp_total = log10(20000))
10^predict(m_child_mort, newdata = new_spending_20k, interval = "prediction")
# The model predicts about 1.1 deaths per 1000.
# Answer: we should trust this very little. USD 20,000 is far outside the
# range of the data (USD 19 to 6900), so we do not know if the relationship
# continues in the same way there (child mortality cannot fall below some
# minimum). And the data are observational, so the model does not tell us
# what would happen if a country changed its spending.


# Part 1, step 9: Reporting the results in words ----
# Answer (key components): describe the direction and size of the
# relationship, give a measure of uncertainty (e.g. a confidence interval),
# then give model context (e.g. sample size, R squared).
# Answer (pattern statement): ??1?? = D (linear), ??2?? = C (negative),
# ??3?? = B (1), ??4?? = A (-0.76).
# Answer (statistical test statement): "(linear regression, 184 degrees of
# freedom for error, t = 24.1, p < 0.0001, r-squared = 0.76)".

# Translate the slope into something easier to understand. In a log-log
# model, doubling x multiplies y by 2^slope.
2^coef(m_child_mort)["log_health_exp_total"]
# Percentage change in child mortality for a doubling of spending:
(2^coef(m_child_mort)["log_health_exp_total"] - 1) * 100
# And the same for the two ends of the confidence interval:
(2^confint(m_child_mort)["log_health_exp_total", ] - 1) * 100
# Look at: doubling spending is associated with about 41% lower child
# mortality (95% CI 38% to 43% lower).

# Model reporting sentence:
# Across 186 countries in 2013, child mortality was strongly and negatively
# related to health care spending. On log-log scales the slope was -0.76
# (95% confidence interval -0.82 to -0.69), meaning that a doubling of per
# capita health care spending was associated with about 41% lower child
# mortality (95% CI 38% to 43% lower) (linear regression, 184 degrees of
# freedom for error, t = 24.1, p < 0.0001, R squared = 0.76).


# Part 1, step 10: Reporting graphically ----
# A publication-quality graph on the log10 scales: regression line with a 95%
# confidence band, clear axis labels with units, readable text and points.
# Special distinction: points coloured by continent (with a key) and some
# countries labelled. We label only a few countries (the extremes), because
# labelling all 186 would be unreadable.
countries_to_label <- c("United States", "Central African Republic",
                        "Eritrea", "Switzerland", "Equatorial Guinea",
                        "Japan", "India")
fh_2013_labels <- fh_2013 |>
  filter(country %in% countries_to_label)

ggplot(fh_2013, aes(x = log_health_exp_total, y = log_child_mort)) +
  geom_smooth(formula = y ~ x, method = "lm", colour = "black") +
  geom_point(aes(colour = continent), size = 2, alpha = 0.8) +
  geom_text(data = fh_2013_labels, aes(label = country),
            size = 3, vjust = -0.9) +
  labs(x = "log10(Health care spending, USD per person per year)",
       y = "log10(Child mortality, deaths per 1000 live births)",
       colour = "Continent") +
  theme_bw(base_size = 14)
# Note: geom_smooth(method = "lm") fits the same model as m_child_mort, so the
# line and the grey band are the fitted line and its 95% confidence band.

# A variation that many readers find easier: the same graph with the raw
# values shown on log-spaced axes, using scale_x_log10() and scale_y_log10().
ggplot(fh_2013, aes(x = health_exp_total, y = child_mort)) +
  geom_smooth(formula = y ~ x, method = "lm", colour = "black") +
  geom_point(aes(colour = continent), size = 2, alpha = 0.8) +
  geom_text(data = fh_2013_labels, aes(label = country),
            size = 3, vjust = -0.9) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Health care spending (USD per person per year, log scale)",
       y = "Child mortality (deaths per 1000 live births, log scale)",
       colour = "Continent") +
  theme_bw(base_size = 14)


# Part 1, step 11: A linear-linear graph ----
# This is the code from the practical: predict on the log10 scale over the
# range of the data, back-transform with 10^, and plot on the raw axes.
new_data <- data.frame(
  health_exp_total = seq(min(fh_2013$health_exp_total),
                         max(fh_2013$health_exp_total),
                         length.out = 100)) |>
  mutate(log_health_exp_total = log10(health_exp_total))
preds <- predict(m_child_mort, newdata = new_data, interval = "confidence")
new_data <- new_data |>
  mutate(child_mort_fit = 10^preds[, "fit"],
         child_mort_lwr = 10^preds[, "lwr"],
         child_mort_upr = 10^preds[, "upr"])

ggplot() +
  geom_point(data = fh_2013, aes(x = health_exp_total, y = child_mort)) +
  geom_line(data = new_data, aes(x = health_exp_total, y = child_mort_fit),
            colour = "blue") +
  geom_ribbon(data = new_data, aes(x = health_exp_total,
                                   ymin = child_mort_lwr,
                                   ymax = child_mort_upr),
              alpha = 0.2) +
  labs(x = "Health Expenditure per Capita (USD)",
       y = "Child Mortality (deaths per 1,000 live births)",
       title = "Relationship between Health Expenditure and Child Mortality")
# Answer: when spending is relatively high (more than about USD 2000 per
# person per year), the relationship with child mortality is weak / shallow.
# A straight line on log-log axes is a curve on the raw axes: steep at low
# spending, nearly flat at high spending.


# Part 1, step 12: Policy implications and recommendations ----
# Should we recommend that countries spending more than about USD 2000 do not
# prioritise more health care spending? Model answer: be very cautious, and
# perhaps do not make this recommendation at all.
# Answer (most important reason): the data are observational, i.e. the
# result is a correlation. We cannot conclude that changing spending would
# change child mortality. Other things (e.g. wealth, education, clean water)
# vary together with spending and may be the real causes. Also, the absolute
# decrease in child mortality is small at high spending, but a small decrease
# can still mean many children's lives.


# Check that the script is reproducible ----
# When you have finished: click Session > Restart R, then run the whole
# script again from the top (Ctrl+Shift+Enter, or Cmd+Shift+Enter on a Mac).
# It should run without errors and give the same answers. If it does not,
# something in your script depends on something you did outside the script.
