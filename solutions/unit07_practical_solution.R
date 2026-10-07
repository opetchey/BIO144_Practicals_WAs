# =============================================================================
# BIO144 Data Analysis in Biology
# Unit 7 practical: example solution
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
# Theory: course book Chapter 7 (Interactions).
# =============================================================================


# Load the packages ----
library(tidyverse)  # read_csv(), dplyr functions, ggplot2
library(ggfortify)  # autoplot() for model diagnostic plots


# Part 1, step 0: The biological question ----
# Question: does the effect of herbivory on algal area depend on height in
# the intertidal?
# Answer (in statistical terms): is there an INTERACTION between tidal height
# and herbivory?
# Answer (experimental factors): height (low = below low tide, mid = middle
# of the intertidal) and herbivores (minus = fenced, plus = open to herbivores).
# Answer (response variable): area of algae remaining after a certain time.
# Answer (expected pattern if there is an interaction): the difference between
# fenced and unfenced plots is larger at one tidal height than at the other.
# Answer (design): a two-way fully factorial experiment, because two factors
# are manipulated and all combinations of their levels are included.
#
# Degrees of freedom, BEFORE fitting the model (64 plots):
# Answer (number of betas / group means, with interaction): 2 x 2 = 4.
# Answer (degrees of freedom for error): 64 - 4 = 60.
# Answer (ANOVA table): rows for height, herbivores, height:herbivores and
# Residuals.
# Answer (total df of the two main effects and the interaction): 1 + 1 + 1 = 3.


# Part 1, step 1: Read the data and check it ----
intertidal <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Practicals_WAs/refs/heads/main/assets/datasets/intertidalalgae.csv")
intertidal

# A first look at the data (the graph given in the practical):
ggplot(intertidal, aes(x = height, y = Area_cm2, colour = herbivores)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  labs(x = "Tidal height", y = "Algal area (cm2)", colour = "Herbivore access") +
  theme_classic()

# Number of plots in each treatment combination:
intertidal |>
  group_by(height, herbivores) |>
  summarise(n = n(), .groups = "drop")
# Look at: 16 in each of the 4 combinations, so the data are balanced.

# Does each treatment have only two levels?
intertidal |>
  distinct(height)
intertidal |>
  distinct(herbivores)
# Yes: low and mid; minus and plus.

# Any missing values? na.omit() removes rows with an NA in any column.
intertidal |>
  na.omit() |>
  group_by(height, herbivores) |>
  summarise(n = n(), .groups = "drop")
# The same numbers as before, so there are no NAs.

# Distribution of the response in each treatment combination:
ggplot(intertidal, aes(x = Area_cm2)) +
  geom_histogram(bins = 7, fill = "lightblue", colour = "black") +
  facet_grid(height ~ herbivores) +
  labs(x = "Algal area (cm2)", y = "Count")
# Answer (many values close to zero): with herbivores (plus), low height.
# A likely reason: herbivores (e.g. limpets) are abundant and active below
# low tide, where it is always wet, and eat almost all the algae. In the mid
# intertidal, conditions (drying, temperature) are harsh for herbivores, so
# they eat less, and algae survive whether or not the plot is fenced.

# Square root transform the response, and use it from now on.
intertidal <- intertidal |>
  mutate(Area_sqrt = sqrt(Area_cm2))


# Part 1, step 2: Visualise the answer to the question ----
# Height on the x axis, herbivores as colour. position_jitterdodge() jitters
# the points a little AND moves ("dodges") the two herbivore treatments apart.
ggplot(intertidal, aes(x = height, y = Area_sqrt, colour = herbivores)) +
  geom_boxplot(position = position_dodge(width = 0.75), outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75),
             alpha = 0.6) +
  labs(x = "Tidal height", y = "Square root of algal area (cm)",
       colour = "Herbivore access")
# (outlier.shape = NA stops the boxplot drawing outliers a second time,
# because we already show all the points.)
# Look at: at low height, much less algae with herbivores; at mid height,
# about the same with and without herbivores. Lines joining the means would
# NOT be parallel: the effect of herbivores depends on height (interaction).

# Mean and standard deviation of each treatment combination:
algae_summary <- intertidal |>
  group_by(height, herbivores) |>
  summarise(mean = mean(Area_sqrt),
            sd = sd(Area_sqrt),
            .groups = "drop")
algae_summary
# A tibble prints only 3 significant digits. To see two decimal places,
# round and print as a data frame:
algae_summary |>
  mutate(across(c(mean, sd), \(x) round(x, 2))) |>
  as.data.frame()
# Answer (mean, no herbivores (minus), low height): 32.91.
# Answer (sd, with herbivores (plus), mid height): 15.56.


# Part 1, step 3: Fit the model ----
# Answer (two ways to write main effects plus interaction):
# y ~ x1 + x2 + x1:x2, or the shorthand y ~ x1 * x2.
m_algae <- lm(Area_sqrt ~ height * herbivores, data = intertidal)


# Part 1, step 4: Check the assumptions ----
autoplot(m_algae, smooth.colour = NA)
# (A warning about removed rows comes from smooth.colour = NA. Ignore it.)
# Model assessment of the five assumptions:
# 1. Linearity / correct model: with only categorical explanatory variables
#    and the interaction included, the model fits each group mean, so there
#    can be no pattern in Residuals vs Fitted. OK.
# 2. Normal residuals: the QQ plot is S-shaped, with the points at both ends
#    closer to zero than the line ("short tails"). Not perfect, but not bad.
# 3. Equal variance: the Scale-Location plot shows similar spread in the four
#    groups. Reasonably OK.
# 4. Independence: from the design (randomly assigned, separate plots) we
#    assume the plots are independent. The plots cannot show this.
# 5. No very influential points: with only categorical explanatory variables
#    and a balanced design, all points have the same leverage ("Constant
#    Leverage" plot), and no residual is extreme. OK.
# Overall: reasonably OK, but the residuals are not perfectly normal. With 16 replicates per group and a balanced design, the analysis
# is fairly robust to this.


# Part 1, step 5: Interpret the model ----
anova(m_algae)
# Check the degrees of freedom first: 1 for height, 1 for herbivores, 1 for
# the interaction, and 60 for Residuals. Exactly what we expected.
# Answer (if residual df are not as expected, and data are complete and
# balanced): the most likely reason is that the model was mis-specified,
# for example the interaction term was left out.

# Total sum of squares (about 18'500):
sum((intertidal$Area_sqrt - mean(intertidal$Area_sqrt))^2)
# Unadjusted R-squared = (sum of the model sums of squares) / total SS:
anova_algae <- anova(m_algae)
ss_model <- sum(anova_algae$`Sum Sq`[1:3])
ss_total <- sum(anova_algae$`Sum Sq`)
ss_model / ss_total
# Check with summary():
summary(m_algae)
# Answer (unadjusted R-squared): 0.23 (= 4218 / 18489).

# Look at the F and p values in the ANOVA table:
# height: F = 0.37, p = 0.54; herbivores: F = 6.36, p = 0.014;
# height:herbivores: F = 11.00, p = 0.0015.
# The interaction is significant, as we expected from the graph.
# Answer (what do the main effects mean when the interaction is significant?):
# they are average effects across the levels of the other factor, and must be
# interpreted with caution. Here the "effect of herbivores" is an average of a
# large effect at low height and no effect at mid height.
# As in the practical, no post-hoc tests are needed: the graph and the
# interaction answer the question we asked.


# Part 1, step 6: A figure good for publication ----
# This figure (academic journal style) shows all the data (honest), and the
# model-estimated mean of each group with its 95% confidence interval
# (uncertainty). Lines join the means of each herbivore treatment, so the
# reader can see directly that the lines are not parallel (the interaction).
algae_means <- expand.grid(height = c("low", "mid"),
                           herbivores = c("minus", "plus"))
algae_pred <- predict(m_algae, newdata = algae_means, interval = "confidence")
algae_means <- cbind(algae_means, algae_pred)
algae_means

dodge <- position_dodge(width = 0.4)
ggplot(intertidal, aes(x = height, y = Area_sqrt, colour = herbivores)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.4),
             alpha = 0.35) +
  geom_line(data = algae_means, aes(y = fit, group = herbivores), position = dodge) +
  geom_pointrange(data = algae_means, aes(y = fit, ymin = lwr, ymax = upr),
                  position = dodge, size = 0.6) +
  scale_x_discrete(labels = c(low = "Below low tide", mid = "Mid intertidal")) +
  scale_colour_manual(values = c(minus = "darkgreen", plus = "darkorange"),
                      labels = c(minus = "Excluded (fenced)", plus = "Present (open)")) +
  labs(x = "Tidal height",
       y = expression(sqrt("Algal area (cm"^2*")")),
       colour = "Herbivores") +
  theme_classic(base_size = 14) +
  theme(legend.position = "top")
# Small points: individual plots. Large points and bars: model means with
# 95% confidence intervals. To make it reproducible, save it with ggsave()
# from the script, e.g. ggsave("algae_figure.pdf", width = 6, height = 5).


# Part 1, step 7: Reporting sentences ----
# Model answer:
# "We analysed the square root of algal area with a two-way ANOVA, with tidal
# height, herbivore access and their interaction as explanatory variables.
# The effect of herbivores depended on tidal height (interaction:
# F(1, 60) = 11.0, p = 0.0015). Below low tide, plots open to herbivores had
# much less algae than fenced plots (mean square-root area 10.4 vs 32.9 cm),
# whereas in the mid intertidal herbivore access made little difference
# (25.6 vs 22.5 cm). This suggests that herbivores strongly limit algae only
# low on the shore."
# Answer (why report the df of the treatment term and of error?): so that
# readers can check that they have understood the design and the analysis
# (e.g. 64 plots - 4 group means = 60 error df).


# Part 1, step 8: Critique and reflection ----
# Some points to discuss:
# - The response has many zeros (low, with herbivores), so the residuals are
#   not perfectly normal, even after the square root transformation.
# - Only two heights: we cannot say how the herbivore effect changes
#   gradually up the shore.
# - Fences might change other things than herbivore access (e.g. shading,
#   water flow). A "fence control" (e.g. a partial fence that herbivores can
#   pass) would help separate these.
# - A hypothesis and prediction should be written down BEFORE the experiment
#   (see the example in the practical), never after seeing the data.


# Part 2, step 0 and 1: Read the GDP and ruggedness data ----
# Question: is the relationship between GDP and terrain ruggedness different
# in Africa from the rest of the world?
# We keep the country name for now, because some questions ask about it.
countries_all <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Practicals_WAs/refs/heads/main/assets/datasets/rugged.csv")

# Replace the 0/1 code by words, and keep only the variables we need.
countries_all <- countries_all |>
  mutate(cont_africa1 = ifelse(cont_africa == 1, "Africa", "not Africa")) |>
  select(country, rugged, rgdppc_2000, cont_africa, cont_africa1)
countries_all

# How many countries are in Africa, and not in Africa?
table(countries_all$cont_africa1)
# Answer (in Africa): 57.
# Answer (not in Africa): 177. (234 countries in total.)


# Part 2, step 1: Distributions ----
ggplot(countries_all, aes(x = rugged)) +
  geom_histogram(bins = 30, fill = "lightblue", colour = "black") +
  labs(x = "Terrain Ruggedness Index", y = "Count")
# Answer (distribution of rugged): really (right) skewed: many low values,
# few high values.

# The flattest country (lowest ruggedness):
countries_all |>
  arrange(rugged) |>
  head(3)
# Answer: Tokelau (ruggedness 0).

# League table, most rugged first. Where is Switzerland?
countries_all |>
  arrange(desc(rugged)) |>
  mutate(rank = row_number()) |>
  filter(country == "Switzerland")
# Answer: 10th.

# Distribution of GDP per person:
ggplot(countries_all, aes(x = rgdppc_2000)) +
  geom_histogram(bins = 30, fill = "lightblue", colour = "black") +
  labs(x = "Real GDP per capita in 2000", y = "Count")
# Very right skewed. (R warns that rows with NA were removed: see below.)

# Missing values in GDP:
sum(is.na(countries_all$rgdppc_2000))
# Answer: 64.
countries_all |>
  group_by(cont_africa1) |>
  summarise(n_NA = sum(is.na(rgdppc_2000)))
# Answer (NAs in Africa): 8. Answer (NAs not in Africa): 56.


# Part 2, step 1: Remove NAs and log10 transform GDP ----
countries <- countries_all |>
  select(rugged, rgdppc_2000, cont_africa1) |>
  na.omit() |>
  mutate(rgdppc_2000_log10 = log10(rgdppc_2000))
nrow(countries)
# 170 rows, as the practical says.
ggplot(countries, aes(x = rgdppc_2000_log10)) +
  geom_histogram(bins = 20, fill = "lightblue", colour = "black") +
  labs(x = "log10 GDP per capita (2000)", y = "Count")
# Much less skewed.

# Degrees of freedom for error of the model rugged * cont_africa1:
# 4 parameters (intercept and slope for each of the two groups): 170 - 4.
# Answer: 166.


# Part 2, step 2: Visualise to answer the question ----
ggplot(countries, aes(x = rugged, y = rgdppc_2000_log10, colour = cont_africa1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Terrain Ruggedness Index", y = "log10 GDP per capita (2000)",
       colour = "Continent")
# Model answer: outside Africa, more rugged countries tend to have lower GDP
# (negative slope), but in Africa, more rugged countries tend to have slightly
# higher GDP (positive slope). Countries outside Africa have higher GDP in
# general. The slopes look different, so we guess the interaction is
# significant (but there is a lot of scatter).


# Part 2, step 3: Fit the model ----
# Different slopes in the two groups = interaction between rugged and cont_africa1.
m_gdp_rugged <- lm(rgdppc_2000_log10 ~ rugged * cont_africa1, data = countries)


# Part 2, step 4: Check the assumptions ----
autoplot(m_gdp_rugged, smooth.colour = NA)
# Look at: no strong pattern in Residuals vs Fitted; QQ plot reasonably close
# to the line (slightly short tails); similar spread across fitted values;
# a few points with higher leverage (the very rugged countries), but none
# with a large residual too. The assumptions are reasonably well met.


# Part 2, step 5: Interpret the model: ANOVA table ----
anova(m_gdp_rugged)
# Degrees of freedom: 1 (rugged), 1 (cont_africa1), 1 (interaction), and
# 166 for Residuals, as we expected.
# The interaction is significant (F = 8.93, df = 1 and 166, p = 0.003): the
# relationship between GDP and ruggedness differs between Africa and the
# rest of the world. Yes, Africa seems special in this respect.


# Part 2, step 5: Interpret the coefficients ----
summary(m_gdp_rugged)
# "Africa" comes first in the alphabet, so it is the reference level.
# Answer (slope in Africa): 0.083 (the coefficient called "rugged").

# Slope outside Africa = slope in Africa + difference in slope:
coefs <- coef(m_gdp_rugged)
coefs
coefs["rugged"] + coefs["rugged:cont_africa1not Africa"]
# Answer (slope not in Africa): -0.088 (= 0.083 + (-0.171)).

# Answer (meaning of cont_africa1not Africa = 0.85): at ruggedness ZERO,
# countries outside Africa are expected to have log10 GDP 0.85 higher than
# African countries. Because the slopes differ, the difference is not the
# same at other values of ruggedness.

# Expected difference at ruggedness 3, from the coefficients:
coefs["cont_africa1not Africa"] + 3 * coefs["rugged:cont_africa1not Africa"]
# The same with predict():
pred_at_3 <- predict(m_gdp_rugged,
                     newdata = data.frame(rugged = 3,
                                          cont_africa1 = c("Africa", "not Africa")))
pred_at_3
pred_at_3[2] - pred_at_3[1]
# Answer: 0.33. In GDP itself: 10^0.33 = about 2.2 times higher outside Africa
# at ruggedness 3, compared with 10^0.85 = about 7 times at ruggedness 0.
10^0.33


# Part 2, step 6: A figure good for publication ----
# We plot the model predictions with 95% confidence bands (made with
# predict(), so they are exactly the fitted model), plus the data.
# (This is the practical's code, with linewidth instead of size for lines,
# as recommended by newer versions of ggplot2.)
new_data <- expand.grid(
  rugged = seq(min(countries$rugged), max(countries$rugged), length.out = 100),
  cont_africa1 = c("Africa", "not Africa")
)
predictions <- predict(m_gdp_rugged, newdata = new_data, interval = "confidence")
new_data <- cbind(new_data, as.data.frame(predictions))
ggplot() +
  geom_point(data = countries,
             aes(x = rugged, y = rgdppc_2000_log10, colour = cont_africa1), alpha = 0.6) +
  geom_ribbon(data = new_data,
              aes(x = rugged, ymin = lwr, ymax = upr, fill = cont_africa1), alpha = 0.2) +
  geom_line(data = new_data, aes(x = rugged, y = fit, colour = cont_africa1),
            linewidth = 1) +
  labs(x = "Terrain Ruggedness Index", y = "log10 GDP per capita (2000)",
       colour = "Continent", fill = "Continent") +
  theme_classic() +
  theme(text = element_text(size = 14),
        legend.position = "top")


# Part 2, step 7: Reporting sentences ----
# Model answer:
# "The relationship between GDP per person and terrain ruggedness differed
# between African countries and the rest of the world (interaction between
# ruggedness and continent: F(1, 166) = 8.9, p = 0.003; linear model of log10
# GDP in 2000, n = 170 countries). Outside Africa, GDP decreased with
# ruggedness (slope -0.088 log10 units per unit of ruggedness), whereas in
# Africa it tended to increase (slope 0.083, 95% CI -0.009 to 0.174). So
# bad geography is associated with lower GDP outside Africa, but not in Africa."
confint(m_gdp_rugged)


# Part 2, step 8: Critique and reflection ----
# Some points to discuss:
# - These are observational data: the model shows associations, not causes.
#   Other variables related to both ruggedness and GDP (e.g. history, such as
#   the slave trades, which were less intense in rugged regions, distance to
#   the coast) could explain the pattern.
# - 64 countries (mostly small territories) had no GDP data and were removed.
# - There are only a few very rugged countries, and they have high leverage.
# - The question came from the dataset's author; ideally we would state our
#   hypothesis before looking at the data.


# Check that the script is reproducible ----
# Finally, check that the whole script runs without errors from a clean start:
# Session > Restart R, then run the whole script from the top (Ctrl+Shift+Enter,
# or Cmd+Shift+Enter on a Mac). If it runs to the end without errors, your
# analysis is reproducible.
