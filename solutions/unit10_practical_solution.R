# =============================================================================
# BIO144 Data Analysis in Biology
# Unit 10 practical: example solution (ordination: PCA and NMDS)
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
# - Theory: course book, Chapter 10 (Ordination).
# =============================================================================


# Load the packages ----
library(tidyverse)  # read_csv(), dplyr, tidyr, ggplot2
library(vegan)      # vegdist(), metaMDS(), stressplot()
# MASS is also needed (for rnegbin() in the simulation), but we call it with
# MASS::rnegbin() instead of loading it, because MASS would mask dplyr::select().


# ============================== Practical part 1 ==============================
# Plant communities along a soil moisture gradient: PCA


# Part 1, step 1: Create a dataset ----
# This is the code from the practical. Because we use the same set.seed(),
# you get exactly the same "random" numbers as everyone else.
set.seed(123)

n_sites <- 30     # number of sites
n_species <- 5    # number of species

# Soil moisture at each site, from a uniform distribution
soil_moisture <- runif(n_sites, min = 10, max = 1000)

# Average abundance of each species (from a log-normal distribution)
abundance_intercepts <- rlnorm(n_species, meanlog = 3, sdlog = 0.5)

# Effect of soil moisture on each species (from a normal distribution)
abundance_slopes <- rnorm(n_species, mean = 1, sd = 1)

# Simulate the abundance of each species at each site
# (negative binomial counts, with a mean that depends on soil moisture)
community <- matrix(NA, nrow = n_sites, ncol = n_species)
for (i in 1:n_species) {
  mu <- pmax(0.1, abundance_intercepts[i] + abundance_slopes[i] * soil_moisture)
  community[, i] <- MASS::rnegbin(n_sites, mu = mu, theta = 2)
}

# Name the species, convert to a data frame, add site IDs and soil moisture
colnames(community) <- paste0("Species_", 1:n_species)
community <- as.data.frame(community)
community <- community |>
  mutate(Site = paste0("Site_", 1:n_sites),
         Soil_Moisture = soil_moisture)

community
# Look at: 30 rows (sites), five species columns, and Site and Soil_Moisture.

# Question: which best describes the species data in this dataset?
# Answer: multivariate response data. WHY: at each site we measured several
# species, and together they are the response we are interested in.


# Part 1, step 2: Visualise species abundances ----
# Make the data long: one row per site and species.
community_long <- community |>
  pivot_longer(cols = starts_with("Species_"),
               names_to = "Species",
               values_to = "Abundance")

ggplot(community_long, aes(x = Soil_Moisture, y = Abundance, color = Species)) +
  geom_point() +
  labs(x = "Soil moisture", y = "Species abundance",
       title = "Species abundances along the soil moisture gradient")
# Look at: some species increase strongly with moisture (e.g. Species_2),
# others hardly change; some species are much more abundant than others.

# Question: why is it better to use ggplot on the long-format data?
# Answer: because the same code works for any number of species and any
# species names, without changing the code. WHY: ggplot maps the Species
# column to colour automatically.

# The practical asks you to think about correlations among the species:
# here are the correlations among the species and with soil moisture.
community |>
  select(starts_with("Species_"), Soil_Moisture) |>
  cor() |>
  round(2)
# Look at: Species_1 and Species_3 are negatively correlated with soil moisture,
# Species_2, 4 and 5 positively. The correlations are moderate (about 0.3-0.5).

# "What do you expect from the PCA? How much variance will PC1 explain?"
# Model answer: all five species respond to ONE underlying gradient (soil
# moisture), so the data are close to one-dimensional, plus a lot of noise.
# So we expect PC1 to explain clearly more than 20% (= 1/5, what each PC would
# explain if the species were independent), but, because the correlations are
# only moderate, well below 100%. Perhaps around half.

# Question: how would standardising (scaling) the species affect the PCA?
# Answer: it makes all species contribute equally, regardless of their
# absolute abundances. WHY: after scaling every species has variance 1, so
# abundant species (with large variance) no longer dominate the PCA.

# Standardise the species data (centre: mean = 0; scale: sd = 1).
# This is the code from the "Code snippets" section of the practical.
community_scaled <- community |>
  select(starts_with("Species_")) |>
  scale() |>
  as.data.frame()
# scale() drops the other columns, so add Site and Soil_Moisture back
community_scaled <- community_scaled |>
  mutate(Site = community$Site,
         Soil_Moisture = community$Soil_Moisture)

# Check the scaling worked
community_scaled |>
  summarise(across(starts_with("Species_"), list(mean = mean, sd = sd)))
# Check: every mean is (almost) 0 (numbers like 1e-17 are zero) and every sd is 1.


# Part 1, step 3: Prepare data for PCA ----
# prcomp() needs only numeric variables: a matrix of the (scaled) species.
species_matrix <- community_scaled |>
  select(starts_with("Species_")) |>
  as.matrix()


# Part 1, step 4: Run PCA ----
# The data are already centred and scaled, so we tell prcomp() not to do it again.
pca_result <- prcomp(species_matrix, center = FALSE, scale. = FALSE)

# Question: why is it reasonable to use center = FALSE and scale. = FALSE?
# Answer: because the data have already been centred and scaled beforehand,
# so doing it again would change nothing. (prcomp(..., scale. = TRUE) on the
# unscaled species would give the same result.)


# Part 1, step 5: Examine PCA results ----
summary(pca_result)
# Look at: the row "Proportion of Variance".

# Question: how much variation does PC1 represent (percentage, 1 decimal place)?
# Answer: 48.5%. WHY: Proportion of Variance for PC1 is 0.485 (0.485 x 100).
# PC2 adds 18.4%, so PC1 and PC2 together represent 66.9%.

# Question: why does PC1 explain much more than 20%?
# Answer: because the species abundances are correlated with each other (some
# positively, some negatively), as they all respond to the same underlying
# gradient (soil moisture). The correlations are only moderate, so PC1
# explains about half of the variation, not nearly all of it.


# Part 1, step 6: Visualise PCA results ----
# PCA scores of the sites (pca_result$x), coloured by soil moisture.
ggplot() +
  geom_point(aes(x = pca_result$x[, 1], y = pca_result$x[, 2],
                 color = community_scaled$Soil_Moisture)) +
  labs(x = "PC1 (48.5%)", y = "PC2 (18.4%)", color = "Soil moisture")
# Look at: the colour changes from one end of PC1 to the other, but not along
# PC2. So PC1 is (mostly) the soil moisture gradient. With the signs on most
# computers, dry sites have high PC1 scores and wet sites low PC1 scores (the
# sign of a PC is arbitrary, so on your computer it may be the other way round).

# The same plot made from a data frame (often easier to read and change):
pca_scores <- community_scaled |>
  mutate(PC1 = pca_result$x[, 1],
         PC2 = pca_result$x[, 2])
ggplot(pca_scores, aes(x = PC1, y = PC2, color = Soil_Moisture)) +
  geom_point(size = 2) +
  labs(x = "PC1 (48.5%)", y = "PC2 (18.4%)", color = "Soil moisture")


# Part 1, step 7: Interpret PCA results more ----
# The loadings: how much each species contributes to each PC.
loadings <- pca_result$rotation
loadings
# Look at the PC1 column: Species_1 and Species_3 have positive loadings
# (about +0.45), Species_2, 4 and 5 negative loadings (about -0.43 to -0.46).

# Question: which species have PC1 loadings with the opposite sign to the others?
# Answer: Species 1 and Species 3. WHY: their PC1 loadings have one sign, those
# of Species 2, 4 and 5 the other sign. (All loadings are similar in size, so
# all five species contribute about equally to PC1.)

# Compare with the true effects of soil moisture used in the simulation:
abundance_slopes
# Look at: Species_1 and Species_3 have (slightly) negative slopes; Species_2,
# 4 and 5 positive slopes. This is the same grouping as in the PC1 loadings.
# Here is the comparison side by side:
tibble(species = colnames(species_matrix),
       true_slope = abundance_slopes,
       PC1_loading = loadings[, "PC1"])
# Note: the size of the loadings does not match the size of the slopes, because
# the species were scaled (each species then has the same weight), and
# because of noise in the simulated counts.


# Part 1, step 8: Linking ordination to soil moisture ----
# Plot the PC1 score of each site against soil moisture.
ggplot(pca_scores, aes(x = Soil_Moisture, y = PC1)) +
  geom_point() +
  labs(x = "Soil moisture", y = "PC1 score")
cor(pca_scores$PC1, pca_scores$Soil_Moisture)
# Look at: a strong correlation, -0.76 here (negative because of the arbitrary
# sign of PC1).

# Model answer (conclusions):
# The main pattern in the plant community (PC1, 48.5% of the variation) is a
# gradient in composition that follows soil moisture: wet sites have more of
# Species 2, 4 and 5, dry sites relatively more of Species 1 and 3. PC2 and
# the later PCs are not related to moisture; here they are mostly noise
# (because we simulated only one gradient). In real data we could not be sure
# that PC1 is soil moisture: we would need the measured moisture (as here) or
# other evidence to interpret the axis.


# Part 1, step 9: Principal components as explanatory variables ----
# This is the code from the practical.
community_pca <- community_scaled |>
  mutate(PC1 = pca_result$x[, 1])
m_moisture_species <- lm(Soil_Moisture ~ Species_1 + Species_2 + Species_3 +
                           Species_4 + Species_5,
                         data = community_pca)
m_moisture_pc1 <- lm(Soil_Moisture ~ PC1, data = community_pca)
summary(m_moisture_species)
summary(m_moisture_pc1)
# Look at (five-species model): only Species_1 has p < 0.05 (p = 0.03);
# Species_5 is borderline (p = 0.055); the others are not clearly different
# from zero, although every species is related to moisture. That is the
# effect of collinearity (Unit 6): correlated explanatory variables "share"
# the explanation, so each slope is imprecise.
# R-squared = 0.63, with 6 parameters (24 residual df).
# Look at (PC1 model): the PC1 slope is very clearly different from zero
# (t = -6.2, p < 0.0001). R-squared = 0.58, with only 2 parameters (28 residual df).

# Question: advantages and costs of using PC1 instead of the five species?
# Answer (all three of these are correct):
# - Advantage: one explanatory variable instead of five, so no collinearity and
#   more residual df, with a clear, precisely estimated relationship.
# - Cost: the slope of PC1 is harder to interpret biologically, because PC1 is
#   a combination of all species (look at the loadings to see what it is).
# - Cost: information in the other PCs is left out, so R-squared can be a
#   little lower (0.58 instead of 0.63 here).
# Using PC1 does NOT prove which species cause changes in soil moisture.


# ============================== Practical part 2 ==============================
# Diet and the human gut microbiome: Bray-Curtis dissimilarity and NMDS


# Part 2, step 1: Get the dataset and load it into R ----
biom <- read_csv("https://raw.githubusercontent.com/opetchey/BIO144_Practicals_WAs/refs/heads/main/assets/datasets/microbiome_data.csv")

# Check the data
dim(biom)           # 60 rows (individuals), 202 columns (SampleID, Diet, 200 taxa)
biom |> count(Diet) # 20 individuals per diet
biom |> select(1:6) |> head()
# Note: the values in this file are not proportions that add up to 1 (each
# row adds up to several hundred). For Bray-Curtis this does not matter much;
# if you want true relative abundances you could divide each row by its sum,
# e.g. with decostand(..., method = "total") from vegan.


# Part 2, step 2: Bray-Curtis dissimilarities ----
# Only the taxon columns (names starting with OTU_) go into the distance.
bray_distances <- vegdist(biom |> select(starts_with("OTU_")), method = "bray")
# There are 60 x 59 / 2 = 1770 dissimilarities, one for each pair of samples.
summary(as.vector(bray_distances))


# Part 2, step 3: NMDS with k = 2, 3 and 10 dimensions ----
# metaMDS() starts from random configurations, so set the seed to get the same
# result every time. trymax = 100 allows up to 100 random starts, to find the
# best solution. (It prints a lot of progress output: that is normal.)
set.seed(144)
nmds_result_2d <- metaMDS(bray_distances, k = 2, trymax = 100)
nmds_result_2d$stress
nmds_result_3d <- metaMDS(bray_distances, k = 3, trymax = 100)
nmds_result_3d$stress
nmds_result <- metaMDS(bray_distances, k = 10, trymax = 100)
nmds_result$stress

# Question: what is the stress? Is it acceptable?
# Answer: stress = 0.28 with k = 2 (poor: above 0.2), 0.22 with k = 3 (still
# poor), and 0.08 with k = 10 (good: below 0.1). These match the values in the
# practical. (Your values may differ slightly if you ran metaMDS() in another
# order, because the random starts differ.)


# Part 2, step 4: Shepard plots (stressplot) ----
stressplot(nmds_result_2d)
stressplot(nmds_result_3d)
stressplot(nmds_result)
# Look at: the "Linear fit, R2" value printed in each plot.
# Answer: linear fit R2 = 0.62 (k = 2), 0.68 (k = 3, i.e. below 0.7: not
# great, but OK for ecological data) and 0.86 (k = 10, very good).
# WHY so many dimensions? The 200 taxa vary largely independently of each
# other, so a few axes cannot summarise the differences among samples well.
# A plot of only the first two axes of the 10-dimensional NMDS therefore shows
# only part of the structure: interpret it cautiously.


# Part 2, step 5: Visualise the NMDS, coloured by diet ----
# The NMDS coordinates of each sample are in nmds_result$points.
nmds_scores <- as.data.frame(nmds_result$points)
nmds_scores$Diet <- biom$Diet
ggplot(nmds_scores, aes(x = MDS1, y = MDS2, color = Diet)) +
  geom_point(size = 3) +
  labs(title = "NMDS of gut microbiome composition by diet",
       x = "NMDS1", y = "NMDS2")

# For comparison, the 2-dimensional solution:
nmds_scores_2d <- as.data.frame(nmds_result_2d$points)
nmds_scores_2d$Diet <- biom$Diet
ggplot(nmds_scores_2d, aes(x = MDS1, y = MDS2, color = Diet)) +
  geom_point(size = 3) +
  labs(title = "NMDS (k = 2) of gut microbiome composition by diet",
       x = "NMDS1", y = "NMDS2")

# Model answer (a few sentences about the NMDS plot):
# The three diet groups are separated along the first NMDS axis, in the order
# omnivore - vegetarian - vegan (a gradient of decreasing animal products),
# although the groups overlap. This suggests that diet is associated with gut
# microbiome composition. The groups also differ in their spread: vegans are
# the most similar to each other (a tight cluster), omnivores the most
# variable. However: this is an observational
# study (people chose their diet), so the association is not proof that diet
# causes the differences; the stress shows that two axes capture only part of
# the structure; and the plot is not a statistical test. NMDS axes have no
# units and no "% variance explained", and their orientation is arbitrary.

# Question: which approach would TEST whether diet groups differ in their
# position (centroid) in multivariate space, using Bray-Curtis?
# Answer: PERMANOVA (adonis2() in vegan), ideally together with PERMDISP
# (betadisper()) to check whether the groups also differ in their spread.
# (In BIO144 you need to know when to use these, not how to run them.)

# Optional (not examinable): this is how you would run them.
set.seed(144)
adonis2(bray_distances ~ Diet, data = biom)  # PERMANOVA: do centroids differ?
anova(betadisper(bray_distances, biom$Diet)) # PERMDISP: do spreads differ?
# Look at: Pr(>F) for Diet in both tables. Both are small (about 0.001 and
# < 0.0001): the diets differ in their centroids AND in their spread, so the
# PERMANOVA result is partly caused by the difference in spread. Diet explains
# only about 10% of the variation (R2 = 0.099 in the adonis2 table).


# Check that the script is reproducible ----
# Finally: Session > Restart R, then run the whole script again from the top
# (Ctrl+Shift+Enter, or Cmd+Shift+Enter on a Mac). If it runs without errors
# and gives the same answers, your analysis is reproducible.
