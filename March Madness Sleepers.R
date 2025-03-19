
Title: "March Madness Sleepers"
Author: "Gavin Baskette"
Date: Sys.Date()


# ---- R Libraries ----

library(dplyr)
library(car)
library(lmtest)


# ---- DATA LOADING AND INITIAL PROCESSING ----

March_madness_sleepers <- read.csv("C:/Users/gavin/OneDrive/Desktop/Sport Models/March Madness Sleepers Clean.csv")
March_madness_sleepers


# ---- DATA CLEANING: REMOVE NON-NUMERIC COLUMNS & HANDLE MISSING DATA ----

non_numeric_columns <- c("Team","Ken.Pom.Rank", "Win.pct..Vs..ranked.teams",
                         "Top.30.in.either.","Pre.Season.Rank", "Seed.Prior.Year",
                         "Upset.in.Tournament.Prior.Year..or.0.wins..",
                         "Highest.rank.for.team","Top.guard.Offensive.Rating","Luck.Rating", 
                         "Made.Conference.Tournament.Semifinals.", "Ken.Pom.AdjO","Kem.Pom.AdjD",
                         "Ken.Pom.AdjO.Rank","Ken.Pom.AdjD.Rank","Top.20.in.Either.",
                         "Top.20.in.Either.and.or.Win.Conference.Tournament.",
                         "Turnover.Differential")

numeric_data <- March_madness_sleepers[, !(names(March_madness_sleepers) %in% 
                                                 non_numeric_columns)]

numeric_data <- numeric_data %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

clean_numeric_data <- numeric_data[complete.cases(numeric_data), ]

clean_numeric_data <- clean_numeric_data[, -which(names(clean_numeric_data) == "Season")]


# ---- INITIAL LINEAR MODEL TO CHECK MULTICOLLINEARITY ----

clean_numeric_data$dummy_y <- rnorm(nrow(clean_numeric_data))
lm_model <- lm(dummy_y ~ ., data = clean_numeric_data)

print(names(clean_numeric_data))

predictors <- clean_numeric_data[, !names(clean_numeric_data) %in% "dummy_y"]
target <- clean_numeric_data$dummy_y


# ---- IDENTIFYING AND REMOVING HIGHLY CORRELATED VARIABLES ----

cor_matrix <- cor(predictors)
high_corr <- which(abs(cor_matrix) > 0.9 & abs(cor_matrix) < 1, arr.ind = TRUE)
print(high_corr)

variables_to_drop <- c("SRS.Rank", "Point.Differential.Rank", "SOS.Rank", 
                       "OER.Rank", "DER.Rank", "HC.Tournament.Wins","Second.half.win..")  # Use high_corr results
predictors_reduced <- predictors[, !(names(predictors) %in% variables_to_drop)]


# ---- UPDATED MODEL AFTER DROPPING HIGHLY CORRELATED VARIABLES ----

final_data <- cbind(predictors_reduced, dummy_y = target)

lm_model_final <- lm(dummy_y ~ ., data = final_data)


# ---- CHECKING FOR ALIASED VARIABLES ----

alias_results_final <- alias(lm_model_final)
print(alias_results_final)

alias(lm_model_final)$Complete

lm_model_final

final_data_filtered <- final_data

na_vars <- names(coef(lm_model_final))[is.na(coef(lm_model_final))]

final_data_filtered <- final_data_filtered[, !names(final_data_filtered) %in% na_vars]

lm_model_final <- lm(dummy_y ~ ., data = final_data_filtered)

summary(lm_model_final)

dim(final_data_filtered)

cor_matrix <- cor(final_data_filtered)
high_corr <- which(abs(cor_matrix) > 0.8 & abs(cor_matrix) < 1, arr.ind = TRUE)
print(high_corr)

vars_to_remove <- unique(rownames(high_corr))  # Get unique variable names

final_data_filtered <- final_data_filtered[, !(names(final_data_filtered) %in% vars_to_remove)]

dim(final_data_filtered)

lm_model_final <- lm(dummy_y ~ ., data = final_data_filtered)
summary(lm_model_final)


# ---- FINAL MODEL SELECTION & VIF ANALYSIS ----

vif_values_final <- vif(lm_model_final)
print(vif_values_final)


# ---- ADJUSTING WEIGHTS BASED ON VIF ----

# Original weights (z-scores from dataset)
weights <- c(4.010572091, 1.865372884, 1.999608784, 0.670274931, 
             1.44883147, 0.806372687, 0.509605527, 1.162441512, 
             0.520765033, 0.749801728, 0.535847054, 0.236938241, 
             0.619432158, 0.544532566, 1.192630819, 0.653226636, 
             1.345474128, 0.194443767, 0.320704683, 0.288359699)

# Actual VIF values from earlier result
vif_values <- c(206.034525, 242.030823, 3411.624892, 7.468942, 
                2959.684249, 3507.668282, 34.835254, 27.839710, 
                22.601392, 16.533583, 28.732699, 8.753929, 4.161234, 
                16.800567, 6.294500, 7.962624, 2.470320, 120.048704, 
                282.074188, 137.926469)


# Calculated adjusted weights based on VIF
adjusted_weights <- weights / sqrt(log(vif_values))

# Normalized adjusted weights to maintain the same total as original weights
normalized_weights <- adjusted_weights / sum(adjusted_weights) * sum(weights)

# Summary table to compare
results <- data.frame(
  Variable = c("Simple.Rating.System..SRS.", "Strength.of.Schedule..SOS.", 
               "Point.Differential", "Wins.vs..ranked.teams", 
               "Offensive.Efficiency.Rating..OER.","Defensive.Effeciency.Rating..DER.", 
               "Points.For.Rank", "Points.Against.Rank", "Pace", "Pace.Rank", 
               "X..of.players.drafted", "Average.starting.5.age",
               "Average.Starting.5.Tournament.Experience", "HC.Tournament.Experience", 
               "Wins.prior.year", "Home.win.pct.", "Win.pct..Vs..other.conference", 
               "X..of.games.decided.by.5.or.less..or.OT..close.games.",
               "Wins.in.close.games", "Win...in.close.games"),
  Original_Weight = weights,
  VIF = vif_values,
  Adjusted_Weight = adjusted_weights,
  Normalized_Weight = normalized_weights
)

print(results)


# ---- FINAL SELECTION OF REMAINING VARIABLES & RUNNING VIF ANALYSIS ----

head(March_madness_sleepers)
head(predictors)

remaining_vars <- c("SRS.Rank", "SOS.Rank", "Wins", "Point.Differential.Rank", 
                    "OER.Rank", "DER.Rank", "Second.half.win..", 
                    "HC.Tournament.Wins", "Away.win.pct.",
                    "Win.pct..Vs..same.conference", "Conference.Result", "Free.Throw..",
                    "X3.Point..", "X..of.attempts.from.3.pt", "Free.Throw.Attempts.per.game", 
                    "Top.Scorer.Guard.3.Point..", "Conference.Win..", "Conference.SRS.Rank")

predictors_with_target <- cbind(predictors, dummy_y = target)

final_data_remaining <- predictors_with_target[, c(remaining_vars, "dummy_y")]

lm_remaining <- lm(dummy_y ~ ., data = final_data_remaining)

vif_values_remaining <- vif(lm_remaining)

print(vif_values_remaining)


# ---- ADJUSTING WEIGHTS BASED ON VIF FOR REMAINING VARIABLES ----

# Original weights (z-scores from dataset)
weights <- c(5.707493208, 2.705063921, 1.989369736, 2.927264388, 
             1.485258739, 0.783252211, 1.064111458, 0.47156548, 
             2.897783881, 1.345474128, 0.854242196, 0.281535125, 
             1.448067739, 0.79100164, 0.3036838, 1.060398815, 
             2.024285797, 2.070333341)

# Actual VIF values from earlier result
vif_values <- c(26.274921, 25.308815, 25.278612, 24.119849, 
                14.657337, 19.145660, 20.503203, 4.178643, 
                14.300807, 34.108117, 9.390713, 3.707988, 
                6.667132, 6.794275, 6.272570, 4.215828, 
                34.346323, 40.197724)


# Calculated adjusted weights based on VIF
adjusted_weights <- weights / sqrt(log(vif_values))

# Normalized adjusted weights to maintain the same total as original weights
normalized_weights <- adjusted_weights / sum(adjusted_weights) * sum(weights)

# Summary table to compare
results <- data.frame(
  Variable = c("SRS.Rank", "SOS.Rank", "Wins", "Point.Differential.Rank", 
               "OER.Rank", "DER.Rank", "Second.half.win..",
               "HC.Tournament.Wins","Away.win.pct.", "Win.pct..Vs..same.conference",
               "Conference.Result", "Free.Throw..", "X3.Point..",
               "X..of.attempts.from.3.pt", "Free.Throw.Attempts.per.game",
               "Top.Scorer.Guard.3.Point..", "Conference.Win..", 
               "Conference.SRS.Rank"),
  Original_Weight = weights,
  VIF = vif_values,
  Adjusted_Weight = adjusted_weights,
  Normalized_Weight = normalized_weights
)

print(results)






