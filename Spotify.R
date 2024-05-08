#Spotify Popularity Data https://www.kaggle.com/datasets/iamsumat/spotify-top-2000s-mega-dataset/data
#This dataset contains audio statistics of the top 2000 tracks on Spotify. The data contains about 15 columns each 
#describing the track and it's qualities from songs released from 1956 to 2019.

#Research Question: Do inherent characteristics about songs make them more 'Popular'?
library(corrplot)
library(brms)
library(ggplot2)
library(tidyverse)
set.seed(123)



#setwd("C:/Users/JULIA_BLAKE/OneDrive - S&P Global/Columbia/Spring 2024/Bayesian/Final Project")
data <- read.csv("Spotify-2000.csv")

#Data Cleaning
summary(data)
str(data)

# Rename columns
names(data)[names(data) == 'Length..Duration.'] <- 'Duration'
names(data)[names(data) == 'Beats.Per.Minute..BPM.'] <- 'BPM'
names(data)[names(data) == 'Loudness..dB.'] <- 'Loudness_db'
names(data)[names(data) == 'Top.Genre'] <- 'Genre'

data$Duration <- as.integer(gsub(",", "", data$Duration))


ggplot(data,aes(y=Popularity)) + geom_boxplot() +
  labs(title="Distribution of Song Popularity", subtitle="Spotify, April 2019")

#Clean up Genre Column (not sure if we want to use this but tried to consolidate)
data$Genre[grepl("pop", data$Genre, ignore.case = TRUE)] <- "pop"
data$Genre[grepl("punk", data$Genre, ignore.case = TRUE)] <- "punk"
data$Genre[grepl("rock|prog", data$Genre, ignore.case = TRUE)] <- "rock"
data$Genre[grepl("hip hop|rap", data$Genre, ignore.case = TRUE)] <- "hip hop/rap"
data$Genre[grepl("country|americana", data$Genre, ignore.case = TRUE)] <- "country/americana"
data$Genre[grepl("folk", data$Genre, ignore.case = TRUE)] <- "folk"
data$Genre[grepl("alternative", data$Genre, ignore.case = TRUE)] <- "alternative"
data$Genre[grepl("house|electronica|electro", data$Genre, ignore.case = TRUE)] <- "dance"
data$Genre[grepl("songwriter|songwrite", data$Genre, ignore.case = TRUE)] <- "singer-songwriter"
data$Genre[grepl("metal", data$Genre, ignore.case = TRUE)] <- "metal"
data$Genre[grepl("dance", data$Genre, ignore.case = TRUE)] <- "dance"
data$Genre[grepl("caberet|cabaret", data$Genre, ignore.case = TRUE)] <- "cabaret"
data$Genre[grepl("indie", data$Genre, ignore.case = TRUE)] <- "indie"


#New Column 
data$yrs_since_release <- 2019 - data$Year

#EDA
par(mfrow=c(1,1))
cor_matrix <- cor(data[, 6:16])
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, diag = FALSE, addCoef.col = "black", number.cex = 0.6)

numeric_vars <- sapply(data, is.numeric)
numeric_data <- data[, numeric_vars]

par(mfrow = c(3, ceiling(sum(numeric_vars) / 3))) # Corrected the function to ceiling()

for (i in 1:ncol(numeric_data)) {
  hist(numeric_data[, i], main = names(numeric_data)[i], xlab = "", col = "skyblue", border = "white")}

par(mfrow=c(1,1))

# Plotting popularity over time with a linear fit line: it appears that older songs are more popular - could be due to just the duration of time allowed to more listens
ggplot(data, aes(x = yrs_since_release, y = Popularity)) +
  geom_point() +  # Scatter plot of popularity over time
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Linear fit line
  labs(title = "Popularity of Songs vs Time since Release",
       x = "Time Since Release",
       y = "Popularity") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20),
        axis.title = element_text(size = 15))

ggplot(data, aes(x = Year, y = Popularity)) +
  geom_point() +  # Scatter plot of popularity over time
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Linear fit line
  labs(title = "Popularity of Songs over Time",
       x = "Year",
       y = "Popularity") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20),
        axis.title = element_text(size = 15))

#Data Centering and scaling

#columns 6 - 14
cns <- function(v){
  mean <- mean(v)
  sd <- sd(v)
  v <- (v-mean)/sd
  return(v)
}

data[,6:14] <- lapply(6:14,function(x){cns(data[,x])})
###############################################################################
#OLS
# Assuming 'response_variable' is the dependent variable and 'predictor_variable1', 'predictor_variable2', etc. are the independent variables

# Fit the OLS model with select variables
model <- lm(Popularity ~ BPM + Energy + Danceability + Loudness_db + 
              Liveness + Valence + Duration + Acousticness + Speechiness + yrs_since_release, 
            data = data)
summary(model)

# Bayesian estimation
brm_model <- brm(Popularity ~ BPM + Energy + Danceability + Loudness_db + 
                   Liveness + Valence + Duration + Acousticness + Speechiness + yrs_since_release, 
                 data = data) # can add MCMC settings

# Summary of the Bayesian regression model
summary(brm_model)
plot(brm_model)
stancode(brm_model)


###############################################################################

#try diff prior than default
custom_priors <- set_prior("normal(120, 5)", class = "b", coef = "BPM") +
  set_prior("normal(50, 5)", class = "b", coef = "Energy") +
  set_prior("normal(50, 5)", class = "b", coef = "Danceability") +
  set_prior("normal(-10, 2)", class = "b", coef = "Loudness_db") + 
  set_prior("normal(20, 5)", class = "b", coef = "Liveness") +
  set_prior("normal(50, 5)", class = "b", coef = "Valence") +
  set_prior("normal(120, 20)", class = "b", coef = "Duration") +
  set_prior("normal(20, 5)", class = "b", coef = "Acousticness") +
  set_prior("normal(10, 5)", class = "b", coef = "Speechiness") 


# Bayesian estimation with custom priors
brm_model_custom_priors <- brm(
  Popularity ~ BPM + Energy + Danceability + Loudness_db + 
    Liveness + Valence + Duration + Acousticness + Speechiness + yrs_since_release, 
  data = data,
  prior = custom_priors
  # You can also add MCMC settings here if needed
)

summary(brm_model_custom_priors)
plot(brm_model_custom_priors)
stancode(brm_model_custom_priors)

waic(brm_model)
waic(brm_model_custom_priors)
#Conclusion:Loudness, Speechiness and How many years since the song was released are the three variables most important for predicting its Popularity score.
# It appears that audio attributes do not necessarily affect how popular a song becomes. 


###############################################################################
#####Try Latent
# Lets see if there is some underlying factor that impacts the popularity score of songs?
eigen(cor_matrix)$values

plot(eigen(cor_matrix)$values, xlab = 'Eigenvalue Number', ylab = 'Eigenvalue Size', main = 'Scree Graph', type = 'b', xaxt = 'n')
axis(1, at = seq(1, 10, by = 1))
abline(h = 1)

(fit = factanal(factors = 5, covmat = cor_matrix))
print(fit, digits = 2, cutoff=.3, sort = TRUE)

# plot factor 1 by factor 2 
load = fit$loadings[,1:2] 
plot(load,type="n") # set up plot
text(load,labels=colnames(cor_matrix),cex=.7) # add variable names
#It seems that the factor analysis results may not indicate a clear underlying latent factor structure that significantly impacts 
# the observed correlations among the variables. The fit statistic (0.0497) suggests that the model does not fit the data very well. 
# This further supports the notion that the identified factors may not adequately capture the underlying structure of the data.
#Maybe instead is it lyrics, some external famousness of the artist, backstory or emotional attachment to the lyrics that makes songs popular.


#####Now Bayesian
#install.packages("blavaan")
library("blavaan")
spotify.latent.model <- ' Vivacity =~ Energy + Loudness_db + BPM 
                          Wordiness =~ Acousticness + Speechiness
                          Happy =~ Danceability + Valence
                          Rawness =~ Liveness + Duration
                          Age =~ yrs_since_release + Popularity'

bfit <- bcfa(spotify.latent.model, data = data)
summary(bfit)
################################################################################
###Mixed Effects Models

#Defines 11 eras in modern music history and assigns each observation to one of them
#through a new column: "Era"

era_breaks <- c(0, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, Inf)
era_labels <- c('Pre-70s','1970-1974','1975-1979','1980-1984','1985-1989',
                '1990-1994','1995-1999','2000-2004','2005-2009','2010-2014',
                '2015-2019')

data$Era <- cut(data$Year, breaks = era_breaks, labels = era_labels, right = FALSE)

eras <- split(data, data$Era)

#EDA for Era groups
summary(data$Era)
mean(summary(data$Era))
sd(summary(data$Era))

#Runs OLS regression for each era and stores results in model_list
coefficients_list <- list()
model_list <- list()

for (i in seq(1:length(eras))) {
  model <- lm(Popularity ~ BPM + Energy + Danceability + Loudness_db + 
       Liveness + Valence + Duration + Acousticness + Speechiness + yrs_since_release, 
     data = eras[[i]])
  coefficients_list[[i]] <- coef(model)
  model_list[[i]] <- model
}

# Plot actual vs predicted popularity for each era

predicted_popularity <- list()

for (i in seq_along(eras)) {
  predicted_popularity[[i]] <- predict(model_list[[i]], newdata = eras[[i]])
}

# Manual assignment of colors for each era
era_colors <- c("red", "blue", "green", "orange", "purple", "cyan",
                "magenta", "yellow", "brown", "pink", "gray")

# Function to calculate R-squared
calculate_r_squared <- function(actual, predicted) {
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r_squared <- 1 - (ss_res / ss_tot)
  return(r_squared)
}

par(mfrow = c(1, 3)) 

for (i in seq_along(eras)) {
  plot(eras[[i]]$Popularity, predicted_popularity[[i]], col = era_colors[i], 
       main = paste(era_labels[i],"Era"), xlab = "Actual Popularity", ylab = "Predicted Popularity")
  
  # Add the line x = y
  abline(a = 0, b = 1, col = "red")
  
  r_squared <- calculate_r_squared(eras[[i]]$Popularity, predicted_popularity[[i]])
  
  text(x = max(eras[[i]]$Popularity) * 0.9, y = max(predicted_popularity[[i]]) * 0.7, 
       labels = paste("R2:", round(r_squared, 2)), pos = 2)
}


# Plot data points with regression lines for each era
ggplot(data, aes(x = Year, y = Popularity, color = factor(Era))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = Era), color = "black") +  
  labs(title = "Popularity over Time, by Era", x = "Year", y = "Population") +
  scale_color_discrete(name = "Era") +  
  theme_minimal()


#### Fit a hierarchical model with brms
#install.packages('performance')
library(performance)

#Unconditional means model

brm_hierarchical_base <- brm(
  formula = Popularity ~ 1 + (1|Era),
  data = data, iter = 10000, family = 'gaussian'
)
summary(brm_hierarchical_base)
icc(brm_hierarchical_base)
variance_decomposition(brm_hierarchical_base)
waic(brm_hierarchical_base)

#Random intercept
brm_hierarchical_int <- brm(
  formula = Popularity ~ BPM + Energy + Danceability + Loudness_db + 
    Liveness + Valence + Duration + Acousticness + Speechiness + 
    yrs_since_release + (1|Era),
  data = data, iter = 10000, family = 'gaussian'
)
summary(brm_hierarchical_int)
icc(brm_hierarchical_int)
variance_decomposition(brm_hierarchical_int)
waic(brm_hierarchical_int)

#Random slope and random intercept
brm_hierarchical_intslp <- brm(
  formula = Popularity ~ BPM + Energy + Danceability + Loudness_db + 
    Liveness + Valence + Duration + Acousticness + Speechiness + 
    yrs_since_release + (BPM + Energy + Danceability + Loudness_db + 
                           Liveness + Valence + Duration + Acousticness + Speechiness|Era),
  data = data, iter = 10000, family = 'gaussian'
)
summary(brm_hierarchical_intslp)
icc(brm_hierarchical_intslp)
variance_decomposition(brm_hierarchical_intslp)
waic(brm_hierarchical_intslp)


library(loo)

psisloo_base <- loo(brm_hierarchical_base)
psisloo_int <- loo(brm_hierarchical_int)
psisloo_intslp <- loo(brm_hierarchical_intslp)

loo_compare(psisloo_int,psisloo_intslp)







