#Spotify Popularity Data https://www.kaggle.com/datasets/iamsumat/spotify-top-2000s-mega-dataset/data
library(corrplot)
library(brms)
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
cor_matrix <- cor(data[, 5:15])
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, diag = FALSE, addCoef.col = "black", number.cex = 0.6)

numeric_vars <- sapply(data, is.numeric)
numeric_data <- data[, numeric_vars]

par(mfrow = c(3, ceiling(sum(numeric_vars) / 3))) # Corrected the function to ceiling()

for (i in 1:ncol(numeric_data)) {
  hist(numeric_data[, i], main = names(numeric_data)[i], xlab = "", col = "skyblue", border = "white")
}

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


####################################

#try diff prior than default

