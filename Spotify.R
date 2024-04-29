#Spotify Popularity Data https://www.kaggle.com/datasets/iamsumat/spotify-top-2000s-mega-dataset/data
#This dataset contains audio statistics of the top 2000 tracks on Spotify. The data contains about 15 columns each 
#describing the track and it's qualities from songs released from 1956 to 2019.

#Research Question: Do inherent characteristics about songs make them more 'Popular'?
library(corrplot)
library(brms)
library(ggplot2)
set.seed(123)



#setwd("C:/Users/JULIA_BLAKE/OneDrive - S&P Global/Columbia/Spring 2024/Bayesian/Final Project")
setwd("C:/Users/julia/OneDrive/Documents/Columbia/Spring 2024/Bayesian (STAT5224)/Final Proj")
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
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear fit line
  labs(title = "Popularity of Songs over Time",
       x = "Year",
       y = "Popularity") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20),
        axis.title = element_text(size = 15))
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

#found these functions
pp_check(brm_model, ndraws = 100, type = 'ecdf_overlay')
hypothesis(brm_model, 'Energy > 50')
(waic1 = waic(brm_model))

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
(waic2 = waic(brm_model_custom_priors))

#Conclusion:Loudness, Speechiness and How many years since the song was released are the three variables most important for predicting its Popularity score.
# It appears that audio attributes do not necessarily affect how popular a song becomes. 

loo_compare(loo(brm_model), loo(brm_model_custom_priors))
# Using loo with cross validation, it appears that the first model is better at predicting 
#(Expected Log Predictive Density is higher).

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
spotify.latent.model <- ' Vivacity =~ Energy + Loudness_db + BPM + Danceability + Valence 
                          Wordiness =~ Acousticness + Speechiness + Liveness'

#Rawness =~ Duration + yrs_since_release + Popularity

bfit <- bcfa(spotify.latent.model, data = data)
summary(bfit)

#HS.model <- ' visual =~ x1 + prior("normal(1,1)")*x2 + x3
#verbal =~ x4 + x5 + x6 '
#bfit <- bcfa(HS.model, data = HolzingerSwineford1939,
#             dp = dpriors(lambda = "normal(1,5)"),
#             burnin = 500, sample = 500, n.chains = 4,
#             save.lvs = TRUE,
#             bcontrol = list(cores = 4))
#summary(bfit)



###############################################################################
#ADDING FROM LAST CLASS EXAMPLE
# Step 3: Fit some Bayesian models
# Option 1: manual computation
# With Metropolis, assuming sigma = 1 (for simplicity)
y<-data$Popularity 
n<-length(y)
X<-as.matrix(cbind(rep(1, n),data[, 5:14]))
p<-dim(X)[2]
pmn.beta<-rep(0,p) # prior expectation
psd.beta<-rep(15,p) # prior sd

var.prop<- var(y)*solve( t(X)%*%X ) # proposal variance

beta<-rep(0,p) # starting value
S<-10000 # number of iterations
BETA<-matrix(0,nrow=S,ncol=p) # saved beta values
acs<-0 # acceptances
# set.seed(1) # initialize RNG

for(s in 1:S)
{
  #propose a new beta
  beta.p<- c(rmvnorm(1, beta, var.prop ))
  # compute r
  lhr<- sum(dnorm(y,(X%*%beta.p),log=T)) -
    sum(dnorm(y,(X%*%beta),log=T)) +
    sum(dnorm(beta.p,pmn.beta,psd.beta,log=T)) -
    sum(dnorm(beta,pmn.beta,psd.beta,log=T))
  # accept or reject
  if( log(runif(1))< lhr ) { beta<-beta.p ; acs<-acs+1 }
  BETA[s,]<-beta
}

# Check convergence
apply(BETA,2,coda::effectiveSize)
plot(BETA[,2], type = "l")
# Discuss and if needed make changes to improve convergence

#Results
apply(BETA,2,mean)
# Write here the equation, interpretation and discuss significance of predictors


