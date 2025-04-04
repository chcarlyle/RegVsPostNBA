##################################################################
#############Final Version of Bayes Models in Writeup#############
##################################################################
#To run for any player, ctrl+f and replace the default player ID##
#with ID for player of interest. Change Titles in plots###########
#Default player ID is jordami01###################################
##################################################################

library(splines) #Used for bs function in the model
library(tidyverse) #Used for plotting and data management
library(brms) #Used for brm function to perform Bayesian regression
library(beepr) #Used to notify when model finishes

#Loading master dataset ~690,000 observations
#Columns are{
#player_id- in the form jordami01
#vorp- metric of interest
#year- used to fit the age curve
#type- factor variable, 'reg' for regular season and 'xpost' for postseason
#xpost is recorded to ensure that it is the output coefficient and reg is 
#the reference group (alphabetical)
#}
master <- read.csv('master.csv')

#Create subset with the player of interest
jordami01 <- master[master$player_id=='jordami01',]
#Sort data by year, then by index to ensure proper game order
jordami01 <- jordami01[order(jordami01$year, rownames(jordami01)), ]
#Reset index and create game index variable to fit the spline on
rownames(jordami01) <- NULL
jordami01$X <- as.numeric(rownames(jordami01))

#############Main Model#############
#vorp by B-Spline on game index with additive postseason effect
#These numbers for iterations, chains, and thinning converge for every player
bayes_mod <- brm(vorp ~ bs(X) + type, data=jordami01, family=gaussian(),
                 iter=55000, warmup=5000, chains=4, thin=5, cores=4); beep()
summary(bayes_mod)

#Creating newdata across the range of Xs and both season types
newdf <- expand.grid(
  X = seq(min(sorted_data$X),max(sorted_data$X), length.out=1000),
  type = c('reg','xpost')
)

#Generating predictions and 95% credible intervals
preds <- posterior_epred(bayes_mod, newdata = newdf)
newdf$predicted <- colMeans(preds)
newdf$lower <- apply(preds, 2, quantile, probs = 0.025)
newdf$upper <- apply(preds, 2, quantile, probs = 0.975)

#Plotting estimates with credible bands and observed data points included
ggplot() +
  geom_point(data = jordami01, aes(x=X,y=vorp,color=type), alpha=0.2)+
  geom_line(data = newdf, aes(x=X,y=predicted,color=type), size=1.2)+
  geom_ribbon(data = newdf, aes(x=X,ymin=lower,ymax=upper,fill=type), alpha=0.5)+
  theme_minimal()+
  ggtitle("Michael Jordan")

#Extract and compile posterior chains
samps <- as_draws(bayes_mod)
samps <- bind_rows(samps$`1`,samps$`2`,samps$`3`,samps$`4`)

#Density plot for posterior distribution of playoff effect with vline @0
ggplot() +
  geom_density(data = samps, aes(x=b_typexpost))+
  theme_minimal()+
  ggtitle("Michael Jordan Posterior Draws")+
  xlab("Postseason Effect")+
  ylab("Density")+
  geom_vline(xintercept = 0, linetype="dashed")
