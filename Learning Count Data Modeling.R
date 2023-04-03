# maybe get to this page one day: https://opal.latrobe.edu.au/articles/media/Generalised_linear_models_and_the_analysis_of_count_data/13060157?file=24989240
# good link on distribution visualization: https://statdist.com/distributions/poisson

lapply(c("tidyverse", "tidyverse"), require, character.only = T)
aadt  <- read.csv("RoadInventorysample.csv")
aadt <- aadt[complete.cases(aadt$AADT),]
hist(sample(aadt$AADT))
mean(aadt$AADT)
var(aadt$AADT)
qqnorm(aadt$AADT)
qqline(aadt$AADT, col = "blue", lwd  = 2)


library(MASS)
data(Boston)
?Boston
hist(Boston$rm)
qqnorm(Boston$rm)
qqline(Boston$rm, col = "blue", lwd  = 2)
mean(Boston$rm)
var(Boston$rm)

# install countreg library to have count datasets readily available to work with!
install.packages("countreg", repos="http://R-Forge.R-project.org", type="source")
library(countreg)
data("Insurance")

var(Insurance$Claims) / mean(Insurance$Claims)
library(countreg)
data()
view(epil)

var(epil$y)
mean(epil$y)

test = rpois(100, lambda = 3)
mean(test)
var(test)

library(AER)
data("Affairs")
?Affairs
mean(Affairs$affairs)
var(Affairs$affairs)

data("DoctorVisits")
mean(DoctorVisits$visits)
var(DoctorVisits$visits)

data("ShipAccidents")
mean(ShipAccidents$incidents)
var(ShipAccidents$incidents)

fitdistr(ShipAccidents$incidents, "poisson") %>% 
     summary()
