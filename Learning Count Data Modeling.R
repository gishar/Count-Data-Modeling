# maybe get to this page one day: https://opal.latrobe.edu.au/articles/media/Generalised_linear_models_and_the_analysis_of_count_data/13060157?file=24989240
# good link on distribution visualization: https://statdist.com/distributions/poisson

# install countreg library to have count datasets readily available to work with!
install.packages("countreg", repos="http://R-Forge.R-project.org", type="source")

# Setting Options
options(digits = 3)
options(scipen = 9)

# Importing libraries
lapply(c("tidyverse", "tidyverse", "AER", "ggplot2", "psych", "MASS"), require, character.only = T)

# Importing some datasets
aadt  <- read.csv("RoadInventorysample.csv")
aadt <- aadt[complete.cases(aadt$AADT),]
var(aadt$AADT) / mean(aadt$AADT)


# Trying to find some dataset with var/mean value close to 1 to fit Poisson model
library(MASS)
data(Boston)
hist(Boston$rm)
mean(Boston$rm) / var(Boston$rm)

library(countreg)
data("Insurance")
var(Insurance$Claims) / mean(Insurance$Claims)
var(epil$y) / mean(epil$y)

library(AER)
data("Affairs")
mean(Affairs$affairs) / var(Affairs$affairs)

data("DoctorVisits")
mean(DoctorVisits$visits) / var(DoctorVisits$visits)

data("ShipAccidents")
var(ShipAccidents$incidents) / mean(ShipAccidents$incidents)

data("CrabSatellites")
var(CrabSatellites$satellites) / mean(CrabSatellites$satellites)
hist(CrabSatellites$satellites)

data("CreditCard")
var(CreditCard$reports) / mean(CreditCard$reports)
hist(CreditCard$reports)

data("EquationCitations")
?EquationCitations
for (i in 5:13){
     print(var(EquationCitations[,i]) / mean(EquationCitations[,i]))
}
hist(EquationCitations$pages)
var(EquationCitations$pages) / mean(EquationCitations$pages)

data("GSS7402")
var(GSS7402$kids) / mean(GSS7402$kids)

data("Medicaid1986")
var(Medicaid1986$visits) / mean(Medicaid1986$visits)

data("NMES1988")
var(NMES1988$visits) / mean(NMES1988$visits)

data("PhDPublications")
var(PhDPublications$articles) / mean(PhDPublications$articles)
hist(PhDPublications$articles)
boxplot(PhDPublications$articles)
table(PhDPublications$articles)

# let's go with the number of articles PhD students from the PhDPublications dataset
data("PhDPublications")
?PhDPublications
glimpse(PhDPublications)

ggplot(PhDPublications, 
       aes(articles)) +
     geom_bar(aes(fill=married), 
              width = 0.8,
              col = 'black') + 
     theme(axis.text.x = element_text(angle=45, 
                                      vjust=0.6)) + 
     labs(title="Histogram of Number of articles by gender and marital status") +
     facet_wrap(~gender,
                ncol = 1)

PhDPublications %>% 
     group_by(married, gender) %>% 
     summarise(minimum = min(articles),
               maximum = max(articles),
               average = mean(articles),
               median = median(articles),
               difference2 = max(articles) - min(articles),
               variance = var(articles),
               StDev = sd(articles),
               IQR = IQR(articles),
               counts = n(),
               MyIndex = var(articles)/mean(articles))

glm(articles ~ ., 
    data = PhDPublications,
    family = "poisson") %>% 
     summary()

# based on the (talk about dispersion) we know not to trust the std errors to decide what's statistically significant (talk details). 



##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
rm(list = ls()) ; dev.off() ; plot.new()
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@