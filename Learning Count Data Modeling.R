# maybe get to this page one day: https://opal.latrobe.edu.au/articles/media/Generalised_linear_models_and_the_analysis_of_count_data/13060157?file=24989240
# good link on distribution visualization: https://statdist.com/distributions/poisson

# install countreg library to have count datasets readily available to work with!
install.packages("countreg", repos="http://R-Forge.R-project.org", type="source")

# Setting Options
options(digits = 3)
options(scipen = 9)

#---- Importing libraries ----
lapply(c("tidyverse", "skimr", "AER", "ggplot2", "psych", "MASS"), 
       require, 
       character.only = T)

#---- Importing some datasets ----
aadt  <- read.csv("RoadInventorysample.csv")
aadt <- aadt[complete.cases(aadt$AADT),]
var(aadt$AADT) / mean(aadt$AADT)


#---- Checking var/mean for dataset to help in Poisson modeling ----
data("EquationCitations")
?EquationCitations
for (i in 5:13){
     print(var(EquationCitations[,i]) / mean(EquationCitations[,i]))
}
hist(EquationCitations$pages)
var(EquationCitations$pages) / mean(EquationCitations$pages)

data("PhDPublications")
var(PhDPublications$articles) / mean(PhDPublications$articles)
hist(PhDPublications$articles)
boxplot(PhDPublications$articles)
table(PhDPublications$articles)

# let's go with the number of articles PhD students from the PhDPublications dataset

# ---- Descriptive Statistics and EDA ----
data("PhDPublications")
str(PhDPublications)
glimpse(PhDPublications)
describe(PhDPublications)
summary(PhDPublications)
skim(PhDPublications)

PhDPublications %>% 
     group_by(married, gender) %>% 
     skim(articles)                     # can leave it empty to get stats for all variables

PhDPublications %>% 
     group_by(married, gender) %>% 
     summarise(Minimum = min(articles),
               Maximum = max(articles),
               Average = mean(articles),
               Median = median(articles),
               Range = max(articles) - min(articles),
               Variance = var(articles),
               StDev = sd(articles),
               IQR = IQR(articles),
               Counts = n(),
               VarOverMean = var(articles)/mean(articles),
               .groups = 'drop')

table(PhDPublications$articles)


# Pairwise plot of all variables and some correlation between them
plot(PhDPublications)
cor(PhDPublications$articles, PhDPublications$prestige)
cor(PhDPublications$articles, PhDPublications$prestige, method = "spearman")
cor(PhDPublications$articles, PhDPublications$prestige, method = "kendall")

# ---- Some histograms to see how article count is distributed
# Histogram of number of article in the dataset
hist(PhDPublications$articles, 
     breaks = 30,
     main = "Frequency of Article Counts",
     col = 2,
     xlab = "Number of Articles")

# Histogram of number of article in the dataset grouped by gender and color coded by marital status
ggplot(PhDPublications, 
       aes(articles)) +
     geom_bar(aes(fill=married), 
              width = 0.8,
              col = 'black') + 
     theme_bw() + 
     labs(title="Histogram of Number of articles by gender and marital status") +
     facet_wrap(~gender,
                ncol = 1)

# Histogram of number of article published by married females
PhDPublications %>% 
     ggplot(aes(x = articles,
                fill = 'brown')) +
     geom_histogram(binwidth = 1,
                    col = 'black',
                    show.legend = F,
                    alpha = 0.6) +
     labs(title = "Histogram of number of article by gender and marital status",
          x = "Articles",
          y = "Frequency") +
     facet_wrap(gender ~ married,
                nrow = 1)

boxplot(articles ~ gender + married, 
        PhDPublications,
        col = "gold2")

#---- GLM ----
# Single variable
Poisson.fit1 = glm(articles ~ prestige, 
                   data = PhDPublications,
                   family = "poisson") ; summary(Poisson.fit1)


Poisson.fit2 = glm(articles ~ . + gender*married - prestige, 
                   data = PhDPublications,
                   family = "poisson") ; summary(Poisson.fit2)



# based on the (talk about dispersion) we know not to trust the std errors to decide what's statistically significant (talk details). 



##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
rm(list = ls()) ; dev.off() ; plot.new()
##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@