install.packages("ggplot2") 
library("ggplot2")
library("plyr")
library("dplyr")
# 0 load data
titanic <- read.csv("C:\\Nilanjana\\Springboard\\titanic_original.csv", header = TRUE)
barplot(titanic$age)
boxplot(titanic$age)
titanic$cabin


#1 POrt of embarkation
titatinc_clean <- titanic %>% mutate(embarked = replace(embarked,embarked == " ","S"))%>%

#2 Age
# median value of age could be used to replace NA. In this case since the data is not normally distributed, 
#i.e. data is not centered around the mean, using median value of ages would be more appropriate.

            mutate(age = replace(age,is.na(age)== TRUE,round(mean(age,na.rm = TRUE)))) %>%

#3 Lifeboat

           mutate(boat = factor(ifelse(boat!="",boat,"NONE"))) %>%
  
#4 Cabin
# It does not make sense to replace missing Cabin numbers with a value because it is a categorical variable that suggests
# the cabin where the passenger would have stayed. We can no replace the mising valuse with Max(Cabin) as there is alimit 
# to the number of paasengers that could have stayed in one cabin and total number of missing values exceeds that limit.
# Moreover, missing Cabin Number could imply that passenger did not survive and thus the data could not be recorded.
  
            mutate(has_cabin_number = factor(ifelse((cabin) != "",1,0)))
           
  
  
#5 
    write.csv(titatinc_clean,"C:\\Nilanjana\\Springboard\\titanic_clean.csv")
