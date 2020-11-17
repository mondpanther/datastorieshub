# a love letter

v1=runif(100)
v2=runif(100) + v1


plot(v1,v2)


#df=read.csv("https://www.dropbox.com/s/g1w75gkw7g91zef/foreigners.csv?dl=1")  


df=read.csv("data/foreigners.csv")  
getwd()

library(dplyr)

library(AER)
library(ggplot2)

