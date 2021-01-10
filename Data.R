---
title: "R Notebook"
output: html_notebook
---


# Please install the following packages.



library(PortfolioAnalytics)
library(openxlsx)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)
library(ROI)
library(tidyquant) # tq_get
library(dplyr) # group_by 
library(timetk)
library(forcats)# fct_reorder
library(tidyr) # spread()
install.packages('plyr')
library('fastDummies')
library(magrittr)
library(tidyverse)
library(rlist)
library(pipeR)
library(float)
library(coop)
library(mapply)
library(tm)
library(purrr)
library(data.table)
library(plyr)


# Set the working directory, this is where our files will be saved finally. 
setwd("C:/Users/DEEXITH REDDY/Desktop")

# Read the training data given by the Appreciate Team:
data1 = read.xlsx("C:/Users/DEEXITH REDDY/Desktop/Apprentice/20201022-Training Dataset_v1(1).xlsx",sheet=2)
data2 = read.xlsx("C:/Users/DEEXITH REDDY/Desktop/Apprentice/20201022-Training Dataset_v1(1).xlsx",sheet=3)
data3 = read.xlsx("C:/Users/DEEXITH REDDY/Desktop/Apprentice/20201022-Training Dataset_v1(1).xlsx",sheet=1)

##Creating demographic data using dummy and categorial
data3 <- dummy_cols(data3, select_columns = 'Gender')
data3 = select(data3, -Gender)
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}
data3[["city_encoded"]] <- encode_ordinal(data3[["City"]])
data3 = select(data3, -City)
rownames(data3) <- data3$Customer_ID
data3 = select(data3, -Customer_ID)

##Making nested lists of including demographic data Eg. [37,50000,4,1,0,5]
d<-list()

for (i in 1:1000){
  h<-list()
  h<-list(as.numeric(data3[i,]))
  d<-append(d,h)
}


##Setting list of tickers
k<-c()
for (i in 1:1000){
 c<-c()
 c<-c(data1 %>% filter(Customer_ID == i) %>% select(Ticker))
 k<-append(k,c)}




data5 = subset(data1,data1$Customer_ID>0 & data1$Customer_ID<2)
data5=data1[data1$Customer_ID==1,]

##Recommender function using cosine similarity between customers based on the demographic data.
##Gives out one nearest customer's similar stock

recommender <- function(n){
 
 c<-c()
 e<-c()
 f<-c()
 for (j in 1:1000){
  if (j!=n) {
   cos_sim=coop::cosine(unlist(d[n]),unlist(d[j]))
   c<-c(c,cos_sim)
   f<-c(f,j)
   e<-c(e,k[j])
  } 
 }
 y<-mapply(c,c,e, SIMPLIFY=F)
 y<-y[order(-c)]
 x <- y[1]
 x<-map(x, tail, -1)
 x<-unlist(x)
 return(sample(x,5))

}

v<-recommender(3)

##Recommender function using cosine similarity between customers based on the demographic data.
##Gives out the stocks for nearest customer when a new customer enters information in the format: 
##  [Age, Income,Risk Score, Isfemale?(0 or 1), IsMale?(0 or 1), City code (1 to 6)]

recommender1 <- function(p,q,r,s,t,u){
 
 
 c<-c()
 e<-c()
 a<-c(p,q,r,s,t,u)
 for (i in 1:1000){
   cos_sim=coop::cosine(a,unlist(d[i]))
   c<-c(c,cos_sim)
   e<-c(e,k[i])
  
 }
 y<-mapply(c,c,e, SIMPLIFY=F)
 y<-y[order(-c)]
 x <- y[1]
 x<-map(x, tail, -1)
 x<-unlist(x)
 return(sample(x,5))

}
recommender1(35,50000,4,1,0,2)
a<-c(37,50000,3,1,0,5)
cos_sim=coop::cosine(a,unlist(d[2]))
print(cos_sim)
 
##Recommender function using cosine similarity between customers based on the demographic data.
##Gives out m nearest customers similar stock preference. n is customer ID and m is the number of similar customers

recommenders <- function(n,m){
  
  c<-c()
  e<-c()
  f<-c()
  for (j in 1:1000){
    if (j!=4) {
      cos_sim=coop::cosine(unlist(d[n]),unlist(d[j]))
      c<-c(c,cos_sim)
      f<-c(f,j)
      e<-c(e,k[j])
    } 
  }
  y<-mapply(c,c,e, SIMPLIFY=F)
  y<-y[order(-c)]
  w<-c()
  for (a in 1:m){
  x <- y[a]
  x<-map(x, tail, -1)
  x<-unlist(x)
  w<-c(w,x)
  }
  w<-unlist(w)
  return(sample(w,5))
  
}

##Recommender function using cosine similarity between customers based on the demographic data.
##Gives out the stocks for nearest customer when a new customer enters information in the format: 
##  [Age, Income,Risk Score, Isfemale?(0 or 1), IsMale?(0 or 1), City code (1 to 6)], based on the number of similar closest customers


recommenders1 <- function(p,q,r,s,t,u,m){
  
  
  c<-c()
  e<-c()
  a<-c(p,q,r,s,t,u)
  for (i in 1:1000){
    cos_sim=coop::cosine(a,unlist(d[i]))
    c<-c(c,cos_sim)
    e<-c(e,k[i])
    
  }
  y<-mapply(c,c,e, SIMPLIFY=F)
  y<-y[order(-c)]
  w<-c()
  for (a in 1:m){
    x <- y[a]
    x<-map(x, tail, -1)
    x<-unlist(x)
    w<-c(w,x)
  }
  w<-unlist(w)
  return(sample(w,5))
  
}

##Stores the stocks in an excel sheet in folder.

for (i in 1:2){
  
  g<-recommender(i)
  write.csv(g, file.path(paste0("Customer"," ",i),
                         "Demographic Suggestions.csv"), row.names=FALSE)
  
}

```


