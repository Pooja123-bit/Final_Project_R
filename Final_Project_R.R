install.packages("tidyverse")
library(tidyverse)
library(stringr)
library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
install.packages("osmdata")
library(ggmap)
library(osmdata)
library(datasets)
getwd()


##Reading-in data
a<-read.csv('HTL-Reg-SnailLength.csv')
a

##Application of the appropriate data storage structure: [2 Points]
##list
l<-list(a)
l

##data frame
a.df<-as.data.frame(a)
a.df

##matrix 
m<-as.matrix(a)
m


##Example of indexing (indexing 2nd row 6th column) [2 points]
a[2,6] 

##Subsetting "SSA" from Habitat variable (SP SSA TSA TSP) [2 points]
unique(a$Habitat)
#Subsetting
sub<-subset(a,Habitat=="SSA")
sub

##(1) Ordering [2 points]
a.ord=a[order(a$ShellHeight),]
a.ord

#(2) Ordering/sorting
a.arrange <- arrange(a, ShellHeight, Site)
a.arrange

##(1) Summarizing [5 points]
sum1=tapply(X=a$ShellHeight,INDEX=list(a$Habitat),FUN=fivenum)
sum1

##(2) Summarizing "SSA" subset of Habitat data
sum2=summary(a[a$Habitat=="SSA",]$SnailID/a[a$Habitat=="SSA",]$ShellHeight)
sum2


##Merge or Join data frames [5 points]
#Mean of ShellHeiight
mean.SH<-tapply(X=a$ShellHeight,INDEX=list(a$Quadrat),FUN=mean)
mean.SH
#convert object to data frame-creating a dataframe from mean of ShellHeight
mean.df = data.frame(Quadrat = names(mean.SH), mean = as.numeric(mean.SH))
mean.df

#Variance of ShellHeight
var.SH<-tapply(X=a$ShellHeight,INDEX=list(a$Quadrat),FUN=var)
var.SH
#convert object to data frame-creating a dataframe from Variance of ShellHeight
var.df = data.frame(Quadrat = names(var.SH), var = as.numeric(var.SH))
var.df

#merging the two data frames of Mean and Variance of ShellHeight
merge<-merge(x=mean.df,y=var.df, by="Quadrat")
merge

#Join
Join<-full_join(mean.df, var.df, by="Quadrat")
Join


##Custom Functions [10 points]
switcheroo.if.then<- function(x) {
  if (x =="SP")
    "Spartina patens"
  else if (x =="SSA")
    "Stunted Spartina alternifora"
  else if (x =="TSA")
    "Tall Spartina alternifora"
  else
    "Transitional Spartina patens"
}
switcheroo.if.then("SP")
switcheroo.if.then("SSA")
switcheroo.if.then("TSA")
switcheroo.if.then("TSP")


##Custom operator(s) [10 points]
'%pooja%'<-function(x,y){2*x + 3*y}
4%pooja%5


##‘ddply’ & 'if else' [10 + 10 points]
a.if<-a[a$Habitat=="SP",]
a.H<- ddply(.data=a, .variables="Habitat",function(x){
H<-unique(x$Habitat)
#ifelse(test=H=="SP", 
Habitat_condition<-function(y){
    if(H=="SP")
      q<-"Spartina patens"
    else if(H=="SSA")
      q<-"Stunted Spartina alternifora"
    else if(H=="TSP")
      q<-"Tall Spartina alternifora"
    else
      q<-"Transitional Spartina patens"
  }
  x$Habitat_H<-Habitat_condition(y=H)
  return(x)
}, .inform=T,.progress = "text")

##Reshaping data with ‘melt’ and/or ‘dcast’ (5 points)
melt1<-melt(data=a,id.vars=c("Quadrat","Habitat"), measure.vars=c("ShellHeight"))
melt1

cast1<-dcast(data=melt1,formula=Quadrat~variable, fun.aggregate=mean)
cast1

##For Loop converting um to m (10 points)
p = data.frame(1:10)
p$meter = NA
names(p) = c("micrometer","meter")
for(x in 1:10){
  p[x,]$meter = (p[x,]$micrometer *.000001)
}

##Histogram (5 points)
his<-ggplot(data = a, aes(x = ShellHeight)) +
   geom_histogram(binwidth = 5, color="black", fill="yellow")+
   facet_wrap(.~Habitat)
his

##Point, bar, or line plot (whichever makes the most sense) (5 points)
point<-ggplot(data = a, aes(x = ShellHeight, y=Habitat)) +
   geom_point(color="deeppink")
point

bar<-ggplot(data = a, aes(x=ShellHeight)) + 
   geom_bar(color="deeppink")+
   facet_grid(Habitat~.)
bar

line<- ggplot(data=a, aes(x=ShellHeight,y=Habitat)) + geom_line(color="deeppink")
line

##‘ggplot’ with at least 2 geoms (e.g. point, bar, tile), use one of the ‘scale_’ geoms,
#and adjusting the theme of the plot (10 points)
ggplot(data=a, aes(x=ShellHeight,y=Habitat)) + 
  geom_point(color="deeppink") +geom_tile()+
  scale_fill_continuous(type='viridis')+
  theme_bw()


##A map of showing the geographic location where the data was collected
getbb('Rhode Island')
map.RI = get_stamenmap(bbox = getbb('Rhode Island'),
                             zoom = 9, map = 'terrain')
ggmap(map.RI)
  
##Exporting data set
dir.create("C:/Users/pooja/OneDrive/Desktop/R/Final_Project_R")
  
##Exporting and saving figures from ggplot
ggsave("a.jpeg")

