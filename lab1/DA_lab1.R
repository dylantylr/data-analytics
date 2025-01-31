# Load necessary libraries
library(ggplot2)

# Read CSV file
EPI_data <- read.csv("C:/Users/taylod9/Downloads/epi2024results06022024.csv")

attach(EPI_data) # sets the ‘default’ object 

EPI.new # prints out values EPI_data$EPI.new

NAs <- is.na(EPI.new) # records True values if the value is NA 

EPI.new.noNAs <- EPI.new[!NAs] # filters out NA values, new array 

summary(EPI.new) # stats

fivenum(EPI.new,na.rm=TRUE) 

stem(EPI.new) # stem and leaf plot 

hist(EPI.new) 

hist(EPI.new, seq(20., 80., 1.0), prob=TRUE) 

lines(density(EPI.new,na.rm=TRUE,bw=1.)) 

rug(EPI.new)

boxplot(EPI.new, APO.new) 

hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)

lines(density(EPI.new,na.rm=TRUE,bw=1.))

rug(EPI.new) 

hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)

lines(density(EPI.new,na.rm=TRUE,bw="SJ")) 

rug(EPI.new) 

x<-seq(20,80,1) 

q<- dnorm(x,mean=42, sd=5,log=FALSE) 

lines(x,q)

lines(x,.4*q) 

q<-dnorm(x,mean=65, sd=5,log=FALSE) 

lines(x,.12*q) 

#exercise 2
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE)
qqnorm(EPI.new); qqline(EPI.new) 
qqplot(rnorm(250), EPI.new, xlab = "Q-Q plot for norm dsn") 
qqline(EPI.new)
qqplot(rt(250, df = 5), EPI.new, xlab = "Q-Q plot for t dsn") 
qqline(EPI.new)

#2a


APO.new <- as.numeric(EPI_data$APO.new)
WRS.new <- as.numeric(EPI_data$WRS.new)

# Remove NAs
APO.new.noNAs <- APO.new[!is.na(APO.new)]
WRS.new.noNAs <- WRS.new[!is.na(WRS.new)]

# Summary statistics
summary(APO.new)
summary(WRS.new)

# Five-number summary
fivenum(APO.new, na.rm=TRUE)
fivenum(WRS.new, na.rm=TRUE)

# Stem-and-leaf plots
stem(APO.new)
stem(WRS.new)

# Histograms
hist(APO.new)
hist(WRS.new)

# Histograms with density overlays
hist(APO.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(APO.new, na.rm=TRUE, bw=1.))
rug(APO.new)

hist(WRS.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(WRS.new, na.rm=TRUE, bw=1.))
rug(WRS.new)

# Boxplots
boxplot(APO.new, WRS.new, names=c("APO.new", "WRS.new"), main="Boxplots of APO and WRS Scores")

# Cumulative density function (CDF)
plot(ecdf(APO.new), do.points=FALSE, verticals=TRUE, main="CDF of APO Scores")
plot(ecdf(WRS.new), do.points=FALSE, verticals=TRUE, main="CDF of WRS Scores")

# Q-Q Plots for normality
qqnorm(APO.new); qqline(APO.new)
qqnorm(WRS.new); qqline(WRS.new)

# Q-Q Plots against normal distribution
qqplot(rnorm(250), APO.new, xlab = "Q-Q plot for norm dsn (APO)")
qqline(APO.new)

qqplot(rnorm(250), WRS.new, xlab = "Q-Q plot for norm dsn (WRS)")
qqline(WRS.new)

# Q-Q Plots against t-distribution (df=5)
qqplot(rt(250, df = 5), APO.new, xlab = "Q-Q plot for t dsn (APO)")
qqline(APO.new)

qqplot(rt(250, df = 5), WRS.new, xlab = "Q-Q plot for t dsn (WRS)")
qqline(WRS.new)

