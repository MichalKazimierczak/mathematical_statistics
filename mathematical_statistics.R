####################################################################################################
####################################################################################################
################This is a code for Mathematical Statistics with Resampling and R####################
####################################################################################################
####################################################################################################

#Chap 2: Exploratory Data Analysis


#Chapter 2.4
x <- c(21.7, 22.6, 26.1, 28.3, 30, 31.2, 31.5, 33.5, 34.7, 36)
qqnorm(x)   # plot points
qqline(x)   # add straight line

getwd()
NCBirths<-read.csv("datasets/data/NCBirths2004.csv")
qqnorm(NCBirths$Weight)
qqline(NCBirths$Weight)

#Chapter 2.5 Empirical cdf function
#R Note
x <- c(3, 6, 15, 15, 17, 19, 24)
plot.ecdf(x)
x <- rnorm(25)              # random sample of size 25 from N(0,1)
plot.ecdf(x, xlim = c(-4, 4))             # adjust x range
curve(pnorm(x), col = "blue", add = TRUE) # impose normal cdf

Beerwings<-read.csv("datasets/data/Beerwings.csv")
beerM <- subset(Beerwings, select = Beer, subset = Gender == "M",
                drop = T)
beerF <- subset(Beerwings, select = Beer, subset = Gender == "F",
                drop = T)

plot.ecdf(beerM, xlab = "ounces")
plot.ecdf(beerF, col = "blue", pch = 2, add = TRUE)
abline(v = 25, lty = 2)
legend(5, .8, legend = c("Males", "Females"),
       col = c("black", "blue"), pch = c(19, 2))


plot(Beerwings$Hotwings,Beerwings$Beer,xlab="Hot wings eaten",ylab="Beer consumed")
#--------------------------

####Excercises

x<-c(3,5,8,15,20,21)
mean(x)
median(x)

mean(log(x))
median(log(x))

x<-(1,2,4,5,6,8,11,15)
x<-sqrt(x)

fd<-read.csv("datasets/data/FlightDelays.csv")

table(fd$Day)
table(fd$Delayed30)
prop.table(table(fd$Day,fd$Delayed30),margin=1)
boxplot(FlightLength~Delayed30,data=fd)


gs<-read.csv("datasets/data/GSS2002.csv")


counts<-table(gs$DeathPenalty)

summary(gs$OwnGun)

table(gs$OwnGun)

prop.table(table(gs$OwnGun,gs$DeathPenalty),margin=1)

vis_miss(gs)

library(visdat)
vis_dat(gs)

gg_miss_var(gs)

##################################################################################################
##################Chapter 3#######################################################################
##################################################################################################

#-------------------------------------
##Chapter 3.3
##Beerwings
#Uncomment below if you haven't read in Beerwings yet
#Beerwings <- read.csv("Beerwings.csv")

tapply(Beerwings$Hotwings, Beerwings$Gender, mean)

observed <- 14.5333- 9.3333 #store observed mean differences    

#Get hotwings variable
hotwings <- subset(Beerwings, select=Hotwings, drop=T)

#set.seed(0)
B <- 10^5-1  #set number of times to repeat this process
result <- numeric(B) # space to save the random differences
for(i in 1:B)
{
  index <- sample(30, size=15, replace = FALSE) # sample of numbers from 1:30
  result[i] <- mean(hotwings[index]) - mean(hotwings[-index])
}

##Plot

hist(result, xlab = "xbarM - xbarF", main="Permutation distribution for hot wings")
abline(v = observed, col = "blue", lty=5)

#-------------------------
#Another visualization of distribution
dev.new()
plot.ecdf(result)
abline(v=observed,col="blue", lty=5)


#Compute P-value
(sum(result >= observed)+1)/(B + 1)  #P-value


#----------------------------------------
#Example 3.3 Verizon
#Permutation test

#Uncomment below if you haven't imported Verizon yet
#Verizon <- read.csv("Verizon.csv")

tapply(Verizon$Time, Verizon$Group, mean)


Time <- subset(Verizon, select=Time, drop=T)
Time.ILEC <- subset(Verizon, select=Time, Group=="ILEC", drop=T)
Time.CLEC <- subset(Verizon, select=Time, Group=="CLEC", drop=T)

observed <- mean(Time.ILEC)-mean(Time.CLEC)
observed


B <- 10^4-1  #set number of times to repeat this process
#set.seed(99)
result <- numeric(B) # space to save the random differences
for(i in 1:B)
{
  index <- sample(1687, size=1664, replace = FALSE) #sample of numbers from 1:1687
  result[i] <- mean(Time[index]) - mean(Time[-index])
}

hist(result, xlab = "xbar1 - xbar2", 
     main="Permutation Distribution for Verizon repair times")
abline(v = observed, col = "blue", lty=5)

(sum(result <= observed)+1)/(B + 1)  #P-value


#-------------------------------------------------------
#Example 3.5, Verizon cont.
#median, trimmed means

tapply(Verizon$Time, Verizon$Group, median)

#Difference in means
observed <- median(Time.ILEC)-median(Time.CLEC)
observed

#Differnce in trimmed means
observed2 <- mean(Time.ILEC, trim=.25)-mean(Time.CLEC,trim=.25)
observed2

B <- 10^4-1  #set number of times to repeat this process
#set.seed(99)
result  <- numeric(B) # space to save the random differences
result2 <- numeric(B)
for(i in 1:B)
{
  index <- sample(1687, size=1664, replace = FALSE) #sample of numbers from 1:1687
  result[i] <- median(Time[index]) - median(Time[-index])
  result2[i] <- mean(Time[index],trim=.25)-mean(Time[-index],trim=.25)
}

hist(result, xlab = "median1 - median2", 
     main="Permutation Distribution for medians")
abline(v = observed, col = "blue", lty=5)


dev.new()
hist(result2, xlab = "trimMean1 - trimMean2", 
     main="Permutation Distribution for trimmed means")
abline(v = observed, col = "blue", lty=5)

#P-value difference in means
(sum(result <= observed) + 1)/(B + 1)  

#P-value difference in trimmed means
(sum(result2 <= observed2) + 1)/(B + 1) 

#------------------------------------------------
#Example 3.5, Verzion continued
#
#difference in proportion of time > 10 
#and ratio of variances
observed <- mean(Time.ILEC > 10) - mean(Time.CLEC > 10)
observed

#ratio of variances
observed2 <- var(Time.ILEC)/var(Time.CLEC)

B <- 10^4-1  #set number of times to repeat this process
#set.seed(99)
result <- numeric(B)
result2 <- numeric(B)

for(i in 1:B)
{
  index <- sample(1687, size=1664, replace = FALSE)
  result[i] <- mean(Time[index] > 10) - mean(Time[-index] > 10)
  result2[i] <- var(Time[index])/var(Time[-index])
}

hist(result, xlab = "Difference in proportions",  main="Repair times > 10 hours")
abline(v = observed, lty=5, col = "blue")


dev.new()
hist(result2, xlab = "variance1/variance2",  main="Ratio of variances")
abline(v = observed, lty=5, col = "blue")


#P-value difference in proportion
(sum(result <= observed)+1)/(B + 1)  #P-value

#P-value ratio of variances
(sum(result2 <= observed2)+1)/(B + 1)  #P-value

#------------------------------------------------
#Chapter 3.4.1
#Here is a function that computes the chi-square
#test statistic

chisq<-function(Obs)
{ #Obs is the observed contingency table
  Expected <- outer(rowSums(Obs),colSums(Obs))/sum(Obs)
  sum((Obs-Expected)^2/Expected)
}


#-------------------------------------------
#Uncomment below if you haven't imported GSS2002 yet.
#GSS2002 <- read.csv("GSS2002.csv")

Education <- subset(GSS2002, select=Education, drop=T)
DeathPenalty <- subset(GSS2002, select=DeathPenalty,drop=T)
#Alternatively
#Education <- GSS2002$Education
#DeathPenalty <- GSS2002$DeathPenalty

table(Education, DeathPenalty)

#Use function created above to calcluate chi-square test statistic
observed <- chisq(table(Education,DeathPenalty))
observed

#Find those rows where there is at least one NA
index<- which(is.na(Education) | is.na(DeathPenalty))

#Remove those rows from the two variables and define Educ2 and
#DeathPenalty2 to be the new vectors with those rows removed
Educ2 <- Education[-index]
DeathPenalty2 <-  DeathPenalty[-index]

B <- 10^4-1
result<-numeric(B)

for (i in 1:B)
{
  DP.permutation <-sample(DeathPenalty2)
  GSS.table <- table(Educ2, DP.permutation)
  result[i]<-chisq(GSS.table) 
}

#Create a histogram
hist(result, xlab="chi-square statistic", main="Distribution of chi-square statistic",
     ylim=c(0,.2))
abline(v=observed,col="blue",lty=5)   


#optional: Create a histogram with the density curve 
#imposed onto the histogram 
#The prob=TRUE option below scales the histogram to have area 1
hist(result, xlab="chi-square statistic", main="Distribution of chi-square statistic",
     ylim=c(0,.2))
curve(dchisq(x, df=4), from=0, to= 25, col="green", add=T) 

#Compute P-value
(sum(result >= observed)+1)/(B + 1)  

#----------------------------------------------------------------

#Chapter 3.6
candy.mat <- rbind(c(42,20,38), c(33,27,50))
candy.mat

chisq.test(candy.mat)

#Chapter 3.8
Homeruns <- subset(Phillies2009, select=Homeruns, drop=T)
#alternatively
#Homeruns <- Phillies2009$Homeruns

lambda <- mean(Homeruns)
dpois(0:5, lambda)
table(Homeruns)

table(Homeruns)/162


#modified LMC 6/24/2011

