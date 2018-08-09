## ------------------------------------------------------------------------
airline.df <- read.csv(paste("SixAirlinesData.csv", sep=""))
attach(airline.df)

## ------------------------------------------------------------------------
library(psych)
describe(airline.df)[,c(2,3,4,5,8,9)]

## ------------------------------------------------------------------------
##  Price Premium versus Price Economy
plot(~PriceEconomy + PricePremium, main="Premium Economy Price vs. Economy Price")
abline(0,1)

## ------------------------------------------------------------------------
pitchDifferenceTable <- table(airline.df$PitchDifference)
pitchDifferenceTable

library(lattice)
histogram(~PitchDifference, data = airline.df,
 main = "Distribution of Pitch Difference", xlab="Difference in Pitch", col='gray' ) 


## ------------------------------------------------------------------------
pd = aggregate(cbind(PriceEconomy,PricePremium, PriceRelative) ~ PitchDifference, 
                   data = airline.df, mean)
pd
library(car)
scatterplot(pd$PitchDifference, pd$PriceRelative , main="Relative Price Difference vs. Pitch", xlab="Pitch Difference", ylab="Relative Price b/w Economy and Premium Economy")



## ------------------------------------------------------------------------
boxplot(PriceRelative~PitchDifference,data=airline.df, main="Relative Price Difference vs. Pitch", ylab="Pitch Difference", xlab="Relative Price b/w Economy and Premium Economy", horizontal=TRUE)

## ------------------------------------------------------------------------
widthDifferenceTable <- table(airline.df$WidthDifference)
widthDifferenceTable


library(lattice)
histogram(~WidthDifference, data = airline.df,
 main = "Distribution of Difference in Seat Width", xlab="Difference in Seat Width", col='gray' ) 

## ------------------------------------------------------------------------
aggregate(cbind(PriceEconomy,PricePremium, PriceRelative) ~ WidthDifference, 
                   data = airline.df, mean)

## ------------------------------------------------------------------------
boxplot(PriceRelative~WidthDifference,data=airline.df, main="Relative Price Difference vs. Seat Width", ylab="Seat Width Difference", xlab="Relative Price b/w Economy and Premium Economy", horizontal=TRUE)

## ------------------------------------------------------------------------
pitchWidthTable <- xtabs(~WidthDifference + PitchDifference, data=airline.df)
ftable(pitchWidthTable) # print table

## ------------------------------------------------------------------------
library(vcd)
mosaic(pitchWidthTable, shade=TRUE, legend=TRUE, main=" Dist. of Diff. in Pitch and Seat Width")


## ------------------------------------------------------------------------
t1 = aggregate(cbind(PriceEconomy,PricePremium, PriceRelative) ~ SeatsTotal, 
                   data = airline.df, mean)
t1

xyplot(PriceRelative ~ SeatsTotal, data = airline.df
       ,type = c("p", "g"),
       xlab = "Total Seats (Economy + Premium Economy Seats)", ylab = "Rel. Price Difference"
       )


## ------------------------------------------------------------------------
boxplot(airline.df$PercentPremiumSeats, data=airline.df, main="Percentage of Premium Economy Seats", 
  	xlab="Percentage of Premium Economy Seats in Plane", ylab="", horizontal=TRUE )

## ------------------------------------------------------------------------
xyplot(PriceRelative ~ PercentPremiumSeats, data = airline.df
       ,type = c("p", "g"),
       xlab = "Percentage of Premium Economy Seats in Plane", ylab = "Rel. Price Difference"
       )

## ------------------------------------------------------------------------
# Scatterplot Matrices from the car Package
library(car)
scatterplotMatrix(~PricePremium+PriceEconomy+PitchDifference+WidthDifference, data=airline.df,
  	main="Premium Economy vs. Economy Airfares")
scatterplotMatrix(~PricePremium+PriceEconomy+SeatsTotal+PercentPremiumSeats, data=airline.df,
  	main="Premium Economy vs. Economy Airfares")

## ------------------------------------------------------------------------
library(Hmisc)
colairlines <- c("PricePremium","PriceEconomy","PitchDifference","WidthDifference")
corMatrix <- rcorr(as.matrix(airline.df[,colairlines]))
corMatrix


colairlines2 <- c("PricePremium","PriceEconomy","SeatsTotal","PercentPremiumSeats")
corMatrix2 <- rcorr(as.matrix(airline.df[,colairlines2]))
corMatrix2

## ------------------------------------------------------------------------
library(Hmisc)
library(car)
library(corrgram)
colairlines <- c("PricePremium","PriceEconomy","PitchDifference","WidthDifference","SeatsTotal","PercentPremiumSeats")
corrgram(airline.df[,colairlines], order=TRUE,
         main="Premium Economy vs. Economy Airfares",
         lower.panel=panel.pts, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)


## ------------------------------------------------------------------------
t.test(PricePremium,PriceEconomy)

## ------------------------------------------------------------------------
t.test(PitchPremium, PitchEconomy)

## ------------------------------------------------------------------------
t.test(WidthPremium, WidthEconomy)

