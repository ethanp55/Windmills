# Read in the windmill data
windmill <- read.table("/Users/mymac/Google_Drive/Statistics/Stat330/hw/hw1/Windmill.txt", 
                       header = TRUE)
head(windmill)


# Exploratory data analysis
scatter.smooth(windmill$RSpd, windmill$CSpd, pch = 20, xlab = "Reference Site 
               Windspeeds (m/s)", ylab = "Candidate Site Wind Speeds (m/s)", 
               main = "Windspeed Scatter Plot")

cov(windmill$RSpd, windmill$CSpd)
cor(windmill$RSpd, windmill$CSpd)


# Fit a simple linear regression model
windmill_slr <- lm(CSpd ~ RSpd, data = windmill)


# Extract the coefficients
summary(windmill_slr)
windmill_slr$coefficients


# Make a plot of the data with the fitted regression line
plot(windmill$RSpd, windmill$CSpd, xlab = "Reference Site Windspeeds (m/s)", 
     ylab = "Candidate Site Wind Speeds (m/s)", main = "Fitted Regression Line 
     on Windspeed Data", pch = 20)
abline(reg = windmill_slr, lwd = 3, col = "green")


# Use the fitted regression line to make a single prediction as an example (along 
#  with a prediction interval)
speeds_for_prediction <- c(12)
predict.lm(windmill_slr, newdata = data.frame(RSpd = speeds_for_prediction), 
           interval = "prediction",level = 0.95)


# Example of extrapolating
speeds_for_prediction <- c(30)
predict.lm(windmill_slr, newdata = data.frame(RSpd = speeds_for_prediction),
           interval = "prediction", level = 0.95)
