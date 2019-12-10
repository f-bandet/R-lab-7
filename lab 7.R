# Name: Faye Bandet
# Date: 10/10/19
# ISTA 116 Section B || Section Leader : Jacob Heller
# Lab Assignment 7
# Collaborator(s): Nick Ackerman

#1
die <- c(0.1667)
points <- c(100, 0, 0, 0, 50, 0)
dataf <- data.frame(prob = die, score = points)
dataf
#    prob score
#1 0.1667   100
#2 0.1667     0
#3 0.1667     0
#4 0.1667     0
#5 0.1667    50
#6 0.1667     0

#2
plot1 <- replicate(10000, {dataf[sample(1:6, 1, replace = TRUE), 2]})
hist(plot1)
# The histogram has three bars, there is potential for 0, 50, and 100, zero has the most probability (over 6000), 50 and 100 have the same probability (around 2000).

#3
mean(plot1)
temp = weighted.mean(points, c(0.1667, 0.1667, 0.1667, 0.1667, 0.1667, 0.1667))
# Mean is 25.63, weighted mean is 25. The mean does not differ because it is a standard function.

#4
sd(plot1)
sqrt(sum((dataf$score - temp)^2)*dataf$prob)
# The standard deviation is 38.52599, the random variable SD is 38.19195.It differs because the variable is random and not the standard data set.

#5
rolls <- c(.10, .10, .10, .10, .5, .10)
dataf2 <- data.frame(prob = rolls, score = points)
dataf2
# The prob column adds up to one.
#   prob score
#1  0.1   100
#2  0.1     0
#3  0.1     0
#4  0.1     0
#5  0.5    50
#6  0.1     0

#6
plot2 <- replicate(10000, {dataf2[sample(1:6, 1, dataf2$prob, replace = TRUE), 2]})
hist(plot2)
# Histogram has a lot more 50s than before because we increased the probability. Zero column is approximately 4000, 50 is around 5000,100 is about 1000.

#7 
mean(plot2)
var2 <- weighted.mean(points, c(.10, .10, .10, .10, .5, .10))
var2
# Sample weighted mean of var2 is 35, expected mean of plot2 is 24.26.

#8
plot3 <- replicate(10000, {dataf2[sample(1:6, 1, dataf2$prob, replace = TRUE), 2] + dataf[sample(1:6, 1, replace = TRUE), 2]})
hist(plot3)
# There are now 5 variables so it is not like the modes of 2 and 6 because there is more potential combinatons.

#9
mean(plot3)
mean(plot1) + mean(plot2)
# Mean of sample plot3 is 60.195, expected value of random sample is 61.205. The expected values are much higher for this game because there are more potential outcomes than the previous games. 

#10
plot4 <- replicate(10000, {dataf2[sample(1:6, 1, dataf2$prob, replace = TRUE), 2] + dataf2[sample(1:6, 1, dataf2$prob, replace = TRUE), 2] + dataf2[sample(1:6, 1, dataf2$prob, replace = TRUE), 2] + dataf[sample(1:6, 1, replace = TRUE), 2] + dataf[sample(1:6, 1, replace = TRUE), 2]})
hist(plot4/5)
expectedvalue <- (mean(plot1) + mean(plot1) + mean(plot2) + mean(plot2) + mean(plot2))/5
# The expected value is 31.597, using the standard formula for linear combinations of random variables. The games would all be fairly similar in probability (around 20) so you would end up with the most net points in any game, but if we didn't divide the loaded die by five there would be a net positive.
