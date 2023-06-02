#Shital Bhandary
#2 June 2023
#Based on 21st Lecture of Statistical Computing with R course
#Masters in Data Science program, School of Mathematical Sciences
#Tribhuvan University, Kirtipur, Nepal

covid_tbl_final <- read.csv("C:/Users/Dell/Documents/covid19_nepal/data/covid_tbl_final.csv")
str(covid_tbl_final)

mean(covid_tbl_final$Confirmed_cases_total)
mean(covid_tbl_final$Confirmed_cases_new)
mean(covid_tbl_final$Confirmed._cases_active)
mean(covid_tbl_final$Recoveries_total)
mean(covid_tbl_final$Recoveries_daily)
mean(covid_tbl_final$Deaths_total)
mean(covid_tbl_final$Deaths_daily)
mean(covid_tbl_final$RT.PCR_tests_total)
mean(covid_tbl_final$RT.PCR_tests_daily)

#Cumulative Daily deaths
#There is a gap and we need to find why
plot(covid_tbl_final$SN, covid_tbl_final$Deaths_total,
     main = "Daily Deaths: 23 Jan 2020 - 31 May 2021",
     xlab = "Day since first case",
     ylab = "Cumulative Daily Deaths")

#Daily deaths
#There are problems as we can see three daily death values that are way more thann usual
#This happened as the death tally from MoHP and Nepali Army did not match
plot(covid_tbl_final$SN, covid_tbl_final$Deaths_daily,
     main = "Daily Deaths: 23 Jan 2020 - 31 May 2021",
     xlab = "Day since first case",
     ylab = "Daily Deaths")

#Cumulative deaths upto 398 cases i.e. 23 Feb 2021
#There are the cumulative daily deaths without adding the deaths reported by Army
plot.data <- covid_tbl_final[covid_tbl_final$SN <=398,-15]
plot(plot.data$SN, plot.data$Deaths_total,
     main = "Daily Covid Deaths, Nepal: 23 Jan - 23 Feb 2021",
     xlab = "Day since first case",
     ylab = "CumulativeDaily Deaths")

#We will fit polynomial models on this data now
#Linear model

#Linear model:
#R-square = 0.7921, F-test is statistically significant
#Coefficient is also statistically significant
lm <- lm(Deaths_total ~ SN, data = plot.data)
summary(lm)

#Plot with linear model
#Clearly shows the underfitting
plot(plot.data$SN, plot.data$Deaths_total,
     main = "Daily Covid Deaths, Nepal: 23 Jan - 23 Feb 2021",
     xlab = "Day since first case",
     ylab = "Cumulative Daily Deaths")
abline(lm(Deaths_total ~ SN, data = plot.data), col = "red", lwd=2)

#Quadratic Linear model
#R-squared = 0.9692, F-test is statistically significant
#Coefficients are also statistically significant
qlm <- lm(Deaths_total ~ poly(SN, 2, raw=T), data = plot.data)
summary(qlm)

#Plot with quadratic linear model
#Clearly shows the good fit but can we do it better
plot(Deaths_total ~ SN, data=plot.data,
     main = "Cumulative Covid Deaths, Nepal: 23 Jan - 23 Feb 2021",
     xlab = "Date",
     ylab = "Cumulative Deaths")
lines(fitted(qlm) ~ SN, data=plot.data, col="red", lwd=2)

#Cubic Linear model
#R-squared = 0.9699, F-test is statistically significant
#Coefficients are also statistically signfincat
clm <- lm(Deaths_total ~ poly(SN, 3, raw=T), data = plot.data)
summary(clm)

#Plot with cubic linear model
#Clearly shows the good fit but can we do it better?
plot(Deaths_total ~ SN, data=plot.data,
     main = "Cumulative Covid Deaths, Nepal: 23 Jan - 23 Feb 2021",
     xlab = "Date",
     ylab = "Cumulative Deaths")
lines(fitted(clm) ~ SN, data=plot.data, col="red", lwd=2)

#Double quadratic or 4th order linear model
#R-squared = 0.9934, F-test is statistically significant
#All the coefficients are also significant
dqlm <- lm(Deaths_total ~ poly(SN, 4, raw=T), data = plot.data)
summary(dqlm)

#Plot with fourth order polynomial model
#Clearly shows the good fit but can we do it better?
plot(Deaths_total ~ SN, data=plot.data,
     main = "Cumulative Covid Deaths, Nepal: 23 Jan - 23 Feb 2021",
     xlab = "Date",
     ylab = "Cumulative Deaths")
lines(fitted(dqlm) ~ SN, data=plot.data, col="red", lwd=2)

#Fifth order polynomial fit
#R-squared = 0.998, F-test is statistically significant
#All the coefficients are also statistically significant
folm <- lm(Deaths_total ~ poly(SN, 5, raw=T), data = plot.data)
summary(folm)

#Plot with fourth order polynomial model
#Clearly shows the good fit but can we do it better?
#Is this a overfitting?
#Does the cumulative death declined? NO!
plot(Deaths_total ~ SN, data=plot.data,
     main = "Cumulative Covid Deaths, Nepal: 23 Jan - 23 Feb 2021",
     xlab = "Date",
     ylab = "Cumulative Deaths")
lines(fitted(folm) ~ SN, data=plot.data, col="red", lwd=2)

#Based on the results obtained above the most likely/plausible model is the 4th order polynomial model

#Let us use the validation set approach in this model
#Creating partition variable ind with 70% and 30% probabilities
ind <- sample(2, nrow(plot.data), replace=T, prob=c(0.7,0.3))
#Creating training data with 70% cases
train.pd <- plot.data[ind==1,]
trest.pd <- plot.data[ind==2,]


#Fifth order polynomial fit on train data
#R-squared = 0.9931, F-test is statistically significant
#All the coefficients are also statistically significant
dqlm.train <- lm(Deaths_total ~ poly(SN, 4, raw=T), data = train.pd)
(dqlm.train.summary <- summary(dqlm.train))

#Plot with fourth order polynomial model of train data
plot(train.pd$Deaths_total ~ train.pd$SN,
     main = "Cumulative Covid Deaths, Nepal",
     xlab = "Date",
     ylab = "Cumulative Deaths")
lines(fitted(dqlm.train) ~ SN, data=train.pd, col="red", lwd=2)

#MSE and RMSE
(MSE.dqlm.train <- mean(dqlm.train$residuals^2))
(RMSE.dqlm.train <- sqrt(MSE.dqlm.train))

#Prediction with test data
prediction <- predict(dqlm.train, trest.pd)
library(caret)
(R2.test.pd <- R2(prediction, trest.pd$Deaths_total))
(RMSE.test.pd <- RMSE(prediction, trest.pd$Deaths_total))

#Comparing R-square and RMSE
fit_indices_train <- c(R2 = dqlm.train.summary$r.squared, RMSE = RMSE.dqlm.train)
fit_indices_test <- c(R2 = R2.test.pd, RMSE = RMSE.test.pd)
(fit_indices <- cbind(train=fit_indices_train, test=fit_indices_test))

#Prediction for new data
new_death <- data.frame(SN=seq(399,499,1))
predicted_cd <- predict(dqlm.train, newdata = new_death)
predicted_cd
#Plot
SNN = seq(1,101,1)
plot(SNN, predicted_cd, main="Prediction of cumulative daily deaths (101 days)", 
     xlab="Day (399 to 499 days)", ylab="Predicted Cumulative Daily Deaths")

#The question is "Does it make sense?"
#Did it happen like this in Nepal between 399 and 499 days since the first COVID19 case?
#Can we improve it further with cross-validation methods?
