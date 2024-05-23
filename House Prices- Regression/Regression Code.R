# R Project- Part A



############################################Libraries To Download#########################################################

install.packages("rlang", repos = "http://cran.us.r-project.org")
install.packages("MASS", repos = "http://cran.us.r-project.org")
install.packages("fitdistrplus", repos = "http://cran.us.r-project.org")
install.packages("magrittr", repos = "http://cran.us.r-project.org")
install.packages("simmer", repos = "http://cran.us.r-project.org")
install.packages("simmer.plot", repos = "http://cran.us.r-project.org")
install.packages("dplyr", repos = "http://cran.us.r-project.org")
install.packages("lazyeval", repos = "http://cran.us.r-project.org")
install.packages("parallel", repos = "http://cran.us.r-project.org")
install.packages("e1071", repos = "http://cran.us.r-project.org")
install.packages("plotly", repos = "http://cran.us.r-project.org")
install.packages("ggplot2", repos = "http://cran.us.r-project.org")
install.packages("triangle", repos = "http://cran.us.r-project.org")
install.packages("sqldf", repos = "http://cran.us.r-project.org")
install.packages("knitr", repos = "http://cran.us.r-project.org")
install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
install.packages("readxl", repos = "http://cran.us.r-project.org")
install.packages("Hmisc")
install.packages("matrixStats")
install.packages("corrplot")
install.packages("ggcorrplot")
install.packages("fBasics")
install.packages("incidence")
install.packages("gapminder")
install.packages("ggplot")
install.packages("strucchange")



############################################Libraries To Run#########################################################

library(rlang)
library(MASS)
library(fitdistrplus)
library(magrittr)
library(dplyr)
library(lazyeval)
library(parallel)
library(e1071)
library(plotly)
library(ggplot2)
library(triangle)
library(sqldf)
library(readxl)
library(knitr)
library(rmarkdown)
library(simmer)
library(simmer.plot)
library(readxl)
library(Hmisc)
library(matrixStats)
library(corrplot)
library(ggcorrplot)
library(fBasics)
library(incidence)
library(ggplot2)
library(dplyr)
library(strucchange)


############################################Loading The Data#########################################################
dataset<-read.csv(file.choose(),header = T)
dataset_copy <- dataset

############################################QUESTION 2#########################################################

##........................................QUESTION 2.1...........................

#Check first variable that we might exclude- yr_built

fit_yr_built <- lm(formula = dataset$price ~ dataset$yr_built , data = dataset)
summary(fit_yr_built)
yr_built_plot <- plot(dataset$yr_built, dataset$price,
                      xlab="yr_built", ylab="price", pch=16, cex=0.7) # create the plot that connects the variables 
abline(fit_yr_built)


#Check second variable that we might exclude- sqft_lot
fit_sqft_lot <- lm(formula = dataset$price ~ dataset$sqft_lot , data = dataset)
summary(fit_sqft_lot)

yr_built_plot <- plot(dataset$sqft_lot, dataset$price,
                      xlab="sqft_lot", ylab="price", pch=16, cex=0.7) # create the plot that connects the variables 
abline(fit_sqft_lot)



#check categorical variable, show different box plots for each condition
plot(x=dataset$condition ,y=dataset$price, xlab="condition" , ylab="price")

ggplot(data = dataset, aes(x = condition, y = price)) +
  geom_boxplot() +
 labs(title = "Boxplot of Price by Category",
     x = "condition",
    y = "Price") +
facet_wrap(~condition, scales = "free_x", ncol = length(unique(dataset$condition)))
#  להשאיר או לא???? כי עשינו אותה דבר למעלה עם פחות קוד


##.......................................QUESTION 2.2............................

##Check if combining condition=4 and condition=5

#condition=4
datasetc4<-  sqldf("select CAST(price AS INTEGER) as price
                    from dataset
                    where condition = 4")

#condition=5
datasetc5<-  sqldf("select CAST(price AS INTEGER) as price
                    from dataset
                    where condition = 5")

#combining both and checking summary
combined_data_2 <- rbind(datasetc4, datasetc5)
summary(combined_data_2)

#data with combined_conditions
#dataset_combined_condition <- dataset

dataset_copy <- dataset_copy %>%
  mutate(new_condition = ifelse(condition == 5, 4, condition))


#..........fitting variables2.2 ...............

##Check if yr_built should become a categorical variable

yr_built_plot <- plot(dataset$yr_built, dataset$price,
                      xlab="yr_built", ylab="price", pch=16, cex=0.7) # create the plot that connects the variables 

##Check if sqft_house should become a categorical variable

sqft_basement_plot <- plot(dataset$sqft_basement, dataset$price,
                           xlab="sqft_basement", ylab="price", pch=16, cex=0.7) # create the plot that connects the variables 


#Changing  sqft_basement to a categorical variable with the options 1-3 (1-small, 2-medium, 3-big)

#sqft_basement_categorical <- dataset

dataset_copy <- dataset_copy %>%
  mutate(sqft_basement = case_when(sqft_basement <= 250 ~ "1",between(sqft_basement, 251, 1000) ~ "2",
                                   sqft_basement >= 1001 ~ "3",))

dataset_copy$sqft_basement <- as.integer(dataset_copy$sqft_basement)

sqft_basement_plot_new <- plot(dataset_copy$sqft_basement, dataset_copy$price,
                           xlab="sqft_basement", ylab="price", pch=16, cex=0.7) # create the plot that connects the variables 


#..........................................QUESTION 2.3...........................
dataset_factorial <- dataset_copy
#dummy variables for condition
dataset_factorial$new_condition <- factor(dataset_factorial$new_condition)  # Convert to factor if not already
dummy_vars_condition <- model.matrix(~ new_condition, data = dataset_factorial) # Create dummy variables 

#dummy variables for rooms
dataset_factorial$rooms <- factor(dataset_factorial$rooms)  # Convert to factor if not already
dummy_vars_rooms <- model.matrix(~ rooms, data = dataset_factorial) # Create dummy variables 

#dummy variables for floors
dataset_factorial$floors <- factor(dataset_factorial$floors)  # Convert to factor if not already
dummy_vars_floors <- model.matrix(~ floors, data = dataset_factorial) # Create dummy variables 

#dummy variables for renovated
dataset_factorial$renovated <- factor(dataset_factorial$renovated)  # Convert to factor if not already
dummy_vars_renovated <- model.matrix(~ renovated, data = dataset_factorial) # Create dummy variables 

#dummy variables for sqft_basement
dataset_factorial$sqft_basement <- factor(dataset_factorial$sqft_basement)  # Convert to factor if not already
dummy_vars_sqft_basement <- model.matrix(~ sqft_basement, data = dataset_factorial) # Create dummy variables 


#.......................................................2.4.......................


  # Renovated with sqft_house (and price) interaction
  coloring<-ifelse(dataset_copy$renovated=="0",c("blue"), c("red"))

  
  plot(x=dataset_copy$sqft_house, y=dataset_copy$price, xlab="sqft_house", ylab="price", col=coloring)
  legend("topleft", legend=c('0','1'),
         col=c("blue", "red"), lty=1, cex=0.7,
         title="renovated", text.font=2)
  abline(lm(dataset_copy$price~dataset_copy$sqft_house,data=dataset_copy,subset=coloring=="blue"),col="blue")
  abline(lm(dataset_copy$price~dataset_copy$sqft_house,data=dataset_copy,subset=coloring=="red"),col="red")

  model<-lm(formula = dataset_copy$price ~ dataset_copy$sqft_house * factor(dataset_copy$renovated))
  summary(model)
  
  
  # Renovated with yr_built (and price) interaction
  coloring<-ifelse(dataset_copy$renovated=="0",c("blue"), c("red"))


  plot(x=dataset_copy$yr_built, y=dataset_copy$price, xlab="yr_built", ylab="price", col=coloring)
  legend("topleft", legend=c('0','1'),
         col=c("blue", "red"), lty=1, cex=0.7,
         title="renovated", text.font=2)
  abline(lm(dataset_copy$price~dataset_copy$yr_built,data=dataset_copy,subset=coloring=="blue"),col="blue")
  abline(lm(dataset_copy$price~dataset_copy$yr_built,data=dataset_copy,subset=coloring=="red"),col="red")
  
  model<-lm(formula = dataset_copy$price ~ dataset_copy$yr_built * factor(dataset_copy$renovated))
  summary(model)
  
  # sqft_basement with sqft_house (and price) interaction
  
  coloring2 <- ifelse(dataset_copy$sqft_basement == "1", "blue", 
              ifelse(dataset_copy$sqft_basement == "2", "red", "green"))  
  
  plot(x=dataset_copy$sqft_house, y=dataset_copy$price, xlab="sqft_house", ylab="price", col=coloring2)
  legend("topleft", legend = c("1", "2", "3"),
         col = c("blue", "red", "green"), pch = 16,
         title = "sqft_basement", cex = 1.2)
  abline(lm(dataset_copy$price~dataset_copy$sqft_house,data=dataset_copy,subset=coloring2=="blue"),col="blue")
  abline(lm(dataset_copy$price~dataset_copy$sqft_house,data=dataset_copy,subset=coloring2=="red"),col="red")
  abline(lm(dataset_copy$price~dataset_copy$sqft_house,data=dataset_copy,subset=coloring2=="green"),col="green")
  
  model<-lm(formula = dataset_copy$price ~ dataset_copy$sqft_house * factor(dataset_copy$sqft_basement))
  summary(model)
  
  
  # sqft_basement with yr_built (and price) interaction
  
  coloring2 <- ifelse(dataset_copy$sqft_basement == "1", "blue", 
                      ifelse(dataset_copy$sqft_basement == "2", "red", "green"))  
  
  plot(x=dataset_copy$yr_built, y=dataset_copy$price, xlab="yr_built", ylab="price", col=coloring2)
  legend("topleft", legend = c("1", "2", "3"),
         col = c("blue", "red", "green"), pch = 16,
         title = "sqft_basement", cex = 1.2)
  abline(lm(dataset_copy$price~dataset_copy$yr_built,data=dataset_copy,subset=coloring2=="blue"),col="blue")
  abline(lm(dataset_copy$price~dataset_copy$yr_built,data=dataset_copy,subset=coloring2=="red"),col="red")
  abline(lm(dataset_copy$price~dataset_copy$yr_built,data=dataset_copy,subset=coloring2=="green"),col="green")
  
  model<-lm(formula = dataset_copy$price ~ dataset_copy$yr_built * factor(dataset_copy$sqft_basement))
  summary(model)
  
  
  
  
#####################################################3.1###################################################
  
  #Full Model
  FM<-lm(formula=dataset_factorial$price~dataset_factorial$rooms+dataset_factorial$sqft_house+dataset_factorial$sqft_lot
         +dataset_factorial$sqft_basement*dataset_factorial$sqft_house
         +dataset_factorial$floors+dataset_factorial$new_condition+
         +dataset_factorial$yr_built+dataset_factorial$renovated*dataset_factorial$sqft_house
         +dataset_factorial$renovated*dataset_factorial$yr_built)
  
  summary(FM)
  
  EMP<-lm(formula=dataset_factorial$price~1,data=dataset_factorial)
  
  ##forward algorithem
  FS <- step(EMP,direction = "forward",scope=formula(FM))
  summary(FS)
  
  ##backword algorithem
  BS <- step(FM,direction = "backward")
  summary(BS)
  

######################################################3.2#############################################
  
  #------residual
  Predicted <- predict(BS)
  unstandardizedResiduals <- resid(BS)
  Residuals <- (unstandardizedResiduals - mean(unstandardizedResiduals)) / sd(unstandardizedResiduals)
  plot(Predicted, Residuals, main = "Residuals vs.Fitted Plot", xlab = "Fitted Values", ylab = "Standardized Residuals", col="red")
  abline(0,0, col="blue")
  
  
  
  #----QQplot and histogram: Normal assumption
  dataset_copy$fitted<-fitted(BS) 
  dataset_copy$residuals<-residuals(BS) 
  se_res <- sqrt(var(dataset_copy$residuals))
  dataset_copy$stan_residuals<-(residuals(BS)/se_res)
  qqnorm(dataset_copy$stan_residuals)
  abline(a=0, b=1)
  hist(dataset_copy$stan_residuals,prob=TRUE, main="Histogram of normalized error", xlab ="Normalized error",ylab = 'Frequency',col="orange")
  lines(density(dataset_copy$stan_residuals),col="red",lwd=2)
  #----------------------KS : Normal assumption 
  

  ######################################################3.3#############################################
  
  
  #-------Chow test
  sctest(BS, type="Chow")
  
  #-------KS test
  ks.test(x= dataset_copy$stan_residuals,y="pnorm",alternative = "two.sided", exact = NULL)
  
  #----Shapirto test
  shapiro.test(dataset_copy$stan_residuals)
  
  
  
  