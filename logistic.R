mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
head(mydata)
mydata$rank <- factor(mydata$rank)
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
summary(mylogit)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -3.989979   1.139951  -3.500 0.000465 ***
# gre          0.002264   0.001094   2.070 0.038465 *  
#gpa          0.804038   0.331819   2.423 0.015388 *  
#rank2       -0.675443   0.316490  -2.134 0.032829 *  
#rank3       -1.340204   0.345306  -3.881 0.000104 ***
#rank4       -1.551464   0.417832  -3.713 0.000205 ***
exp(coef(mylogit))
#model evaluation ROC AUC 

---------------------------------------------------------------------
data=("mtcars")
mtcars
attach(mtcars)
mylogit2 <- glm(vs ~ wt + disp, data = mtcars, family = "binomial")
exp(coef(mylogit2))
hoslem.test(vs,fitted(mylogit2))

---------------------------------------------------------------------
prob=predict(mylogit2,type=c("response"))
mtcars$prob=prob
library(pROC)
h=roc(vs~prob,data=mtcars)
plot(h)
install.packages("pscl")
pR2(mylogit2)

---------------------------------------------------------------------
##multiregression problems

