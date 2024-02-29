
##### Linear Regression Functions #####
# The below functions are intended to be threaded into a linear regression analysis 
# Author: Trevor Seeger

##### How to use #####
# These functions will assume that there is a dataset, labelled dat here, but can be named whatever the user would like it each function. This dataset needs one variable to use as the Y or dependent variable, and can suit multiple X or independent variables. 

# Functions will also require a linear regression model. The set up for that object to be carried out is written below. 

#```{r}
#Linear regression with all variables
# formula2test = formula("y~x1 + x2 + x3")
# LinReg1 <- lm(formula2test, data = dat)
# summary(LinReg1)
# confint(LinReg1)
#```

# Y and X# are all seated as placeholders for other variable names. 

# These functions can be used as a companion to the "Regressions Functions.Rmd" file or as standalone functions


##### Testing Assumptions of Linear Regressions ####

LinReg.Linearity = function(model = LinReg1){
  par(mfrow=c(1,1))
  plot(fitted(model), residuals(model),xlab="Fitted Values", ylab="Residuals")
  abline(h=0,lty=1) # lty = linetype
  title("Residual vs Fitted")
}

LinReg.NormErrors = function(model = LinReg1) {
  par(mfrow=c(1,1)) # Setting to 1, 1 makes one figure per print out, not grouping them tightly
  
  #normal Q-Q plot
  plot(model, which = 2, add.smooth=F) 
  print("A normal group of residuals will fall approximately straight along the line")
  
  plot(density(model$res),main="density plot of residuals")
  abline(v=0)
  print("A normal group of residuals will appear approximately normal and symetrical across the zero line")
  
  
  plot(model, which = 1, add.smooth=F)
  print("A normal group of residuals will be relatively even vertical distances from the line as you progress horizontally")
}




LinReg.NormErrors.Shapiro = function(model = LinReg1, alpha = 0.05) {
  shapout = shapiro.test(residuals(LinReg1)) #h_0: a sample x1, ..., xn came from a normally distributed population
  
  if(shapout$p.value > alpha) {
    conclusion = ", therefore, we accept the Null Hypothesis, and the sample is normally distributed"
  } else {
    conclusion = ", therefore, we reject the Null Hypothesis, and the sample has a distribution that is not normal"
  }
  out = paste0(  
    # "h_0: a sample x1, ..., xn came from a normally distributed population",
    shapout$method, 
    ": W=",   round(unname(shapout$statistic), 2), 
    ", ",
    "p=", round(shapout$p.value, 3) , 
    conclusion
  )
  
  print(out)
  return(shapout)
}

LinReg.Variance = function(model = LinReg1, alpha = 0.05) {
  require(lmtest)
  par(mfrow=c(1,1))
  plot(fitted(model), residuals(model),xlab="Fitted Values", ylab="Residuals")
  abline(h=0,lty=1)
  title("Residual vs Fitted")
  
  
  
  # Heteroscedasticity test (Breusch-Pagan test)
  library(lmtest)
  bpout = bptest(model)
  
  if(unname(bpout$p.value) > alpha) {
    conclusion = ", therefore, we accept the Null Hypothesis, and the the variance of the error term is constant"
  } else {
    conclusion = ", therefore, we reject the Null Hypothesis, and the the variance of the error term is NOT constant"
  out = paste0(  
    bpout$method, 
    ": BP (", unname(bpout$parameter), ")=",   round(unname(bpout$statistic), 2), 
    ", ",
    "p=", round(bpout$p.value, 3) , 
    conclusion
  )
  print(out)
  return(bpout)
  }
}

LinReg.Indep = function(model = LinReg1) {
  plot(model, which=c(1, 2, 3, 4, 5, 6)) 
  
}


LinReg.Indep.Multico = function(model = LingReg1) {
  library(car)
  test = vif(model)
  # test[3] = 6
  
  # test>5
  # length(test)
  # names(test)[which(unname(test) > 5)]
  summary(model)
  print("If any of the Variance inflation factor (VIF) >5, we re-test the model without that variable to see if the beta's change by more than 10%")
  # formula2test
  for(retest in names(test)[which(unname(test) > 5)]) {
    model2temp = update(model, paste(".~.-",retest))
    print(paste("Removing ", retest))
    print(summary(model2temp))
  }
  
}



LinReg.Indep.InflPts = function(model = LinReg1, dat = dat, y, outlier.crit = 2, remove.outliers = FALSE, .formula2test = formula2test){
  par(mfrow=c(1,2))
  plot(model,which=5)
  plot(model,pch=18,col="red",which=c(4))
  
  #   Leverage points (hi)
  # Points that fall far from the line are points of high leverage
  # If hi>2p/n
  # 
  #     flag
  
  
  
  lev <- hatvalues(model)
  p   <- length(coef(model))
  n   <- length(dat)
  
  
  
  ht.lowest = min(c(min(lev)-.1*mean(lev), 1.8 *p/n))
  ht.highest = max(c(max(lev)+.1*mean(lev), 3.2 *p/n))
  
  par(mfrow=c(1,1))
  plot(1:length(dat[[y]]),
       lev, 
       main = "Leverage1", 
       xlim=c(1, length(dat[[y]])),
       ylim=c(ht.lowest, ht.highest),
       pch=19,
       xlab="observation",
       ylab = "Leverage1 Value")
  abline(h = 2 *p/n, lty = 1)
  abline(h = 3 *p/n, lty = 1)
  
  outlier <- lev[lev>(outlier.crit*p/n)]
  print(outlier)
  
  if(remove.outliers){ 
    print(summary(model))
    print("REMOVING")
    dat.temp = dat[-which(lev>(outlier.crit*p/n)),]
    
    LinReg.Temp <- lm(.formula2test, data = dat.temp)
    print(LinReg.Temp) 
    print(summary(LinReg.Temp))
  }
  
  
}
