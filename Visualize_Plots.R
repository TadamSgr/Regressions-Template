
##### Visualise data #####

# For an individual relationship to review
plot.dat = function(dat, #The dataset, must be a matrix of at least two columns
                    y, #Dependent Variable
                    x1 #Another variable to compare to Y, or list of variable names
) {
  
  for(i in 1:(length(x1))) {
    plot(dat[[y]],dat[[x1[i]]],
         #xlim=c(3,5),ylim=c(8,26),pch=19,
         xlab=x1[i],ylab=y,las=1)
    mtext("y",side=2,las=1,line=2)
  }
  
}

# Can also use pairs(dat) for all vriables at once

