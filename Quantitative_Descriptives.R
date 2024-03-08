




## Quantitative Functions
# -------------------- Describe Quant ---------------------------
mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}




quant.describe = function(df = df, 
                          measure = "age_wes", 
                          shapiro = TRUE, 
                          round_to = 1
){
  require(moments)
  
  
  
  if(shapiro == TRUE) {
    df.out = df %>% 
      summarise(Count_Patients = sum(!is.na(!!as.name(measure))),
                Mean = round(mean(!!as.name(measure), na.rm = TRUE), round_to), 
                SD = round(sd(!!as.name(measure), na.rm = TRUE), round_to),  
                Median = round(median(!!as.name(measure), na.rm = TRUE), round_to), 
                Mode  = round(mode(!!as.name(measure), na.rm = TRUE), round_to), 
                Sum = round(sum(!!as.name(measure), na.rm = TRUE), round_to), 
                Skew = round(moments::skewness(!!as.name(measure), na.rm = TRUE), round_to), 
                Kurtosis = round(moments::kurtosis(!!as.name(measure), na.rm = TRUE), round_to), 
                IQR = round(IQR(!!as.name(measure), na.rm = TRUE),2), 
                Min = round(min(!!as.name(measure), na.rm = TRUE), round_to), 
                Max = round(max(!!as.name(measure), na.rm = TRUE), round_to), 
                Shapiro_W = round(unname(shapiro.test(!!as.name(measure))$statistic), round_to), 
                Shapiro_p = format(signif(shapiro.test(!!as.name(measure))$p.value, 3), format = "e", digits = 2),
                Count = n(), 
                Quantile = list(stats::quantile(!!as.name(measure), probs = seq(.25, 1, by = .25), na.rm = TRUE))
      ) 
  } else {
    df.out = df %>% 
      summarise(Count_Patients = sum(!is.na(!!as.name(measure))),
                Mean = round(mean(!!as.name(measure), na.rm = TRUE), round_to), 
                SD = round(sd(!!as.name(measure), na.rm = TRUE), round_to),  
                Median = round(median(!!as.name(measure), na.rm = TRUE), round_to), 
                Mode  = round(mode(!!as.name(measure), na.rm = TRUE), round_to), 
                Sum = round(sum(!!as.name(measure), na.rm = TRUE), round_to), 
                Skew = round(moments::skewness(!!as.name(measure), na.rm = TRUE), round_to), 
                Kurtosis = round(moments::kurtosis(!!as.name(measure), na.rm = TRUE), round_to), 
                IQR = round(IQR(!!as.name(measure), na.rm = TRUE), round_to), 
                Min = round(min(!!as.name(measure), na.rm = TRUE), round_to), 
                Max = round(max(!!as.name(measure), na.rm = TRUE), round_to), 
                Shapiro_W = "Cannot compute: Groups must be between 3 and 5000",
                Shapiro_p = "Cannot compute: Groups must be between 3 and 5000", 
                Count = n(), 
                Quantile = list(stats::quantile(!!as.name(measure), probs = seq(.25, 1, by = .25), na.rm = TRUE))
      ) 
  }
  
  
  df.out = 
    df.out %>% 
    unnest_wider(Quantile) %>% #names()
    
    mutate(Group_by  = "", 
           Measure = measure, 
           `25%` = round(as.numeric(`25%`), round_to), 
           `50%` = round(as.numeric(`50%`), round_to), 
           `75%` = round(as.numeric(`75%`), round_to), 
           `100%` = round(as.numeric(`100%`), round_to), 
    ) %>% 
    # mutate_each(funs(prettyNum(., big.mark=","))) %>%
    
    
    # mutate(Group_by  = "", 
    #        Measure = measure, 
    #        `10%` = round(as.numeric(`10%`), round_to), 
    #        `20%` = round(as.numeric(`20%`), round_to), 
    #        `30%` = round(as.numeric(`30%`), round_to), 
    #        `40%` = round(as.numeric(`40%`), round_to), 
    #        `50%` = round(as.numeric(`50%`), round_to), 
    #        `60%` = round(as.numeric(`60%`), round_to), 
  #        `70%` = round(as.numeric(`70%`), round_to), 
  #        `80%` = round(as.numeric(`80%`), round_to), 
  #        `90%` = round(as.numeric(`90%`), round_to), 
  #        `100%` = round(as.numeric(`100%`), round_to), 
  # ) %>% 
  dplyr::select(Measure, Group_by, Count, everything())
  
  
  
  
  
  
  return(df.out)
}




# ------------------------- Normal Plots ---------------------------------#

normal.plots = function(df2, 
                        measure = "", ## Must match column name
                        densityfigtitle = "default", 
                        densityxlab = "", 
                        qqtitle = "default") {
  require("ggpubr")
  
  df.fig = df2 %>% 
    mutate(fig = !!as.name(measure))
  
  if(densityfigtitle == "default") {
    densityfigtitle2 = paste0("Density plot of ", measure)
  } else {
    densityfigtitle2 = densityfigtitle
  }
  
  print(ggpubr::ggdensity(data = df.fig, 
                          x = "fig", 
                          main = densityfigtitle2,
                          xlab = densityxlab))
  
  
  
  if(qqtitle == "default") {
    qqtitle2 = paste0("QQ plot of ", measure)
  } else {
    qqtitle2 = qqtitle
  }
  
  print(ggplot(df.fig, aes(sample=fig)) +
          stat_qq() + 
          ggtitle(qqtitle2))
}





formattingnumbers = function(df, prefix = "", units = "", digits.inherit) {
  df %>% 
    mutate(across(any_of(c("Mean", "Median", "Mode")), 
                  ~ paste0(prefix, formatC(.x, format="f", big.mark=",", digits=digits.inherit), units))) %>% 
    mutate(across(any_of(c("Sum", "Min", "Max")), 
                  ~ paste0( prefix, formatC(Min, format="f", big.mark=",", digits=0), units))) %>% 
    mutate(across(any_of(c("SD", "IQR", "percent.n", "percent.sum")), 
                  ~ formatC(.x, format="f", big.mark=",", digits=digits.inherit))) 
}



