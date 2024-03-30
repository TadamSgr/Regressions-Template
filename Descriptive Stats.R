
mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}




DescriptiveTables = function(df = dataframe, 
                             measure, # The column upon which to do analysis
                             # shapiro = TRUE, 
                             round_to = 1
                             ){
  require(moments)
  
  
  shap.df = 
    tryCatch(
      {
        x.temp = shapiro.test(df[,measure])
        x = data.frame(
          "p.value" = format(signif(x.temp$statistic, 3), format = "e", digits = 2),#round(unname(x.temp$p.value), 3), 
          "statistic" = format(signif(x.temp$statistic, 2), format = "e", digits = 2)#round(unname(x.temp$statistic), 3)
        )
      }, 
      error = function(df) {
        x = data.frame(
          "p.value" = "Cannot compute: Groups must be between 3 and 5000", 
          "statistic" = "Cannot compute: Groups must be between 3 and 5000"
        )
        return(x)
      }
    )
  
  
  
  
  df.out = 
    df %>% 
    summarise(
      Count_Patients = sum(!is.na(!!as.name(measure))),
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
      Count = n(),
      Quantile = list(stats::quantile(!!as.name(measure), probs = seq(.25, 1, by = .25), na.rm = TRUE))
    ) %>% 
    cbind(shap.df)
  
  
  
  # if(shapiro == TRUE) {
  #   df.out = df %>% 
  #     summarise(Count_Patients = sum(!is.na(!!as.name(measure))),
  #               Mean = round(mean(!!as.name(measure), na.rm = TRUE), round_to), 
  #               SD = round(sd(!!as.name(measure), na.rm = TRUE), round_to),  
  #               Median = round(median(!!as.name(measure), na.rm = TRUE), round_to), 
  #               Mode  = round(mode(!!as.name(measure), na.rm = TRUE), round_to), 
  #               Sum = round(sum(!!as.name(measure), na.rm = TRUE), round_to), 
  #               Skew = round(moments::skewness(!!as.name(measure), na.rm = TRUE), round_to), 
  #               Kurtosis = round(moments::kurtosis(!!as.name(measure), na.rm = TRUE), round_to), 
  #               IQR = round(IQR(!!as.name(measure), na.rm = TRUE), round_to), 
  #               Min = round(min(!!as.name(measure), na.rm = TRUE), round_to), 
  #               Max = round(max(!!as.name(measure), na.rm = TRUE), round_to), 
  #               Shapiro_W = round(unname(shapiro.test(!!as.name(measure))$statistic), round_to), 
  #               Shapiro_p = format(signif(shapiro.test(!!as.name(measure))$p.value, 3), format = "e", digits = 2),
  #               Count = n(), 
  #               Quantile = list(stats::quantile(!!as.name(measure), probs = seq(.25, 1, by = .25), na.rm = TRUE))
  #     ) 
  # } else {
  #   df.out = df %>% 
  #     summarise(Count_Patients = sum(!is.na(!!as.name(measure))),
  #               Mean = round(mean(!!as.name(measure), na.rm = TRUE), round_to), 
  #               SD = round(sd(!!as.name(measure), na.rm = TRUE), round_to),  
  #               Median = round(median(!!as.name(measure), na.rm = TRUE), round_to), 
  #               Mode  = round(mode(!!as.name(measure), na.rm = TRUE), round_to), 
  #               Sum = round(sum(!!as.name(measure), na.rm = TRUE), round_to), 
  #               Skew = round(moments::skewness(!!as.name(measure), na.rm = TRUE), round_to), 
  #               Kurtosis = round(moments::kurtosis(!!as.name(measure), na.rm = TRUE), round_to), 
  #               IQR = round(IQR(!!as.name(measure), na.rm = TRUE), round_to), 
  #               Min = round(min(!!as.name(measure), na.rm = TRUE), round_to), 
  #               Max = round(max(!!as.name(measure), na.rm = TRUE), round_to), 
  #               Shapiro_W = "Cannot compute: Groups must be between 3 and 5000",
  #               Shapiro_p = "Cannot compute: Groups must be between 3 and 5000", 
  #               Count = n(), 
  #               Quantile = list(stats::quantile(!!as.name(measure), probs = seq(.25, 1, by = .25), na.rm = TRUE))
  #     ) 
  # }
  
  
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





# ------------------ Categorical function ------------------- 



report.chisq = function(df.out) {
  x = chisq.test(df.out$Freq)
  chi = round(as.numeric(as.character(x[[1]])), 3)
  df = as.numeric(as.character(x[[2]]))
  p = signif(as.numeric(as.character(x[[3]])), 3)
  
  
  report = paste0("chi-sq(", df, ")= ", chi, ", p=", p)
  return(report)
}


graph.cat = function(data = p_id_inf, column){
  ggplot(data = data, aes(column) ) + 
    geom_bar(stat = "identity")
}





describe.cat = function(p_id_inf2 = p_id_inf, 
                        column = "", 
                        measure.name = "", 
                        cap = "") {
  # column must be a df$col format
  df.out = 
    as.data.frame(table({{column}})) %>% 
    rename(Levels = Var1) %>% 
    mutate(Measure = measure.name) %>% 
    dplyr::select(Measure, Levels, Freq)  
  
  
  df.tot = data.frame(
    Measure = measure.name, 
    Levels = "Total", 
    Freq = sum(df.out$Freq)
  )
  
  df.out = rbind(df.out, df.tot)
  
  df.out =
    df.out %>%
    mutate(Percentage = round(Freq/nrow(p_id_inf2)*100, 3),
           chisq = report.chisq(df.out))
  
  
  
  # print(graph.cat(data = df.out, Freq) )
  
  print(
    ggplot(data = df.out, aes(x = Levels, y = Freq) ) + 
      geom_bar(stat = "identity") + 
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
      ggtitle(unique(df.out$Measure)) + 
      labs(
        caption = cap)
  )
  
  return(df.out)
}




#----------------- Tables with Marginal Means -------------------------------
  
  
  formattingnumbers = function(df, prefix = "", units = "", digits.inherit) {
    df %>% 
      mutate(across(any_of(c("Mean", "Median", "Mode")), 
                    ~ paste0(prefix, formatC(.x, format="f", big.mark=",", digits=digits.inherit), units))) %>% 
      mutate(across(any_of(c("Sum2", "Min", "Max")), 
                    ~ paste0( prefix, formatC(Min, format="f", big.mark=",", digits=0), units))) %>% 
      mutate(across(any_of(c("SD", "IQR", "percent.n", "percent.sum")), 
                    ~ formatC(.x, format="f", big.mark=",", digits=digits.inherit))) 
  }









descriptivetables = function(
    df, # needs to be in long format: study_id, column1, column2, measure.columnname
    column1 = 'q3_pheno',
    column2 = 'Test.Category', 
    measure.columnname = "", 
    measure.unitname = "", 
    shapiro.logical = FALSE,
    round_to = 1,
    prefix.inherit = "", 
    units.inherit = "", 
    outputfolder = "C:/Users/tadam/University of Calgary/Karen MacDonald - C4R SOLVE/Aim 1 - trajectories/Results output tables figures/",
    save.table = TRUE,
    csv.name = "" # does not contain .csv
) {
  
  
  
  
  
  
  df.all.all =
    df %>% 
    ungroup() %>% 
    group_by(study_id) %>% 
    mutate(!!as.name(measure.columnname) := sum(!!as.name(measure.columnname))) %>% 
    ungroup() %>% 
    # distinct(study_id, q17_res_typ)
    distinct(study_id, .keep_all = TRUE) %>% #filter(is.na(q17_res_typ))
    quant.describe(df = .,
                   measure = measure.columnname, 
                   shapiro = shapiro.logical) %>% 
    mutate(Measure = measure.unitname, 
           !!as.name(column2) := "All", 
           !!as.name(column1) := "All", )%>% 
    dplyr::select(-Group_by)%>% 
    dplyr:: select(!!as.name(column2), !!as.name(column1), Measure, everything()) %>% #names()
    formattingnumbers(., 
                      prefix = prefix.inherit, 
                      units = units.inherit, 
                      digits.inherit = round_to
    )
  
  df.all.c1 =   
    df %>%
    group_by(study_id, !!as.name(column1)) %>%
    dplyr::summarise(total = sum(!!as.name(measure.columnname))) %>% 
    ungroup(   ) %>%
    group_by(!!as.name(column1)) %>% 
    quant.describe(df = .,
                   measure = "total", 
                   shapiro = shapiro.logical) %>% 
    mutate(Measure = measure.unitname) %>% 
    mutate(!!as.name(column2) := "All", 
           !!as.name(column1) := !!as.name(column1)
    )  %>% 
    dplyr::select(-Group_by)%>% 
    dplyr:: select(!!as.name(column2), !!as.name(column1), Measure, everything())%>% #names()
    formattingnumbers(., 
                      prefix = prefix.inherit, 
                      units = units.inherit, 
                      digits.inherit = round_to
    )  
  
  df.c2.all = 
    df %>%   
    group_by(study_id, !!as.name(column2)) %>%
    dplyr::summarise(total = sum(!!as.name(measure.columnname))) %>% 
    ungroup(   ) %>%
    group_by(!!as.name(column2)) %>% 
    quant.describe(df = .,
                   measure = "total", 
                   shapiro = shapiro.logical) %>% 
    mutate(Measure = measure.unitname) %>% 
    mutate(!!as.name(column2) := !!as.name(column2), 
           !!as.name(column1) := "All"
    ) %>% 
    dplyr::select(-Group_by )%>% 
    dplyr:: select(!!as.name(column2), Measure, everything()) %>% #names()
    formattingnumbers(., 
                      prefix = prefix.inherit, 
                      units = units.inherit, 
                      digits.inherit = round_to
    )  
  
  
  df.c2.c1 = 
    df %>%   
    group_by(study_id, !!as.name(column1), !!as.name(column2)) %>%
    dplyr::summarise(total = sum(!!as.name(measure.columnname))) %>% 
    ungroup(   ) %>%
    group_by(!!as.name(column1), !!as.name(column2)) %>% 
    quant.describe(df = .,
                   measure = "total", 
                   shapiro = shapiro.logical) %>% 
    mutate(Measure = measure.unitname) %>% 
    dplyr::select(-Group_by )%>% 
    dplyr:: select(!!as.name(column2), !!as.name(column1), Measure, everything())%>% #names()
    formattingnumbers(., 
                      prefix = prefix.inherit, 
                      units = units.inherit, 
                      digits.inherit = round_to
    )  
  
  
  
  df.out = 
    rbind( df.all.all, df.all.c1, df.c2.all, df.c2.c1 ) #%>% 
  #print()
  if(save.table) {
    write.csv(df.out, paste0(outputfolder, csv.name, "_full.csv"))
    
  }
  
  naming.columns = 
    df.c2.c1 %>% 
    mutate(Sum = round(Sum, round_to)) %>% 
    select(!!as.name(column1), !!as.name(column2), Sum)  %>% 
    pivot_wider(names_from = !!as.name(column1), values_from = Sum, values_fill = 0) %>% names()
  
  
  
  output4manu = output4manuscripts(df.c2.c1a = df.c2.c1, 
                                   df.c2.alla = df.c2.all, 
                                   df.all.c1a = df.all.c1, 
                                   df.all.alla = df.all.all, 
                                   round_to.inherit = round_to,
                                   column1a = column1, 
                                   column2a = column2
  )
  
  
  if(save.table){
    write.csv(output4manu, paste0(outputfolder, csv.name, "_clean.csv"))
  }
  
  
  
  return(output4manu)
}



