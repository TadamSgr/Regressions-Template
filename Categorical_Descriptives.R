

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

