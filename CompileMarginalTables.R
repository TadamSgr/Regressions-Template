CompileMarginalTables = function(df.c2.c1a, # = df.c2.c1, 
                              df.c2.alla, # = df.c2.all, 
                              df.all.c1a, #  = df.all.c1, 
                              df.all.alla, #  = df.all.all, 
                              round_to.inherit, #  = round_to,
                              column1a, #  = column1, 
                              column2a #  = column2
) {
  MargTab =   
    
    
    rbind(
      ## Sum % 
      left_join( 
        df.c2.c1a %>% 
          # mutate(Sum = round(Sum, round_to.inherit)) %>%
          select(!!as.name(column1a), !!as.name(column2a), Sum)  
        , 
        df.c2.c1a %>% 
          mutate(Percent = round(Sum/df.all.alla$Sum*100, round_to.inherit)) %>% 
          select(!!as.name(column1a), !!as.name(column2a), Percent)
        , 
        by = c(column1a, column2a)
      ) %>% 
        mutate(Sum.Percent = paste0(Sum, " (", Percent, ")")) %>% 
        select(-Sum, -Percent) %>% 
        pivot_wider(names_from = !!as.name(column1a), values_from = Sum.Percent, values_fill = "0 (0)") %>% 
        rbind(., 
              left_join( 
                df.all.c1a %>%  
                  # mutate(Sum = round(Sum, round_to)) %>% 
                  select(!!as.name(column1a), !!as.name(column2a), Sum)
                , 
                df.all.c1a %>%  
                  mutate(Percent = round(Sum/df.all.alla$Sum*100, round_to.inherit)) %>% 
                  select(!!as.name(column1a), !!as.name(column2a), Percent)
                , 
                by = c(column1a, column2a)
              ) %>% 
                mutate(Sum.Percent = paste0(Sum, " (", Percent, ")")) %>% 
                select(-Sum, -Percent) %>% 
                pivot_wider(names_from = !!as.name(column1a), values_from = Sum.Percent, values_fill = "0 (0)") 
        ) %>% 
        left_join(
          .,
          rbind(
            left_join(df.c2.alla %>% 
                        # mutate(Sum = round(Sum, round_to)) %>% 
                        select(!!as.name(column1a), !!as.name(column2a), Sum) %>% 
                        dplyr::select(-!!as.name(column1a))
                      , 
                      df.c2.alla %>% 
                        mutate(Percent = round(Sum/df.all.alla$Sum*100, round_to.inherit)) %>% 
                        select(!!as.name(column1a), !!as.name(column2a), Percent) %>% 
                        dplyr::select(-!!as.name(column1a)) 
                      ,
                      by = column2a
            ) %>% 
              mutate(Sum.Percent = paste0(Sum, " (", Percent, ")"))%>% 
              select(-Sum, -Percent)
            , 
            left_join(
              df.all.alla %>% 
                # mutate(Sum = round(Sum, round_to)) %>%
                select(!!as.name(column1a), !!as.name(column2a), Sum)%>% 
                dplyr::select(-!!as.name(column1a)) 
              , 
              df.all.alla %>% 
                mutate(Percent = round(Sum/df.all.alla$Sum*100, round_to.inherit)) %>%
                select(!!as.name(column1a), !!as.name(column2a), Percent)%>% 
                dplyr::select(-!!as.name(column1a)) 
              , 
              by = column2a
            ) %>% 
              mutate(Sum.Percent = paste0(Sum, " (", Percent, ")"))%>% 
              select(-Sum, -Percent)
            
          )
          
          , 
          by = column2a
        ) %>% 
        mutate(Metric = "Sum (%)") %>% 
        rename(All = Sum.Percent)
      
      ,
      ## Mean (SD)
      df.c2.c1a %>% 
        select(!!as.name(column1a), !!as.name(column2a), Mean, SD) %>% 
        mutate(#Mean = round(Mean, round_to), 
          #SD = round(SD, round_to), 
          text1 = " (", 
          text2 = ")") %>% 
        
        unite(Value, Mean, text1, SD, text2, sep= "") %>% 
        pivot_wider(names_from = !!as.name(column1a), values_from = Value) %>% 
        rbind(., 
              df.all.c1a %>%  
                select(!!as.name(column1a), !!as.name(column2a), Mean, SD) %>% 
                mutate(#Mean = round(Mean, round_to), 
                  #SD = round(SD, round_to), 
                  text1 = " (", 
                  text2 = ")") %>%    
                unite(Value, Mean, text1, SD, text2, sep= "") %>% 
                pivot_wider(names_from = !!as.name(column1a), values_from = Value)
        ) %>% 
        left_join(., 
                  rbind(
                    df.c2.alla %>% 
                      select(!!as.name(column1a), !!as.name(column2a), Mean, SD) %>% 
                      select(!!as.name(column1a), !!as.name(column2a),  Mean, SD) %>% 
                      mutate(#Mean = round(Mean, round_to), 
                        #SD = round(SD, round_to), 
                        text1 = " (", 
                        text2 = ")") %>%     
                      unite(Value, Mean, text1, SD, text2, sep= "") %>% 
                      dplyr::select(-!!as.name(column1a)) 
                    , 
                    df.all.alla %>% 
                      select(!!as.name(column1a), !!as.name(column2a), Mean, SD)%>% 
                      select(!!as.name(column1a), !!as.name(column2a),  Mean, SD) %>% 
                      mutate(#Mean = round(Mean, round_to), 
                        #SD = round(SD, round_to), 
                        text1 = " (", 
                        text2 = ")") %>%     
                      unite(Value, Mean, text1, SD, text2, sep= "") %>% 
                      dplyr::select(-!!as.name(column1a)) 
                  ), 
                  by = column2a) %>% 
        rename(All = Value) %>% 
        mutate(Metric = "Mean (SD)")
      
      ,
      ## Median (IQR)
      df.c2.c1a %>% 
        select(!!as.name(column1a), !!as.name(column2a), Median, IQR) %>% 
        mutate(#Median = round(Median, round_to), 
          #IQR = round(IQR, round_to), 
          text1 = " (", 
          text2 = ")") %>% 
        
        unite(Value, Median, text1, IQR, text2, sep= "") %>% 
        pivot_wider(names_from = !!as.name(column1a), values_from = Value) %>% 
        rbind(., 
              df.all.c1a %>%  
                select(!!as.name(column1a), !!as.name(column2a), Median, IQR) %>% 
                mutate(#Median = round(Median, round_to), 
                  #IQR = round(IQR, round_to), 
                  text1 = " (", 
                  text2 = ")") %>%    
                unite(Value, Median, text1, IQR, text2, sep= "") %>% 
                pivot_wider(names_from = !!as.name(column1a), values_from = Value)
        ) %>% 
        left_join(., 
                  rbind(
                    df.c2.alla %>% 
                      select(!!as.name(column1a), !!as.name(column2a), Median, IQR) %>% 
                      mutate(#Median = round(Median, round_to), 
                        #IQR = round(IQR, round_to), 
                        text1 = " (", 
                        text2 = ")") %>%     
                      unite(Value, Median, text1, IQR, text2, sep= "") %>% 
                      dplyr::select(-!!as.name(column1a)) 
                    , 
                    df.all.alla %>% 
                      select(!!as.name(column1a), !!as.name(column2a), Median, IQR)%>% 
                      mutate(#Median = round(Median, round_to), 
                        #IQR = round(IQR, round_to), 
                        text1 = " (", 
                        text2 = ")") %>%     
                      unite(Value, Median, text1, IQR, text2, sep= "") %>% 
                      dplyr::select(-!!as.name(column1a)) 
                  ), 
                  by = column2a) %>% 
        rename(All = Value) %>% 
        mutate(Metric = "Median (IQR)")
      ,
      ## Sample Size
      left_join( 
        df.c2.c1a %>% 
          # mutate(Sum = round(Sum, round_to.inherit)) %>%
          select(!!as.name(column1a), !!as.name(column2a), Count_Patients)  
        , 
        df.c2.c1a %>% 
          mutate(Percent.n = round(Count_Patients/df.all.alla$Count_Patients*100, round_to.inherit)) %>% 
          select(!!as.name(column1a), !!as.name(column2a), Percent.n)
        , 
        by = c(column1a, column2a)
      )  %>% 
        mutate(n.Percent = paste0(Count_Patients, " (", Percent.n, ")"))%>% 
        select(-Count_Patients, -Percent.n)%>%
        pivot_wider(names_from = !!as.name(column1a), values_from = n.Percent) %>%
        mutate(Metric = "n (%)", 
               Test.Category = "All") %>% 
        distinct(.) %>% 
        left_join(., 
                  df.all.alla %>%
                    select(!!as.name(column1a), !!as.name(column2a), Count_Patients)%>%
                    # dplyr::select(-!!as.name(column1a))  
                    mutate(Percent.n = round(Count_Patients/df.all.alla$Count_Patients*100, 0)) %>% mutate(All = paste0(Count_Patients, " (", Percent.n, ")"))%>% 
                    select(-Count_Patients, -Percent.n, -!!as.name(column1a))
                  
        )
      
    ) %>% 
    # filter(!(Test.Category %in% c("Biochemistry", "Cytogenetic_Molecular Genetic Tests", "Electrical", "Imaging", "Pathology", "WES") & Metric == "Percent") ) %>% 
    filter(!(Test.Category %in% c("Biochemistry", "Cytogenetic_Molecular Genetic Tests", "Electrical", "Imaging", "Pathology", "WES") & 
               Metric == "n (%)") ) %>% 
    filter(!is.na(Metric)) %>%
    dplyr::select(!!as.name(column2a), Metric, everything()) %>% 
    arrange(!!as.name(column2a))  %>% #names()
    rename(
      any_of(
        c(
          Diagnostic = "diagnostic",
          `Partially Diagnostic` = "partially-diagnostic", 
          `Potentially Diagnostic` = "potentially-diagnostic", 
          `Non-Diagnostic` = "non-diagnostic", 
          
          
          `0+WES` = "0", 
          `1+WES` = "1", 
          `2+WES` = "2", 
          `>=3+WES` = "3"
        )
      )
    ) %>% 
    select(Test.Category, Metric, 
           any_of(
             c(
               "Diagnostic","Partially Diagnostic", "Potentially Diagnostic", "Non-Diagnostic",
               
               "Diagnostic/Partially Diagnostic", "Potentially Diagnostic/Non-Diagnostic",
               
               
               "Isolated ID", "Multiple congenital anomalies no ID", "Multisystem disorder no ID" , "Single organ disorder no ID",  "Syndromic ID", 
               "<1 Year Before WES", "1-2 Years Prior", "2-3 Years Prior", "3-4 Years Prior", "4-5 Years Prior", "5-9 Years Prior", ">9 Years Prior", 
               "0+WES", "1+WES", "2+WES", ">=3+WES"
             )
           ), All) %>% 
    set.output4manulevels(.) %>% 
    rename(`Test Category` = "Test.Category") %>% 
    arrange(`Test Category`, Metric)
  
  return(MargTab)
}





autofit.flxtbl = function(temp = temp, 
                          caption.string = "",
                          mergehoriz = TRUE, 
                          mergevert = TRUE,
                          pgwidth = 6, 
                          preview = FALSE){
  ft = flextable(temp)
  
  
  
  i = 16.5 # width of the side borders in the word_document output (in centimeters)
  w = i*0.3937 # width of the side borders in the word_document output (in inches)
  ft = set_caption(ft, 
                   as_paragraph(
                     as_chunk(caption.string, props = fp_text_default(font.family = "Cambria"))
                   ), word_stylename = "Table Caption")
  
  if(mergehoriz == TRUE){ft = merge_h(ft)}
  if(mergevert == TRUE){ft = merge_v(ft)}
  
  
  
  ft2 = autofit(ft) #%>% set_table_properties(layout = "autofit")# renders nothing when tested on its own, needs other lines. 
  # auto_widths <- dim(ft2)$widths/sum(dim(ft2)$widths)
  # 
  ft2 = width(ft2,  width = dim(ft2)$widths*pgwidth /(flextable_dim(ft2)$widths))#w*auto_widths)
  
  
  if(preview == TRUE) {
    print(ft2, preview = "docx")
  }
  return(ft2)
  # print(ft2)
}




meansumspercenttables = function(ft, 
                                 Footnotes = Table.Footnotes, 
                                 footnote.npercent.index = c(1),
                                 footnote.npercent.text = "n represents the sample size within that column, and the percentage indicates the proportion of the entire cohort wthin that column.",
                                 footnote.sum.index = c(2,  5, 8, 11, 14, 17, 20), 
                                 footnote.sum.text = "Sums are the total count of tests within a given category.",
                                 footnote.mean.index = c(3, 6, 9, 12, 15, 18, 21), 
                                 footnote.mean.text = "Means and standard deviations (SD) are calculated for the total within a category for each patient, and then the means of those by patient, by category values are calculated.", 
                                 footnote.median.index = c(4, 7, 10, 13, 16, 19, 22), 
                                 footnote.median.text = "Medians and Interquartile Ranges (IQR) are calculated for the total within a category for each patient, and then the means of those by patient, by category values are calculated.", 
                                 footnote.percent.index = c(2,  5, 8, 11, 14, 17, 20), 
                                 footnote.percent.text = "Internal Percentage are the percentage of the total number of tests captured across all patients within that grouping of the data; Medial Percentages are the total percentage contained in that row or column")  {
  
  # Footnotes = Table.Footnotes
  
  if(Footnotes == TRUE) {
    ft %>% 
      hline(., border = fp_border(width = 0.5), part="all") %>% 
      bold(., part = "header") %>% 
      border_outer() %>% 
      align( ., align = "center", part = "all" ) %>% 
      # n.percent footnote
      footnote( ., i = footnote.npercent.index, j = c(2),
                value = as_paragraph(
                  footnote.npercent.text
                ),
                ref_symbols = c("a"),
                # part = "header", 
                inline = FALSE) %>% 
      
      # Sum footnote
      footnote( ., i = footnote.sum.index, j = c(2),
                value = as_paragraph(
                  footnote.sum.text
                ),
                ref_symbols = c("b"),
                # part = "header", 
                inline = FALSE)  %>% 
      # Sum percent footnote
      footnote( ., i = footnote.percent.index, j = c(2),
                value = as_paragraph(
                  footnote.percent.text 
                ),
                ref_symbols = c("c"),
                # part = "header", 
                inline = FALSE)%>% 
      
      ## Mean Footnote
      footnote( ., i = footnote.mean.index, j = c(2),
                value = as_paragraph(
                  footnote.mean.text
                ),
                ref_symbols = c("d"),
                # part = "header", 
                inline = FALSE) %>% 
      
      # Median Footnote
      footnote( ., i = footnote.median.index, j = c(2),
                value = as_paragraph(
                  footnote.median.text 
                ),
                ref_symbols = c("e"),
                # part = "header", 
                inline = FALSE)
  } else {
    ft
  }
  
}

