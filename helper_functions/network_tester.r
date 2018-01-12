test_network<- function(languages, features, r=3000){
  analysis_df<- data.frame(matrix(ncol = 7, nrow = 500))
  colnames(analysis_df)<-c("language", "type", "CI lower", "Optim Par", "CI upper", "Full Model nLLK","Random Model nLLK")
  i=1
  for (language in languages){
    for (feature in features){
      file_path<-paste(getwd(),"/out_files/", language, "_", feature, ".csv", sep = "")
      if (file.exists(file_path)){
        final_df <<- read.csv(file_path, as.is = T)
        analysis_df[i,]<- get_confint(language = language, type = feature, r = r)
        i=i+1
      }
    }
  }
  analysis_df <- analysis_df %>% filter(!is.na(language))
  write.csv(analysis_df, file = "analysis_df.csv", row.names = F)
  return(analysis_df)
}


######################################################################################################
# test the significance of DEGREE, before and after controling frequency and length
significance_testing<- function(lang, type){
  file_name<-paste(lang,"_",type, sep = "")
  
  if (file.exists(paste(getwd(),"/out_files/", file_name,".csv", sep = ""))){
    
    parent_freq<- read.csv(paste(getwd(),"/in_files/log_freq.csv",sep = ""), as.is = T) 
    df<- read.csv(paste(getwd(),"/out_files/", file_name,".csv", sep = ""), as.is = T) %>%
      trim_all_definition() %>%
      trim_all_unilemma() %>%
      rowwise() %>%
      mutate(IPA=Speak(lang = lang, word = definition)) %>%
      trim_IPA_completely() %>%
      mutate(value2=str_count(IPA)) %>%
      ungroup() %>% 
      filter(learned==1) %>% 
      left_join(parent_freq) %>% 
      mutate(freq=ifelse(is.na(freq),0,freq)) #if can't find the freq of word, set it to 0
    
    degree<- df$value
    length<- df$value2
    freq<- df$freq
    age<- df$age
    a_d<- lm(age ~ degree)
    a_fl<- lm(age ~ freq+length)
    a_dlf<- lm(age ~ length+freq+degree)
    
    degree_coef<- round(summary(a_d)$coefficients[2,1], digits = 3)
    degree_coef_control<- round(summary(a_dlf)$coefficients[4,1], digits =3)
    degree_sig<- significance_level(summary(a_d)$coefficients[2,4])
    degree_sig_control<- significance_level(summary(a_dlf)$coefficients[4,4])
    lrtest_sig<- significance_level(lrtest(a_fl, a_dlf)[2,5])
    rsquare_dl<- summary(a_fl)$r.squared
    rsquare_diff<- summary(a_dlf)$r.squared-summary(a_fl)$r.squared
    return(c(lang,
             type,
             degree_coef,
             degree_sig,
             degree_coef_control,
             degree_sig_control,
             lrtest_sig,
             rsquare_dl,
             rsquare_diff))
  }
}

######################################################################################################
# test significance level: NS, *, **, ***
significance_level<-function(p_value){
  if (p_value<0.001){
    return("***")
  } else if (p_value<0.01){
    return("**")
  } else if (p_value<0.05){
    return("*")
  } else {
    return("NS")
  }
}
######################################################################################################


# test_network<- function(languages, features, r=3000){
#   analysis_df<- data.frame(matrix(ncol = 7, nrow = 500))
#   colnames(analysis_df)<-c("language", "type", "CI lower", "Optim Par", "CI upper", "Full Model nLLK","Random Model nLLK")
#   i=1
#   for (language in languages){
#     for (feature in features){
#       file_path<-paste(getwd(),"/out_files/", language, "_", feature, ".csv", sep = "")
#       if (file.exists(file_path)){
#         final_df <<- read.csv(file_path, as.is = T)
#         analysis_df[i,]<- get_confint(language = language, type = feature, r = r)
#         i=i+1
#       }
#     }
#   }
#   analysis_df <- analysis_df %>% filter(!is.na(language))
#   write.csv(analysis_df, file = "analysis_df.csv", row.names = F)
#   return(analysis_df)
# }
# 
# 
# ######################################################################################################
# significance_testing<- function(lang, type){
#   file_name<-paste(lang,"_",type, sep = "")
#   
#   if (file.exists(paste(getwd(),"/out_files/", file_name,".csv", sep = ""))){
#     
#     parent_freq<- read.csv(paste(getwd(),"/in_files/log_freq.csv",sep = ""), as.is = T) 
#     df<- read.csv(paste(getwd(),"/out_files/", file_name,".csv", sep = ""), as.is = T) %>%
#       trim_all_definition() %>%
#       trim_all_unilemma() %>%
#       rowwise() %>%
#       mutate(IPA=Speak(lang = lang, word = definition)) %>%
#       trim_IPA_completely() %>%
#       mutate(value2=str_count(IPA)) %>%
#       ungroup() %>% 
#       filter(learned==1) %>% 
#       left_join(parent_freq) %>% 
#       mutate(freq=ifelse(is.na(freq),0,freq)) #if can't find the freq of word, set it to 0
#     
#     degree<- df$value
#     length<- df$value2
#     freq<- df$freq
#     month<- df$month
#     m_d<- lm(month ~ degree)
#     m_fl<- lm(month ~ freq+length)
#     m_dlf<- lm(month ~ length+freq+degree)
#     
#     degree_coef<- round(summary(m_d)$coefficients[2,1], digits = 3)
#     degree_coef_control<- round(summary(m_dlf)$coefficients[4,1], digits =3)
#     degree_sig<- significance_level(summary(m_d)$coefficients[2,4])
#     degree_sig_control<- significance_level(summary(m_dlf)$coefficients[4,4])
#     lrtest_sig<- significance_level(lrtest(m_fl, m_dlf)[2,5])
#     return(c(lang,
#              type,
#              degree_coef,
#              degree_sig,
#              degree_coef_control,
#              degree_sig_control,
#              lrtest_sig))
#   }
# }
# 
# 
# significance_level<-function(p_value){
#   if (p_value<0.001){
#     return("***")
#   } else if (p_value<0.01){
#     return("**")
#   } else if (p_value<0.05){
#     return("*")
#   } else {
#     return("NS")
#   }
# }
# ######################################################################################################
