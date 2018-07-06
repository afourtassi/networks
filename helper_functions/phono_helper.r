######################################################################################################
# Espeak function
Speak<- function(lang, word){
  langs<- c("English (British)","English (American)", "English (Australian)", "Spanish (European)", "Spanish (Mexican)",  "Croatian", 
            "Italian", "Danish", "Norwegian", "Turkish", "Slovak", "Swedish", "Russian", "German", "French (French)",
            "French (Quebecois)", "Portuguese (European)", "Czech", "Greek (Cypriot)","Latvian" )
  abbr <- c("en-rp", "en-us", "en", "es", "es", "hr", "it", "da", "no", "tr","sk", "sv","ru","de", "fr", "fr", "pt-pt","cs", "el", "lv")
  lang_abbr<- abbr[which(langs==lang)]
  IPA<- system(paste("espeak -q -v ", lang_abbr, " --ipa ", "\"", word, "\"",sep=""), intern=T)
  return(IPA)
}

######################################################################################################
# trim IPA
trim_IPA<- function(IPA_list){
  IPA_list<- IPA_list %>%
    mutate(IPA= gsub(" ","", IPA)) %>%
    mutate(IPA= gsub("[[:punct:]]", "",IPA))
  return(IPA_list)
}

######################################################################################################
# trim IPA, more fiercely
trim_IPA_completely<- function(IPA_list){
  IPA_list<- IPA_list %>%
    mutate(IPA= gsub(" ","", IPA)) %>%
    mutate(IPA= gsub("ː","",IPA)) %>%
    mutate(IPA= gsub("[[:punct:]]", "",IPA))
  return(IPA_list)
}

######################################################################################################
# set threshold for IPA distance
IPA_threshold<- function(IPA_list, threshold){
  IPA_list<- IPA_list %>%
    mutate(link=as.numeric(dist<=threshold))
  return(IPA_list)
}

######################################################################################################