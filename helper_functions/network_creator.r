create_combo <- function(languages){
  suffix<<- "_p"
  for (language in languages){
    lang_name<<- language
    # age of acquisition data frame
    aoa_frame<- make_aoa_dataframe(lang = lang_name)
    # list of lemmas of learnt words
    first_age<- aoa_frame$age[1]
    lemma_list<- aoa_frame %>%
      trim_all_unilemma() %>%
      filter(age==first_age) %>%
      select(item, uni_lemma) 
    # list of definitions of learnt words
    def_list<- aoa_frame %>%
      trim_all_definition() %>% 
      filter(age==first_age) %>%
      select(item, definition)
    
    # make pairs of words and show whether they have connection via associative norms
    assoc_pairs<- make_assoc_pairs(lemma_list = lemma_list) # here we used the unilemma
    phono_pairs<- make_IPA_pairs(def_list = def_list, lang = lang_name) #here we use the defintion
    
    assoc_PAC<- PAC_generator(vocab_age = aoa_frame, word_pairs = assoc_pairs) %>% 
      rename(PAC_assoc = value) %>% select(-definition, -uni_lemma)
    assoc_PAT<- PAT_generator(vocab_age = aoa_frame, word_pairs = assoc_pairs) %>% 
      rename(PAT_assoc = value) %>% select(-definition, -uni_lemma)
    phono_PAC_t1<- PAC_generator(vocab_age = aoa_frame, word_pairs = phono_pairs %>% IPA_threshold(1)) %>% 
      rename(PAC_phono_t1 = value) %>% select(-definition, -uni_lemma)
    phono_PAT_t1<- PAT_generator(vocab_age = aoa_frame, word_pairs = phono_pairs %>% IPA_threshold(1)) %>% 
      rename(PAT_phono_t1 = value) %>% select(-definition, -uni_lemma)
    phono_PAC_t2<- PAC_generator(vocab_age = aoa_frame, word_pairs = phono_pairs %>% IPA_threshold(2)) %>% 
      rename(PAC_phono_t2 = value) %>% select(-definition, -uni_lemma)
    phono_PAT_t2<- PAT_generator(vocab_age = aoa_frame, word_pairs = phono_pairs %>% IPA_threshold(2)) %>% 
      rename(PAT_phono_t2 = value) %>% select(-definition, -uni_lemma)
    phono_PAC_nt<- PAC_nt_generator(vocab_age = aoa_frame, word_pairs = phono_pairs %>% rename(link = dist)) %>% 
      rename(PAC_phono_nt = value) %>% select(-definition, -uni_lemma)
    phono_PAT_nt<- PAT_nt_generator(vocab_age = aoa_frame, word_pairs = phono_pairs %>% rename(link = dist)) %>% 
      rename(PAT_phono_nt = value) %>% select(-definition, -uni_lemma)
    parent_freq<- read.csv(paste(getwd(),"/in_files/log_freq.csv",sep = ""), as.is = T) 
    
    freq_len_df <- aoa_frame %>%
      trim_all_definition() %>%
      mutate(uni_lemma=gsub(" \\s*\\([^\\)]+\\)","", uni_lemma)) %>%
      mutate(uni_lemma=gsub("[*].*$","", uni_lemma)) %>%
      rowwise() %>%
      mutate(IPA=Speak(lang = language, word = definition)) %>%
      trim_IPA_completely() %>%
      mutate(length = str_count(IPA)) %>%
      filter(learned==1) %>% 
      left_join(parent_freq) %>% 
      mutate(freq=ifelse(is.na(freq) && !is.na(uni_lemma),0,freq)) %>% #if can't find the freq of word, set it to 0
      ungroup() %>% 
      select(-definition, -uni_lemma, -age, -learned)
    
    combo <- aoa_frame %>% left_join(freq_len_df) %>% 
      left_join(assoc_PAC) %>% left_join(assoc_PAT) %>% 
      left_join(phono_PAC_t1) %>% left_join(phono_PAT_t1) %>% 
      left_join(phono_PAC_t2) %>% left_join(phono_PAT_t2) %>%
      left_join(phono_PAC_nt) %>% left_join(phono_PAT_nt)
    write_out_csv(var = combo, lang = lang_name, type = paste("combo", sep = ""))

  }
}

###################################################################################################

create_network_haspoly<- function(languages, features, thresholds){
  suffix<<- "_p"
  for (language in languages){
    lang_name<<- language
    # age of acquisition data frame
    aoa_frame<- make_aoa_dataframe(lang = lang_name)
    # list of lemmas of learnt words
    first_age<- aoa_frame$age[1]
    lemma_list<- aoa_frame %>%
      trim_all_unilemma() %>%
      filter(age==first_age) %>%
      select(item, uni_lemma) 
    # list of definitions of learnt words
    def_list<- aoa_frame %>%
      trim_all_definition() %>% 
      filter(age==first_age) %>%
      filter(item %in% lemma_list$item) %>% # I MIGHT NOT NEED TO DO THIS
      select(item, definition)
    if ("assoc_PAC" %in% features || "assoc_PAT" %in% features)
      assoc_pairs<- make_assoc_pairs(lemma_list = lemma_list)
    if ("assoc_PAC" %in% features){
      # make pairs of words and show whether they have connection via associative norms
      assoc_PAC = create_assoc_PAC(aoa_frame = aoa_frame, assoc_pairs = assoc_pairs)
    }
    if ("assoc_PAT" %in% features){
      assoc_PAT = create_assoc_PAT(aoa_frame = aoa_frame, assoc_pairs = assoc_pairs)
    }
    
    if ("McRae_PAC" %in% features || "McRae_PAT" %in% features)
      McRae_pairs<- make_McRae_pairs(words_list = lemma_list) 
    if ("McRae_PAC" %in% features){
      # make pairs of words and show how many McRae features they share
      for (threshold in thresholds){
        McRae_PAC = create_McRae_PAC(aoa_frame = aoa_frame, 
                         McRae_pairs = McRae_pairs %>% McRae_threshold(threshold = 1),
                         threshold = threshold)
      }
    }
    if ("McRae_PAT" %in% features){
      for (threshold in thresholds){
        McRae_PAT = create_McRae_PAT(aoa_frame = aoa_frame, 
                         McRae_pairs = McRae_pairs %>% McRae_threshold(threshold = 1),
                         threshold = threshold)
      }
    }
    
    if ("phono_PAC" %in% features || "phono_PAT" %in% features || 
        "phono_PAC_nt" %in% features || "phono_PAT_nt" %in% features)
      IPA_pairs<- make_IPA_pairs(def_list = def_list, lang = lang_name)
    if ("phono_PAC" %in% features){
      for (threshold in thresholds){
        phono_PAC = create_phono_PAC(aoa_frame = aoa_frame, 
                         phono_pairs = IPA_pairs %>% IPA_threshold(threshold = threshold),
                         threshold = threshold)
      }
    }
    if ("phono_PAT" %in% features){
      for (threshold in thresholds){
        phono_PAT = create_phono_PAT(aoa_frame = aoa_frame, 
                         phono_pairs = IPA_pairs %>% IPA_threshold(threshold = threshold),
                         threshold = threshold)
      }
    }
    if ("phono_PAC_nt" %in% features){
        phono_PAC_nt = create_phono_PAC_nt(aoa_frame = aoa_frame, 
                         phono_pairs = IPA_pairs)
    }
    if ("phono_PAT_nt" %in% features){
        phono_PAT_nt = create_phono_PAT_nt(aoa_frame = aoa_frame, 
                         phono_pairs = IPA_pairs)
    }
    
  }
}

###################################################################################################
create_network_nopoly<- function(languages, features, thresholds){
  suffix<<- "_np"
  for (language in languages){
    
    lang_name<<- language
    # age of acquisition data frame
    aoa_frame<- make_aoa_dataframe(lang = lang_name)
    # list of lemmas of learnt words
    first_age<- aoa_frame$age[1]
    lemma_list<- aoa_frame %>%
      filter(age==first_age) %>%
      select(item, uni_lemma)
    
    if ("assoc_PAC" %in% features || "assoc_PAT" %in% features)
      assoc_pairs<- make_assoc_pairs(lemma_list = lemma_list)
    if ("assoc_PAC" %in% features){
      # make pairs of words and show whether they have connection via associative norms
      assoc_PAC = create_assoc_PAC(aoa_frame = aoa_frame, assoc_pairs = assoc_pairs)
    }
    if ("assoc_PAT" %in% features){
      assoc_PAT = create_assoc_PAT(aoa_frame = aoa_frame, assoc_pairs = assoc_pairs)
    }
    
    if ("McRae_PAC" %in% features || "McRae_PAT" %in% features)
      McRae_pairs<- make_McRae_pairs(words_list = lemma_list) 
    if ("McRae_PAC" %in% features){
      # make pairs of words and show how many McRae features they share
      for (threshold in thresholds){
        McRae_PAC = create_McRae_PAC(aoa_frame = aoa_frame, 
                         McRae_pairs = McRae_pairs %>% McRae_threshold(threshold = 1),
                         threshold = threshold)
      }
    }
    if ("McRae_PAT" %in% features){
      for (threshold in thresholds){
        McRae_PAT = create_McRae_PAT(aoa_frame = aoa_frame, 
                         McRae_pairs = McRae_pairs %>% McRae_threshold(threshold = 1),
                         threshold = threshold)
      }
    }
    
  }
}

###################################################################################################
create_McRae_PAC<-function(aoa_frame, McRae_pairs, threshold){
  McRae_PAC<- PAC_generator(vocab_age = aoa_frame, word_pairs = McRae_pairs)
  write_out_csv(var = McRae_PAC, lang = lang_name, type = paste("McRae_PAC_t", threshold, suffix, sep = ""))
  return(McRae_PAC)
}

create_McRae_PAT<-function(aoa_frame, McRae_pairs, threshold){
  McRae_PAT<- PAT_generator(vocab_age = aoa_frame, word_pairs = McRae_pairs )
  write_out_csv(var = McRae_PAT, lang = lang_name, type = paste("McRae_PAT_t", threshold, suffix, sep = ""))
  return(McRae_PAT)
}

create_McRae_distinct_PAC<-function(aoa_frame, McRae_distinct_pairs, threshold){
  McRae_PAC<- PAC_generator(vocab_age = aoa_frame, word_pairs = McRae_pairs)
  write_out_csv(var = McRae_PAC, lang = lang_name, type = paste("McRaeD_PAC_t", threshold, suffix, sep = ""))
  return(McRae_PAC)
}

create_McRae_distinct_PAT<-function(aoa_frame, McRae_distinct_pairs, threshold){
  McRae_PAT<- PAT_generator(vocab_age = aoa_frame, word_pairs = McRae_pairs)
  write_out_csv(var = McRae_PAT, lang = lang_name, type = paste("McRaeD_PAT_t", threshold, suffix, sep = ""))
  return(McRae_PAT)
}


create_assoc_PAC<- function(aoa_frame, assoc_pairs){
  assoc_PAC<- PAC_generator(vocab_age = aoa_frame, word_pairs = assoc_pairs )
  write_out_csv(var = assoc_PAC, lang = lang_name, type = paste("assoc_PAC", suffix, sep = ""))
  return(assoc_PAC)
}

create_assoc_PAT<- function(aoa_frame, assoc_pairs){
  assoc_PAT<- PAT_generator(vocab_age = aoa_frame, word_pairs = assoc_pairs )
  write_out_csv(var = assoc_PAT, lang = lang_name, type = paste("assoc_PAT", suffix, sep = ""))
  return(assoc_PAT)
}

create_phono_PAC<- function(aoa_frame, phono_pairs, threshold){
  phono_PAC<- PAC_generator(vocab_age = aoa_frame, word_pairs = phono_pairs)
  write_out_csv(var = phono_PAC, lang = lang_name, type = paste("phono_PAC_t", threshold, sep = ""))
  return(phono_PAC)
}

create_phono_PAT<- function(aoa_frame, phono_pairs, threshold){
  phono_PAT<- PAT_generator(vocab_age = aoa_frame, word_pairs = phono_pairs)
  write_out_csv(var = phono_PAT, lang = lang_name, type = paste("phono_PAT_t", threshold, sep = ""))
  return(phono_PAT)
}

create_phono_PAC_nt<- function(aoa_frame, phono_pairs){
  phono_PAC<- PAC_generator(vocab_age = aoa_frame, word_pairs = phono_pairs)
  write_out_csv(var = phono_PAC, lang = lang_name, type = "phono_PAC_nt")
  return(phono_PAC)
}

create_phono_PAT_nt<- function(aoa_frame, phono_pairs){
  phono_PAT<- PAT_generator(vocab_age = aoa_frame, word_pairs = phono_pairs)
  write_out_csv(var = phono_PAT, lang = lang_name, type = "phono_PAT_nt")
  return(phono_PAT)
}

