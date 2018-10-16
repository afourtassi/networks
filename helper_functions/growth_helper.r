# PAT_generator
# word_pairs argument should have the format below:
# column name: 
# item, item.definition, pair, pair.definition, link


PAT_generator<-function(vocab_age, word_pairs){
  vocab_age<-vocab_age %>%
    filter(item %in% word_pairs$item) %>%
    mutate(value=NA) %>%
    arrange(age, item)
  all_ages<- (vocab_age %>%
    group_by(age) %>%
    summarise(n=n()))$age
  ages<- all_ages[2:length(all_ages)]
  for (i in ages){
    #current items in this age 
    curr_items<- (vocab_age %>%
                    filter(age==i) %>%
                    select(item))$item
    #item learnt in previous ages
    exist_words<-(vocab_age %>%
                    filter(age<i,learned==1) %>%
                    select(item))$item
    #calculating the degree of nodes in the existing network
    sem_acq<- word_pairs %>%
      filter(item %in% exist_words, pair %in% exist_words) %>%
      group_by(item) %>%
      summarise(value=sum(link))

    row_n<-which(vocab_age$age==i & vocab_age$item==curr_items[1])
    for (j in curr_items){
      #calculating d
      
      #here determine the nodes to which candidate word "j" is related
      link_to_exist<-(word_pairs %>% filter(pair==j, item %in% exist_words, link==1))$item
      PAT_value<-sem_acq %>%
        filter(item %in% link_to_exist)
      vocab_age$value[row_n]<- mean(PAT_value$value) #here the "mean" is the crucial thing
      row_n<-row_n+1
    }
  }
  #filter first age since there are no PAT value
  vocab_age<-vocab_age %>% filter(age!=(all_ages[1]))
  return(vocab_age)
}

######################################################################################################################

PAT_nt_generator<-function(vocab_age, word_pairs){
  vocab_age<-vocab_age %>%
    filter(item %in% word_pairs$item) %>%
    mutate(value=NA) %>%
    arrange(age, item)
  all_ages<- (vocab_age %>%
                group_by(age) %>%
                summarise(n=n()))$age
  ages<- all_ages[2:length(all_ages)]
  for (i in ages){
    #current items in this age
    curr_items<- (vocab_age %>%
                    filter(age==i) %>%
                    select(item))$item
    #item learnt in previous ages
    exist_words<-(vocab_age %>%
                    filter(age<i,learned==1) %>%
                    select(item))$item
    #calculating the degree of nodes in the existing network
    sem_acq<- word_pairs %>%
      filter(item %in% exist_words, pair %in% exist_words) %>%
      group_by(item) %>%
      summarise(value=sum(link))
    
    row_n<-which(vocab_age$age==i & vocab_age$item==curr_items[1])
    for (j in curr_items){
      #calculating d
      link_to_exist<-(word_pairs %>% filter(pair==j, item %in% exist_words))$item
      PAT_value<-sem_acq %>%
        filter(item %in% link_to_exist)
      vocab_age$value[row_n]<- mean(PAT_value$value)
      row_n<-row_n+1
    }
  }
  #filter first age since there are no PAT value
  vocab_age<-vocab_age %>% filter(age!=(all_ages[1]))
  return(vocab_age)
}

######################################################################################################################

PAC_generator<- function(vocab_age, word_pairs){
  
  #This condition is what reduces the number of words, keeping only items that are in the list of pairs
  #To be kept in the list of pairs, a word has to appear as a cue or as target in the free associations  
  vocab_age<- vocab_age %>% filter((item %in% word_pairs$item) | (item %in% word_pairs$pair) )
  
  word_pairs<- word_pairs %>% filter(item %in% vocab_age$item, pair %in% vocab_age$item)
  
  item_value<- word_pairs %>% 
    group_by(item) %>%
    summarise(value=sum(link))
  
  PAC<-vocab_age %>%
    mutate(value=0) %>%
    rowwise() %>%
    mutate(value=ifelse((item %in% item_value$item), item_value$value[which(item_value$item ==item)], 0)) %>%
    ungroup()
  
  return(PAC)
}

######################################################################################################################

PAC_nt_generator<- function(vocab_age, word_pairs){
  vocab_age<- vocab_age %>% filter((item %in% word_pairs$item) | (item %in% word_pairs$pair) )
  word_pairs<- word_pairs %>% filter(item %in% vocab_age$item, pair %in% vocab_age$item)
  
  item_value<- word_pairs %>% 
    group_by(item) %>%
    summarise(value=sum(link))
  
  PAC<-vocab_age %>%
    mutate(value=0) %>%
    rowwise() %>%
    mutate(value=ifelse((item %in% item_value$item), item_value$value[which(item_value$item ==item)], 0)) %>%
    ungroup()
  
  return(PAC)
}

######################################################################################################################

# DAC_generator<-function(vocab_age, word_pairs){
#   vocab_age<-vocab_age %>%
#     filter(item %in% word_pairs$item) %>%
#     mutate(value=NA) %>%
#     arrange(age, item)
#   ages<- (vocab_age %>%
#             group_by(age) %>%
#             summarise(n=n()))$age
#   ages<- ages[2:length(ages)]
#   for (i in ages){
#     #current items in this age
#     curr_items<- (vocab_age %>%
#                     filter(age==i) %>%
#                     select(item))$item
#     #item learnt in previous ages
#     exist_words<-(vocab_age %>%
#                     filter(age<i,learned==1) %>%
#                     select(item))$item
#     
#     row_n<-which(vocab_age$age==i & vocab_age$item==curr_items[1])
#     for (j in curr_items){
#       word_value<- word_pairs %>% filter(pair == j, item %in% exist_words) %>% 
#         group_by(pair) %>% 
#         summarise(n=sum(link))
#       
#       vocab_age$value[row_n]<- word_value$n[which(word_value$pair==j)]
#       row_n<-row_n+1
#     }
#   }
#   #filter first age since there are no PAT value
#   vocab_age<-vocab_age %>% filter(age!=(ages[1]-1))
#   return(vocab_age)
# }



# # PAT_generator
# # not just for semantic feature networks
# # First argument should have the format below:
# # column name: 
# # item, item.definition, pair, pair.definition, link
# 
# PAT_generator<-function(vocab_month, word_pairs){
#   vocab_month<-vocab_month %>%
#     filter(item %in% word_pairs$item) %>%
#     mutate(value=NA) %>%
#     arrange(month, item)
#   first<- vocab_month$month[1]+1
#   last<- vocab_month$month[length(vocab_month$month)]
#   for (i in first:last){
#     #items learnt in this month
#     curr_items<- (vocab_month %>%
#                     filter(month==i) %>%
#                     select(item))$item
#     #item learnt in previous months
#     exist_words<-(vocab_month %>%
#                     filter(month<i,learned==1) %>%
#                     select(item))$item
#     #calculating the degree of nodes in the existing network
#     sem_acq<- word_pairs %>%
#       filter(item %in% exist_words, pair %in% exist_words) %>%
#       group_by(item) %>%
#       summarise(value=sum(link))
# 
#     row_n<-which(vocab_month$month==i & vocab_month$item==curr_items[1])
#     for (j in curr_items){
#       #calculating d
#       link_to_exist<-(word_pairs %>% filter(pair==j, item %in% exist_words, link==1))$item
#       PAT_value<-sem_acq %>%
#         filter(item %in% link_to_exist)
#       vocab_month$value[row_n]<- sum(PAT_value$value)
#       row_n<-row_n+1
#     }
#   }
#   #filter first month since there are no PAT value
#   vocab_month<-vocab_month %>% filter(month!=(first-1))
#   return(vocab_month)
# }
# 
# ######################################################################################################################
# 
# PAC_generator<- function(vocab_month, word_pairs){
#   vocab_month<- vocab_month %>% filter((item %in% word_pairs$item) | (item %in% word_pairs$pair) )
#   word_pairs<- word_pairs %>% filter(item %in% vocab_month$item, pair %in% vocab_month$item)
#   
#   item_value<- word_pairs %>% 
#     group_by(item) %>%
#     summarise(value=sum(link))
#   
#   PAC<-vocab_month %>%
#     mutate(value=0) %>%
#     rowwise() %>%
#     mutate(value=ifelse((item %in% item_value$item), item_value$value[which(item_value$item ==item)], 0)) %>%
#     ungroup()
#   
#   return(PAC)
# }
# 
# ######################################################################################################################
# 
