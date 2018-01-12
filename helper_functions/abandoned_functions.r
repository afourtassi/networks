

# 
# make_assoc_pairs <- function(lemma_list) {
#   #Filter all the cues and targets that are not learnt after 30 months and are not normed
#   cue_target <- read.csv(paste(getwd(), "/in_files/assoc_table.csv", sep = ""), as.is = T) %>%
#     filter(cue %in% lemma_list$uni_lemma,
#            target %in% lemma_list$uni_lemma)
#   
#   #make a association network dataframe with item number
#   #rename stuffs so it could conform to the format PAT_generator needs
#   #item corresponds to target ;  pair corresponds to cue
#   assoc_link <- cue_target %>%
#     rename(pair.definition = cue) %>%
#     left_join(lemma_list, c("pair.definition" = "uni_lemma")) %>%
#     rename(pair = item, item.definition = target) %>%
#     left_join(lemma_list, c("item.definition" = "uni_lemma")) %>%
#     select(item, item.definition, pair, pair.definition, link) %>%
#     arrange(item, pair)
#   
#   return(assoc_link)
# }

#########################################################################################################

# make_assoc_pairs <- function(lemma_list) {
#   #Filter all the cues and targets that are not learnt after 30 months and are not normed
#   cue_target <- read.csv(paste(getwd(), "/in_files/assoc_table.csv", sep = ""), as.is = T) %>%
#     filter(cue %in% lemma_list$uni_lemma,
#            target %in% lemma_list$uni_lemma)
# 
#   #make a association network dataframe with item number
#   #rename stuffs so it could conform to the format PAT_generator needs
#   #item corresponds to target ;  pair corresponds to cue
#   assoc_link <- cue_target %>%
#     select(cue, target) %>%
#     rename(pair.definition = cue) %>%
#     left_join(lemma_list, c("pair.definition" = "uni_lemma")) %>%
#     rename(pair = item, item.definition = target) %>%
#     left_join(lemma_list, c("item.definition" = "uni_lemma")) %>%
#     select(item, item.definition, pair, pair.definition) %>%
#     arrange(item, pair) %>%
#     mutate(link = 1)
#   
#   return(assoc_link)
# }

# ######################################################################################################
# #change representation of CMUdict
# dict.conv<-function(str_list){
#   phoneme<-c("AA","AE","AH","AO","AW","AY","B","CH","D","DH",
#              "EH","ER","EY","F","G","HH","IH","IY","JH","K",
#              "L","M","N","NG","OW","OY","P","R","S","SH","T",
#              "TH","UH","UW","V","W","Y","Z","ZH","1","2")
#   converted<-c("a","b","c","d","e","f","g","h","i","j","k","l",
#                "m","n","o","p","q","r","s","t","u","v","w","x",
#                "y","z","A","B","C","D","E","F","G","H","I","J",
#                "K","L","M","1","2")
#   for (x in str_list){
#     index<- which(str_list==x)
#     str_list[index]=converted[which(phoneme==x)]
#   }
#   return(str_list)
# }
# 
# ######################################################################################################
# #look up the phonetic transcription in CMUdict
# #phon<- read.csv(paste(getwd(),"/in_files/phon_dict.csv",sep = ""), as.is = TRUE)
# lookup<- function(someword){
#   phon<- read.csv(paste(getwd(),"/in_files/phon_dict.csv",sep = ""), as.is = TRUE)
#   index<- which(phon$word==someword)
#   return(phon$phonetic[index])
# }
# 
# ######################################################################################################
# #Define phonological distance function
# phon.distance <- function(x,y){
#   x<-lookup(x)
#   y<-lookup(y)
#   x<-gsub("0","",x)
#   y<-gsub("0","",y)
#   x<-gsub("1",":1",x)
#   x<-gsub("2",":2",x)
#   y<-gsub("1",":1",y)
#   y<-gsub("2",":2",y)
#   x_vec <- unlist(strsplit(x, ":","1","2"))
#   y_vec <- unlist(strsplit(y, ":","1","2"))
#   x_conv <- dict.conv(x_vec)
#   y_conv <- dict.conv(y_vec)
#   x_str <- paste(x_conv,collapse = "")
#   y_str <- paste(y_conv,collapse = "")
#   return(adist(x_str,y_str))
# }

# ######################################################################################################

# draw_PAC<-function(){
# lm_PAC<- assoc_PAC %>% filter(learned==1) %>%  select(month,value) 
# 
# growth_value<-lm_PAC$value
# month<-lm_PAC$month
# paclm<-lm(growth_value ~ month)
# summary(paclm)
# 
# 
# ggplot(lm_PAC , aes(x=month, y=growth_value))+
#   geom_point(alpha=1/2, position = position_jitter(h=0))+
#   scale_y_continuous(breaks = seq(0,40, by=10))+
#   scale_x_continuous(breaks = seq(16,30, by=1))+
#   geom_smooth(method = "lm", se = FALSE)
# 
# }

# ##########################################################################################################
# PAC_generator_od<- function(vocab_month, pairs){
#   vocab_month<- vocab_month %>% filter((item %in% assoc_pairs$item) | (item %in% assoc_pairs$pair) )
#   
#   item_value<- pairs %>% 
#     group_by(pair) %>%
#     summarise(value=sum(link)) %>%
#     rename(item=pair)
#   
#   PAC<-vocab_month %>%
#     mutate(value=0) %>%
#     rowwise() %>%
#     mutate(value=ifelse((item %in% item_value$item), item_value$value[which(item_value$item ==item)], 0)) 
#   
#   return(PAC)
# }
# 
# ###########################################################################################################