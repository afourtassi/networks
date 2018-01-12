get_confint<- function(language, type, r = 3000){
  optim_par<- optim(0, loglike1)$par
  full_model_nLLK<- optim(0, loglike1)$value
  null_model_nLLK<- optim(0, loglike0)$value
  dataset=final_df %>% mutate(n=row_number())
  ci_value1 <- vector(mode = "numeric", length = r)
  #ci_value2 <- vector(mode = "numeric", length = r)
  n_sample <- length(dataset[, 1])
  for (i in 1:r) {
    cur_sample <- as.data.frame(dataset$n[sample(n_sample, n_sample, replace = T)])
    colnames(cur_sample) <- "n"
    final_df <<- cur_sample %>% left_join(dataset)
    ci_value1[i] <- optim(0, loglike1)$par[1]
    #ci_value2[i] <- optim(c(0,0), loglike2)$par[2]
  }
  ci_value1 <- sort(ci_value1, decreasing = F)
  len <- length(ci_value1)
  #ci_value2 <- sort(ci_value2, decreasing = F)
  
  new_row<-c(language, type,
             ci_value1[as.integer(0.025 * len+1)],  optim_par,ci_value1[as.integer(0.975 * len)], 
             full_model_nLLK, null_model_nLLK)
  return(new_row)
}


# A bunch of optims
######################################################################################################
# Return the nLLK of each model
optim_nllk <- function(par_n) {
  if (par_n == 2) {
    random_model <- optim(c(0, 0), loglike00)$value
    two_par_model <- optim(c(0, 0), loglike11)$value
    first_par_model <- optim(c(0, 0), loglike10)$value
    second_par_model <- optim(c(0, 0), loglike01)$value
    
    return(data.frame(random_model,
                      two_par_model,
                      first_par_model,
                      second_par_model, row.names = "nLLK"))
  }
  else {
    random_model <- optim(0, loglike0)$value
    model <- optim(0, loglike1)$value
    return(data.frame(random_model, model, row.names = "nLLK"))
  }
}

######################################################################################################
# Return the optimized parameters
optim_par <- function(par_n) {
  if (par_n == 2) {
    random_model <- optim(c(0, 0), loglike00)$par
    two_par_model <- optim(c(0, 0), loglike11)$par
    first_par_model <- optim(c(0, 0), loglike10)$par
    second_par_model <- optim(c(0, 0), loglike01)$par
    return(data.frame(random_model,
                      two_par_model,
                      first_par_model,
                      second_par_model,
                      row.names = c("parameter_1", "parameter_2")))
  }
  else {
    random_model <- optim(0, loglike0)$par
    model <- optim(0, loglike1)$par
    return(data.frame(random_model, model,row.names = "parameter"))
  }
}
######################################################################################################
# This is the loglikelihood function we're trying to MINIMIZE
loglike1 <- function(beta){
  
  # calculate numerator
  learned_words<- final_df %>%
    filter(learned==1) %>%
    mutate(numerator=exp(value*beta)) %>%
    select(age, numerator)
  
  # calculate denumerator
  all_words<- final_df %>%
    group_by(age) %>%
    summarise(denominator=sum(exp(value*beta))) %>%
    select(age, denominator)
  
  # calculate the return value
  ret<- learned_words %>%
    left_join(all_words) %>%
    summarise(nLLK=sum(log(numerator/denominator)))
  
  return(-ret$nLLK)
  
}

######################################################################################################
loglike0 <- function(beta){
  beta=0
  
  # calculate numerator
  learned_words<- final_df %>%
    filter(learned==1) %>%
    mutate(numerator=exp(value*beta)) %>%
    select(age, numerator)
  
  # calculate denumerator
  all_words<- final_df %>%
    group_by(age) %>%
    summarise(denominator=sum(exp(value*beta))) %>%
    select(age, denominator)
  
  # calculate the return value
  ret<- learned_words %>%
    left_join(all_words) %>%
    summarise(nLLK=sum(log(numerator/denominator)))
  
  return(-ret$nLLK)
  
}
######################################################################################################
loglike10 <- function(betas){
  beta<-betas[1]
  
  # calculate numerator
  learned_words<- final_df %>%
    filter(learned==1) %>%
    mutate(numerator=exp(value*beta)) %>%
    select(age, numerator)
  
  # calculate denumerator
  all_words<- final_df %>%
    group_by(age) %>%
    summarise(denominator=sum(exp(value*beta))) %>%
    select(age, denominator)
  
  # calculate the return value
  ret<- learned_words %>%
    left_join(all_words) %>%
    summarise(nLLK=sum(log(numerator/denominator)))
  
  return(-ret$nLLK)
  
}

######################################################################################################
loglike01 <- function(betas){
  
  beta2<- betas[2]
  
  # calculate numerator
  learned_words<- final_df %>%
    filter(learned==1) %>%
    mutate(numerator=exp(value2*beta2)) %>%
    select(age, numerator)
  
  # calculate denumerator
  all_words<- final_df %>%
    group_by(age) %>%
    summarise(denominator=sum(exp(value2*beta2))) %>%
    select(age, denominator)
  
  # calculate the return value
  ret<- learned_words %>%
    left_join(all_words) %>%
    summarise(nLLK=sum(log(numerator/denominator)))
  
  return(-ret$nLLK)
  
}

######################################################################################################
loglike11 <- function(betas){
  beta<-betas[1]
  beta2<- betas[2]
  
  # calculate numerator
  learned_words<- final_df %>%
    filter(learned==1) %>%
    mutate(numerator=exp(value*beta+value2*beta2)) %>%
    select(age, numerator)
  
  # calculate denumerator
  all_words<- final_df %>%
    group_by(age) %>%
    summarise(denominator=sum(exp(value*beta+value2*beta2))) %>%
    select(age, denominator)
  
  # calculate the return value
  ret<- learned_words %>%
    left_join(all_words) %>%
    summarise(nLLK=sum(log(numerator/denominator)))
  
  return(-ret$nLLK)
  
}

######################################################################################################
loglike00 <- function(betas){
  
  beta=0
  beta2=0
  # calculate numerator
  learned_words<- final_df %>%
    filter(learned==1) %>%
    mutate(numerator=exp(value*beta+value2*beta2)) %>%
    select(age, numerator)
  
  # calculate denumerator
  all_words<- final_df %>%
    group_by(age) %>%
    summarise(denominator=sum(exp(value*beta+value2*beta2))) %>%
    select(age, denominator)
  
  # calculate the return value
  ret<- learned_words %>%
    left_join(all_words) %>%
    summarise(nLLK=sum(log(numerator/denominator)))
  
  return(-ret$nLLK)
  
}

######################################################################################################
loglike111 <- function(betas, control){
  
  beta<- betas[1]
  beta2<- betas[2]
  beta3<- betas[3]
  
  # calculate numerator
  learned_words<- final_df %>%
    filter(learned==1) %>%
    mutate(numerator=exp(value*beta+value2*beta2+value3*beta3)) %>%
    select(age, numerator)
  
  # calculate denumerator
  all_words<- final_df %>%
    group_by(age) %>%
    summarise(denominator=sum(exp(value*beta+value2*beta2+value3*beta3))) %>%
    select(age, denominator)
  
  # calculate the return value
  ret<- learned_words %>%
    left_join(all_words) %>%
    summarise(nLLK=sum(log(numerator/denominator)))
  
  return(-ret$nLLK)
  
}


######################################################################################################
loglike011 <- function(betas, control){
  
  beta2<- betas[2]
  beta3<- betas[3]
  
  # calculate numerator
  learned_words<- final_df %>%
    filter(learned==1) %>%
    mutate(numerator=exp(value2*beta2+value3*beta3)) %>%
    select(age, numerator)
  
  # calculate denumerator
  all_words<- final_df %>%
    group_by(age) %>%
    summarise(denominator=sum(exp(value2*beta2+value3*beta3))) %>%
    select(age, denominator)
  
  # calculate the return value
  ret<- learned_words %>%
    left_join(all_words) %>%
    summarise(nLLK=sum(log(numerator/denominator)))
  
  return(-ret$nLLK)
  
}