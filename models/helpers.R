helper <- '

var levels = function(a, lvl){ return _.uniq(_.pluck(a, lvl)) }

var months = levels(data, "age");

var newData = map(function(month){

//Select the data that correspond to each month
var itemInfo = {age: month}
var itemData = _.where(data, itemInfo)

//prepare data for a dictionary with the utilies (freq, length, Sem_PAC, Phono_PAC)

//frequency
//var value_freq  =  map(function(single){
//return _.object([single.item], [single.freq])
//}, itemData)

//Length 
//var value_len  =  map(function(single){
//return _.object([single.item], [single.length])
//}, itemData)

//Sem PAC
var value_PAC_assoc  =  map(function(single){
return _.object([single.item], [single.PAC_assoc])
}, itemData)

//Sem PAT
var value_PAT_assoc  =  map(function(single){
return _.object([single.item], [single.PAT_assoc])
}, itemData)

//Phono PAC t2
var value_PAC_phono  =  map(function(single){
return _.object([single.item], [single.PAC_phono_t2])
}, itemData)

//Phono PAT t2
var value_PAT_phono  =  map(function(single){
return _.object([single.item], [single.PAT_phono_t2])
}, itemData)

//Also prepare data for a dictionary with "definitions" as keys and boolean "learned" as values
var learned_pair = map(function(single){
return _.object([single.item], [single.learned])
}, itemData)

//now get the items
var word = map(function(w){_.keys(w)[0]}, value_PAC_assoc)

//get the utilities scores
//var util_freq = map(function(w){w[_.keys(w)[0]]}, value_freq)
//var util_len = map(function(w){w[_.keys(w)[0]]}, value_len)
var util_PAC_assoc = map(function(w){w[_.keys(w)[0]]}, value_PAC_assoc)
var util_PAC_phono = map(function(w){w[_.keys(w)[0]]}, value_PAC_phono)
var util_PAT_assoc = map(function(w){w[_.keys(w)[0]]}, value_PAT_assoc)
var util_PAT_phono = map(function(w){w[_.keys(w)[0]]}, value_PAT_phono)

//get "if learned" boolean score
var learned_score = map(function(w){w[_.keys(w)[0]]}, learned_pair)    

//output the utility dictionary
//var frequency= _.object(word, util_freq)
//var length= _.object(word, util_len)
var PAC_assoc= _.object(word, util_PAC_assoc)
var PAC_phono= _.object(word, util_PAC_phono)
var PAT_assoc= _.object(word, util_PAT_assoc)
var PAT_phono= _.object(word, util_PAT_phono)

//output the "if learned" dictionary
var isLearnt = _.object(word, learned_score)

//Combine everything

//var month_sub_dict = _.object(["frequency","length","PAC_assoc","PAC_phono", "PAT_assoc", "PAT_phono", "isLearnt"],
//[frequency, length, PAC_assoc, PAC_phono, PAT_assoc, PAT_phono, isLearnt])

var month_sub_dict = _.object(["PAC_assoc","PAC_phono", "PAT_assoc", "PAT_phono", "isLearnt"],
[PAC_assoc, PAC_phono, PAT_assoc, PAT_phono, isLearnt])

var month_dict =  _.object([month], [month_sub_dict])

return month_dict

}, months)

var month_keys = map(function(w){_.keys(w)[0]}, newData )  
var month_values = map(function(w){w[_.keys(w)[0]]}, newData)

var learning = _.object(month_keys, month_values)

'