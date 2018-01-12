freq <- '
var model = cache(function(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month) {
return Infer({method:"enumerate"}, function(){
var wordlist = _.keys(learning[month]["isLearnt"])
var word = uniformDraw(wordlist);
var utility_freq = learning[month]["frequency"]
factor(alpha1 * utility_freq[word])

return word;
});
});
'


len <- '
var model = cache(function(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month) {
return Infer({method:"enumerate"}, function(){
var wordlist = _.keys(learning[month]["isLearnt"])
var word = uniformDraw(wordlist);
var utility_len = learning[month]["length"]
factor(alpha1 * utility_len[word])

return word;
});
});

'


sem_PAC <- '
var model = cache(function(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month) {
return Infer({method:"enumerate"}, function(){
var wordlist = _.keys(learning[month]["isLearnt"])
var word = uniformDraw(wordlist);
var utility_sem = learning[month]["PAC_assoc"]
factor(alpha1 * utility_sem[word])

return word;
});
});

'

sem_PAT <- '
var model = cache(function(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month) {
return Infer({method:"enumerate"}, function(){
var wordlist = _.keys(learning[month]["isLearnt"])
var word = uniformDraw(wordlist);
var utility_sem = learning[month]["PAT_assoc"]
factor(alpha1 * utility_sem[word])

return word;
});
});

'

phono_PAC <- '
var model = cache(function(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month) {
return Infer({method:"enumerate"}, function(){
var wordlist = _.keys(learning[month]["isLearnt"])
var word = uniformDraw(wordlist);
var utility_phono = learning[month]["PAC_phono"]
factor(alpha1 * utility_phono[word])
return word;
});
});

'

phono_PAT <- '
var model = cache(function(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month) {
return Infer({method:"enumerate"}, function(){
var wordlist = _.keys(learning[month]["isLearnt"])
var word = uniformDraw(wordlist);
var utility_phono = learning[month]["PAT_phono"]
factor(alpha1 * utility_phono[word])
return word;
});
});

'