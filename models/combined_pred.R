freq_len <- '
var model = cache(function(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month) {
return Infer({method:"enumerate"}, function(){
var wordlist = _.keys(learning[month]["isLearnt"])
var word = uniformDraw(wordlist);
var utility_freq = learning[month]["frequency"]
var utility_len = learning[month]["length"]
factor(alpha1 * utility_freq[word]
+ alpha2 * utility_len[word]
);
return word;
});
});

'

sem_phono <- '
var model = cache(function(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month) {
return Infer({method:"enumerate"}, function(){
var wordlist = _.keys(learning[month]["isLearnt"])
var word = uniformDraw(wordlist);
var utility_sem= learning[month]["PAC_assoc"]
var utility_phono= learning[month]["PAC_phono"]
factor( alpha1 * utility_sem[word]
+ alpha2* utility_phono[word]
);
return word;
});
});
'



freq_sem <- '
var model= cache(function(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month) {
return Infer({method:"enumerate"}, function(){
var wordlist = _.keys(learning[month]["isLearnt"])
var word = uniformDraw(wordlist);
var utility_freq = learning[month]["frequency"]
var utility_sem= learning[month]["PAC_assoc"]
factor(alpha1 * utility_freq[word]
+ alpha2 * utility_sem[word]
);
return word;
});
});
'

freq_phono <- '
var model= cache(function(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month) {
return Infer({method:"enumerate"}, function(){
var wordlist = _.keys(learning[month]["isLearnt"])
var word = uniformDraw(wordlist);
var utility_freq = learning[month]["frequency"]
var utility_phono= learning[month]["PAC_phono"]
factor(alpha1 * utility_freq[word]
+ alpha2 * utility_phono[word]
);
return word;
});
});
'


freq_sem_phono<- '
var model= cache(function(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month) {
return Infer({method:"enumerate"}, function(){
var wordlist = _.keys(learning[month]["isLearnt"])
var word = uniformDraw(wordlist);
var utility_freq = learning[month]["frequency"]
var utility_sem= learning[month]["PAC_assoc"]
var utility_phono= learning[month]["PAC_phono"]
factor(alpha1 * utility_freq[word]
+ alpha2 * utility_sem[word]
+ alpha3 * utility_phono[word]
);
return word;
});
});
'


len_sem <- '
var model= cache(function(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month) {
return Infer({method:"enumerate"}, function(){
var wordlist = _.keys(learning[month]["isLearnt"])
var word = uniformDraw(wordlist);
var utility_len = learning[month]["length"]
var utility_sem= learning[month]["PAC_assoc"]
factor(alpha1 * utility_len[word]
+ alpha2 * utility_sem[word]
);
return word;
});
});
'


len_phono <- '
var model= cache(function(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month) {
return Infer({method:"enumerate"}, function(){
var wordlist = _.keys(learning[month]["isLearnt"])
var word = uniformDraw(wordlist);
var utility_len = learning[month]["length"]
var utility_phono= learning[month]["PAC_phono"]
factor(alpha1 * utility_len[word]
+ alpha2 * utility_phono[word]
);
return word;
});
});
'


len_sem_phono<- '
var model= cache(function(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month) {
return Infer({method:"enumerate"}, function(){
var wordlist = _.keys(learning[month]["isLearnt"])
var word = uniformDraw(wordlist);
var utility_len = learning[month]["length"]
var utility_sem= learning[month]["PAC_assoc"]
var utility_phono= learning[month]["PAC_phono"]
factor(alpha1 * utility_len[word]
+ alpha2 * utility_sem[word]
+ alpha3 * utility_phono[word]
);
return word;
});
});
'


freq_len_sem_phono <- '
var model = cache(function(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month) {
return Infer({method:"enumerate"}, function(){
var wordlist = _.keys(learning[month]["isLearnt"])
var word = uniformDraw(wordlist);
var utility_freq = learning[month]["frequency"]
var utility_len = learning[month]["length"]
var utility_sem= learning[month]["PAC_assoc"]
var utility_phono= learning[month]["PAC_phono"]
factor(alpha1 * utility_freq[word]
+ alpha2 * utility_len[word]
+ alpha3 * utility_sem[word]
+ alpha4 * utility_phono[word]
);
return word;
});
});
'


semPAC_semPAT <- '
var model= cache(function(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month) {
return Infer({method:"enumerate"}, function(){
var wordlist = _.keys(learning[month]["isLearnt"])
var word = uniformDraw(wordlist);
var utility_semPAC = learning[month]["PAC_assoc"]
var utility_semPAT = learning[month]["PAT_assoc"]
factor(alpha1 * utility_semPAC[word]
+ alpha2 * utility_semPAT[word]
);
return word;
});
});
'

phonoPAC_phonoPAT <- '
var model= cache(function(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month) {
return Infer({method:"enumerate"}, function(){
var wordlist = _.keys(learning[month]["isLearnt"])
var word = uniformDraw(wordlist);
var utility_phonoPAC = learning[month]["PAC_phono"]
var utility_phonoPAT = learning[month]["PAT_phono"]
factor(alpha1 * utility_phonoPAC[word]
+ alpha2 * utility_phonoPAT[word]
);
return word;
});
});
'

semPAC_phonoPAC <- '
var model= cache(function(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month) {
return Infer({method:"enumerate"}, function(){
var wordlist = _.keys(learning[month]["isLearnt"])
var word = uniformDraw(wordlist);
var utility_semPAC = learning[month]["PAC_assoc"]
var utility_phonoPAC = learning[month]["PAC_phono"]
factor(alpha1 * utility_semPAC[word]
+ alpha2 * utility_phonoPAC[word]
);
return word;
});
});
'

semPAT_phonoPAT <- '
var model= cache(function(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month) {
return Infer({method:"enumerate"}, function(){
var wordlist = _.keys(learning[month]["isLearnt"])
var word = uniformDraw(wordlist);
var utility_semPAT = learning[month]["PAT_assoc"]
var utility_phonoPAT = learning[month]["PAT_phono"]
factor(alpha1 * utility_semPAT[word]
+ alpha2 * utility_phonoPAT[word]
);
return word;
});
});
'

semPAT_phonoPAC <- '
var model= cache(function(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month) {
return Infer({method:"enumerate"}, function(){
var wordlist = _.keys(learning[month]["isLearnt"])
var word = uniformDraw(wordlist);
var utility_semPAT = learning[month]["PAT_assoc"]
var utility_phonoPAC = learning[month]["PAC_phono"]
factor(alpha1 * utility_semPAT[word]
+ alpha2 * utility_phonoPAC[word]
);
return word;
});
});
'

semPAC_phonoPAT <- '
var model= cache(function(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month) {
return Infer({method:"enumerate"}, function(){
var wordlist = _.keys(learning[month]["isLearnt"])
var word = uniformDraw(wordlist);
var utility_semPAC = learning[month]["PAC_assoc"]
var utility_phonoPAT = learning[month]["PAT_phono"]
factor(alpha1 * utility_semPAC[word]
+ alpha2 * utility_phonoPAT[word]
);
return word;
});
});
'

semPAC_phonoPAC_semPAT_phonoPAT <- '
var model= cache(function(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month) {
return Infer({method:"enumerate"}, function(){
var wordlist = _.keys(learning[month]["isLearnt"])
var word = uniformDraw(wordlist);
var utility_semPAC = learning[month]["PAC_assoc"]
var utility_phonoPAC = learning[month]["PAC_phono"]
var utility_semPAT = learning[month]["PAT_assoc"]
var utility_phonoPAT = learning[month]["PAT_phono"]

factor(alpha1 * utility_semPAC[word]
+ alpha2 * utility_phonoPAC[word] 
+ alpha3 * utility_semPAT[word] 
+ alpha4 * utility_phonoPAT[word] 
);
return word;
});
});
'

freq_len_semPAC_phonoPAC_semPAT_phonoPAT <- '
var model= cache(function(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month) {
return Infer({method:"enumerate"}, function(){
var wordlist = _.keys(learning[month]["isLearnt"])
var word = uniformDraw(wordlist);
var utility_freq = learning[month]["frequency"]
var utility_len = learning[month]["length"]
var utility_semPAC = learning[month]["PAC_assoc"]
var utility_phonoPAC = learning[month]["PAC_phono"]
var utility_semPAT = learning[month]["PAT_assoc"]
var utility_phonoPAT = learning[month]["PAT_phono"]

factor(alpha1 * utility_freq[word]
+ alpha2 * utility_len[word]
+ alpha3 * utility_semPAC[word]
+ alpha4 * utility_phonoPAC[word] 
+ alpha5 * utility_semPAT[word] 
+ alpha6 * utility_phonoPAT[word] 
);
return word;
});
});
'

