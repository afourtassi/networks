optimize <- '
var AOA = function(){

//_.range(-0.1,0.1, 0.005)

var alpha1 = sample(Uniform({a: -0.5, b: 0.5}))
var alpha2 = sample(Uniform({a: -0.5, b: 0.5}))
var alpha3 = sample(Uniform({a: -0.5, b: 0.5}))
var alpha4 = sample(Uniform({a: -0.5, b: 0.5}))
var alpha5 = sample(Uniform({a: -0.5, b: 0.5}))
var alpha6 = sample(Uniform({a: -0.5, b: 0.5}))

var predictions = map(function(month){

//List of all words that have not been learned yet
var wordlist = _.keys(learning[month]["isLearnt"])

//posterior dsitribution Dsitribution over these wrods (choose a model)
var Posterior =  model(alpha1, alpha2, alpha3, alpha4, alpha5, alpha6,  month);

//Select the words that have been learned that month 
var isLearned = learning[month]["isLearnt"]
var learned = filter(function(d){return isLearned[d]==1}, _.keys(isLearned))

//Fit the distribution to the words that have been learned that month
map(function(w){observe(Posterior, w)}, learned)

//What are the words predicted by the model?
//We select the highest probability words in our posterior distribution
/////////////

//map words to their scores in the distribution
var scores = map(function(w){
_.object(["word", "score"],[w, Posterior.score(w)])
}, Posterior.supp)

//Sort this list of objects based on the score property
var scores_sorted = sort(scores, gt, function(x) { return x.score; })


//Now select the highest score 
var N_learned = learned.length
var predicted_objects = scores_sorted.slice(0,N_learned)
var predicted_words = map(function(obj){
obj.word
}, predicted_objects)

//Random words selected from the words not yet learned
var random_words = _.sample(wordlist, N_learned)

//Compute the overlap between predicted and words learned in this month
var intersection_model = _.intersection(predicted_words, learned)
var intersection_random = _.intersection(random_words, learned)
var accuracy_model = intersection_model.length/N_learned
var accuracy_random = intersection_random.length/N_learned

//Finally return the values of the accuracy for both the model and rando   

return _.object([month], [{model:accuracy_model, random:accuracy_random, number:N_learned}])

}, _.keys(learning))

var pred = predictions  
var pred_month = map(function(w){_.keys(w)[0]}, pred)
var pred_acc_model = map(function(w){w[_.keys(w)[0]]["model"]}, pred)
var pred_acc_random = map(function(w){w[_.keys(w)[0]]["random"]}, pred)
var number_words = map(function(w){w[_.keys(w)[0]]["number"]}, pred)
var pred_model= _.object(pred_month, pred_acc_model)
var pred_random= _.object(pred_month, pred_acc_random)
var pred_number = _.object(pred_month, number_words)


return {
parameters : {alpha1:alpha1, alpha2:alpha2, alpha3:alpha3, alpha4:alpha4, alpha5:alpha5, alpha6:alpha6},
predictives_model: pred_model,
predictives_random: pred_random,
number: pred_number
}
}

'