  # # use the wordle dictionary
D = read.table("wordle_words.txt")
D = unlist(D)
# this results in a dictionary of 9330 words
length(D)

# split into matrix of characters
D_ = strsplit(D, "")
D_ = t(sapply(D_, function(x) {x}))

# for now a fixed target word
true_target = "prick"
true_target_ = strsplit(true_target, "")[[1]]
true_target %in% D

guess = "sandy"
guess_ = strsplit(guess, "")[[1]]
guess %in% D

# helper function to rate a guess
feedback = function(guess_, true_target_) {
  # initialize as all false
  feedback = rep("grey", 5)
  # # in word, but incorrect spot
  # feedback[guess_ %in% true_target_] = "yellow"
  # # overwrite when in correct spot
  # feedback[guess_ == true_target_] = "green"
  
  # this should give correct feedback in presence of double and triple letters
  true_target_c = true_target_
  guess_c = guess_
  for (i in 1:5) {
    if (guess_[i] == true_target_c[i]) {
      feedback[i] = "green"
      true_target_c[i] = "-1"
      guess_c[i] = "-2"
    }
  }
  for (i in 1:5) {
    if (guess_c[i] %in% true_target_c) {
      feedback[i] = "yellow"
      true_target_c[which(true_target_c %in% guess_c[i])] = "-1"
    }
  }
  return(feedback)
}
# tests
feedback(guess_ =       c("t", "a", "r", "e", "s"), 
         true_target_ = c("p", "r", "i", "c", "k"))

feedback(guess_ =       c("t", "a", "r", "e", "s"), 
         true_target_ = c("f", "u", "r", "r", "y"))

feedback(guess_ =       c("f", "u", "r", "o", "r"), 
         true_target_ = c("p", "r", "i", "c", "k"))

feedback(guess_ =       c("m", "o", "u", "l", "d"), 
         true_target_ = c("i", "n", "c", "u", "r"))

feedback(guess_ =       c("p", "i", "n", "k", "y"), 
         true_target_ = c("p", "r", "i", "c", "k"))

# helper function to reduce dictionary given a guess and its feedback
reduce_dictionary = function(guess_, this.feedback, D_) {
  this.D_ = D_

  # core function
  if (length(this.D_) == 5) this.D_ = matrix(this.D_, nrow = 1)
  if (nrow(this.D_) > 0) {
    possible = rep(F, nrow(this.D_))
    for (j in 1:nrow(this.D_)) {
      possible[j] = all(feedback(guess_, true_target_ = this.D_[j,]) == this.feedback)
    }
    this.D_ = this.D_[possible,]
    # if only one entry is left force D_ to stay in matrix form
    if (sum(possible) == 1) this.D_ = matrix(this.D_, nrow = 1)
  }
  return(this.D_)
}
# Dprime_ = reduce_dictionary(guess_, this.feedback, D_)
Dprime_ = reduce_dictionary(guess_ = c("t", "a", "r", "e", "s"), 
                            this.feedback = c("grey", "grey", "yellow", "grey","grey"),
                            D_)
nrow(Dprime_)
Dprime_ = reduce_dictionary(guess_ = c("p", "a", "p", "e", "r"), 
                            this.feedback = c("green", "grey", "grey", "grey","grey"),
                            D_)
nrow(Dprime_)
Dprime_ = reduce_dictionary(guess_ = c("a", "u", "d", "i", "o"), 
                            this.feedback = c("grey", "grey", "grey", "yellow","grey"),
                            D_)
nrow(Dprime_)

# function to get estimated reduction in a (potentially) reduced dictionary
est_reduction = function(guess_, Dprime_) {
  reduction = rep(0, nrow(Dprime_))
  for (i in 1:nrow(Dprime_)) {
    # compute feedback this were the true target
    this.feedback = feedback(guess_, Dprime_[i,])
    # compute reduction if it were the true target
    reduction[i] = nrow(Dprime_) - nrow(reduce_dictionary(guess_, this.feedback, Dprime_))
  }
  return(mean(reduction))
}
est_reduction(guess_ = c("j", "u", "r", "o", "r"), Dprime_)

# get estimated reduction for all permissible words
# note that this is not optimal as some words that are not possible target words
# could lead to a greater expected reduction in possible target words
res_data = data.frame(guess = apply(Dprime_, 1, paste0, collapse = ""),
                      est_reduction = NA)
for (i in 1:nrow(Dprime_)) {
  res_data$est_reduction[i] = est_reduction(guess_ = Dprime_[i,], Dprime_ = Dprime_)
}
# there are many good options
res_data[order(res_data$est_reduction, decreasing = TRUE),]
hist(res_data$est_reduction)

# # second guess
D2prime_ = reduce_dictionary(guess_ = c("m", "o", "u", "l", "d"), 
                            this.feedback = c("grey", "grey", "grey", "grey","grey"),
                            Dprime_)
nrow(D2prime_)
# get estimated reduction for all permissible words
# note that this is not optimal as some words that are not possible target words
# could lead to a greater expected reduction in possible target words
res_data = data.frame(guess = apply(D2prime_, 1, paste0, collapse = ""),
                      est_reduction = NA)
for (i in 1:nrow(D2prime_)) {
  res_data$est_reduction[i] = est_reduction(guess_ = D2prime_[i,], Dprime_ = D2prime_)
}
# there are many good options
res_data[order(res_data$est_reduction, decreasing = TRUE),]
hist(res_data$est_reduction)

# # third guess
D3prime_ = reduce_dictionary(guess_ = c("p", "i", "n", "k", "y"), 
                             this.feedback = c("green", "yellow", "grey", "yellow","grey"),
                             D2prime_)
nrow(D3prime_)


# # a better option would be to query all words in the dictionary
# res_data = data.frame(guess = apply(D_, 1, paste0, collapse = ""),
#                       est_reduction = NA)
# # draw a random sample from thes words for computational feasability
# samp = sample(1:nrow(res_data), min(nrow(res_data), 100))
# samp = sort(samp)
# # samp = 1:nrow(res_data)
# for (i in samp) {
#   print(i)
#   res_data$est_reduction[i] = est_reduction(guess_ = D_[i,], Dprime_ = Dprime_)
# }
# # there are many good options
# head(res_data[order(res_data$est_reduction, decreasing = TRUE),])
# hist(res_data$est_reduction)


# ##############################################################################
# this code can be used to identify good start words
res_data = data.frame(guess = apply(D_, 1, paste0, collapse = ""),
                      est_reduction = NA)
# draw a random sample from these words for computational feasibility
samp = sample(1:nrow(res_data), 500)
samp = sort(samp)
for (i in samp) {
  print(i)
  # also draw a random sample of target words to speed up computation
  res_data$est_reduction[i] = est_reduction(guess_ = D_[i,], 
                                            Dprime_ = D_[sample(1:nrow(D_), 100),])
}
# there are multiple good options
head(res_data[order(res_data$est_reduction, decreasing = TRUE),], 15)
hist(res_data$est_reduction)

saveRDS(res_data, "first_guess.RDS")

# rerun with larger sample for best guesses
best_guesses = res_data$est_reduction > 95
sum(best_guesses, na.rm = TRUE)
res_data2 = data.frame(guess = apply(D_[which(best_guesses),], 1, paste0, collapse = ""),
                      est_reduction = NA)
for (i in 1:nrow(res_data2)) {
  print(i)
  # also draw a random sample of target words to speed up computation
  res_data2$est_reduction[i] = est_reduction(guess_ = D_[i,], 
                                            Dprime_ = D_[sample(1:nrow(D_), 1000),]) / 10
}
res_data2 = res_data2[order(res_data2$est_reduction, decreasing = TRUE),]
saveRDS(res_data2, "second_guess.RDS")


