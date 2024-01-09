# use the wordle dictionary
D = read.table("wordle_words.txt")
D = unlist(D)

# this results in a dictionary of 2315 words
length(D)

# split into matrix of characters
D_ = strsplit(D, "")
D_ = t(sapply(D_, function(x) {x}))

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

# this is an easier version of the function above
# this is an easier version of the function above
reduce_dictionary = function(guess_, this.feedback, D_) {
  this.D_ = D_
  
  # # # narrow down using green feedback
  # green = which(this.feedback == "green")
  # # green has to be in the exact same location
  # if (length(green) > 0) {
  #   keep_green = rep(TRUE, nrow(this.D_))
  #   for (i in green) {
  #     keep_green =  keep_green & (this.D_[,i] == guess_[i])
  #   }
  #   this.D_ = this.D_[keep_green,]
  # }
  # # # narrow down using yellow feedback
  # yellow = which(this.feedback == "yellow")
  # if (length(yellow) > 0) {
  #   keep_yellow = rep(TRUE, nrow(this.D_))
  #   for (i in yellow) {
  #     keep_yellow =  keep_yellow & (rowSums(this.D_ == guess_[i]) >= 1)
  #   }
  #   this.D_ = this.D_[keep_yellow,]
  # }
  
  # # # narrow down using green feedback
  # green = which(this.feedback == "green")
  # # green has to be in the exact same location
  # if (length(green) > 0) {
  #   keep_green =  rowMeans(matrix(this.D_[,green] == guess_[green], ncol = length(green))) == 1
  #   this.D_ = this.D_[keep_green,]
  # }
  # # # narrow down using yellow feedback
  # yellow = which(this.feedback == "yellow")
  # # yellow just has to be in there
  # if (length(yellow) > 0 & length(this.D_) > 5) {
  #   keep_yellow =  rowSums(matrix(this.D_ %in% guess_[yellow], ncol = 5)) >= length(yellow)
  #   this.D_ = this.D_[keep_yellow,]
  # }
  
  # # # narrow down using grey feedback
  # # # this breaks in the presence of multiple letters in the guess where one got a grey and the other did not
  # grey = which(this.feedback == "grey")
  # # grey can't be anywhere
  # if (length(grey) > 0) {
  #   keep_grey =  rowMeans(matrix(this.D_ %in% guess_[grey], ncol = 5)) == 0
  #   this.D_ = this.D_[keep_grey,]
  # }
  
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
# function to get estimated reduction and variability of
# the estimated reduction in a (potentially) reduced dictionary
# not currently used
est_reduction_sd = function(guess_, Dprime_) {
  reduction = rep(0, nrow(Dprime_))
  for (i in 1:nrow(Dprime_)) {
    # compute feedback this were the true target
    this.feedback = feedback(guess_, Dprime_[i,])
    # compute reduction if it were the true target
    reduction[i] = nrow(Dprime_) - nrow(reduce_dictionary(guess_, this.feedback, Dprime_))
  }
  return(c(mean(reduction), sd(reduction)))
}
# est_red_sd = est_reduction_sd(this.guess, D_[sample(1:nrow(D_), 200),])
# est_red = est_red_sd[1] / 2
# est_CI = est_red + 
#   qnorm(p = c(0.025, 0.975)) * est_red_sd[2] / sqrt(100) *
#   sqrt((nrow(D_) - 100) / (nrow(D_) - 1))

# # get estimated reduction for all permissible words
# # note that this is not optimal as some words that are not possible target words
# # could lead to a greater expected reduction in possible target words
# res_data = data.frame(guess = apply(Dprime_, 1, paste0, collapse = ""),
#                       est_reduction = NA)
# for (i in 1:nrow(Dprime_)) {
#   res_data$est_reduction[i] = est_reduction(guess_ = Dprime_[i,], Dprime_ = Dprime_)
# }
# 
# max(res_data$est_reduction, na.rm = TRUE)
# res_data[which.max(res_data$est_reduction),]
# # there are many good options
# res_data[order(res_data$est_reduction, decreasing = TRUE),]
# hist(res_data$est_reduction)