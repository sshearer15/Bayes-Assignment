# Bayes-Assignment
#Question 1
##cum prob of first is the probability of first bad review with manager 0; result was just a bad choice of name for the storage vector
result <- vector("numeric", 100) 
for (j in 1:100){
  result[j] <- 0.25*0.1875^j
  cum_prob_of_first<- sum(result) + 0.25
}
cum_prob_of_first


#Question 2
# "1" is defined as the event "Y/N"; "0" is defined as all other events 
##probabilities in signal vector are of the true prob. event "1", or Yes/N, (0.9*0.1 = .09) and events that make up "0"(1-0.09)
###probabilities in the accuracy vector are the prob. of accurate recipet of event "1" (0.75*0.75) and prob. of all other events (i.e accurate
recipet of one signal, accurate recipet of neither signal)
signal <- sample(c(1, 0), 100000, prob = c(0.09, 0.91), replace = TRUE)
accuracy <- sample(c(1,0), 100000, prob = c(0.5625, 0.4375), replace = TRUE)
retrieval <- rep(NA, 100000)
retrieval[accuracy == 1] <- signal[accuracy == 1]
retrieval[accuracy == 0 & signal == 0] <- 1
retrieval[accuracy == 0 & signal == 1] <- 0
#This is the final step in which we calculate the inverse probability. Mean is taken to determine the probability over 100000, which should
we know, given the size of the trials, should be infinately close the to the true probability.
mean(signal[retrieval == 1])

#Question3
##set up two vectors to be able to compare the switch/stay probabilities of success for winding up with one of the two good managers
###good manager  establishes the prior probability
###choose manager is selecting a random manager
###open simulates the finding out one of the managers; the info gained cannot be the same as the random selection in choose manager and can't be one of the two good managers
managers <- c("A", "B", "C", "D", "E")
stay_outcome <- c(0,10000)
switch_outcome <- c(0,10000)
for(i in 1:10000) {
  good_manager <- sample(managers)[1:2]
  choose_manager <- sample(managers)[1]
  open <- sample(managers[managers!= choose_manager & !(managers %in% good_manager)])[1]
  stay_outcome[i] <- choose_manager %in% good_manager
  myswitch <- sample(managers[managers!= choose_manager & managers!= open])[1]
  switch_outcome[i] <- myswitch %in% good_manager
}
cat('Probability of selecting a good manager if you stay: ', mean(stay_outcome), '\n')
cat('Probability of selecting a good manager if you switch: ', mean(switch_outcome), '\n')
