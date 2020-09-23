#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(shiny)
library(stringr)
library(dplyr)
library(conflicted)
library(quanteda)
library(dplyr)
library(tidyr)
conflict_prefer("filter", "dplyr")
#setwd("~/")
one_gram <- readRDS("one_gram.rds")
two_gram <- readRDS("two_gram.rds")
three_gram <- readRDS("three_gram.rds")
four_gram <- readRDS("four_gram.rds")

input_split<-function(input){
  #clean input
  input<-clean(input)
  l<-length(input[[1]])
  
  #unwrap input from app user
  test_inputs<-c()
  for(i in 1:3){
    if (i==1){
      test_inputs<-c(input[[1]][l])
    }
    if (i==2 & i<=l){
      test_inputs[2]<-c(paste(input[[1]][l-1],input[[1]][l],sep=" "))
    }
    if (i==3 & i<=l){
      test_inputs[3]<-c(paste(input[[1]][l-2],input[[1]][l-1],input[[1]][l],sep=" "))
    }
  }
  test_inputs<-rev(test_inputs)
  test_inputs
}

clean<-function(data){
  data<-tokens(data, 
               remove_punct = TRUE, 
               remove_numbers = TRUE,
               remove_symbols = TRUE,
               remove_url = TRUE,
               remove_separators = TRUE,
               split_hyphens = FALSE
  )
  #data<-tokens_wordstem(data)
  data <- tokens_tolower(data)
  data
}




next_word<-function(input) {
  test_split<-input_split(clean(input))
  lng<-length(test_split)
  if(lng >= 3){
    candidate<-four_gram[four_gram$input ==test_split[lng-2],]
    
    if(dim(candidate)[1] < 5) {
      candidate<-rbind(candidate,three_gram[three_gram$input ==test_split[lng-1],])
    }
    if(dim(candidate)[1] < 5) {
      candidate<-rbind(candidate,two_gram[two_gram$input ==test_split[lng],])
    }
    candidate <- candidate[order(-candidate$prob), ]
    head(candidate,10)[,c(2,3,5)]
  }
  
  else if(lng >= 2){
    candidate<-three_gram[three_gram$input ==test_split[lng-1],]
    
    if(dim(candidate)[1] < 5) {
      candidate<-rbind(candidate,two_gram[two_gram$input ==test_split[lng],])
    }
    candidate <- candidate[order(-candidate$prob), ]
    head(candidate,10)[,c(2,3,5)]
  }
  else if(lng >= 1) {
    candidate<-two_gram[two_gram$input ==test_split[lng],]
    candidate <- candidate[order(-candidate$prob), ]
    head(candidate,10)[,c(2,3,5)]
  }
  candidate <- candidate[order(-candidate$prob), ]
  head(candidate,10)[,c(2,3,5)]
}


next_word2<-function(input, one_gram.=one_gram,two_gram.=two_gram,three_gram.=three_gram){
  
  two_gram_discount <- 0.5  # bigram discount
  three_gram_discount <- 0.5  # trigram discount
  one_gram<-one_gram[one_gram$freq>40,]
  two_gram<-two_gram[two_gram$freq>40,]
  three_gram<-three_gram[three_gram$freq>40,]
  
  #Observed Trigrams
  
  
  f_seen_three_gram<-function(input,three_gram){
    test_split<-input_split(clean(input))
    lng<-length(test_split)
    seen_three_gram<-three_gram[three_gram$input ==test_split[lng-1],]
    seen_three_gram
  }
  
  f_seen_three_gram_p <- function(seen_three_gram, two_gram, input, three_gram_discount=0.5){
    if(nrow(seen_three_gram) < 1) return(NULL)
    test_split<-input_split(clean(input))
    lng<-length(test_split)
    count_two_grams_with_three_gram_12 <- filter(two_gram, original==test_split[lng-1])$freq
    seen_three_gram_p<-seen_three_gram
    seen_three_gram_p$p<-seen_three_gram$freq
    seen_three_gram_p$p<-(seen_three_gram_p$p-three_gram_discount)/count_two_grams_with_three_gram_12
    seen_three_gram_p
  }
  
  
  #Unobserved trigrams
  
  f_not_seen_three_gram_ends <- function(seen_three_gram, one_gram) {
    seen_trigram_ends <- seen_three_gram$output
    not_seen_three_gram_ends <- one_gram[!(one_gram$input %in% seen_trigram_ends), ]$input
    not_seen_three_gram_ends
  }
  
  #Discounted probability mass for the two gram
  alfa_two_gram <- function(one_gram, two_gram, input, two_gram_discount=0.5){
    test_split<-input_split(clean(input))
    lng<-length(test_split)
    one_gram_for_bigram_start<- one_gram[one_gram$input == test_split[lng],]
    two_gram_with_one_gram <- two_gram[two_gram$input==one_gram_for_bigram_start$input,]
    if(nrow(two_gram_with_one_gram) < 1) return(0)
    AlfaTwogram  <- 1 - (sum(two_gram_with_one_gram$freq - two_gram_discount) / one_gram_for_bigram_start$freq)
    AlfaTwogram
  }
  
  
  f_back_off_two_gram <- function(input, not_seen_three_gram_ends) {
    test_split<-input_split(clean(input))
    lng<-length(test_split)
    back_off_two_gram <- paste(test_split[lng], not_seen_three_gram_ends, sep = " ")
    back_off_two_gram
  }
  
  f_seen_back_off_two_gram <- function(input, back_off_two_gram, two_gram) {
    seen_back_off_two_gram <- two_gram[two_gram$original %in% back_off_two_gram, ]
    seen_back_off_two_gram
  }
  
  f_seen_back_off_two_gram_p <- function(seen_back_off_two_gram, one_gram, two_gram_discountt=0.5) {
    seen_back_off_two_gram_start <- seen_back_off_two_gram$input
    count_one_gram_with_two_gram_1 <- one_gram[one_gram$input %in% seen_back_off_two_gram_start, ]
    seen_back_off_two_gram_p<-seen_back_off_two_gram
    seen_back_off_two_gram_p$p<-seen_back_off_two_gram_p$freq
    seen_back_off_two_gram_p$p <- (seen_back_off_two_gram_p$freq - two_gram_discountt) / count_one_gram_with_two_gram_1$freq
    seen_back_off_two_gram_p 
  }
  
  f_not_seen_back_off_two_gram <- function(input, back_off_two_gram, two_gram) {
    not_seen_back_off_two_gram <- two_gram[!(two_gram$original %in% back_off_two_gram), ]
    not_seen_back_off_two_gram
  }
  
  f_not_seen_back_off_two_gram_p  <- function(not_seen_back_off_two_gram, one_gram, alfa_two) {
    # get the unobserved bigram tails
    not_seen_back_off_two_gram_start <- not_seen_back_off_two_gram$output
    #w_in_Aw_iminus1 <- onegrams[!(onegrams$original %in% unseen_bigram_outputs), ]
    # convert to data.frame with counts
    count_one_gram_with_two_gram_1 <- one_gram[one_gram$input %in% not_seen_back_off_two_gram_start, ]
    not_seen_back_off_two_gram_p<-not_seen_back_off_two_gram
    not_seen_back_off_two_gram_p$p<-not_seen_back_off_two_gram$freq
    not_seen_back_off_two_gram_p$p <- (not_seen_back_off_two_gram$freq * alfa_two) / sum(not_seen_back_off_two_gram$freq)
    not_seen_back_off_two_gram_p
  }
  
  
  
  
  #w_in_Aw_iminus1 <- onegrams[!(onegrams$original %in% unseen_bigram_outputs), ]
  # convert to data.frame with counts
  
  
  
  
  alfa_three_gram <- function(seen_three_gram_p, two_gram, input, three_gram_discount=0.5) {
    if(nrow(seen_three_gram_p) < 1) return(1)
    test_split<-input_split(clean(input))
    lng<-length(test_split)
    two_gram_in_three<-two_gram[two_gram$original %in% test_split[lng-1], ]
    AlphaTrigram <- 1 - sum((seen_three_gram_p$freq - three_gram_discount) / two_gram_in_three$freq[1])
    return(AlphaTrigram)
  }
  
  f_not_seen_three_gram <- function(input, all_back_off_two_gram, alfa_three) {
    all_back_off_two_gram <- all_back_off_two_gram[order(-all_back_off_two_gram$p), ]
    sum_p <- sum(all_back_off_two_gram$p)
    first_input_word <- clean(input)
    lng<-length(first_input_word[[1]])
    first_input_word<-first_input_word[[1]][lng-1]
    not_seen_three_gram<-all_back_off_two_gram
    not_seen_three_gram$original <- paste(first_input_word, all_back_off_two_gram$original, sep=" ")
    not_seen_three_gram$p <- alfa_three * all_back_off_two_gram$p / sum_p
    not_seen_three_gram_temp<-separate(not_seen_three_gram,original,c("input", "output")," (?!.* )") 
    not_seen_three_gram$input<-not_seen_three_gram_temp$input
    not_seen_three_gram$output<-not_seen_three_gram_temp$output
    not_seen_three_gram
  }
  
  f_prediction <- function(seen_three_gram_p,not_seen_three_gram_p) {
    all_trigrams <- rbind(seen_three_gram_p[,c(2,3,6)], not_seen_three_gram[,c(2,3,6)])
    all_trigrams <- all_trigrams[order(-all_trigrams$p), ]
    # pull last word of highest prob three gram
    prediction <- all_trigrams[1:10,]
    prediction
  }
  
  
  seen_three_gram<-f_seen_three_gram(input,three_gram)
  seen_three_gram_p<-f_seen_three_gram_p(seen_three_gram, two_gram, input)
  not_seen_three_gram_ends<-f_not_seen_three_gram_ends(seen_three_gram, one_gram)
  alfa_two<-alfa_two_gram(one_gram, two_gram, input, two_gram_discount)
  back_off_two_gram<-f_back_off_two_gram(input, not_seen_three_gram_ends)
  seen_back_off_two_gram<-f_seen_back_off_two_gram(input, back_off_two_gram, two_gram)
  not_seen_back_off_two_gram<-f_not_seen_back_off_two_gram(input, back_off_two_gram, two_gram)
  seen_back_off_two_gram_p<-f_seen_back_off_two_gram_p(seen_back_off_two_gram, one_gram, two_gram_discountt=0.5)
  not_seen_back_off_two_gram_p<-f_not_seen_back_off_two_gram_p(not_seen_back_off_two_gram, one_gram, alfa_two)
  all_back_off_two_gram<-rbind(seen_back_off_two_gram_p, not_seen_back_off_two_gram_p)
  alfa_three<-alfa_three_gram(seen_three_gram_p, two_gram, input, three_gram_discount)
  not_seen_three_gram<-f_not_seen_three_gram(input,all_back_off_two_gram,alfa_three)
  t<-f_prediction(seen_three_gram_p, not_seen_three_gram_p)
  t
}




shinyServer(function(input, output) {
    
    
    output$predicted1 <- renderPrint(cat(paste(next_word(input$input)[1,2], collapse='\n' ) ))
    output$head1 <- renderPrint(cat(paste(next_word(input$input)[2:10,2], collapse='\n' ) ))
    output$predicted2 <- renderPrint(cat(paste(next_word2(input$input,one_gram,two_gram,three_gram)[1,2], collapse='\n' ) ))
    output$head2 <- renderPrint(cat(paste(next_word2(input$input,one_gram,two_gram,three_gram)[2:10,2], collapse='\n' ) ))

})


