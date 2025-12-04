#libs 
library(tidyverse)
library(stringr)

#import input data
# inst <- as.tibble(readLines("input_test.txt"))
inst <- (readLines("inputday2.txt"))

ids <- unlist((strsplit(inst, ",")))

numbers <- vector()

for (i in 1:length(ids)) {
  print(ids[i])
  numbers <- append(numbers, strsplit(ids[i], '-')[[1]][1]:strsplit(ids[i], '-')[[1]][2])
}


#funs

# exploder <- function(limits){
#   
#   start <- strsplit(limits, '-')[[1]][1]
#   end <- strsplit(limits, '-')[[1]][2]
#   
#   print(sprintf("Start: %s, End: %s",start,end))
#   
#   vector <- rep(1:2) 
#   
# }

splitter <- function(element){
  
  element_chars <- nchar(as.character(element))
  
  if (element_chars %% 2 == 0){
    firs_half <- str_sub(element, 1, element_chars/2)
    second_half <- str_sub(element, element_chars-(element_chars/2)+1, element_chars)
    if (firs_half == second_half) {
      return(FALSE)
    }
    return(TRUE)
  } else {
    return(TRUE)
  }
}

#star 1

invalid <- vector()

for (i in 1:length(numbers)) {
  print(numbers[i])
  
  if (splitter(numbers[i]) == FALSE) {
    invalid <- append(invalid, numbers[i])
  } else{
    next
  }
  
}
sum(invalid)

#Star 2

repeated_pattern <- function(element) {
  stringr::str_detect(element, "^(\\d+)\\1+$", negate = TRUE)
}

invalid <- vector()

for (i in 1:length(numbers)) {
  print(sprintf("Perc completed: %s", round(i/length(numbers), 2)))
  if (repeated_pattern(numbers[i]) == FALSE) {
    invalid <- append(invalid, numbers[i])
  } else{
    next
  }
  
}

sum(invalid)



#star 1 refactor // slower ... lol
# Start 2 refactor actually faster

split_pattern <- function(element) {
  element_chars <- nchar(element)
  if (element_chars %% 2 == 0) {
    stringr::str_detect(element, sprintf("^(\\d{%s})(\\1)$", element_chars/2), negate = TRUE)
  } else{
    TRUE
  }
}
  
invalid <- vector()

for (i in 1:length(numbers)) {
  print(sprintf("Perc completed: %s", round(i/length(numbers), 2)))
  if (split_pattern(numbers[i]) == FALSE) {
    invalid <- append(invalid, numbers[i])
  } else{
    next
  }
  
}

sum(invalid)

start <- vector()
end <- vector()

for (i in 1:length(ids)) {
  start <- append(start,strsplit(ids[i], '-')[[1]][1])
  end <- append(end,strsplit(ids[i], '-')[[1]][2])
}


df <- tibble(
  start= start,
  end= end
)

sum_invalid_ids <- function(ranges_df) {
  ranges_df %>%
    mutate(
      ids_chr = map2(start, end, ~ as.character(seq(.x, .y)))
    ) %>%
    transmute(
      invalid_chr = map(ids_chr, ~ .x[!(repeated_pattern(.x))])
    ) %>%
    pull(invalid_chr) %>%
    unlist() %>%
    as.double() %>%
    sum()
}

sum_invalid_ids(df)
