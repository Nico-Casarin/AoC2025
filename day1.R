#libs 
library(tidyverse)

#import imput data
inst <- as.tibble(readLines("input_test.txt"))
inst <- as.tibble(readLines("inputday1.txt"))


#data prep

inst_full <- inst %>% 
  mutate(
    steps = as.numeric(str_sub(value, 2)),
    direction = str_sub(value,,1),
  ) %>% 
  mutate(full = if_else(direction=="L",steps*-1,steps))

index_shift <- function(start, shift, n = 100) {
  (start + shift) %% n
}  

#combination

zeroes <- 0
index <- 50

for (i in 1:nrow(inst_full[4])) {
  res <- index_shift(index, inst_full[i,4])
  if (res == 0) {
    zeroes <- zeroes + 1
  }
  print(sprintf("%s >> Indice: %s, step: %s, resto: %s, zeroes: %s", i, index, inst_full[i,4], res, zeroes))
  index <- res
}

# second star

zero_passed <- function(first, second, direction) {
  if (direction=='L' & first == 0) {
    (((100-first)+abs(second)) %/% 100)-1
  } else if (direction=='L' & first != 0) {
    ((100-first)+abs(second)) %/% 100
  } else {
    (first+second) %/% 100
  }
}

zeroes <- 0
index <- 50
zero_click <- 0

for (i in 1:nrow(inst_full[4])) {
  
  res <- index_shift(index, inst_full[i,4])
  res_zero <- zero_passed(index, inst_full[i,4], inst_full[i,3])
  
  if (res == 0) {
    zeroes <- zeroes + 1
    if (index == 0){
      zero_click <- (res_zero - 2) + ( zero_click)
    } else {
    zero_click <- (res_zero -1 ) +  (zero_click )}
  } else {
    zero_click <- (res_zero + zero_click)
  }

  print(sprintf("%s >> Indice: %s, step: %s, resto: %s, zeroes: %s, res_zero: %s, zero_click: %s", i, index, inst_full[i,4], res, zeroes,res_zero, zero_click))
  index <- res
}
