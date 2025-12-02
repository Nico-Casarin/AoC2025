#libs 
library(tidyverse)

#import imput data
inst <- as.tibble(readLines(path))

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

zero_passed <- function(first, second) {
  (first+ abs(second)) %/% 100
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

