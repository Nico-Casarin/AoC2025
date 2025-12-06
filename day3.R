#library

library(tidyverse)

# import

inst <- readLines("inputday3.txt")

#data manipulation
vec_max <- vector()


for (h in 1:length(inst)) {
  print(inst[h])
  row <- strsplit(inst[h], "")[[1]]
  max <- 0
  for (i in 1:99) {
    #max1 <- if (char > max1) char else max1
    for (j in (i + 1):100) {
      print(sprintf("Primo: %s, Secondo: %s", i, j))
      #print(paste0(row[i],row[j]))
      current <- paste0(row[i], row[j])
      max = if (current > max) current else max
    }
  }
  vec_max <- append(vec_max, as.numeric(max))
}

sum(vec_max)
