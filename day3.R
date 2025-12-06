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

# star 2

max_battery <- function (row, d) {
  digits <- strsplit(row, "")[[1]]
  n <- length(digits)

  remove <- n - d
  pila <- character(0)

  for (i in digits){
    while(length(pila)> 0 && remove > 0 && tail(pila ,1) < i ){
      pila <- head(pila, -1)
      remove <- remove - 1
    }
    pila <- c(pila, i)
  }

  if (length(pila) > d) {
    pila <- pila[1:d]
  }
  paste0(pila, collapse = "")
}


vec_max <- vector()
for (rows in 1:length(inst)) {
  res <- max_battery(inst[rows], 12)
  vec_max <- append(vec_max, as.numeric(res))
}
format(sum(vec_max), scientific = FALSE)