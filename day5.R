# lib

library(purrr)

# import

inst <- readLines("inputday5.txt")

ranges <- inst[1:(which(inst %in% "")-1)]
codes <- data.frame(codes = as.numeric(inst[(which(inst %in% "")+1):length(inst)]))
spoiled <- vector()
#star 1 

### Fails for real use case. Vector too big
for (range in ranges){
  print(range)
  spoiled <- append(spoiled,(strsplit(range, "-")[[1]][1]:strsplit(range, "-")[[1]][2]))
}
codes$spoiled <- ifelse(codes$codes %in% spoiled, TRUE, FALSE)


### <> approach
spoiled <- list()
for (i in seq_along(ranges)){
  print(i)
  spoiled[[i]] <- as.numeric(strsplit(ranges[i], "-")[[1]])
  
}
spoiled_fun <- function (value, ranges){
  any(sapply(ranges, function(r) value >= r[1] && value <= r[2]))
}
codes$spoiled <- map_lgl(codes$codes, ~ spoiled_fun(.x, spoiled ))
sum(codes$spoiled[codes$spoiled == TRUE])

# Star 2

spoiled <- spoiled[order(sapply(spoiled,head,1))]

overlapper <- function (lst){

  #lst <- lst[order(sapply(lst, `[[`, 1)), ]
  lst <- lst[order(sapply(spoiled,head,1))]

  res <- list()
    res <- list(lst[[1]])
  
  for (i in 2:length(lst)) {
    last <- res[[length(res)]]
    curr <- lst[[i]]
    
    if (curr[[1]] <= last[[2]]) {
      last[[2]] <- max(last[[2]], curr[[2]])
      res[[length(res)]] <- last
    } else {
      res[[length(res) + 1]] <- curr
    }
  }
  res
}

ranges <- overlapper(spoiled)
totale <- 0
for (range in ranges){
  print(length(range[1]:range[2]))
  totale <- length(range[1]:range[2])+totale
}
