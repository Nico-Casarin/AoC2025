# lib


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
codes$spoiled <- purrr::map_lgl(codes$codes, ~ spoiled_fun(.x, spoiled ))
sum(codes$spoiled[codes$spoiled == TRUE])

# Star 2