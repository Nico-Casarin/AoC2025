library(tidyverse)

# Input

inst <- readLines("inputday6.txt")

df <- tibble(first = t(inst[1]), second= t(inst[2]), third = t(inst[3]), fourth = t(inst[4]), ope = t(inst[5]))

a <- (df |>  select(first) |>  separate_longer_delim(first, delim=regex("[ ]+")))
b <- (df |>  select(second) |>  separate_longer_delim(second, delim=regex("[ ]+")))
c <- (df |>  select(third) |>  separate_longer_delim(third, delim=regex("[ ]+")))
d <- (df |>  select(fourth) |>  separate_longer_delim(fourth, delim=regex("[ ]+")))
e <- (df |>  select(ope) |>  separate_longer_delim(ope, delim=regex("[ ]+")))

df_unnest <- 
  tibble(
    a=a$first[1:1000],b=b$second,c=c$third,d=d$fourth,e=e$ope[1:1000]
  )

df_unnest[,1:4] <-  sapply(df_unnest[,1:4], as.numeric)

df_unnest |> 
  mutate(
    res = ifelse(e=="+", a+b+c+d, (a*b*c*d))
  ) |> 
  summarise(sum(res))
