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

# star 1

df_unnest |> 
  mutate(
    res = ifelse(e=="+", a+b+c+d, (a*b*c*d))
  ) |> 
  summarise(sum(res))

# star 2


rows_num <- inst[1:4]
row_op   <- inst[5]

W <- max(nchar(c(rows_num, row_op)))
rows_num <- str_pad(rows_num, W, side = "right", pad = " ")
row_op   <- str_pad(row_op,   W, side = "right", pad = " ")

grid <- rbind(
  str_split(rows_num[1], "", simplify = TRUE),
  str_split(rows_num[2], "", simplify = TRUE),
  str_split(rows_num[3], "", simplify = TRUE),
  str_split(rows_num[4], "", simplify = TRUE),
  str_split(row_op,      "", simplify = TRUE)
)
is_sep <- apply(grid, 2, function(col) all(col == " "))

cols <- seq_len(W)
blocks <- split(cols[!is_sep], cumsum(is_sep)[!is_sep])

block_value <- function(block_cols) {
  op <- grid[5, block_cols[1]] 

  nums <- c()
  for (j in rev(block_cols)) {
    s <- paste0(grid[1:4, j], collapse = "")
    s <- gsub(" ", "", s)
    if (nchar(s) == 0) next
    nums <- c(nums, as.integer(s))
  }

  if (op == "+") sum(nums) else prod(nums)
}

res_star2 <- sum(vapply(blocks, block_value, numeric(1)))
res_star2