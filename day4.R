#library

library(tidyverse)

# Star1

inst <- readLines("inputday4.txt")
inst_matrix <- do.call(rbind, strsplit(inst, ""))

inst_matrix[1, 1]

inst_matrix2 <- cbind(
  a = rep("a", nrow(inst_matrix) + 2),
  rbind(rep("a", ncol(inst_matrix)), inst_matrix, rep("a", ncol(inst_matrix))),
  a = rep("a", nrow(inst_matrix) + 2)
)

counter <- 0

for (rows in 2:(nrow(inst_matrix2) - 1)) {
  for (columns in 2:(ncol(inst_matrix2) - 1)) {
    print(inst_matrix2[rows, columns])
    cat(sprintf(
      "%s%s%s\n%s%s%s\n%s%s%s",
      inst_matrix2[rows - 1, columns - 1],
      inst_matrix2[rows - 1, columns],
      inst_matrix2[rows - 1, columns + 1],
      inst_matrix2[rows, columns - 1],
      "c",
      inst_matrix2[rows, columns + 1],
      inst_matrix2[rows + 1, columns - 1],
      inst_matrix2[rows + 1, columns],
      inst_matrix2[rows + 1, columns + 1]
    ))
    if (inst_matrix2[rows, columns] %in% "@") {
      vicini <- paste0(
        inst_matrix2[rows - 1, columns - 1],
        inst_matrix2[rows - 1, columns],
        inst_matrix2[rows - 1, columns + 1],
        inst_matrix2[rows, columns - 1],
        "c",
        inst_matrix2[rows, columns + 1],
        inst_matrix2[rows + 1, columns - 1],
        inst_matrix2[rows + 1, columns],
        inst_matrix2[rows + 1, columns + 1]
      )
      print(vicini)
      print(str_count(vicini, pattern = "@"))
      if (str_count(vicini, pattern = "@") < 4) {
        counter <- counter+1
      }
    }
  }
}
