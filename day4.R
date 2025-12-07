#library

library(tidyverse)
library(magick)

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
nst_history <- list() 
nst_history[[1]] <- inst_matrix2
giro <- 0

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
      print(str_count(vicini, pattern = "@|x"))
      if (str_count(vicini, pattern = "@|x") < 4) {
        counter <- counter+1
        inst_matrix2[rows, columns] <- "x"
        giro <- giro+1
        nst_history[[giro]] <- inst_matrix2
      }
      #giro <- giro+1
      #nst_history[[giro]] <- inst_matrix2

    }
  }
}

## Plot

library(ggplot2)

plot_nst(inst_matrix2)

plot_nst <- function(inst_matrix) {
  nr <- nrow(inst_matrix)
  nc <- ncol(inst_matrix)

  df <- expand.grid(
    row = seq_len(nr),
    col = seq_len(nc)
  )
  df$val <- as.vector(inst_matrix)

  df$val <- factor(df$val, levels = c(".", "@", "x", "a" ))

  ggplot(df, aes(x = col, y = row, fill = val)) +
    geom_tile(
      aes(color = val == "c"),
      linewidth = 0.4
    ) +
    scale_fill_manual(values = c(
      "." = "white",   
      "@" = "black",   
      "x" = "red",   
      "a" = "white"
    )) +
    scale_color_manual(values = c(
      `TRUE`  = "yellow", # bordo celle "c"
      `FALSE` = "grey70"
    )) +
    scale_y_reverse(breaks = 1:nr) +   
    coord_fixed() +
    theme_void() +
    theme(legend.position = "none")
}

frames <- image_graph(width = 500, height = 500, res = 96)

indices <- seq(1,length(nst_history), by=50)

for (s in indices) {
  p <- plot_nst(nst_history[[s]])
  print(p)   
  print(s)
}
-
dev.off()   

gif <- image_animate(frames, fps = 5)  

image_write(gif, "nst_evolution.gif") 
