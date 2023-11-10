x  <- c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0,
        0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 0, 0)
len_of_x <- length(x)
in_row_0_t <- 13
in_row_1_t <- 12

count <- function(vec) {
  in_row_0 <- 0
  in_row_1 <- 0

  last_element <- "a"

  for (elem in vec){
    if (elem != last_element) {
      if (elem == 0)
        in_row_0 <- in_row_0 + 1
      else in_row_1 <- in_row_1 + 1
    }
    last_element <- elem
  }
  cat("Liczba 0: ", in_row_0, "Liczba 1: ", in_row_1, "\n")
}

for (i in 1:100) {
  cat("Iteracja: ", i, "\n")
  x <- sample(c(0, 1), i * 10, replace = TRUE)
  count(x)
}
