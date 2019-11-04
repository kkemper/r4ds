library(tidyverse)
library(stringr)
library(microbenchmark)

# Chapter 21 - Iteration

## 21.2 For Loops

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

median(df$a)
median(df$b)
median(df$c)
median(df$d)

output <- vector("double", ncol(df))
for (i in seq_along(df)) {
  output[[1]] <- median(df[[i]])
}
output

y <- vector("double", 0)
seq_along(y)
1:length(y)

### 21.2.1

#### 1.1

mtmean <- vector("double", ncol(mtcars))
names(mtmean) <- names(mtcars)
for (i in names(mtcars)) {
  mtmean[i] <- mean(mtcars[[i]])
}
mtmean

#### 1.2

column_type <- vector("list", ncol(nycflights13::flights))
names(column_type) <- names(nycflights13::flights)
for (i in names(nycflights13::flights)) {
  column_type[[i]] <- class(nycflights13::flights[[i]])
}
column_type

#### 1.3

data("iris")
iris_unique <- vector("double", ncol(iris))  
names(iris_unique) <- names(iris)
for (i in names(iris)) {
  iris_unique[i] <- length(unique(iris[[i]]))
}
iris_unique


#### 1.4

n <- 10
mu <- c(-10, 0, 10, 100)
normals <- vector("list", length(mu))
for (i in seq_along(normals)) {
  normals[[i]] <- rnorm(n, mean = mu[i])
}
normals

#### 2.1

str_c(letters, collapse = "")

#### 2.2

sd(x)

#### 2.3

all.equal(cumsum(x), out)

#### 3.1

humps <- c("five", "four", "three", "two", "one", "no")
for (i in humps) {
  cat(str_c("Alice the Camel has ", rep(i, 3), " humps.", collapse = "\n"), "\n")
  if (i == "no") {
    cat("Now Alice is a horse.\n")
  } else {
    cat("So go, Alice, go.\n")
  }
  cat("\n")
  }

#### 3.2

numbers <- c("ten", "nine", "eight", "seven", "six", "fice", "four", "three", "two", "one")
for (i in numbers) {
  cat(str_c("There were ", i, " in the bed\n"))
  cat("and the little one said\n")
  if (i== "one") {
    cat("I'm lonely...")
  } else {
    cat("Roll over, roll over\n")
    cat("So they all rolled over and one fell out.\n")
  }
  cat("\n")
}

#### 3.3

# bottles <- function(n) {
#   if(n > 1) {
#     str_c(n, " bottles")
#   } else if (n==1) {
#     "1 bottle"
#   } else {
#     "no more bottles"
#   }
# }
# 
# beer_bottles <- function(total_bottles) {
#   for(current_bottles in seq(total_bottles, 0)) {
#     cat(str_to_sentence(str_c(bottles(current_bottles), " of beer on the wall, ", bottles(current_bottles), " of beer.\n")))
#     if (current_bottles > 0) {
#       cat(str_c(
#         "Take one down, pass it around, ", bottles(current_bottles - 1), " of beer on the wall.\n"
#         ))
#     } else {
#       cat(str_c("Go to the store and buy some more, ", bottles(total_bottles), " of beer on the wall.\n"))
#     }
# cat("\n")
#     }
# }
# 
# beer_bottles(3)

### 21.3.1 - Modifying an Existing Object

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

df$a <- rescale01(df$a)
df$b <- rescale01(df$b)
df$c <- rescale01(df$c)
df$d <- rescale01(df$d)

df

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

for (i in seq_along(df)) {
  df[[i]] <- rescale01([[i]])
}
df

