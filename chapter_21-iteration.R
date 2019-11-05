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

### 21.3.3 Unknown Output Length

means <- c(0, 1, 2)

output <- double()
for (i in seq_along(means)) {
  n <- sample(100, 1)
  output <- c(output, rnorm(n, means[[i]]))
}
              
str(output)

#=========================

out <- vector("list", length(means))
for(i in  seq_along(means)) {
  n <- sample(100, 1)
  out[[i]] <- rnorm(n, means[[i]])
}
str(out)
str(unlist(out))

### 21.3.5 - Exercises

#### 1.
# files <- dir("data/", patter = "\\.csv$", full.names = TRUE)
# files
df_list <- vector("list", length(files))
for(i in seq_along(files)) {
  df_list[[i]] <- read_csv(files[[i]])
}

#### 2.1

x <- c(11, 12, 13)
print(names(x))
for (nm in names(x)) {
  print(nm)
  print(x[[nm]])
}
length(NULL)



#### 2.2

x <- c(a = 11, 12, c = 13)
names(x)
for (nm in names(x)) {
  print(nm)
  print(x[[nm]])
}

x <- c(a = 11, a = 12, c = 13)
names(x)
for (nm in names(x)) {
  print(nm)
  print(x[[nm]])
}

## 21.4 - For Loops vs. Functionals

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

output <- vector("double", length(df))
for (i in seq_along(df)) {
  output[[i]] <- mean(df[[i]])
}
output

#=============================================
col_mean <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[[i]] <- mean(df[[i]])
  }
 output 
}

col_median <- function(df) {
  output <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[[i]] <- median(df[[i]])
  }
  output 
}

col_sd(df)
col_median(df)
col_mean(df)

col_summary <- function(df, fun) {
  out <- vector("double", length(df))
  for (i in seq_along(df)) {
    output[i] <- fun(df[[i]])
  }
output
}
col_summary(df, mean)

### 21.4.1 - Exercises

#### 1.

X <- matrix(rnorm(15), nrow = 5)
X            

apply(X, 1, mean)

#===============================

X_row_means <- vector("numeric", length = nrow(X))
for (i in seq_len(nrow(X))) {
  X_row_means[[i]] <- mean(X[i, ])
}
X_row_means

#================================

apply(X, 2, mean)

#================================

X_col_means <- vector("numeric", length = ncol(X))
for (i in seq_len(ncol(X))) {
  X_col_means[[i]] <- mean(X[, i])
}
X_col_means

#### 2.

col_summary2 <- function(df, fun) {
  numeric_cols <- vector("logical", length(df))
  for (i in seq_along(df)) {
    numeric_cols[[i]] <- is.numeric(df[[i]])
  }
  idxs <- which(numeric_cols)
  n <- sum(numeric_cols)
  out <- vector("double", n)
  for (i in seq_along(idxs)) {
    out[[i]] <- fun(df[[idxs[[i]]]])
  }
  names(out) <- names(df[idxs])
  out
}

df <- tibble(
  X1 = c(1, 2, 3),
  X2 = c("A", "B", "C"),
  X3 = c(0, -1, 5),
  X4 = c(TRUE, FALSE, TRUE)
)

col_summary2(df, mean)
col_summary2(df, median)

## 21.5 Map Functions

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)

df %>% map_dbl(mean)
df %>% map_dbl(median)
df %>% map_dbl(sd)
map_dbl

map_dbl(df, mean, trim = 0.5)

z <- list(x = 1:3, y = 4:5)
map_int(z, length)

## 21.5.1

models <- mtcars %>%
  split(.$cyl) %>%
  map(function(df) lm(mpg ~ wt, data = df))
models

models <- mtcars %>%
  split(.$cyl) %>%
  map(~lm(mpg ~ wt, data = .))
models

models %>%
  map(summary) %>%
  map_dbl(~.$r.squared)

models %>%
  map(summary) %>%
  map_dbl("r.squared")

x <- list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9))
x %>% map_dbl(2)

### 21.5.2

x1 <- list(
  c(0.27, 0.37, 0.57, 0.91, 0.20),
  c(0.90, 0.94, 0.66, 0.63, 0.06),
  c(0.21, 0.18, 0.69, 0.38, 0.77)
)
x2 <- list(
  c(0.50, 0.72, 0.99, 0.38, 0.78),
  c(0.93, 0.21, 0.65, 0.13, 0.27),
  c(0.39, 0.01, 0.38, 0.87, 0.34)
)

threshold <- function(x, cutoff = 0.8) x[x > cutoff]
x1 %>% sapply(threshold) %>% str()
x2 %>% sapply(threshold) %>% str()

### 21.5.3 - Exercises

#### 1.1

map_dbl(mtcars, mean)

#### 1.2

map_chr(nycflights13::flights, typeof)

#### 1.3

length(unique(iris$Species))
map_int(iris, function(x) length(unique(x)))
map_int(iris, ~length(unique(.)))

#### 1.4

map(c(-10, 0, 10, 100), ~ rnorm(n = 10, mean = .))

#### 2.

is.factor(diamonds$color)

map_lgl(diamonds, is.factor)

#### 3.

map(1:5, runif)
map(c(TRUE, FALSE, TRUE), ~ !.)
map(c("Hello", "World"), str_to_upper)
map(1:5, ~ rnorm(.))
map(c(-0.5, 0, 1), ~ rnorm(1, mean = .))

#### 4.

map(-2:2, rnorm, n = 5)

map_dbl(-2:2, rnorm, n  = 5)

flatten_dbl(map(-2:2, rnorm, n = 5))

#### 5.

x <- split(mtcars, mtcars$cyl)
map(x, function(df) lm(mpg ~ wt, data = df))
map(x, ~ lm(mpg ~ wt, data = .))
