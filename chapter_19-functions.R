# Chapter 19 - Functions

### 19.2.1 - Exercises

#### 1.

rescale01 <- function(x) {
  rng <- range(x, na.rm = F, finite = F)
  (x - rng[1]) / (rng[2] - rng[1])
}
x <- c(1:10, NA)
rescale01(x)

#### 2.

rescale01 <- function(x) {
  rng <- range(x, na.rm = T, finite = T)
  y <- (x - rng[1]) / (rng[2] - rng[1])
  y[y == -Inf] <- 0
  y[y == Inf] <- 1
  y
}

rescale01(c(Inf,  -Inf, 0:5, NA))

#### 3.

##### a.
prop_na <- function(x) {
  mean(is.na(x))
}
prop_na(c(0, 1, 2, NA, 4, NA))

##### b.
sum_to_one <- function(x) {
  x / sum(x, na.rm = T)
}

sum_to_one(c(1, 2, 3, 4, 5))

##### c.
coef_variation <- function(x) {
  sd(x, na.rm = T) / mean(x, na.rm = T)
}

coef_variation(c(1:5))

#### 4.

##### a.
variance <- function(x, na.rm = T) {
  n <- length(x)
  m <- mean(x, na.rm = T)
  sq_err <- (x-m)^2
  sum(sq_err) / (n - 1)
}

var(1:10)

variance(1:10)

##### b.
skewness <- function(x, na.rm = T){
  n <- length(x)
  m <- mean(x, na.rm = na.rm)
  v <- var(x, na.rm = na.rm)
  (sum((x - m)^3) / (n - 2)) / v^(3/2)
}

skewness(c(1,2, 5, 100))

#### 5.
