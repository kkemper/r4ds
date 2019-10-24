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


