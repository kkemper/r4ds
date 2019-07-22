# Chapter 10 - Tibbles

## 10.2

library(tidyverse)

as_tibble(iris)

tibble(
  x = 1:5,
  y = 1,
  z = x ^ 2 + y
)

tb <-  tibble(
  `:)` = "smile",
  ` ` = "space",
  `2000` = "number"
)

tb

tribble(
  ~x, ~y, ~z,
  #--|--|----
  "a", 2, 3.6,
  "b", 1, 8.5
)

## 10.3 - Tibbles vs. data.frame

### 10.3.1 - Printing

tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)

nycflights13::flights %>%
  print(n = 10, width = Inf)

nycflights13::flights %>%
 View() 

### 10.3.2 - Subsetting

df <- tibble(
  x = runif(5),
  y = rnorm(5)
)
df

df$x

df[["x"]]

df[[1]]
df %>% .$x
df %>% .[["x"]]

## 10.4 - Interacting with Older Code

class(as.data.frame(tb))

## 10.5 - Exercises

### 1

mtcars %>% 
  print(n=10, width = Inf)

### 2

df <- data.frame(abc = 1, xyz = "a")
df$x
df[,"xyz"]
df[, c("abc", "xyz")]

### 3



### 4

annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)
annoying

annoying$`1`

ggplot(annoying, aes(`1`, `2`)) +
  geom_point()

annoying$`3` = annoying$`2` / annoying$`1`
annoying

annoying <- rename(annoying, "one" = `1`, "two" = `2`, "three" = `3`)
annoying

### 5

?tibble::enframe
