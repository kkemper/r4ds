library(tidyverse)

# Chapter 19 - Vectors

## 20.3.1 - Logical Atomic Vectors

1:10 %% 3 == 0

c(TRUE< TRUE, FALSE, NA)

### 20.3.2 - Numeric Atomic Vectors

typeof(1)
typeof(1L)
1.5L

x <- sqrt(2) ^ 2
x

x - 2

c(-1, 0, 1) / 0

### 20.3.3 - Character Atomic Vector

x <- "This is a reasonably long string."
pryr::object_size(x)

y <- rep(x, 1000)
pryr::object_size(y)

NA
NA_integer_
NA_real_
NA_character_


### 20.4.1 - Coercion

x <- sample(20, 100, replace = T)
y <- x>10
sum(y)

typeof(c(TRUE, 1L))
typeof(c(1L, 1.5))
typeof(c(1.5, "a"))

### 20.4.3 - Scalars and Recycling Rules

tibble(x = 1:4, y = 1:2)
tibble(x = 1:4, y = rep(1:2, 2))
tibble(x = 1:4, y = rep(1:2, each = 2))

### 20.4.4

c(x = 1, y = 2, z = 4)
set_names(1:3, c("a", "b", "c"))
x <- c("one", "two", "three", "four", "five")
x[c(3, 2, 5)]
x[c(1, 1, 1, 5, 5, 5, 5, 2)]
x[c(-1, -3, -5)]


x <- c(10, 3, NA, 5, 8, 1, NA)
x[!is.na(x)]
x[x %% 2 == 0]

x <- c(abc = 1, def = 3, xyz = 5)
x[c("xyz", "def")]

## 20.5 - Recursive Vectors (Lists)

x <- list(1, 2, 3)
x

str(x)
x_named <- list(a = 1, b = 2, c = 3)
str(x_named)

y <- list("a", 1L, 1.5, TRUE)
str(y)

z <- list(list(1, 2), list(3, 4))
str(z)

### 20.5.5 Subsetting Lists

a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))
str(a[1:2])
str(a[4])
str(a[[1]])
str(a[[4]])
a$a
a[["a"]]

## 20.6 Attributes

x <- 1:10
attr(x, "greeting")
attr(x, "greeting") <- "Hi!"
attr(x, "farewell") <- "Bye!"
attributes(x)

## 20.7 Augmented Vectors

### 20.7.1 - Factors

x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
typeof(x)

### 20.7.2 Dates and Date-Times

x <- as.Date("1971-01-01")
unclass(x)
typeof(x)
attributes(x)

x <- lubridate::ymd_hm("1970-01-01 01:00")
unclass(x)
typeof(x)
attributes(x)

attr(x, "tzone") <- "US/Pacific"
x
attr(x, "tzone") <- "US/Eastern"
x

y <- as.POSIXlt(x)
typeof(y)
attributes(y)

### 20.7.3 Tibbles

tb <- tibble::tibble(x = 1:5,  y = 5:1)
typeof(tb)
attributes(tb)
tb

df <- data.frame(x = 1:5, y = 5:1)
typeof(df)
attributes(df)

### 20.7.4 - Exercises

#### 1.
q <- hms::hms(3600)
print(q)
attributes(q)
typeof(q)

### 3.
tibble <- tibble(x = 1:5, y = list(list(1,2),3, 4, 5, 6))
tibble
view(tibble)
