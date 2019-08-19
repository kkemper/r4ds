library(tidyverse)
library(stringr)

# Chapter 14 - Strings

## 14.2 - String Basics

string1 <- "This is a string"
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'

x <- c("\"", "\\")
x
writeLines(x)

### 14.2.1

str_length(c("a", "R for Data Science", NA))

### 14.2.2 Combining Strings

str_c("x", "y")

str_c("x", "y", "z")

str_c("x", "y", sep = ", ")

x <- c("abc", NA)
str_c("|-", x, "-|")

str_c("|-", str_replace_na(x), "-|")

str_c("prefix-", c("a", "b", "c"), "-suffix")

name <- "Hadley"
time_of_day <- "morning"
birthday <- FALSE

str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
  )

str_c(c("x", "y", "z"), collapse = ", ")

### 14.2.3 - Subsetting Strings

x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)
str_sub(x, -3, -1)

str_sub("a", 1, 5)

str_sub(x, 1, 1) <- str_to_lower(str_sub(x, 1, 1))
x

### 14.2.4 - Locales

str_to_upper(c("i", "ī"))
str_to_upper(c("i", "ī"), locale = "tr")

x <- c("apple", "eggplant", "banana")
str_sort(x, locale = "en")
str_sort(x, locale = "haw")

### 14.2.5 - Exercises

str_trim(" abc")
str_trim(" abc ", side = "left")
str_trim(" abc ", side = "right")

str_pad("abc", 5, side = "both")
str_pad("abc", 4, side = "left")
str_pad("abc", 4, side = "right")

## 14.3 Matching Patterns with Regular Expressions

### 14.3.1 - Basic Matches
x <- c("apple", "banana", "pear")
str_view(x, "an")

str_view(x, ".a.")

dot <- "\\."
writeLines(dot)
str_view(c("abc", "a.c", "bef"), "a\\.c")

x <- "a\\b"
writeLines(x)
str_view(x, "\\\\")

#### 14.3.1.1 - Exercises
y <- "\"'\\"
writeLines(y)

### 14.3.2 Anchors

x <- c("apple", "banana", "pear")
str_view(x, "^a")
str_view(x, "a$")

x <- c("apple pie", "apple", "apple cake")
str_view(x, "^apple$")

#### 14.3.2.1 - Exercises

##### 1.

str_view("$^$", "^\\$\\^\\$$")

##### 2. 

str_view(stringr::words, "^y", match = TRUE)
str_view(stringr::words, "x$", match = TRUE)
str_view(stringr::words, "^...$", match = TRUE)
str_view(stringr::words, ".......", match = TRUE)

### 14.3.3 Character Classes and Alternatives

str_view(c("abc", "a.c", "a*c", "a c"), "a[.]c")
str_view(c("abc", "a.c", "a*c", "a c"), "a[*]c")
str_view(c("abc", "a.c", "a*c", "a c"), "a[ ]c")
str_view(c("abc", "a.c", "a*c", "a c"), "a[b]c")

str_view(c("grey", "gray"), "gr(e|a)y")

#### 14.3.3.1 - Exercises

##### 1. 

str_view(stringr::words, "^(a|e|i|o|u)", match = TRUE)
str_view(stringr::words, "^[^aeiou]+$", match = TRUE)
str_view(stringr::words, "[^e]ed$", match = TRUE)
str_view(stringr::words, "ise|ing$", match = TRUE)

##### 2.

str_view(stringr::words, "[^c]ie|cei", match = TRUE)
str_view(stringr::words, "[^c]ei|cie", match = TRUE)

##### 3.

str_view(stringr::words, "qu", match = TRUE)
str_view(stringr::words, "q[^u]", match = TRUE)

##### 4.

str_view(stringr::words, "our$", match = TRUE)

##### 5.

str_view(317-348-4288, "\\d{3}-\\d{3}-\\d{4}", match = TRUE)

### 14.3.4

x <- "1888 is the longest year un Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")
str_view(x, "CC+")
str_view(x, "C[LX]+")
str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,3}")
str_view(x, "C{2,3}?")
str_view(x, "C[LX]+?")

#### 14.3.4.1 - Exercises

##### 3.

str_view(stringr::words, "^[^aeiou]{3}", match = TRUE)
str_view(stringr::words, "[aeiou]{3}", match = TRUE)
str_view(stringr::words, "([aeiou][^aeiou]){2,}", match = TRUE)

### 14.3.5 - Grouping and Backreferences

str_view(fruit, "(..)\\1", match = TRUE)

#### 14.3.5.1 - Exercises

##### 1.

str_view(words, "^(.)((.*\\1$)|\\1?$)", match = TRUE)

##### 2.

str_view(words, "([A-Za-z][A-Za-z]).*\\1", match = TRUE)

##### 3.

str_view(words, "([A-Za-z]).*\\1.*\\1", match = TRUE)
