library(tidyverse)
library(stringr)
library(microbenchmark)

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

## 14.4 - Tools

x <- c("apple", "banana", "pear")
str_detect(x, "e")
sum(str_detect(words, "^t"))
mean(str_detect(words, "[aeiou]$"))

no_vowels_1 <- !str_detect(words, "[aeiou]")
no_vowels_2 <- str_detect(words, "^[^aeiou]+$")
identical(no_vowels_1, no_vowels_2)


words[str_detect(words, "x$")]
str_subset(words, "x$")

df <- tibble(
  word = words,
  i = seq_along(word)
)
df %>%
  filter(str_detect(word, "x$"))

x <- c("apple", "banana", "pear")
str_count(x, "a")

mean(str_count(words, "[aeiou]"))

df %>%
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants  = str_count(word, "[^aeiou]")
  )

str_count("abababa", "aba")
str_view_all("abababa", "aba")

#### 14.4.1.1 - Exercises

##### 1. 

str_view(words, "[(^x)|(x$)]", match = TRUE)

starts_with_x = str_detect(words, "^x")
ends_with_x = str_detect(words, "x$")
words[starts_with_x|ends_with_x]

##### 2.

str_view(words, "^[aeiou].*[^aeiou]$", match = TRUE)

starts_with_vowel <- str_detect(words, "^[aeiou]")
end_with_consonant <-  str_detect(words, "[^aeiou]$")
words[starts_with_vowel&end_with_consonant]

##### 3.

a <- str_detect(words, "a")
e <- str_detect(words, "e")
i <- str_detect(words, "i")
o <- str_detect(words, "o")
u <- str_detect(words, "u")
words[a&e&i&o&u]

### 14.4.2 - Extract Matches

length(sentences)
head(sentences)

colors <- c("red", "orange", "yellow", "green", "blue", "purple")
color_match <-  str_c(colors, collapse = "|")
color_match
has_color <- str_subset(sentences, color_match) 
matches <- str_extract(has_color, color_match)
head(matches)

more <- sentences[str_count(sentences, color_match) > 1]
str_view_all(more, color_match)
str_extract(more, color_match)

str_extract_all(more, color_match)
str_extract_all(more, color_match, simplify = TRUE)

x <- c("a", "a b", "a b c")
str_extract_all(x, "[a-z]", simplify = TRUE)

#### - 14.4.2.1 - Exercises

##### 1.

colors <- c("red", "orange", "yellow", "green", "blue", "purple")
color_match <-  str_c("\\b(", str_c(colors, collapse = "|"), ")\\b")
color_match
has_color <- str_subset(sentences, color_match) 
matches <- str_extract(has_color, color_match)
head(matches)
more <- sentences[str_count(sentences, color_match) > 1]
str_view_all(more, color_match)

##### 2.

### 14.2.3

noun <- "(a|the) ([^ ]+)"
has_noun <- sentences %>%
  str_subset(noun) %>%
  head(10)
has_noun %>%
  str_extract(noun)

has_noun %>%
  str_match(noun)

tibble(sentence = sentences) %>%
  tidyr::extract(
    sentence, c("article", "noun"), "(a|the) ([^ ]+)", remove = FALSE
  )

#### 14.4.3.1 - Exercises

##### 1.

numeric <- "\\b(one|two|three|four|five|six|seven|eight|nine|ten) +(\\w+)"
sentences[str_detect(sentences, numeric)] %>%
  str_extract(numeric)

##### 2. 

contraction <- "(\\b[A-Za-z]+\\b)'(\\b[A-Za-z]+\\b)"
sentences[str_detect(sentences, contraction)] %>%
  str_extract(contraction) %>%
  str_split("'")

### 14.4.4 - Replacing Matches

x <- c("apple", "banana", "pear")
str_replace(x, "[aeiou]", "-")
str_replace_all(x, "[aeiou]", "-")

x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three"))

sentences %>%
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1, \\3, \\2") %>%
  head(5)

#### 14.4.4.1 - Exercises



###14.4.5 - Splitting

sentences %>%
  head(5) %>%
  str_split(" ")

"a|b|c|d" %>%
  str_split("\\|") %>%
  .[[1]]


fields <- c("Name: Hadley", "Country: NZ", "Age: 35")
fields %>% str_split(": ", n=2, simplify = TRUE)

x <- "This is a sentence. This is another sentence."
str_view_all(x, boundary("word"))
str_split(x, " ") [[1]]
str_split(x, boundary("word"))[[1]]

#### 14.4.5.1 - Exercises

##### 1.

fruit <- "apples, pears, and bananas"
str_split(fruit, boundary("word"))
str_split(fruit, boundary("line_break"))

## 14.5 - Other Types of Patterns

str_view(fruit, "nana")
str_view(fruit, regex("nana"))

bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")
str_view(bananas, regex("banana", ignore_case = TRUE))

x <- "Line 1\nLine 2\nLine 3"
str_extract_all(x, "^Line")[[1]]
str_extract_all(x, regex("^Line", multiline = TRUE))[[1]]         

phone <- regex("
               \\(?     # optional opening parens
               (\\d{3}) # area code
               [) -]?   # optional closing parens, space, or dash
               (\\d{3}) # another three numbers
               [ -]?    # another optional space or dash
               (\\d{3}) # three more numbers
               ", comments = TRUE)
str_match("514-791-8141", phone)

microbenchmark::microbenchmark(
  fixed = str_detect(sentences, fixed("the")),
  regex = str_detect(sentences, "the"),
  times = 20
)

a1 <- "\u00e1"
a2 <- "a\u0301"
c(a1, a2)
a1 == a2
str_detect(a1, fixed(a2))
str_detect(a1, coll(a2))

stringi::stri_locale_info()

x <- "This is a sentence."
str_view_all(x, boundary("word"))
str_extract_all(x, boundary("word"))

### 14.5.1 - Exercises


##14.6 - Other Uses of Regular Expressions

apropos("replace")

head(dir(pattern = "\\.Rmd$"))

