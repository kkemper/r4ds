# Chapter 25 - Many Models

library(modelr)
library(tidyverse)

## 25.2 Gapminder

library(gapminder)
gapminder

gapminder %>%
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)

nz <- filter(gapminder, country == "New Zealand")
nz %>%
  ggplot(aes(year, lifeExp)) +
  geom_line() +
  ggtitle("Full Data = ")

nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>%
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) +
  geom_line() +
  ggtitle("Linear Trend + ")

nz %>%
  add_residuals(nz_mod) %>%
  ggplot(aes(year, resid)) +
  geom_hline(yintercept = 0, color = "white", size = 3) +
  geom_line() +
  ggtitle("Remaining Pattern")

### 25.2.1 - Nested Data
by_country <- gapminder %>%
  group_by(country, continent) %>%
  nest()
by_country
by_country$data[[1]]

### 25.2.2 - List-Columns
country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

by_country <- by_country %>%
  mutate(model = map(data, country_model))
by_country

by_country %>%
  filter(continent == "Europe")

by_country %>%
  arrange(continent, country)

### 25.2.3 - Unnesting

by_country <- by_country %>%
  mutate(
    resids = map2(data, model, add_residuals)
  )
by_country

resids <- unnest(by_country, resids)
resids

resids %>%
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 /3) +
  geom_smooth(se = FALSE)

resids %>%
  ggplot(aes(year, resid, group = country)) +
  geom_line(alpha = 1 /3) +
  facet_wrap(~continent)

### 25.2.4 - Model Quality

broom::glance(nz_mod)
by_country %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance)

glance <- by_country %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance, .drop = T)
glance
glance %>%
  arrange(r.squared)

glance %>%
  ggplot(aes(continent, r.squared)) +
  geom_jitter(width = 0.5)

bad_fit <- filter(glance, r.squared < 0.25)
gapminder %>%
  semi_join(bad_fit, by = "country") %>%
  ggplot(aes(year, lifeExp, color = country)) +
  geom_line()

### 25.2.5 - Exercises

lifeExp ~ poly(year, 2)

country_model <- function(df) {
  lm(lifeExp ~ poly(year - median(year), 2), data = df)
}

by_country <- gapminder %>%
  group_by(country, continent) %>%
  nest()
by_country <- by_country %>%
  mutate(model = map(data, country_model))
by_country <- by_country %>%
  mutate(resids = map2(data, model, add_residuals))
by_country

unnest(by_country, resids) %>%
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) +
  geom_smooth(se = F)

by_country %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance, .drop = T) %>%
  ggplot(aes(continent, r.squared)) +
  geom_jitter(width = 0.5)


#### 2.
library("ggbeeswarm")
by_country %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance, .drop = T) %>%
  ggplot(aes(continent, r.squared)) +
  geom_beeswarm()

#### 3.

gapminder %>%
  group_by(country, continent) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(lifeExp ~ year, .))) %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance) %>%
  unnest(data) %>%
  filter(r.squared < 0.25) %>%
  ggplot(aes(year, lifeExp)) +
  geom_line(aes(color = country))

## 25.3 List-Columns

data.frame(x = list(1:3, 3:5))
data.frame(
  x = I(list(1:3, 3:5)),
  y = c("1, 2", "3, 4, 5")
)

tibble(
  x = list(1:3, 3:5),
  y = c("1, 2", "3, 4, 5")
)

tribble(
  ~x, ~y,
  1:3, "1, 2",
  3:5, "3, 4, 5"
)

## 25.4 - Creating List-Columns

### 25.4.1 - Creating List-Columns with Nesting

gapminder %>%
  group_by(country, continent) %>%
  nest()

gapminder %>%
  nest(year:gdpPercap)

### 25.4.2 - Creating List-Columns from Vectorized Functions

df <- tribble(
  ~x1,
  "a, b, c",
  "d, e, f, g"
)

df  %>% mutate(x2 = stringr::str_split(x1, ","))
df %>%
  mutate(x2 = stringr::str_split(x1, ",")) %>%
  unnest()

sim <- tribble(
  ~f, ~params,
  "runif", list(min = -1, max =  1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)
sim %>%
  mutate(sims = invoke_map(f, params, n = 10))

### 25.4.3 - Creating List-Columns from Multivalued Summaries

mtcars %>%
  group_by(cyl) %>%
  summarize(quantile(mpg))
mtcars %>%
  group_by(cyl) %>%
  summarize(q = list(quantile(mpg)))
probs <- c(0.01, 0.25, 0.5, 0.75, 0.99)
mtcars %>%
  group_by(cyl) %>%
  summarize(p = list(probs), q = list(quantile(mpg, probs))) %>%
  unnest()

### 25.4.3 - Creating List-Columns from a Named List

x <- list(
  a = 1:5,
  b = 3:4,
  c = 5:6
)

df <- enframe(x)
df

df %>%
  mutate(smry = map2_chr(name, value, ~ stringr::str_c(.x, ": ", .y[1])))

str_split(sentences[1:3], " ")
str_match_all(c("abc", "aa", "aabaa", "abbbc"), "a+")
map(1:3, runif)

range(mtcars$mpg)
fivenum(mtcars$mpg)
boxplot.stats(mtcars$mpg)
