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
