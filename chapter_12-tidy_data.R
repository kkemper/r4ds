library(tidyverse)

# 12 - Tidy Data

## 12.3 Spreading and Gathering

### 1.

stocks <-  tibble(
  year = c(2015, 2015, 2016, 2016),
  half = c( 1, 2, 1, 2),
  return = c(1.88, 0.59, 0.92, 0.17)
)

stocks %>%
  spread(year, return) %>%
  gather("year", "return", `2015`:`2016`)

## 12.4 - Separating and Uniting

### 12.4.3 - Exercises

#### 1.

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three"), extra = "warn")

tibble(x = c("a,b,c", "d,e", "h,i,j")) %>%
  separate(x, c("one", "two", "three"), fill = "right")

## 12.5 - Missing Values

stocks <- tibble(
  year = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr = c(1, 2, 3, 4, 2, 3, 4),
  return = c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.66)
)

stocks %>%
  spread(year, return) %>%
  gather(year, return, `2015`:`2016`, na.rm = TRUE)

stocks %>%
  complete(year, qtr)

treatment <-  tribble(
  ~ person, ~treatment, ~response,
  "Derrick Whitmore", 1, 7,
  NA, 2, 10,
  NA, 3, 9,
  "Katherine Burke", 1, 4
)

treatment %>%
  fill(person)

## 12.6 - Case Study

who1 <- who %>%
  gather(new_sp_m014:newrel_m014:newrel_f65, key = "key", value = "cases", na.rm = TRUE)

who1 %>%
  count(key)

who2 <- who1 %>%
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))

who3 <- who2 %>%
  separate(key, c("new", "type", "sexage"), sep="_")

who3 %>%
  count(new)

who4 <-  who3 %>%
  select(-new, -iso2, -iso3)

who5 <- who4 %>%
  separate(sexage, c("sex", "age"), sep = 1)

### 12.6.1 - Exercises

#### 4.

who6 <-  who5 %>%
  group_by(country, year, sex) %>%
  summarize(count = sum(cases))

ggplot(data = who6, mapping = aes(x = year, y = count, color = sex, label = country)) + geom_point()
