library(tidyverse)
library(nycflights13)

## 7.3.1 - Visualizing Distributions

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

diamonds %>% count(cut)

ggplot(data = diamonds) + 
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

diamonds %>% count(cut_width(carat, 0.5))

smaller <- diamonds %>%
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat)) + 
  geom_histogram(binwidth = 0.1)

ggplot(data = smaller, mapping = aes(x = carat, color = cut)) +
  geom_freqpoly(binwidth = 0.1)

## 7.3.2 - Typical Values

ggplot(data = smaller, mapping = aes(x = carat)) + 
  geom_histogram(binwidth = 0.01)

ggplot(data = faithful, mapping = aes(x = eruptions)) +
  geom_histogram(binwidth = 0.25)

## 7.3.3 - Unusual Values

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) + 
  coord_cartesian(ylim = c(0, 50))

unusual <- diamonds %>%
  filter(y < 3 | y > 20) %>%
  select(price, x, y, z) %>%
  arrange(y)
unusual


# 7.3.4 - Exercises

### 1 - distribution of dimensions (x, y, z)
ggplot(diamonds) + geom_histogram(mapping = aes(x = x))
ggplot(diamonds) + geom_histogram(mapping = aes(x = y))
ggplot(diamonds) + geom_histogram(mapping = aes(x = z))

### 2  - distribution of price
ggplot(diamonds) + geom_histogram(mapping = aes(x = price), binwidth = 1000)

### 3 - how many diamonds are 0.99 carat vs. 1.00 carat
diamonds %>%
  filter(carat == 0.99) %>%
  count()

diamonds %>%
  filter(carat == 1.00)
  count()

## 7.4 - Missing Values
  
diamonds2 <-  diamonds %>%
  filter(between(y, 3, 20))

diamonds2 <- diamonds %>%
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point()

ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point(na.rm = TRUE)

flights %>%
  mutate(
    canceled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>%
  ggplot(mapping = aes(sched_dep_time)) +
  geom_freqpoly(mapping = aes(color = canceled), binwidth = 1/4)

## 7.4.1 - Exercises

### 1

ggplot(data = diamonds2, mapping = aes(x = x)) +
  geom_histogram()

ggplot(data = diamonds2, mapping = aes(x = x)) +
  geom_point()

### 2

flights %>%
  mean(dep_time, na.rm = TRUE)

flights %>%
  sum(dep_time)

## 7.5.1 - A Categorical and Continuous Variable

ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

ggplot(diamonds) + 
  geom_bar(mapping = aes(x = cut))

ggplot(diamonds, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) + 
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()

ggplot(data = mpg) + geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))

ggplot(data = mpg) + 
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) + 
  coord_flip()

### 7.5.1.1 - Exercises

#### 1

flights %>%
  mutate(
    canceled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>%
  ggplot(mapping = aes(x = canceled, y = sched_dep_time, color = canceled)) +
  geom_boxplot()

#### 2

ppc <- mutate(diamonds, price_per_carat = price / carat)

ggplot(data = ppc, mapping = aes(x = cut, y = price_per_carat)) + 
  geom_boxplot()

#### 3



#### 4

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) + 
  geom_lv()

#### 5

ggplot(data = ppc, mapping = aes(x = cut, y = price)) + 
  geom_violin()

ggplot(data = diamonds, mapping = aes(x = cut, color = price)) + geom_histogram()

## 7.5.2 Two Categorical Variables

ggplot(data = diamonds) + 
  geom_count(mapping =aes(x = cut, y = color))

diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

### 7.5.2.1 - Exercises

#### 1



#### 2
 


#### 3



#### 4



## 7.5.3 - Two Continuous Variables

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price)) +
  geom_smooth(mapping = aes(x = carat, y = price))

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price), alpha = 1 / 100)

ggplot(data = smaller) + geom_bin2d(mapping = aes(x = carat, y = price))

ggplot(data = smaller) + 
  geom_hex(mapping = aes(x = carat, y = price))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))

ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

### 7.5.3.1 - Exercises

#### 1

ggplot(data = smaller, mapping = aes(x = carat, y = price)) +
  geom_freqpoly(mapping = aes(group = cut_number(carat, 20)))

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_freqpoly(mapping = aes(group = cut_number(carat, 20)))

#### 2

ggplot(data = diamonds) +
  geom_hex(mapping = aes(x = price, y = carat))

#### 3



#### 4
ggplot(data = diamonds) +
  geom_hex(mapping = aes(x = price, y = carat, fill = cut))

ggplot(data = diamonds) +
  geom_bin2d(mapping = aes(x = price, y = carat, fill = cut))

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = price, y = carat, color = cut))

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = price, y = carat, alpha = cut))


#### 5



## 7.6 = Patterns and Models

ggplot(data = faithful) +
  geom_point(mapping = aes(x = eruptions, y = waiting))

library(modelr)

mod <- lm(log(price) ~ log(carat), data = diamonds)

diamonds2 <-  diamonds %>%
  add_residuals(mod) %>%
  mutate(resid = exp(resid))

ggplot(data = diamonds2) +
  geom_point(mapping = aes(x = carat, y = resid))

ggplot(data = diamonds2) + 
  geom_boxplot(mapping = aes(x = cut, y = resid))

## 7.7 - ggplot2 calls

ggplot(data = faithful, mapping = aes(x = eruptions)) +
  geom_freqpoly(binwidth = 0.25)

ggplot(faithful, aes(eruptions)) +
  geom_freqpoly(binwidth = 0.25)
diamonds %>%
  count(cut, clarity) %>%
  ggplot(aes(clarity, cut, fill = n)) + 
  geom_tile()

## 7.8 - Learning More