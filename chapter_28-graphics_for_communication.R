# Chapter 28 - Graphics for Communication

library(tidyverse)

## 28.2 - Label

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = F) +
  labs(title = "Fuel efficiency generally decreases with engine size",
       subtitle = "Two seaters (sports cars) are an exception due to their light weight",
       caption = "Data from fueleconomy.gov")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = F) +
  labs(x = "Engine Displacement (L)",
       y = "Highway Fuel Economy (mpg)",
       color = "Car Type")

df <- tibble(
  x = runif(10),
  y = runif(10)
)

ggplot(df, aes(x, y)) +
  geom_point() +
  labs(
    x = quote(sum(x[i] ^ 2, i = 1, n)),
    y = quote(alpha + beta + frac(delta, theta))
  )

### 28.2.1 - Exercises

#### 1.

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = F) +
  labs(title = "Fuel efficiency generally decreases with engine size",
       subtitle = "Two seaters (sports cars) are an exception due to their light weight",
       caption = "Data from fueleconomy.gov",
       x = "Engine Displacement (L)",
       y = "Highway Fuel Economy (mpg)",
       color = "Car Type")

#### 2.

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  labs(
    title = "Fuel Efficiency Decreases with Engine Size",
    caption = "Data from fueleconomy.gov",
    y = "Highway Miles per Gallon",
    x = "Engine Displacement"
  )
  
ggplot(mpg, aes(displ, hwy, color = class)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  labs(
    title = "Fuel Efficiency Decreases with Engine Size",
    subtitle = "Subcompact Carries Fuel Efficiency Varies by Engine Size",
    caption = "Data from fueleconomy.gov",
    y = "Highway Miles per Gallon",
    x = "Engine Displacement"
  )

mod <- lm(hwy ~ class, data = mpg)
mpg %>%
  add_residuals(mod) %>%
  ggplot(aes(x = displ, y = resid)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = F) +
  labs(
    title = "Engine size has little effect on fuel efficiency",
    subtitle = "After accounting for car class",
    caption = "Data from fueleconomy.gov",
    y = "Highway MPG Relative to Class Average",
    x = "Engine Displacement"
  )

#### 3.

steps <- tibble(
  date = as.Date(x=seq.Date(as.Date("2019-08-09"), as.Date("2019-08-18"), "days")),
  fitbit = c(6476, 6853, 6181, 6607, 11023, 8115, 11992, 7162, 3505, 3619),
  misfit = c(4868, 6226, 5922, 5598, 9200, 6844, 11322, 5844, 3380, 2806),
  apple_watch = c(3820, 5350, 4019, 5042, 10060, 6059, 12229, 6296, 1980, 2292),
  spire = c(3594, 4773, 3154, NA, 6389, 2497, 8370, 3922, NA, 976),
  lumo_lift = c(4202, 4710, 3139, 3322, 6911, 3823, 8789, 4527, NA, 866)
)

steps2 <- steps %>%
  gather('fitbit', 'misfit', 'apple_watch', 'spire', 'lumo_lift', key = 'source', value = 'steps_per_day') %>%
  group_by(date) %>%
  mutate(mean = mean(steps_per_day, na.rm = T)) %>%
  mutate(variance = (steps_per_day - mean))

steps3 <- steps2 %>%
  group_by(source) %>%
  summarize(mean_variance = mean(variance, na.rm = T))

# Add mean and standard deviation columns to the table.

# Create line plots of the data
ggplot(data  = steps2) +
  geom_line(mapping = aes(x = date, y = steps_per_day, color = source)) +
  labs(
    title = "Fitness trackers do not count steps equally",
    subtitle = "There is a relative agreement, though",
    color = "Fitness Tracker",
    x = "Date",
    y = "Steps Per Day"
  )

## 28.3 - Annotations
