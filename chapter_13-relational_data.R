library(tidyverse)
library(nycflights13)

#Chapter 13 - Relational Data

## 13.2 - NYC Flights

airlines

airports

planes

weather

## 13.3 - Keys

planes %>%
  count(tailnum) %>%
  filter(n > 1)

weather %>%
  count(year, month, day, hour, origin) %>%
  filter(n > 1)

flights %>%
  count(year, month, day, flight) %>%
  filter(n > 1)

flights %>%
  count(year, month, day, tailnum) %>%
  filter(n > 1)

### 13.3.1 - Exercises

flights %>%
  mutate(flight_id = row_number()) %>%
  glimpse()

## 13.4 - Mutating Joins

flights2 <-  flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)

flights2

### left_join()
flights2 %>%
  select(-origin, -dest) %>%
  left_join(airlines, by = "carrier")

### match()
flights2 %>%
  select(-origin, -dest) %>%
  mutate(name = airlines$name[match(carrier, airlines$carrier)])

### 13.4.1 - Understanding Joins

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3"
)

y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

### 13.4.2 - Inner Joins

x %>%
  inner_join(y, by = "key")

### 13.4.3 - Outer Joins

### 13.4.4 - Duplicate Keys

#### One table has duplicate keys
x <-  tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4",
)

y <-  tribble(
  ~key, ~val,
  1, "y1",
  2, "y2"
)

left_join(x,y, b = "key")

#### Both tables have duplicate keys

x <- tribble(
  ~key, ~val,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4",
  )

y <- tribble(
  ~key, ~val,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)

inner_join(x, y, by = "key")

### 13.4.5 - Defining the Key Columns

flights2 %>%
  left_join(weather)

flights2 %>%
  left_join(planes, by = "tailnum")

flights2 %>%
  left_join(airports, c("dest" = "faa"))

flights2 %>%
  left_join(airports, c("origin" = "faa"))

### 13.4.6 Exercises

#### 1.

avg_delay <- flights %>%
  group_by(dest) %>%
  summarize(delay = mean(arr_delay, na.rm = TRUE)) %>% 
  inner_join(airports, by = c("dest" = "faa"))

avg_delay %>%  
  ggplot(aes(lon, lat, color = delay)) + 
  borders("state") +
  geom_point() +
  coord_quickmap()

#### 2.

origin_and_dest <- flights %>%
  left_join(airports, by = c("dest" = "faa")) %>%
  left_join(airports, by = c("origin" = "faa"))

view(origin_and_dest)

#### 3. 

plane_age <- flights %>%
  group_by(tailnum) %>%
  summarize(delay = mean(arr_delay, na.rm = TRUE)) %>%
  inner_join(planes) %>%
  mutate(age = 2019 - year)

plane_age %>%
  ggplot(aes(age, delay)) +
  geom_point()

#### 4.

flight_weather <- flights %>%
  inner_join(weather, by = c(
    "origin" = "origin",
    "year" = "year",
    "month" = "month",
    "day" = "day",
    "hour" = "hour"
    ))
view(flight_weather)

flight_weather %>%
  group_by(precip) %>%
  summarize(delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = precip, y = delay)) +
  geom_line() + 
  geom_point()

#### 5.

flights %>%
  filter(year == 2013, month == 6, day == 13) %>%
  group_by(dest) %>%
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  inner_join(airports, by = c("dest" = "faa")) %>%
  ggplot(aes(y = lat, x = lon, size = delay, color = delay)) +
  borders("state") + 
  geom_point() + 
  coord_quickmap() + 
  scale_color_viridis_c()
  
 ## 13.5 Filtering Joins

top_dest <- flights %>%
  count(dest, sort = TRUE) %>%
  head(10)
top_dest

flights %>%
  filter(dest %in% top_dest$dest)

flights %>%
  semi_join(top_dest)

no_tailnum <- flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(tailnum, sort = TRUE)

### 13.5.1 - Exercises

#### 1. 

flights %>%
  filter(is.na(tailnum), !is.na(arr_time)) %>%
  nrow()

flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(carrier, sort = TRUE) %>%
  mutate(p = n / sum(n))

flights %>%
  distinct(carrier, tailnum) %>%
  left_join(planes, by = "tailnum") %>%
  group_by(carrier) %>%
  summarize(
    total_planes = n(),
    not_in_planes = sum(is.na(model))
  ) %>%
  mutate(missing_pct = not_in_planes / total_planes) %>%
  arrange(desc(missing_pct))

#### 2.

planes_gtr_than_100 <- flights %>%
  filter(!is.na(tailnum)) %>%
  group_by(tailnum) %>%
  count() %>%
  filter(n >= 100)

flights %>% semi_join(planes_gtr_than_100, by = "tailnum")

#### 3.

fueleconomy::vehicles %>%
  semi_join(fueleconomy::common, by = c("make", "model"))

#### 4.

worst_hours <- flights %>%
  mutate(hour = sched_dep_time %/% 100) %>%
  group_by(origin, year, month, day, hour) %>%
  summarize(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(dep_delay)) %>%
  slice(1:48)

weather_most_delayed <- semi_join(weather, worst_hours, by = c(
  "origin", "year", "month", "day", "hour"))

select(weather_most_delayed, temp, wind_speed, precip) %>%
  print(n = 48)

ggplot(weather_most_delayed, aes(x = precip, y = wind_speed, color = temp)) + 
  geom_point()

#### 6.

planes_carriers <- flights %>%
  filter(!is.na(tailnum)) %>%
  distinct(tailnum, carrier)

planes_carriers

planes_carriers %>%
  count(tailnum) %>%
  filter(n>1) %>%
  nrow()

carrier_transfer_tbl <- planes_carriers %>%
  group_by(tailnum) %>%
  filter(n() >1) %>%
  left_join(airlines, by = "carrier") %>%
  arrange(tailnum, carrier)

## 13.6 - Join Problems

airports %>%
  count(alt, lon) %>%
  filter(n > 1)

## 13.7 - Set Operations

df1 <- tribble(
  ~x, ~y,
  1, 1,
  2, 1
)

df2 <-  tribble(
  ~x, ~y,
  1, 1, 
  1, 2
)

intersect(df1, df2)

union(df1, df2)

setdiff(df1, df2)

setdiff(df2, df1)

