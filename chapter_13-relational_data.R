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

flights2 <-  flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)

flights2

