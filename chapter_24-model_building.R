library(tidyverse)
library(modelr)
options(na.action = na.warn)
library(nycflights13)
library(lubridate)

## 24.2 - Why are Low Quality Diiamonds So Expensive

ggplot(diamonds, aes(cut, price)) + geom_boxplot()
ggplot(diamonds, aes(color, price)) + geom_boxplot()
ggplot(diamonds, aes(clarity, price)) + geom_boxplot()

### 24.2.1 - Price and Carat

ggplot(diamonds, aes(carat, price)) + geom_hex(bins = 50)

diamonds2 <- diamonds %>%
  filter(carat <= 2.5) %>%
  mutate(lprice = log2(price), lcarat = log2(carat))
ggplot(diamonds2, aes(lcarat, lprice)) +
  geom_hex(bins = 50)

mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)
grid <- diamonds2 %>%
  data_grid(carat = seq_range(carat, 20)) %>%
  mutate(lcarat = log2(carat)) %>%
  add_predictions(mod_diamond, "lprice") %>%
  mutate(price = 2 ^ lprice)
ggplot(diamonds2, aes(carat, price)) +
  geom_hex(bins = 50) +
  geom_line(data = grid, color = "red", size = 1)

diamonds2 <- diamonds2 %>%
  add_residuals(mod_diamond, "lresid") 
ggplot(diamonds2, aes(lcarat, lresid)) +
  geom_hex(bins = 50)

ggplot(diamonds2, aes(cut, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(color, lresid)) + geom_boxplot()
ggplot(diamonds2, aes(clarity, lresid)) + geom_boxplot()

### 24.2.2 - A More Complicated Model

mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)
grid <- diamonds2 %>%
  data_grid(cut, .model = mod_diamond2) %>%
  add_predictions(mod_diamond2)
grid
ggplot(grid, aes(cut, pred)) +
  geom_point()

diamonds2 <- diamonds2 %>%
  add_residuals(mod_diamond2, "lresid2")

ggplot(diamonds2, aes(lcarat, lresid2)) +
  geom_hex(bins = 50)

diamonds2 %>%
  filter(abs(lresid2) > 1) %>%
  add_predictions(mod_diamond2) %>%
  mutate(pred = round(2 ^ pred)) %>%
  select(price, pred, carat:table, x:z) %>%
  arrange(price)

### 24.2.3 - Exercises

#### 2.

mod_log <- lm(log2(price) ~ log2(carat), data = diamonds)
mod_log

tibble(carat = seq(0.25, 5, by = 0.25)) %>%
  add_predictions(mod_log) %>%
  ggplot(aes(x = carat, y = 2^pred)) +
  geom_line() +
  labs(x = "carat", y = "price")


2^coef(mod_log)[2]
2^(predict(mod_log, newdata = tibble(carat = 2)) - predict(mod_log, newdata = tibble(carat = 1)))
2^(predict(mod_log, newdata = tibble(carat = 4)) - predict(mod_log, newdata = tibble(carat = 2)))
2^(predict(mod_log, newdata = tibble(carat = 1)) - predict(mod_log, newdata = tibble(carat = 0.5)))

#### 4.

ggplot(diamonds2, aes(lcarat, lresid2)) +
  geom_hex(bins = 50)

lresid2_summary <- summarize(diamonds2, 
                             rmse = sqrt(mean(lresid2^2)),
                             mae = mean(abs(lresid2)),
                             p025 = quantile(lresid2, 0.025),
                             p975 = quantile(lresid2, 0.975)
                             )
lresid2_summary

## 24.3 - What Affects the Number of Daily Flights?

daily <- flights %>%
  mutate(date = make_date(year, month, day)) %>%
  group_by(date) %>%
  summarize(n = n())
daily
ggplot(daily, aes(date, n)) +
  geom_line()
daily <- daily %>%
  mutate(wday = wday(date, label = TRUE))
ggplot(daily, aes(wday, n)) +
  geom_boxplot()

mod <- lm(n ~ wday, data = daily)

grid <- daily%>%
  data_grid(wday) %>%
  add_predictions(mod, "n")
ggplot(daily, aes(wday, n)) +
  geom_boxplot() +
  geom_point(data = grid, color = "red", size = 4)

daily <- daily %>%
  add_residuals(mod)
daily %>%
  ggplot(aes(date, resid)) +
  geom_ref_line(h = 0) +
  geom_line()

ggplot(daily, aes(date, resid, color = wday)) +
  geom_ref_line(h = 0) +
  geom_line()

daily %>%
  filter(resid < -100)

daily %>%
  ggplot(aes(date, resid)) +
  geom_ref_line(h = 0) +
  geom_line(color = "grey50") +
  geom_smooth(se = FALSE, span = 0.20)

### 24.3.2 - Seasonal Saturday Effect

daily %>%
  filter(wday == "Sat") %>%
  ggplot(aes(date, n)) +
  geom_point() +
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

term <- function(date) {
  cut(date,
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall")
      )
}

daily <- daily%>%
  mutate(term = term(date))

daily %>%
  filter(wday == "Sat") %>%
  ggplot(aes(date, n, color = term)) +
  geom_point(alpha = 1/3) +
  geom_line() +
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

daily %>%
  ggplot(aes(wday, n, color = term)) +
  geom_boxplot()

mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday * term, data = daily)
daily %>%
  gather_residuals(without_term = mod1, with_term = mod2) %>%
  ggplot(aes(date, resid, color = model)) +
  geom_line(alpha = 0.75)

grid <- daily %>%
  data_grid(wday, term) %>%
  add_predictions(mod2, "n")

ggplot(daily, aes(wday, n)) +
  geom_boxplot() +
  geom_point(data = grid, color = "red") +
  facet_wrap(~ term)

mod3 <- MASS::rlm(n ~ wday * term, data = daily)
daily %>%
  add_residuals(mod3, "resid") %>%
  ggplot(aes(date, resid)) +
  geom_hline(yintercept = 0, size = 2, color = "white") +
  geom_line()

### 24.3.3 Computed Variables

compute_vars <- function(data) {
  data %>%
    mutate(
      term = term(date),
      wday = wday(date, label = TRUE)
    )
}

wday2 <- function(x) wday(x, label = TRUE)
mod3 <- lm(n ~ wday2(date) * term(date), data = daily)

### 24.3.4 - Time of Year: An Alternative Approach

library(splines)
mod <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)

daily %>%
  data_grid(wday, date = seq_range(date, n = 13)) %>%
  add_predictions(mod) %>%
  ggplot(aes(date, pred, color = wday)) +
  geom_line() + 
  geom_point()

### 24.3.5 - Exercises

#### 2.

top_n(daily, 3, resid)

#### 3.

daily <- daily %>%
  mutate(
    wday2 = 
      case_when(
        wday == "Sat" & term == "summer" ~"Sat-summer",
        wday == "Sat" & term == "fall" ~ "Sat-fall",
        wday == "Sat" & term == "spring" ~ "sat-spring",
        TRUE ~ as.character(wday)
      )
  )

mod3 <- lm(n ~ wday2, data = daily)

daily %>%
  gather_residuals(sat_term = mod3, all_interact = mod2) %>%
  ggplot(aes(date, resid, color = model)) +
  geom_line(alpha = 0.75)

daily %>%
  spread_residuals(sat_term = mod3, all_interact = mod2) %>%
  mutate(resid_diff = sat_term - all_interact) %>%
  ggplot(aes(date, resid_diff)) +
  geom_line(alpha = 0.75)

library(generics)
glance(mod3) %>% select(r.squared, sigma, AIC, df)
glance(mod2) %>% select(r.squared, sigma, AIC, df)

#### 4.

holidays_2013 <- 
  tribble(
    ~holiday, ~date,
    "New Year's Day", 20130101,
    "Martin Luther King, Jr. Day", 20130121,
    "President's Day", 20130218,
    "Memorial Day", 20130527,
    "Independence Day", 20130704,
    "Labor Day", 20130902,
    "Columbus Day", 20131028,
    "Veteran's Day", 20131111,
    "Thanksgiving Day", 20131128,
    "Christmas Day", 20131225
  ) %>%
  mutate(date = lubridate::ymd(date))
daily <- daily %>%
  mutate(
    wday3 =
      case_when(
        date %in% (holidays_2013$date - 1L) ~ "day before holiday",
        date %in% (holidays_2013$date + 1L) ~ "day after holiday",
        date%in% holidays_2013$date ~ "holiday",
        .$wday == "Sat" & .$term == "summer" ~ "Sat-summer",
        .$wday == "Sat" & .$term == "fall" ~ "Sat-fall",
        .$wday == "Sat" & .$term == "spring" ~ "Sat-spring",
        TRUE ~ as.character(.$wday)
      )
  )

mod4 <- lm(n ~ wday3, data = daily)

daily %>%
  spread_residuals(resid_sat_terms = mod3, resid_holidays = mod4) %>%
                     mutate(resid_diff = resid_holidays - resid_sat_terms) %>%
                     ggplot(aes(date, resid_diff)) +
                     geom_line(alpha = 0.75)

#### 5.

daily <- mutate(daily, month = factor(lubridate::month(date)))
mod6 <- lm(n ~ wday * month, data = daily)
print(summary(mod6))

#### 6.

mod7 <- lm(n ~ wday + ns(date, 5), data = daily)
mod8 <- lm(n ~ wday * ns(date, 5), data = daily)

daily %>%
  gather_residuals(mod7, mod8) %>%
  ggplot(aes(x = date, y = resid, color = model)) +
geom_line(alpha = 0.75)

#### 7.

flights %>%
  mutate(
    date = make_date(year, month, day),
    wday = wday(date, label = TRUE)
  ) %>%
  ggplot(aes(y = distance, x = wday)) +
  geom_boxplot() +
  labs(x = "Days of Week", y = "Average Distance")

flights %>%
  mutate(
    date = make_date(year, month, day),
    wday = wday(date, label = TRUE)
  ) %>%
  ggplot(aes(y = distance, x = wday)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "Days of Week", y = "Average Distance")

flights %>%
  mutate(
    date = make_date(year, month, day),
    wday = wday(date, label = TRUE)
  ) %>%
  ggplot(aes(y = distance, x = wday)) +
  stat_summary() +
  labs(x = "Day of Week", y = "Average Distance")

flights %>%
  mutate(
    date = make_date(year, month, day),
    wday = wday(date, label = TRUE)
  ) %>%
  ggplot(aes(y = distance, x = wday)) +
  geom_violin() +
  labs(x = "Days of Week", y = "Average Distance")

flights %>%
  mutate(
    date = make_date(year, month, day),
    wday = wday(date, label = T)
  ) %>%
  filter(
    distance < 3000,
    hour >= 5, hour <= 21
  ) %>%
  ggplot(aes(x = hour, color = wday, y = ..density..)) +
  geom_freqpoly(binwidth = 1)

flights %>%
  mutate(
    date = make_date(year, month, day),
    wday = wday(date, label = T)
  ) %>%
  filter(
    distance < 3000,
    hour >= 5, hour <= 21
  ) %>%
  group_by(wday, hour) %>%
  summarize(distance = mean(distance)) %>%
  ggplot(aes(x = hour, color = wday, y = distance)) +
  geom_line()

flights %>%
  mutate(
    date = make_date(year, month, day),
    wday = wday(date, label = T)
  ) %>%
  filter(
    distance < 3000,
    hour >= 5, hour <= 21
  ) %>%
  group_by(wday, hour) %>%
  summarize(distance = sum(distance)) %>%
  group_by(wday) %>%
  mutate(prop_distance = distance / sum(distance)) %>%
  ungroup() %>%
  ggplot(aes(x = hour, color = wday, y = prop_distance)) +
  geom_line()

#### 8.

monday_first <- function(x) {
  fct_relevel(x, levels(x)[-1])
}

daily <- daily %>%
  mutate(wday = wday(date, label = T))
ggplot(daily, aes(monday_first(wday), n)) +
  geom_boxplot() +
  labs(x = "Days of Week", y = "Number of Flights")

## 24.4 - Learning More About Models

