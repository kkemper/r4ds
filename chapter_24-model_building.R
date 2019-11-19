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
