# Chapter 23 - Model Basics

library(tidyverse)
library(modelr)
options(na.action = na.warn)

## 23.2 - A Simple Model

ggplot(sim1, aes(x, y)) +
  geom_point()

models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) +
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point()

model1 <- function(a, data) {
  a[1]+ data$x * a[2]
}

model1(c(7, 1.5), sim1)

measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}

measure_distance(c(7, 1.5), sim1)

sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>%
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(aes(intercept = a1, slope = a2, color = -dist), data = filter(models, rank(dist) <= 10))

ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, color = "red") +
  geom_point(aes(color = -dist))

grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>%
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>%
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, color = "red") +
  geom_point(aes(color = -dist))

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(
    aes(intercept = a1, slope = a2, color = -dist),
    data = filter(grid, rank(dist) <= 10)
  )

best <- optim(c(0,0), measure_distance, data = sim1)
best$par

ggplot(sim1, aes(x, y)) +
  geom_point(size = 2, color = "grey30") +
  geom_abline(intercept = best$par[1], slope = best$par[2])

sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

### 23.2.1 - Exercises

#### 1.

sim1a <- tibble (
  x = rep(1:10, each = 3),
  y = x *1.5 +6 + rt(length(x), df = 2)
)

ggplot(sim1a, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#### 1.b

simt <- function(i) {
  tibble(
    x = rep(1:10, each = 3),
    y = x * 1.5 + 6 + rt(length(x), df = 2),
    .id = i
  )
}

sims <- map_df(1:12, simt)

ggplot(sims, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~.id, ncol = 4)

#### 1.c

sim_norm <- function(i) {
  tibble(
    x = rep(1:10, each = 3),
    y = x * 1.5 + 6 + rnorm(length(x)),
    .id = i
  )
}

simdf_norm <- map_df(1:12, sim_norm)

ggplot(simdf_norm, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~.id, ncol = 4)

#### 1.d

tibble(
  x = seq(-5, 5, length.out = 100),
  normal = dnorm(x),
  student_t = dt(x, df = 2)
) %>%
  gather(distribution, density, -x) %>%
  ggplot(aes(x = x, y = density, color = distribution)) +
  geom_line()

pnorm(2, lower.tail = FALSE)
pt(2, df = 2, lower.tail = FALSE)

#### 2.

measure_distance <- function(mod, data) {
  diff <- data$y - make_prediction(mod, data)
  mean(abs(diff))
}

make_prediction <-function(mod, data) {
  mod[1] + mod[2] * data$x
}

best <- optim(c(0,0), measure_distance, data = sim1a)
best$par

measure_distance_ls <- function(mod, data) {
  diff <- data$y - (mod[1] + mod[2] * data$x)
  sqrt(mean(diff^2))
}

best <- optim(c(0,0), measure_distance_ls, data = sim1a) 
best$par

#### 3.

model3 <- function(a, data) {
  a[1] + data$x * a[2] +a[3]
}

measure_distance_3 <- function(a, data) {
  diff <- data$y - model3(a, data)
  sqrt(mean(diff^2))
}

best3a <- optim(c(0, 0, 0), measure_distance_3, data = sim1)
best3a$par

best3b <- optim(c(0, 0, 1), measure_distance_3, data = sim1)
best3b$par

best3c <- optim(c(0, 0, 5), measure_distance_3, data = sim1)
best3c$par

## 23.3 - Visualizing Models

# 23.3.1 - Predictions
grid <- sim1 %>%
  data_grid(x)
grid

grid <- grid %>%
  add_predictions(sim1_mod)
grid

sim1_mod

ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, color = "red", size = 1)

### 23.3.2 -Residuals

sim1 <- sim1 %>%
  add_residuals((sim1_mod))
sim1

ggplot(sim1, aes(resid)) +
  geom_freqpoly(binwidth = 0.5)

ggplot(sim1, aes(x, resid)) +
  geom_ref_line(h = 0) +
  geom_point()

### 23.3.3 - Exercises

sim1_loess <- loess(y ~x, data = sim1)
sim1_lm <- lm(y~x, data = sim1)

grid_loess <- sim1 %>%
  add_predictions(sim1_loess)

sim1 <- sim1 %>%
  add_residuals(sim1_lm) %>%
  add_predictions(sim1_lm) %>%
  add_residuals(sim1_loess, var = "resid_loess") %>%
  add_predictions(sim1_loess, var = "pred_loess")

sim1

plot_sim1_loess <- 
  ggplot(sim1, aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(x = x, y = pred), data = grid_loess, color = "red")
plot_sim1_loess

plot_sim1_loess +
  geom_smooth(method = "loess", color = "blue", se = FALSE, alpha = 0.20)

ggplot(sim1, aes(x = x)) +
  geom_ref_line(h = 0) +
  geom_point(aes(y = resid)) +
  geom_point(aes(y = resid_loess), color = "red")

#### 4.

sim1_mod <- lm(y ~ x, data = sim1)
grid <- sim1 %>%
  data_grid(x)

grid %>%
  add_predictions(sim1_mod, var = "pred_lm") %>%
  add_predictions(sim1_loess, var = "pre_loess")
grid %>%
  gather_predictions(sim1_mod, sim1_loess)
grid %>%
  spread_predictions(sim1_mod, sim1_loess)
grid %>%
  gather_predictions(sim1_mod, sim1_loess) %>%
  spread(model, pred)

#### 3.

sim1_mod <- lm(y ~ x, data = sim1)
sim1 <- sim1 %>%
  add_residuals(sim1_mod)

ggplot(sim1, aes(x = abs(resid))) +
  geom_freqpoly(binwidth = 0.5)

## 23.4 - Formulas and Model Families

df <- tribble(
  ~y, ~x1, ~x2,
  4, 2, 5,
  5, 1, 6
)

model_matrix(df, y ~ x1)
model_matrix(df, y ~x1 - 1)
model_matrix(df, y ~ x1 + x2)
  
### 23.4.1 - Categorical Variables

df <- tribble(
  ~sex, ~response,
  "male", 1,
  "female", 2, 
  "male", 1
)

model_matrix(df, response ~ sex)
ggplot(sim2) +
  geom_point(aes(x, y))

mod2 <- lm(y ~ x, data = sim2)
grid <- sim2 %>%
  data_grid(x) %>%
  add_predictions(mod2)
grid

ggplot(sim2, aes(x)) +
  geom_point(aes(y = y)) +
  geom_point(data = grid, aes(y = pred), color = "red", size = 4)

tibble(x = "e") %>%
  add_predictions(mod2)

### 23.4.2 - Interactions (Continuous and Categorical)

ggplot(sim3, aes(x1, y)) +
  geom_point(aes(color = x2))

mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)
mod1
mod2

grid <- sim3 %>%
  data_grid(x1, x2) %>%
  gather_predictions(mod1, mod2)
grid

ggplot(sim3, aes(x1, y, color = x2)) +
  geom_point() +
  geom_line(data = grid, aes(y = pred)) +
  facet_wrap(~ model)

sim3 <- sim3 %>%
  gather_residuals(mod1, mod2)

ggplot(sim3, aes(x1, resid, color = x2)) +
  geom_point() +
  facet_grid(model ~ x2)

### 23.4.3 - Interactions (Two Continuous)

mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)
grid <- sim4 %>%
  data_grid(
    x1 = seq_range(x1, 5),
    x2 = seq_range(x2, 5)
  ) %>%
  gather_predictions(mod1, mod2)
grid

seq_range(c(0.0123, 0.923423), n = 5)
seq_range(c(0.0123, 0.923423), n = 5, pretty = TRUE)

x1 <- rcauchy(100)
seq_range(x1, n = 5)
seq_range(x1, n = 5, trim = 0.10)
seq_range(x1, n = 5, trim = 0.25)
seq_range(x1, n = 5, trim = 0.5)

x2 <- c(0, 1)
seq_range(x2, n = 5)
seq_range(x2, n = 5, expand = 0.10)
seq_range(x2, n = 5, expand = 0.25)
seq_range(x2, n = 5, expand = 0.50)

ggplot(grid, aes(x1, x2)) +
  geom_tile(aes(fill = pred)) +
  facet_wrap(~ model)

ggplot(grid, aes(x1, pred, color = x2, group = x2)) +
  geom_line() +
  facet_wrap(~ model)
ggplot(grid, aes(x2, pred, color = x1, group = x1)) +
  geom_line() +
  facet_wrap(~ model)

 ### 23.4.3 - Transformations

df <- tribble(
  ~y, ~x,
  1, 1,
  2, 2,
  3, 3
)
model_matrix(df, y ~x^2 + x)
model_matrix(df, y ~ I(x^2) + x)

model_matrix(df, y ~ poly(x, 2))

library(splines)
model_matrix(df, y ~ ns(x, 2))

sim5 <- tibble(
  x = seq(0, 3.5 * pi, length = 50),
  y = 4 * sin(x) + rnorm(length(x))
)
ggplot(sim5, aes(x, y)) +
  geom_point()

mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)

grid <- sim5 %>%
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>%
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")

ggplot(sim5, aes(x, y)) +
  geom_point() +
  geom_line(data = grid, color = "red") +
  facet_wrap(~ model)

### 23.4.5 - Exercises

#### 1.

mod2a <- lm(y ~ x - 1, data = sim2)
mod2 <- lm(y ~ x, data = sim2)
grid <- sim2 %>%
  data_grid(x) %>%
  spread_predictions(mod2, mod2a)
grid

#### 2.

x3 <- model_matrix(y ~ x1 * x2, data = sim3)
x3

all(x3[["x1:x2b"]] == (x3[["x1"]] * x3[["x2b"]]))
all(x3[["x1:x2c"]] == (x3[["x1"]] * x3[["x2c"]]))
all(x3[["x1:x2d"]] == (x3[["x1"]] * x3[["x2d"]]))


x4 <- model_matrix(y ~ x1 * x2, data = sim4)
x4
all(x4[["x1"]] * x4[["x2"]] == x4[["x1:x2"]])

#### 3.

mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)

levels(sim3$x2)

model_matrix_mod1 <- function(.data) {
  mutate(.data,
         x2b = as.numeric(x2 == "b"),
         x2c = as.numeric(x2 == "c"),
         x2d = as.numeric(x2 == "d"),
         `(Intercept)` = 1
  ) %>%
    select(`(Intercept)`, x1, x2b, x2c, x2d)
}
model_matrix_mod1(sim3)

model_matrix_mod1b <- function(.data) {
  lvls <- levels(.data$x2)
  lvls <- lvls[2:length(lvls)]
  for(lvl in lvls) {
    varname <- str_c("x2", lvl)
    .data[[varname]] <- as.numeric(.data$x2 == lvl)
  }
  x2_variables <- str_c("x2", lvls)
  .data[["(Intercept)"]] <- 1
  select(.data, `(Intercept)`, x1, one_of(x2_variables))
}
model_matrix_mod1b(sim3)

model_matrix_mod2 <- function(.data) {
  mutate(.data,
         `(Intercept)` = 1,
         x2b = as.numeric(x2 == "b"),
         x2c = as.numeric(x2 == "c"),
         x2d = as.numeric(x2 == "d"),
         `x1:x2b` = x1 * x2b,
         `x1:x2c` = x1 * x2c,
         `x1:x2d` = x1 * x2d
         ) %>%
    select(`(Intercept)`, x1, x2b, x2c, x2d, `x1:x2b`, `x1:x2c`, `x1:x2d`)
}

model_matrix_mod2(sim3)

model_matrix_mod2b <- function(.data) {
  out <- model_matrix_mod1b(.data)
  x2cols <- str_subset(colnames(out), "^x2")
  for (varname in x2cols) {
    newvar <- str_c("x1:", varname)
    out[[newvar]] <- out$x1 * out[[varname]]
  }
  out
}
model_matrix_mod2b(sim3)

#### 4.

mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)
sim4_mods <- gather_residuals(sim4, mod1, mod2)
ggplot(sim4_mods, aes(x = resid, color = model)) +
  geom_freqpoly(binwidth = 0.5) +
  geom_rug()
ggplot(sim4_mods, aes(x = abs(resid), color = model)) +
  geom_freqpoly(binwidth = 0.5) +
  geom_rug()

sim4_mods %>%
  group_by(model) %>%
  summarize(resid = sd(resid))

##23.5 - Missing Values

df <- tribble(
  ~x, ~y,
  1, 2.2,
  2, NA,
  3, 3.5,
  4, 8.3,
  NA, 10
)
mod <- lm(y ~ x, data = df)
mod <- lm(y ~ x, data = df, na.action = na.exclude)
nobs(mod)