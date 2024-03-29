# Chapter 28 - Graphics for Communication

library(tidyverse)
library(lubridate)

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

best_in_class <- mpg%>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) == 1)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_label(aes(label = model), data = best_in_class, nudge_y = 2, alpha = 0.5)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_point(size = 3, shape = 1, data = best_in_class) +
  ggrepel::geom_label_repel(aes(label = model), data = best_in_class)

class_avg <- mpg %>%
  group_by(class) %>%
  summarize(
    displ = median(displ),
    hwy = median(hwy)
  )

ggplot(mpg, aes(displ, hwy, color = class)) +
  ggrepel::geom_label_repel(aes(label = class),
                            data = class_avg,
                            size = 6,
                            label.size = 0,
                            segment.color = NA
                            ) +
  geom_point() +
  theme(legend.position = "none")


label <- mpg %>%
  summarize(
    displ = max(displ),
    hwy = max(hwy),
    label = "Increasing engine size is \nrelated to decreasing fuel economy."
  )

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label), data = label, vjust = "top", hjust = "right")

label <- tibble(
  displ = Inf,
  hwy = Inf,
  label = "Increasing engine size is \nrelated to decreasing fuel economy."
)

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label), data = label, vjust = "top", hjust = "right")

### 28.3.1 - Exercises

## 28.4 - Scales

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_color_discrete()

### 28.4.1 Axis Ticks and Legend Keys

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_y_continuous(breaks = seq(15, 40, by = 5))


ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_y_continuous(labels =  NULL) +
  scale_x_continuous(labels = NULL)

presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_x_date(NULL, breaks = presidential$start, date_labels = "'%y")

### 28.4.2 - Legend Layout

base <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class))

base + theme(legend.position = "left")
base + theme(legend.position = "top")
base + theme(legend.position = "bottom")
base + theme(legend.position = "right")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = F) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1, override.aes = list(size = 4)))

###28.4.3 - Replacing a Scale

ggplot(diamonds, aes(carat, price)) +
  geom_bin2d()

ggplot(diamonds, aes(log10(carat), log10(price))) +
  geom_bin2d()

ggplot(diamonds, aes(carat, price)) +
  geom_bin2d() +
  scale_x_log10() +
  scale_y_log10()

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv))


ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv, shape = drv)) +
  scale_color_brewer(palette = "PuBu")

presidential %>%
  mutate(id = 33 + row_number()) %>%
  ggplot(aes(start, id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_color_manual(values = c(Republican = "red", Democratic = "blue"))

df <- tibble(
  x = rnorm(10000),
  y = rnorm(10000)
)
ggplot(df, aes(x, y)) +
  geom_hex() +
  coord_fixed()

ggplot(df, aes(x, y)) +
  geom_hex() +
  viridis::scale_fill_viridis() +
  coord_fixed()

### 28.4.4 - Exercises

#### 1.

ggplot(df, aes(x, y)) +
  
  geom_hex() +
  scale_fill_gradient(low = "blue", high = "purple") +
  coord_fixed()

#### 3.

fouryears <- lubridate::make_date(seq(year(min(presidential$start)),
                                       year(max(presidential$end)),
                                      by = 4
                                      ), 1, 1)
presidential %>%
  mutate(id = 33 + row_number(),
         name_id = fct_inorder(str_c(name, " (", id, ")"))
         ) %>%
  ggplot(aes(start, name_id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = name_id)) +
  scale_color_manual("Party", values = c(Republican = "red", Democratic = "blue")) +
  scale_x_date(NULL, 
               breaks = presidential$start, date_labels = "'%y",
               minor_breaks = fouryears
               ) +
    ggtitle("Terms of US Presidents",
            subtitle = "Roosevelt (34th) to Obama (44th)"
            ) +
    theme(
      panel.grid.minor = element_blank(),
      axis.ticks.y = element_blank()
    )

#### 4.

ggplot(diamonds, aes(carat, price)) +
  geom_point(aes(color = cut), alpha = 1 / 20) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1, override.aes = list(alpha = 1)))

## 28.5 - Zooming

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  coord_cartesian(xlim = c(5, 7), ylim = c(10, 30))

mpg %>%
  filter(displ >= 5, displ <= 7, hwy <= 30) %>%
  ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth()

suv <- mpg %>% filter(class == "suv")
compact <- mpg %>% filter(class == "compact") 

ggplot(suv, aes(displ, hwy, color = drv)) +
  geom_point()

ggplot(compact, aes(displ, hwy, color = drv)) +
  geom_point()

x_scale <- scale_x_continuous(limits = range(mpg$displ))
y_scale <- scale_y_continuous(limits = range(mpg$hwy))
col_scale <- scale_color_discrete(limits = unique(mpg$drv))

ggplot(suv, aes(displ, hwy, color = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale

ggplot(compact, aes(displ, hwy, color = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale

## 28.6 - Themes

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = F) +
  theme_bw()
