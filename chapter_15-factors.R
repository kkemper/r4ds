library(tidyverse)
library(forcats)

# Chapter 15 - Factors

## 15.2 - Creating Factors

x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar")
month_levels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

y1 <- factor(x1, levels = month_levels)
y1
sort(y1)

y2 <- factor(x2, level = month_levels)
y2
y2 <- parse_factor(x2, levels = month_levels)

factor(x1)

f1 <- factor(x1, levels = unique(x1))
f1

f2 <- x1 %>% factor() %>% fct_inorder()
f2
levels(f2)

## 15.3 - General Social Survey

gss_cat %>% count(race)
ggplot(gss_cat, aes(race)) +
  geom_bar()

ggplot(gss_cat, aes(race)) +
  geom_bar()+
scale_x_discrete(drop = FALSE)


### 15.3.1 - Exercises

#### 1.

ggplot(gss_cat, aes(rincome)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gss_cat %>%
  filter(!rincome %in% c("Not applicable")) %>%
  mutate(rincome = fct_recode(rincome, "Less than $1000" = "Lt $1000")) %>%
  mutate(rincome_na = rincome %in% c("Refused", "Don't know", "No answer")) %>%
  ggplot(aes(x = rincome, fill = rincome_na)) +
  geom_bar() +
  coord_flip() +
  scale_y_continuous("Number of Respondents", labels = scales::comma) +
  scale_x_continuous("Respondent's Income") +
  scale_fill_manual(values = c("FALSE" = "black", "TRUE" = "gray")) +
  theme(legend.position = "None")

#### 2.

gss_cat %>% count(relig)
gss_cat %>% count(partyid)

#### 3.
levels(gss_cat$denom)
gss_cat %>%
  filter(!denom %in% c("No answer", "Other", "Not applicable", "No denomination", "Don't know")) %>%
  count(relig)

gss_cat %>%
count(relig, denom) %>%
  ggplot(aes(x = relig, y = denom, size = n)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90))

##  15.4 - Modifying Factor Order

relig_summary <- gss_cat %>%
  group_by(relig) %>%
  summarize(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(relig_summary, aes(tvhours, relig)) + geom_point()

ggplot(relig_summary, aes(tvhours, fct_reorder(relig, tvhours))) + 
  geom_point()

relig_summary %>%
  mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) +
  geom_point()

rincome_summary <- gss_cat %>%
  group_by(rincome) %>%
  summarize(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )
  
ggplot(rincome_summary, aes(age, fct_reorder(rincome, age))) +
  geom_point()

ggplot(rincome_summary, aes(age, fct_relevel(rincome, "Not applicable"))) +
  geom_point()

by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  count(age, marital) %>%
  group_by(age) %>%
  mutate(prop = n / sum(n))

ggplot(by_age, aes(age, prop, color = marital)) +
  geom_line(na.rm = TRUE)

ggplot(by_age, aes(age, prop, color = fct_reorder2(marital, age, prop))) +
  geom_line() +
  labs(color = "marital")

gss_cat %>%
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital)) + 
  geom_bar()

### 15.4.1 - Exercises

#### 1.

summary(gss_cat)

#### 2.

levels(gss_cat$marital)
levels(gss_cat$race)
levels(gss_cat$rincome)
levels(gss_cat$partyid)
levels(gss_cat$relig)
levels(gss_cat$denom)

## 15.5 Modifying Factor Levels

gss_cat %>% count(partyid)
gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong" = "Strong republican",
                              "Republican, weak" = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak" = "Not str democrat",
                              "Democrat, strong" = "Strong democrat")) %>%
  count(partyid)

gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Ind,near dem", "Independent"),
                                dem = c("Not str democrat", "Strong democrat")
  )) %>%
  count(partyid)

gss_cat %>%
  mutate(relig = fct_lump(relig)) %>%
  count(relig)

gss_cat %>%
  mutate(relig = fct_lump(relig, n = 10)) %>%
  count(relig, sort = TRUE) %>%
  print(n = Inf)

### 15.5.1 - Exercises

#### 1.

simple_parties <- gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Ind,near dem", "Independent"),
                                dem = c("Not str democrat", "Strong democrat")))