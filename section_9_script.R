# 図9-5の再現
library(tidyverse)
library(readr)
library(haven)
library(estimatr)

munic <- haven::read_dta("munic.dta")
glimpse(munic)

munic <- munic |> 
  mutate(treat = voters96 >= 40500) |> 
  mutate(bin_voters96 = floor(voters96 / 4000) * 4000)

munic <- munic |> 
  group_by(bin_voters96) |> 
  mutate(
    mean_util94 = mean(r_util94, na.rm = TRUE),
    mean_util98 = mean(r_util98, na.rm = TRUE),
    mean_util02 = mean(r_util02, na.rm = TRUE)
  )

munic <- munic |> 
  pivot_longer(cols = c("mean_util94", "mean_util98", "mean_util02")) |> 
  mutate(name = factor(name, levels = c("mean_util94",
                                        "mean_util98", "mean_util02")))

labels <- tibble(
  "mean_util94" = "有効票/投票数-1994（紙のみ）",
  "mean_util98" = "有効票/投票数-1998（4万500人以上のみ電子投票）",
  "mean_util02" = "有効票/投票数-2002（電子投票のみ）"
)

munic |> 
  filter(4500 <= voters96 & voters96 <= 100000 ) |> 
  ggplot() +
  geom_point(aes(x = bin_voters96, y = value,
                 color = name, shape = name)) +
  scale_color_manual(name = element_blank(),
                     labels = labels,
                     values = c("1", "2", "3")) +
  scale_shape(name = element_blank(),
              labels = labels) +
  geom_smooth(data = munic |> filter(treat & name == "mean_util94"),
              aes(x = bin_voters96, y = value),
              method = lm, formula = "y ~ x + I(x^2)", 
              se = FALSE, color = 1) +
  geom_smooth(data = munic |> filter(!treat & name == "mean_util94"),
              aes(x = bin_voters96, y = value),
              method = lm, formula = "y ~ x + I(x^2)", 
              se = FALSE, color = 1) +
  geom_smooth(data = munic |> filter(treat & name == "mean_util98"),
              aes(x = bin_voters96, y = value),
              method = lm, formula = "y ~ x + I(x^2)", 
              se = FALSE, color = 2) +
  geom_smooth(data = munic |> filter(!treat & name == "mean_util98"),
              aes(x = bin_voters96, y = value),
              method = lm, formula = "y ~ x + I(x^2)", 
              se = FALSE, color = 2) +
  geom_smooth(data = munic |> filter(treat & name == "mean_util02"),
              aes(x = bin_voters96, y = value),
              method = lm, formula = "y ~ x + I(x^2)", 
              se = FALSE, color = 3) +
  geom_smooth(data = munic |> filter(!treat & name == "mean_util02"),
              aes(x = bin_voters96, y = value),
              method = lm, formula = "y ~ x + I(x^2)", 
              se = FALSE, color = 3) +
  geom_vline(xintercept = 40000, lty = "dashed") +
  xlim(4500, 100000) +
  labs(x = "有権者登録数-1996",
       y = element_blank(),
       caption = "(出所) Fujiwara (2015), Figure 2, p.435.") +
  theme_gray() +
  theme(legend.position = "bottom", legend.direction = "vertical")



