# 図9-5の再現
library(tidyverse)
library(readr)
library(haven)
library(estimatr)

munic <- haven::read_dta("munic.dta")
glimpse(munic)

munic <- munic |> 
  mutate(treat = voters96 > 40500) |> 
  mutate(bin_voters96 = floor(munic$voters96 / 4000) * 4000)

munic <- munic |> 
  filter(voters96 <= 100000) |> 
  group_by(bin_voters96) |> 
  mutate(
    r_util94 = mean(r_util94, na.rm = TRUE),
    r_util98 = mean(r_util98, na.rm = TRUE),
    r_util02 = mean(r_util02, na.rm = TRUE)
  )

munic <- munic |> 
  pivot_longer(cols = c("r_util94", "r_util98", "r_util02")) |> 
  mutate(name = factor(name, 
                       levels = c("r_util94", "r_util98", "r_util02")))

labels <- c(r_util94 = "有効票/投票数 - 1994選挙 (紙のみ)",
            r_util98 = "有効票/投票数 - 1998 選挙 (4万人以上のみ電子投票)",
            r_util02 = "有効票/投票数 - 2002 選挙 (電子投票のみ)")

munic |> 
  ggplot() + 
  geom_point(aes(x = bin_voters96, y = value,
             color = name, shape = name)) +
  scale_color_manual(name = element_blank(),
                     labels = labels,
                     values = c("1", "2", "3")) +
  scale_shape(name = element_blank(), labels = labels) +
  scale_fill_manual(values = element_blank()) +
  geom_smooth(data = munic |> filter(treat & name == "r_util94"),
              aes(x = bin_voters96, y = value), method = lm,
              formula = "y ~ x + I(x^2)", se = FALSE, color = 1) +
  geom_smooth(data = munic |> filter(!treat & name == "r_util94"),
              aes(x = bin_voters96, y = value), method = lm,
              formula = "y ~ x + I(x^2)", se = FALSE, color = 1) +
  geom_smooth(data = munic |> filter(treat & name == "r_util98"),
              aes(x = bin_voters96, y = value), method = lm,
              formula = "y ~ x + I(x^2)", se = FALSE, color = 2) +
  geom_smooth(data = munic |> filter(!treat & name == "r_util98"),
              aes(x = bin_voters96, y = value), method = lm,
              formula = "y ~ x + I(x^2)", se = FALSE, color = 2) +
  geom_smooth(data = munic |> filter(treat & name == "r_util02"),
              aes(x = bin_voters96, y = value), method = lm,
              formula = "y ~ x + I(x^2)", se = FALSE, color = 3) +
  geom_smooth(data = munic |> filter(!treat & name == "r_util02"),
              aes(x = bin_voters96, y = value), method = lm,
              formula = "y ~ x + I(x^2)", se = FALSE, color = 3) +
  geom_vline(xintercept = 40500, lty = "dashed") +
  labs(x = "登録有権者数 - 1996",
       y = element_blank(),
       caption = "(出所) Fujiwara (2015), Figure 2, p.435.") +
  theme(legend.position = "bottom", legend.direction = "vertical") +
  xlim(4500, 100000)

  