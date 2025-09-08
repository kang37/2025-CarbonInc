library(stringr)
sh_pop_age <- read.csv("data_raw/shanghai_pop_by_age.csv") %>% 
  tibble() %>% 
  rename_with(~ c("age", "resident", "huji", "no_huji")) %>% 
  slice(-1) %>% 
  select(-no_huji) %>% 
  mutate(
    across(c(age, resident, huji), ~ str_trim(.x)), 
    across(c(resident, huji), ~ as.numeric(.x)), 
    # 合并年龄组。
    age_grp = case_when(
      age %in% c("0～4岁", "5～9岁", "10～14岁", "15～19岁") ~ "<18",
      age == "20～24岁" ~ "18-25",
      age == "25～29岁" ~ "26-30",
      age %in% c("30～34岁", "35～39岁") ~ "31-40",
      age %in% c("40～44岁", "45～49岁") ~ "41-50",
      TRUE ~ ">=51",
    )
  ) %>% 
  group_by(age_grp) %>% 
  summarise(resident = sum(resident), huji = sum(huji)) %>% 
  ungroup() %>% 
  # 计算比例。
  mutate(
    resident_prop = resident / sum(resident) * 100, 
    huji_prop = huji / sum(huji) * 100
  )

# 问卷样本数分年龄。
pop_ques_age <- data.frame(
  age_grp = c("<18", "18-25", "26-30", "31-40", "41-50", ">=51"), 
  pop = c(7, 71, 151, 306, 72, 24)
) %>% 
  mutate(prop = pop / sum(pop) * 100)

sh_pop_age %>% 
  select(age_grp, ends_with("prop")) %>% 
  left_join(
    pop_ques_age %>% select(age_grp, ques_prop = prop), by = "age_grp"
  ) %>% 
  pivot_longer(
    cols = ends_with("prop"), names_to = "data_src", values_to = "prop"
  ) %>% 
  mutate(
    age_grp = factor(age_grp, levels = c(
      "<18", "18-25", "26-30", "31-40", "41-50", ">=51"
    )), 
    data_src = factor(data_src, levels = c(
      "resident_prop", "huji_prop", "ques_prop"
    ))
  ) %>% 
  ggplot() + 
  geom_col(aes(age_grp, prop, fill = data_src), position = "dodge") + 
  scale_fill_manual(
    breaks = c("resident_prop", "huji_prop", "ques_prop"), 
    values = c("lightgreen", "lightblue", "pink")
  ) +
  theme_bw()


