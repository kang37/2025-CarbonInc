# APP感知与使用因素的群体差异分析
# 分析不同人群（性别、年龄、婚姻状况、教育水平、APP使用情况）在各题上的差异
# 包括：Q4感知易用性/有用性 + Q20其他影响因素

library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)

# ============================================================
# 配置：分析变量和分组变量
# ============================================================

# Q4: 感知易用性和感知有用性
q4_vars <- c(
  "q4_ui_simple" = "1.界面简洁明了",
  "q4_integrate_platform" = "2.与常用平台打通",
  "q4_auto_record" = "3.自动记录数据",
  "q4_clear_guidance" = "4.清晰引导参与",
  "q4_raise_awareness" = "5.提高低碳意识",
  "q4_info_feedback_useful" = "6.信息反馈实用",
  "q4_quicker_green_choice" = "7.更快做出环保选择",
  "q4_know_carbon_credit" = "8.了解碳普惠机制"
)

# Q20: 其他影响APP使用的因素
q20_vars <- c(
  "q20_ranking_motivation" = "11.排行成就激励",
  "q20_image_inspire" = "12.图像动画更有趣",
  "q20_cumulative_inspire" = "13.累计成效激励", 
  "q20_celeb_endorsement" = "9.明星代言更关注",
  "q20_video_intro" = "10.视频介绍更吸引"
)

# 合并所有分析变量
all_vars <- c(q4_vars, q20_vars)

group_vars <- c(
  "gender" = "性别",
  "age" = "年龄组",
  "marital_status" = "婚姻状况",
  "education" = "教育水平", 
  "q1_used_app" = "APP使用情况"
)

# 表格1: 整体描述性统计
data %>%
  select(all_of(names(all_vars))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "score") %>%
  mutate(score = as.numeric(score)) %>%
  filter(!is.na(score)) %>%
  group_by(var) %>%
  summarise(
    N = n(),
    平均分 = mean(score),
    SD = sd(score),
    中位数 = median(score),
    Q1 = quantile(score, 0.25),
    Q3 = quantile(score, 0.75),
    .groups = "drop"
  ) %>%
  mutate(题目 = all_vars[var]) %>%
  arrange(match(var, names(all_vars))) %>%
  select(题目, N, 平均分, SD, 中位数, Q1, Q3) %>%
  kable(digits = 2, format = "simple") %>%
  print()

# 表格2: 分群体描述性统计
for (i in seq_along(group_vars)) {
  gvar <- names(group_vars)[i]
  glabel <- group_vars[i]

  cat("\n--- ", glabel, " ---\n", sep = "")

  data %>%
    select(group = all_of(gvar), all_of(names(all_vars))) %>%
    filter(!is.na(group)) %>%
    pivot_longer(-group, names_to = "var", values_to = "score") %>%
    mutate(score = as.numeric(score)) %>%
    filter(!is.na(score)) %>%
    group_by(群体 = group, var) %>%
    summarise(
      N = n(),
      平均分 = mean(score),
      SD = sd(score),
      Q1 = quantile(score, 0.25),
      中位数 = median(score),
      Q3 = quantile(score, 0.75),
      .groups = "drop"
    ) %>%
    mutate(题目 = all_vars[var]) %>%
    arrange(match(var, names(all_vars)), 群体) %>%
    select(题目, 群体, N, 平均分, SD, Q1, 中位数, Q3) %>%
    kable(digits = 2, format = "simple") %>%
    print()
}

# 热图: 群体差异显著性检验
# 执行检验并生成热图数据
expand.grid(gvar = names(group_vars), qvar = names(all_vars), stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(
    p.value = {
      d <- data %>%
        select(g = all_of(gvar), s = all_of(qvar)) %>%
        filter(!is.na(g) & !is.na(s)) %>%
        mutate(s = as.numeric(s))
      n_grp <- n_distinct(d$g)
      if (n_grp < 2 || nrow(d) < 10) NA_real_
      else if (n_grp == 2) suppressWarnings(wilcox.test(s ~ g, data = d)$p.value)
      else suppressWarnings(kruskal.test(s ~ g, data = d)$p.value)
    }
  ) %>%
  ungroup() %>%
  mutate(
    维度 = factor(group_vars[gvar], levels = group_vars),
    题目 = factor(all_vars[qvar], levels = all_vars),
    显著性 = case_when(
      is.na(p.value) ~ "NA",
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    neg_log_p = pmin(-log10(pmax(p.value, 1e-4)), 4)
  ) -> heatmap_data

# 输出p值表格
cat("--- p值表格 ---\n")
heatmap_data %>%
  mutate(p_label = ifelse(is.na(p.value), "NA", sprintf("%.3f%s", p.value,
    ifelse(显著性 == "ns", "", 显著性)))) %>%
  select(维度, 题目, p_label) %>%
  pivot_wider(names_from = 题目, values_from = p_label) %>% 
  kable(format = "simple") %>% 
  print()

# 绘制热图
ggplot(heatmap_data, aes(x = 题目, y = 维度, fill = c(!显著性 == "ns"))) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = 显著性), color = "black", size = 4) +
  labs(
    title = "各人口学维度下APP感知与使用因素的群体差异显著性",
    subtitle = "* p<0.05, ** p<0.01, *** p<0.001, ns=不显著 | Q4:感知易用/有用性, Q20:其他影响因素",
    x = "问题", y = "人口学维度"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )
