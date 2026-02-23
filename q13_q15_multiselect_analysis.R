# 多选题分析：APP吸引点、奖励类型、使用障碍
# Q13: 吸引用户的功能点（多选）
# Q14: 期望的奖励类型（单选）
# Q15: 使用障碍（多选）

library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)

# ============================================================
# 配置：变量定义
# ============================================================

# Q13: 吸引点（多选，0/1编码）
q13_vars <- c(
  "q13_attract_points" = "碳积分兑换商品",
  "q13_attract_credit" = "绿色信用/政策优惠",
  "q13_attract_daily_task" = "每日打卡/任务",
  "q13_attract_ranking" = "排行榜/成就徽章",
  "q13_attract_game_fun" = "抽奖等娱乐玩法",
  "q13_attract_viz_carbon" = "可视化碳足迹"
)

# Q14: 期望奖励（单选）
q14_var <- "q14_desired_reward"

# Q15: 使用障碍（多选，0/1编码）
q15_vars <- c(
  "q15_barrier_trouble" = "觉得操作麻烦",
  "q15_barrier_privacy" = "担心信息不安全",
  "q15_barrier_low_reward" = "积分奖励太少",
  "q15_barrier_unknown" = "没听说过此类APP"
)

# 分组变量
group_vars <- c(
  "gender" = "性别",
  "age" = "年龄组",
  "marital_status" = "婚姻状况",
  "education" = "教育水平",
  "q1_used_app" = "APP使用情况"
)

# ============================================================
# 辅助函数
# ============================================================

# 计算多选题各选项的选择人数和比例
calc_multiselect_freq <- function(data, vars, labels) {
  data %>%
    select(all_of(names(labels))) %>%
    summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "var", values_to = "选择人数") %>%
    mutate(
      选项 = labels[var],
      总人数 = nrow(data),
      比例 = 选择人数 / 总人数
    ) %>%
    arrange(desc(选择人数)) %>%
    select(选项, 选择人数, 总人数, 比例)
}

# 按分组计算多选题各选项的选择比例
calc_multiselect_by_group <- function(data, vars, labels, gvar) {
  data %>%
    filter(!is.na(.data[[gvar]])) %>%
    group_by(group = .data[[gvar]]) %>%
    summarise(
      n = n(),
      across(all_of(names(labels)), ~ sum(. == 1, na.rm = TRUE) / n()),
      .groups = "drop"
    ) %>%
    pivot_longer(-c(group, n), names_to = "var", values_to = "比例") %>%
    mutate(选项 = labels[var])
}

# 对多选题进行卡方检验（各分组 vs 某选项）
test_multiselect_by_group <- function(data, vars, labels, gvar) {
  results <- lapply(names(labels), function(v) {
    d <- data %>%
      filter(!is.na(.data[[gvar]]) & !is.na(.data[[v]])) %>%
      mutate(选择 = as.factor(.data[[v]]), 组别 = as.factor(.data[[gvar]]))

    if (n_distinct(d$组别) < 2 || n_distinct(d$选择) < 2) {
      return(data.frame(var = v, p.value = NA_real_))
    }

    tryCatch({
      test <- chisq.test(table(d$组别, d$选择))
      data.frame(var = v, p.value = test$p.value)
    }, error = function(e) {
      # 尝试Fisher精确检验
      tryCatch({
        test <- fisher.test(table(d$组别, d$选择), simulate.p.value = TRUE)
        data.frame(var = v, p.value = test$p.value)
      }, error = function(e2) {
        data.frame(var = v, p.value = NA_real_)
      })
    })
  })
  bind_rows(results) %>% mutate(选项 = labels[var])
}

# Cochran's Q检验（比较同一组内不同选项的选择率是否有差异）
cochran_q_test <- function(data, vars) {
  # 准备数据矩阵
  mat <- data %>%
    select(all_of(vars)) %>%
    mutate(across(everything(), ~ as.numeric(. == 1))) %>%
    na.omit() %>%
    as.matrix()

  if (nrow(mat) < 10) return(list(statistic = NA, p.value = NA, n = nrow(mat)))

  k <- ncol(mat)
  n <- nrow(mat)

  # Cochran's Q统计量
  row_sums <- rowSums(mat)
  col_sums <- colSums(mat)
  T_total <- sum(mat)

  Q <- (k - 1) * (k * sum(col_sums^2) - T_total^2) / (k * T_total - sum(row_sums^2))
  p_value <- 1 - pchisq(Q, df = k - 1)

  list(statistic = Q, p.value = p_value, n = n, df = k - 1)
}

# ============================================================
# Part 1: Q13 吸引点分析
# ============================================================
cat("Part 1: Q13 吸引用户的功能点分析（多选题）\n")

# 1.1 整体选择频率
cat("\n--- 1.1 各选项选择人数与比例 ---\n")
calc_multiselect_freq(data, q13_vars, q13_vars) %>%
  mutate(比例 = sprintf("%.1f%%", 比例 * 100)) %>%
  kable(format = "simple") %>%
  print()

# 1.2 Cochran's Q检验
cat("\n--- 1.2 Cochran's Q检验（各选项选择率是否有差异）---\n")
q13_cochran <- cochran_q_test(data, names(q13_vars))
cat(sprintf("Q统计量 = %.2f, df = %d, p = %.4f, n = %d\n",
            q13_cochran$statistic, q13_cochran$df, q13_cochran$p.value, q13_cochran$n))
if (!is.na(q13_cochran$p.value) && q13_cochran$p.value < 0.05) {
  cat("结论：各选项的选择率存在显著差异\n")
} else {
  cat("结论：各选项的选择率无显著差异\n")
}

# 1.3 分维度热图
# 计算各维度×各选项的比例和p值
q13_heatmap_data <- lapply(names(group_vars), function(gvar) {
  props <- calc_multiselect_by_group(data, q13_vars, q13_vars, gvar)
  tests <- test_multiselect_by_group(data, q13_vars, q13_vars, gvar)

  props %>%
    left_join(tests %>% select(var, p.value), by = "var") %>%
    mutate(
      维度 = group_vars[gvar], 维度变量 = gvar, 
      group = as.character(group)
    )
}) %>% 
  bind_rows()

# 输出p值表格
q13_heatmap_data %>%
  select(维度, 选项, p.value) %>%
  distinct() %>%
  mutate(
    显著性 = case_when(
      is.na(p.value) ~ "NA",
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    p_label = ifelse(is.na(p.value), "NA", sprintf("%.3f%s", p.value,
      ifelse(显著性 == "ns", "", 显著性)))
  ) %>%
  select(维度, 选项, p_label) %>%
  pivot_wider(names_from = 选项, values_from = p_label) %>%
  kable(format = "simple") %>%
  print()

# 绘制Q13热图（比例差异）
q13_heatmap_data %>%
  mutate(
    维度 = factor(维度, levels = group_vars),
    选项 = factor(选项, levels = q13_vars)
  ) %>%
  ggplot(aes(x = 选项, y = interaction(group, 维度, sep = " - "), fill = 比例)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.0f%%", 比例 * 100)), size = 3) +
  scale_fill_gradient(low = "white", high = "#2E7D32", name = "选择比例") +
  facet_grid(维度 ~ ., scales = "free_y", space = "free_y") +
  labs(title = "Q13 各维度下不同群体选择各吸引点的比例", x = "吸引点", y = "") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text.y = element_text(angle = 0, face = "bold"),
    panel.grid = element_blank()
  ) -> q13_plot

print(q13_plot)
ggsave("data_proc/q13_attract_heatmap.png", q13_plot, width = 12, height = 10, dpi = 300, bg = "white")

# Q13显著性热图
q13_heatmap_data %>%
  select(维度, 选项, p.value) %>%
  distinct() %>%
  mutate(
    维度 = factor(维度, levels = group_vars),
    选项 = factor(选项, levels = q13_vars),
    显著性 = case_when(
      is.na(p.value) ~ "NA",
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    neg_log_p = pmin(-log10(pmax(p.value, 1e-4)), 4)
  ) %>%
  ggplot(aes(x = 选项, y = 维度, fill = c(显著性 != "ns"))) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = 显著性), size = 4) +
  labs(title = "Q13 各维度下吸引点选择的群体差异显著性",
       subtitle = "* p<0.05, ** p<0.01, *** p<0.001",
       x = "吸引点", y = "维度") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid = element_blank()
  ) -> q13_sig_plot

print(q13_sig_plot)
ggsave("data_proc/q13_attract_significance.png", q13_sig_plot, width = 10, height = 5, dpi = 300, bg = "white")


# ============================================================
# Part 2: Q14 期望奖励分析
# ============================================================

cat("\n\n", strrep("=", 70), "\n")
cat("Part 2: Q14 期望的奖励类型分析（单选题）\n")
cat(strrep("=", 70), "\n")

# 2.1 整体分布
cat("\n--- 2.1 各选项选择人数与比例 ---\n")
data %>%
  filter(!is.na(.data[[q14_var]])) %>%
  count(选项 = .data[[q14_var]], name = "选择人数") %>%
  mutate(比例 = sprintf("%.1f%%", 选择人数 / sum(选择人数) * 100)) %>%
  arrange(desc(选择人数)) %>%
  kable(format = "simple") %>%
  print()

# 2.2 分维度卡方检验
cat("\n--- 2.2 分维度差异分析 ---\n")

q14_tests <- lapply(names(group_vars), function(gvar) {
  d <- data %>%
    filter(!is.na(.data[[gvar]]) & !is.na(.data[[q14_var]])) %>%
    mutate(组别 = as.factor(.data[[gvar]]), 选择 = as.factor(.data[[q14_var]]))

  tryCatch({
    test <- chisq.test(table(d$组别, d$选择))
    data.frame(维度 = group_vars[gvar], p.value = test$p.value, 方法 = "卡方检验")
  }, error = function(e) {
    tryCatch({
      test <- fisher.test(table(d$组别, d$选择), simulate.p.value = TRUE)
      data.frame(维度 = group_vars[gvar], p.value = test$p.value, 方法 = "Fisher精确检验")
    }, error = function(e2) {
      data.frame(维度 = group_vars[gvar], p.value = NA_real_, 方法 = "检验失败")
    })
  })
}) %>% bind_rows()

q14_tests %>%
  mutate(
    显著性 = case_when(
      is.na(p.value) ~ "NA",
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  kable(digits = 4, format = "simple") %>%
  print()

# 2.3 分维度可视化
q14_group_data <- lapply(names(group_vars), function(gvar) {
  data %>%
    filter(!is.na(.data[[gvar]]) & !is.na(.data[[q14_var]])) %>%
    count(group = .data[[gvar]], 选项 = .data[[q14_var]]) %>%
    group_by(group) %>%
    mutate(
      比例 = n / sum(n), 维度 = group_vars[gvar], 
      group = as.character(group)
    ) %>%
    ungroup()
}) %>% bind_rows()

q14_group_data %>%
  mutate(维度 = factor(维度, levels = group_vars)) %>%
  ggplot(aes(x = 选项, y = interaction(group, 维度, sep = " - "), fill = 比例)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.0f%%", 比例 * 100)), size = 3) +
  scale_fill_gradient(low = "white", high = "#1976D2", name = "选择比例") +
  facet_grid(维度 ~ ., scales = "free_y", space = "free_y") +
  labs(title = "Q14 各维度下不同群体期望奖励类型的分布", x = "奖励类型", y = "") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text.y = element_text(angle = 0, face = "bold"),
    panel.grid = element_blank()
  ) -> q14_plot

print(q14_plot)
ggsave("data_proc/q14_reward_heatmap.png", q14_plot, width = 12, height = 10, dpi = 300, bg = "white")


# ============================================================
# Part 3: Q15 使用障碍分析
# ============================================================

cat("\n\n", strrep("=", 70), "\n")
cat("Part 3: Q15 使用障碍分析（多选题）\n")
cat(strrep("=", 70), "\n")

# 3.1 整体选择频率
cat("\n--- 3.1 各选项选择人数与比例 ---\n")
calc_multiselect_freq(data, q15_vars, q15_vars) %>%
  mutate(比例 = sprintf("%.1f%%", 比例 * 100)) %>%
  kable(format = "simple") %>%
  print()

# 3.2 Cochran's Q检验
cat("\n--- 3.2 Cochran's Q检验（各选项选择率是否有差异）---\n")
q15_cochran <- cochran_q_test(data, names(q15_vars))
cat(sprintf("Q统计量 = %.2f, df = %d, p = %.4f, n = %d\n",
            q15_cochran$statistic, q15_cochran$df, q15_cochran$p.value, q15_cochran$n))
if (!is.na(q15_cochran$p.value) && q15_cochran$p.value < 0.05) {
  cat("结论：各选项的选择率存在显著差异\n")
} else {
  cat("结论：各选项的选择率无显著差异\n")
}

# 3.3 分维度热图
cat("\n--- 3.3 分维度差异分析 ---\n")

q15_heatmap_data <- lapply(names(group_vars), function(gvar) {
  props <- calc_multiselect_by_group(data, q15_vars, q15_vars, gvar)
  tests <- test_multiselect_by_group(data, q15_vars, q15_vars, gvar)

  props %>%
    left_join(tests %>% select(var, p.value), by = "var") %>%
    mutate(
      维度 = group_vars[gvar], 维度变量 = gvar, 
      group = as.character(group)
    )
}) %>% bind_rows()

# 输出p值表格
q15_heatmap_data %>%
  select(维度, 选项, p.value) %>%
  distinct() %>%
  mutate(
    显著性 = case_when(
      is.na(p.value) ~ "NA",
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    p_label = ifelse(is.na(p.value), "NA", sprintf("%.3f%s", p.value,
      ifelse(显著性 == "ns", "", 显著性)))
  ) %>%
  select(维度, 选项, p_label) %>%
  pivot_wider(names_from = 选项, values_from = p_label) %>%
  kable(format = "simple") %>%
  print()

# 绘制Q15热图（比例差异）
q15_heatmap_data %>%
  mutate(
    维度 = factor(维度, levels = group_vars),
    选项 = factor(选项, levels = q15_vars)
  ) %>%
  ggplot(aes(x = 选项, y = interaction(group, 维度, sep = " - "), fill = 比例)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.0f%%", 比例 * 100)), size = 3) +
  scale_fill_gradient(low = "white", high = "#F57C00", name = "选择比例") +
  facet_grid(维度 ~ ., scales = "free_y", space = "free_y") +
  labs(title = "Q15 各维度下不同群体选择各使用障碍的比例", x = "使用障碍", y = "") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text.y = element_text(angle = 0, face = "bold"),
    panel.grid = element_blank()
  ) -> q15_plot

print(q15_plot)
ggsave("data_proc/q15_barrier_heatmap.png", q15_plot, width = 10, height = 10, dpi = 300, bg = "white")

# Q15显著性热图
q15_heatmap_data %>%
  select(维度, 选项, p.value) %>%
  distinct() %>%
  mutate(
    维度 = factor(维度, levels = group_vars),
    选项 = factor(选项, levels = q15_vars),
    显著性 = case_when(
      is.na(p.value) ~ "NA",
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    neg_log_p = pmin(-log10(pmax(p.value, 1e-4)), 4)
  ) %>%
  ggplot(aes(x = 选项, y = 维度, fill = c(显著性 != "ns"))) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = 显著性), size = 4) +
  labs(title = "Q15 各维度下使用障碍选择的群体差异显著性",
       subtitle = "* p<0.05, ** p<0.01, *** p<0.001",
       x = "使用障碍", y = "维度") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid = element_blank()
  ) -> q15_sig_plot

print(q15_sig_plot)
ggsave("data_proc/q15_barrier_significance.png", q15_sig_plot, width = 9, height = 5, dpi = 300, bg = "white")


# ============================================================
# 汇总：所有显著差异
# ============================================================

cat("\n\n", strrep("=", 70), "\n")
cat("汇总：所有显著的群体差异 (p < 0.05)\n")
cat(strrep("=", 70), "\n\n")

bind_rows(
  q13_heatmap_data %>% select(维度, 选项, p.value) %>% distinct() %>% mutate(问题 = "Q13吸引点"),
  q15_heatmap_data %>% select(维度, 选项, p.value) %>% distinct() %>% mutate(问题 = "Q15障碍"),
  q14_tests %>% mutate(选项 = "全部选项", 问题 = "Q14奖励") %>% select(维度, 选项, p.value, 问题)
) %>%
  filter(!is.na(p.value) & p.value < 0.05) %>%
  mutate(显著性 = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01 ~ "**",
    TRUE ~ "*"
  )) %>%
  arrange(p.value) %>%
  select(问题, 维度, 选项, p值 = p.value, 显著性) %>%
  kable(digits = 4, format = "simple") %>%
  print()

cat("\n分析完成。\n")
