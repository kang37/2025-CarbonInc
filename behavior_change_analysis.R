# APP使用前后行为变化分析
# 包括整体分析和分群体分析（性别、年龄、婚姻、教育程度）

# ============================================================
# 配置：行为变量映射和分组变量
# ============================================================

behavior_map <- list(
  "公共交通" = c("q16_pre_public_trans", "q18_change_public_trans"),
  "骑行或步行" = c("q16_pre_bike_walk", "q18_change_bike_walk"),
  "关闭电源" = c("q16_pre_turn_off_power", "q18_change_turn_off_power"),
  "垃圾分类" = c("q16_pre_garbage_sort", "q18_change_garbage_sort"),
  "使用环保袋" = c("q16_pre_reusable_bag", "q18_change_reusable_bag"),
  "选择节能电器" = c("q16_pre_choose_energy_eff", "q18_change_choose_energy_eff")
)

group_vars <- c(
  "gender" = "性别",
  "age" = "年龄组",
  "marital_status" = "婚姻状况",
  "education" = "教育水平"
)

# ============================================================
# Part 1: 整体描述性统计（使用APP前后）
# ============================================================

cat("\n", strrep("=", 70), "\n")
cat("Part 1: APP使用前后各行为描述性统计（仅APP用户）\n")
cat(strrep("=", 70), "\n")

# 准备长格式数据
app_users <- data %>% filter(q1_used_app == 1)

behavior_stats <- lapply(names(behavior_map), function(b) {
  cols <- behavior_map[[b]]
  app_users %>%
    select(Pre = all_of(cols[1]), Post = all_of(cols[2])) %>%
    mutate(across(everything(), as.numeric)) %>%
    pivot_longer(everything(), names_to = "时期", values_to = "得分") %>%
    filter(!is.na(得分)) %>%
    group_by(时期) %>%
    summarise(
      N = n(),
      平均分 = mean(得分),
      SD = sd(得分),
      Q1 = quantile(得分, 0.25),
      中位数 = median(得分),
      Q3 = quantile(得分, 0.75),
      .groups = "drop"
    ) %>%
    mutate(
      行为 = b,
      时期 = ifelse(时期 == "Pre", "使用前", "使用后"), 
      时期 = factor(时期, levels = c("使用前", "使用后"))
    )
}) %>% 
  bind_rows() %>% 
  arrange(行为, 时期)

cat("\n--- 1.1 各行为使用前后得分统计 ---\n")
behavior_stats %>%
  select(行为, 时期, N, 平均分, SD, Q1, 中位数, Q3) %>%
  kable(digits = 2, format = "simple") 

# ============================================================
# Part 2: 整体行为变化统计检验（Wilcoxon配对检验）
# ============================================================

cat("\n\n", strrep("=", 70), "\n")
cat("Part 2: APP使用前后行为变化统计检验（Wilcoxon配对符号秩检验）\n")
cat(strrep("=", 70), "\n")

overall_tests <- lapply(names(behavior_map), function(b) {
  cols <- behavior_map[[b]]
  d <- app_users %>%
    select(Pre = all_of(cols[1]), Post = all_of(cols[2])) %>%
    mutate(across(everything(), as.numeric)) %>%
    filter(!is.na(Pre) & !is.na(Post)) %>%
    mutate(Diff = Post - Pre)

  test <- wilcox.test(d$Post, d$Pre, paired = TRUE, alternative = "greater", exact = FALSE)

  data.frame(
    行为 = b,
    N = nrow(d),
    使用前均值 = round(mean(d$Pre), 2),
    使用后均值 = round(mean(d$Post), 2),
    均值差异 = round(mean(d$Diff), 3),
    中位数差异 = median(d$Diff),
    Cohen_d = round(mean(d$Diff) / sd(d$Diff), 3),
    W统计量 = round(test$statistic, 1),
    P值 = test$p.value,
    显著性 = case_when(
      test$p.value < 0.001 ~ "***",
      test$p.value < 0.01 ~ "**",
      test$p.value < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  )
}) %>% bind_rows()

cat("\n--- 2.1 整体行为变化检验结果 ---\n")
overall_tests %>%
  mutate(P值 = format.pval(P值, digits = 3)) %>% 
  kable(format = "simple")

# ============================================================
# Part 3: 分群体行为变化分析
# ============================================================

cat("\n\n", strrep("=", 70), "\n")
cat("Part 3: 分群体行为变化分析\n")
cat(strrep("=", 70), "\n")

# 分析函数：按群体计算描述性统计和检验
analyze_by_group <- function(data, behavior_name, pre_col, post_col, group_var, group_label) {
  d <- data %>%
    filter(q1_used_app == 1 & !is.na(.data[[group_var]])) %>%
    select(group = all_of(group_var), Pre = all_of(pre_col), Post = all_of(post_col)) %>%
    mutate(across(c(Pre, Post), as.numeric)) %>%
    filter(!is.na(Pre) & !is.na(Post)) %>%
    mutate(Diff = Post - Pre)

  # 描述性统计
  desc_stats <- d %>%
    group_by(group) %>%
    summarise(
      N = n(),
      使用前均值 = round(mean(Pre), 2),
      使用前SD = round(sd(Pre), 2),
      使用后均值 = round(mean(Post), 2),
      使用后SD = round(sd(Post), 2),
      均值差异 = round(mean(Diff), 3),
      Q1_Diff = quantile(Diff, 0.25),
      中位数_Diff = median(Diff),
      Q3_Diff = quantile(Diff, 0.75),
      .groups = "drop"
    )

  # 组内配对检验
  within_tests <- d %>%
    group_by(group) %>%
    summarise(
      p_within = tryCatch(
        wilcox.test(Post, Pre, paired = TRUE, alternative = "greater", exact = FALSE)$p.value,
        error = function(e) NA_real_
      ),
      .groups = "drop"
    ) %>%
    mutate(显著性_组内 = case_when(
      is.na(p_within) ~ "NA",
      p_within < 0.001 ~ "***",
      p_within < 0.01 ~ "**",
      p_within < 0.05 ~ "*",
      TRUE ~ "ns"
    ))

  # 组间差异检验
  n_groups <- n_distinct(d$group)
  if (n_groups == 2) {
    between_test <- tryCatch(
      wilcox.test(Diff ~ group, data = d, exact = FALSE),
      error = function(e) list(p.value = NA_real_)
    )
  } else if (n_groups > 2) {
    between_test <- tryCatch(
      kruskal.test(Diff ~ group, data = d),
      error = function(e) list(p.value = NA_real_)
    )
  } else {
    between_test <- list(p.value = NA_real_)
  }

  result <- desc_stats %>%
    left_join(within_tests, by = "group") %>%
    mutate(
      行为 = behavior_name,
      维度 = group_label,
      p_between = between_test$p.value,
      显著性_组间 = case_when(
        is.na(p_between) ~ "NA",
        p_between < 0.001 ~ "***",
        p_between < 0.01 ~ "**",
        p_between < 0.05 ~ "*",
        TRUE ~ "ns"
      )
    ) %>%
    rename(群体 = group)

  result
}

# 预处理：合并教育分组（大专和高中及以下 -> 大专及以下）
data_processed <- data %>%
  mutate(
    education_merged = case_when(
      education %in% c("大专", "高中及以下") ~ "大专及以下",
      TRUE ~ as.character(education)
    )
  )

# 更新分组变量（教育使用合并后的变量）
group_vars_processed <- c(
  "gender" = "性别",
  "age" = "年龄组",
  "marital_status" = "婚姻状况",
  "education_merged" = "教育水平"
)

# 批量分析所有行为×所有维度
all_group_results <- lapply(names(behavior_map), function(b) {
  cols <- behavior_map[[b]]
  lapply(names(group_vars_processed), function(gvar) {
    analyze_by_group(data_processed, b, cols[1], cols[2], gvar, group_vars_processed[gvar])
  }) %>% bind_rows()
}) %>%
  bind_rows()

# 按维度输出结果
for (dim_name in unique(all_group_results$维度)) {
  cat("\n\n--- 3.", which(group_vars_processed == dim_name), " ", dim_name, "分组分析 ---\n", sep = "")

  all_group_results %>%
    filter(维度 == dim_name) %>%
    select(行为, 群体, N, 使用前均值, 使用前SD, 使用后均值, 使用后SD,
           均值差异, Q1_Diff, 中位数_Diff, Q3_Diff, 显著性_组内, 显著性_组间) %>%
    kable(digits = 2, format = "simple",
          col.names = c("行为", "群体", "N", "前均值", "前SD", "后均值", "后SD",
                        "均值差", "Q1", "中位数", "Q3", "组内", "组间")) %>%
    print()
}

# ============================================================
# Part 4: 热图可视化（显著性差异）
# ============================================================

cat("\n\n", strrep("=", 70), "\n")
cat("Part 4: 热图可视化\n")
cat(strrep("=", 70), "\n")

# 4.1 组内显著性热图（各群体前后变化是否显著）
within_heatmap_data <- all_group_results %>%
  mutate(
    维度群体 = paste0(维度, " - ", 群体),
    行为 = factor(行为, levels = names(behavior_map)),
    neg_log_p = pmin(-log10(pmax(p_within, 1e-4)), 4)
  )

within_heatmap_data %>%
  ggplot(aes(x = 行为, y = 维度群体, fill = c(显著性_组内 != "ns"))) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = 显著性_组内), size = 3.5) +
  facet_grid(维度 ~ ., scales = "free_y", space = "free_y") +
  labs(
    title = "各群体APP使用前后行为变化显著性（组内配对检验）",
    subtitle = "* p<0.05, ** p<0.01, *** p<0.001",
    x = "行为", y = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text.y = element_text(angle = 0, face = "bold"),
    panel.grid = element_blank()
  ) -> within_plot

print(within_plot)
ggsave("data_proc/behavior_change_within_group.png", within_plot,
       width = 11, height = 10, dpi = 300, bg = "white")

# 4.2 组间差异热图
between_heatmap_data <- all_group_results %>%
  select(行为, 维度, p_between, 显著性_组间) %>%
  distinct() %>%
  mutate(
    行为 = factor(行为, levels = names(behavior_map)),
    维度 = factor(维度, levels = group_vars_processed),
    neg_log_p = pmin(-log10(pmax(p_between, 1e-4)), 4)
  )

between_heatmap_data %>%
  ggplot(aes(x = 行为, y = 维度, fill = c(显著性_组间 != "ns"))) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = 显著性_组间), size = 4) +
  labs(
    title = "不同群体行为变化幅度的差异显著性（组间检验）",
    subtitle = "* p<0.05, ** p<0.01, *** p<0.001 | 二分类: Mann-Whitney U, 多分类: Kruskal-Wallis",
    x = "行为", y = "人口学维度"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40", size = 9),
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid = element_blank()
  ) -> between_plot

print(between_plot)
ggsave("data_proc/behavior_change_between_group.png", between_plot,
       width = 10, height = 5, dpi = 300, bg = "white")


