# 三组行为得分对比分析
# APP使用前 vs APP使用后 vs 非APP用户

# ============================================================
# 配置：行为变量映射（三个时期）
# ============================================================

behavior_map <- list(
  "公共交通" = list(
    pre = "q16_pre_public_trans",
    post = "q18_change_public_trans",
    non_user = "q17_usual_public_trans"
  ),
  "骑行/步行" = list(
    pre = "q16_pre_bike_walk",
    post = "q18_change_bike_walk",
    non_user = "q17_usual_bike_walk"
  ),
  "关闭电源" = list(
    pre = "q16_pre_turn_off_power",
    post = "q18_change_turn_off_power",
    non_user = "q17_usual_turn_off_power"
  ),
  "垃圾分类" = list(
    pre = "q16_pre_garbage_sort",
    post = "q18_change_garbage_sort",
    non_user = "q17_usual_garbage_sort"
  ),
  "使用环保袋" = list(
    pre = "q16_pre_reusable_bag",
    post = "q18_change_reusable_bag",
    non_user = "q17_usual_reusable_bag"
  ),
  "选择节能产品" = list(
    pre = "q16_pre_choose_energy_eff",
    post = "q18_change_choose_energy_eff",
    non_user = "q17_usual_choose_energy_eff"
  )
)

group_levels <- c("使用前", "使用后", "非用户")

# ============================================================
# Part 1: 描述性统计表格
# ============================================================

cat("\n", strrep("=", 70), "\n")
cat("Part 1: 三组行为得分描述性统计\n")
cat(strrep("=", 70), "\n\n")

desc_stats <- lapply(names(behavior_map), function(b) {
  cols <- behavior_map[[b]]

  # APP用户使用前
  pre <- data %>%
    filter(q1_used_app == 1) %>%
    pull(cols$pre) %>%
    as.numeric()

  # APP用户使用后
  post <- data %>%
    filter(q1_used_app == 1) %>%
    pull(cols$post) %>%
    as.numeric()

  # 非APP用户
  non_user <- data %>%
    filter(q1_used_app != 1) %>%
    pull(cols$non_user) %>%
    as.numeric()

  bind_rows(
    data.frame(群体 = "使用前", score = pre[!is.na(pre)]),
    data.frame(群体 = "使用后", score = post[!is.na(post)]),
    data.frame(群体 = "非用户", score = non_user[!is.na(non_user)])
  ) %>%
    group_by(群体) %>%
    summarise(
      N = n(),
      平均分 = mean(score),
      SD = sd(score),
      Q1 = quantile(score, 0.25),
      中位数 = median(score),
      Q3 = quantile(score, 0.75),
      .groups = "drop"
    ) %>%
    mutate(行为 = b, 群体 = factor(群体, levels = group_levels))
}) %>%
  bind_rows() %>%
  arrange(行为, 群体)

cat("--- 三组得分统计 ---\n")
desc_stats %>%
  select(行为, 群体, N, 平均分, SD, Q1, 中位数, Q3) %>%
  kable(digits = 2, format = "simple")

# ============================================================
# Part 2: 两两比较统计检验
# ============================================================

cat("\n\n", strrep("=", 70), "\n")
cat("Part 2: 两两比较统计检验 (Wilcoxon秩和检验, Bonferroni校正)\n")
cat(strrep("=", 70), "\n\n")

# 定义比较对
comparisons <- list(
  c("使用前", "使用后"),
  c("使用前", "非用户"),
  c("使用后", "非用户")
)

pairwise_tests <- lapply(names(behavior_map), function(b) {
  cols <- behavior_map[[b]]

  # 准备数据
  pre <- data %>%
    filter(q1_used_app == 1) %>%
    pull(cols$pre) %>%
    as.numeric()

  post <- data %>%
    filter(q1_used_app == 1) %>%
    pull(cols$post) %>%
    as.numeric()

  non_user <- data %>%
    filter(q1_used_app != 1) %>%
    pull(cols$non_user) %>%
    as.numeric()

  scores <- list(
    "使用前" = pre[!is.na(pre)],
    "使用后" = post[!is.na(post)],
    "非用户" = non_user[!is.na(non_user)]
  )

  # 两两比较
  lapply(comparisons, function(pair) {
    g1 <- pair[1]
    g2 <- pair[2]

    test <- tryCatch(
      wilcox.test(scores[[g1]], scores[[g2]], exact = FALSE),
      error = function(e) list(p.value = NA_real_)
    )

    # Bonferroni校正 (3次比较)
    p_adj <- min(test$p.value * 3, 1)

    data.frame(
      行为 = b,
      比较 = paste(g1, "vs", g2),
      组1 = g1,
      组2 = g2,
      p_raw = test$p.value,
      p_adj = p_adj,
      显著性 = case_when(
        is.na(p_adj) ~ "NA",
        p_adj < 0.001 ~ "***",
        p_adj < 0.01 ~ "**",
        p_adj < 0.05 ~ "*",
        TRUE ~ "ns"
      ),
      是否显著 = !is.na(p_adj) & p_adj < 0.05
    )
  }) %>% bind_rows()
}) %>% bind_rows()

cat("--- 两两比较结果 ---\n")
pairwise_tests %>%
  mutate(
    p_raw = format.pval(p_raw, digits = 3),
    p_adj = format.pval(p_adj, digits = 3)
  ) %>%
  select(行为, 比较, p_raw, p_adj, 显著性) %>%
  kable(format = "simple", col.names = c("行为", "比较", "原始p", "校正p", "显著性"))

# ============================================================
# Part 3: 显著性热图
# ============================================================

cat("\n\n", strrep("=", 70), "\n")
cat("Part 3: 显著性热图\n")
cat(strrep("=", 70), "\n\n")

# 热图数据
heatmap_data <- pairwise_tests %>%
  mutate(
    行为 = factor(行为, levels = names(behavior_map)),
    比较 = factor(比较, levels = c("使用前 vs 使用后", "使用前 vs 非用户", "使用后 vs 非用户"))
  )

# 绘制热图（颜色表示是否显著）
heatmap_data %>%
  ggplot(aes(x = 行为, y = 比较, fill = 是否显著)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = 显著性), size = 5, fontface = "bold") +
  scale_fill_manual(
    values = c("TRUE" = "#4CAF50", "FALSE" = "#EEEEEE"),
    labels = c("TRUE" = "显著 (p<0.05)", "FALSE" = "不显著"),
    name = ""
  ) +
  labs(
    title = "三组行为得分两两比较显著性",
    subtitle = "Wilcoxon秩和检验 + Bonferroni校正 | * p<0.05, ** p<0.01, *** p<0.001",
    x = "行为",
    y = "组间比较"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40", size = 10),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 11),
    axis.text.y = element_text(size = 11),
    panel.grid = element_blank(),
    legend.position = "bottom"
  ) -> sig_heatmap

print(sig_heatmap)
ggsave("data_proc/three_group_significance.png", sig_heatmap,
       width = 10, height = 5, dpi = 300, bg = "white")

# ============================================================
# Part 4: 均值对比条形图
# ============================================================

cat("\n绘制均值对比图...\n")

desc_stats %>%
  mutate(行为 = factor(行为, levels = names(behavior_map))) %>%
  ggplot(aes(x = 行为, y = 平均分, fill = 群体)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = 平均分 - SD / sqrt(N), ymax = 平均分 + SD / sqrt(N)),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  geom_text(
    aes(label = sprintf("%.2f", 平均分)),
    position = position_dodge(width = 0.8),
    vjust = -1.5, size = 3
  ) +
  scale_fill_manual(
    values = c("使用前" = "#FCA5A5", "使用后" = "#86EFAC", "非用户" = "#93C5FD"),
    name = "群体"
  ) +
  labs(
    title = "三组行为得分对比",
    subtitle = "误差线表示标准误 (SE)",
    x = "行为", y = "平均得分"
  ) +
  ylim(0, 5.5) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "bottom"
  ) -> mean_barplot

print(mean_barplot)
ggsave("data_proc/three_group_means.png", mean_barplot,
       width = 11, height = 6, dpi = 300, bg = "white")

# ============================================================
# Part 5: 汇总
# ============================================================

cat("\n\n", strrep("=", 70), "\n")
cat("Part 5: 显著差异汇总\n")
cat(strrep("=", 70), "\n\n")

pairwise_tests %>%
  filter(是否显著) %>%
  select(行为, 比较, 显著性, p_adj) %>%
  mutate(校正p值 = format.pval(p_adj, digits = 4)) %>%
  select(-p_adj) %>%
  kable(format = "simple") %>%
  print()

cat("\n分析完成。\n")
cat("输出文件:\n")
cat("  - data_proc/three_group_significance.png (显著性热图)\n")
cat("  - data_proc/three_group_means.png (均值对比图)\n")
