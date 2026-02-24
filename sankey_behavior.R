# 行为变化桑基图
# 展示APP使用前后各行为频率的转变流向
# ============================================================
# 配置：行为变量映射
# ============================================================

behavior_map <- list(
  "公共交通" = c("q16_pre_public_trans", "q18_change_public_trans"),
  "骑行或步行" = c("q16_pre_bike_walk", "q18_change_bike_walk"),
  "关闭电源" = c("q16_pre_turn_off_power", "q18_change_turn_off_power"),
  "垃圾分类" = c("q16_pre_garbage_sort", "q18_change_garbage_sort"),
  "使用环保袋" = c("q16_pre_reusable_bag", "q18_change_reusable_bag"),
  "选择节能电器" = c("q16_pre_choose_energy_eff", "q18_change_choose_energy_eff")
)

# ============================================================
# 函数：绘制行为前后变化的桑基图
# ============================================================

plot_behavior_sankey <- function(data, pre_col_str, post_col_str, behavior_label) {
  pre_sym <- sym(pre_col_str)
  post_sym <- sym(post_col_str)

  # 数据准备
  analysis_data <- data %>%
    select(!!pre_sym, !!post_sym) %>%
    mutate(
      Pre = as.factor(as.numeric(!!pre_sym)),
      Post = as.factor(as.numeric(!!post_sym))
    ) %>%
    filter(!is.na(Pre) & !is.na(Post))

  # 计算流量（Pre -> Post 的转换）
  flow_data <- analysis_data %>%
    group_by(Pre, Post) %>%
    summarise(Count = n(), .groups = "drop")

  # 转换为 ggsankey 格式
  sankey_data <- flow_data %>%
    make_long(Pre, Post, value = Count)

  # 提取得分数字用于颜色映射
  sankey_data$score <- as.character(gsub(".*(\\d)$", "\\1", sankey_data$node))

  # 颜色方案
  score_palette <- c(
    "1" = "#D32F2F",
    "2" = "#F57C00",
    "3" = "#FDD835",
    "4" = "#66BB6A",
    "5" = "#2E7D32"
  )

  # 绘制桑基图
  ggplot(sankey_data, aes(
    x = x,
    next_x = next_x,
    node = node,
    next_node = next_node,
    fill = score,
    value = value
  )) +
    geom_sankey(flow.alpha = 0.5, node.color = "gray30", width = 0.1) +
    scale_fill_manual(
      values = score_palette,
      name = "行为得分",
      labels = c("1" = "1 (很少)", "2" = "2", "3" = "3", "4" = "4", "5" = "5 (频繁)")
    ) +
    scale_x_discrete(labels = c("Pre" = "使用前", "Post" = "使用后")) +
    labs(title = behavior_label, x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      legend.position = "none",
      axis.text.x = element_text(size = 9),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank()
    )
}

# ============================================================
# 批量生成桑基图
# ============================================================

sankey_plots <- lapply(names(behavior_map), function(b) {
  cols <- behavior_map[[b]]
  plot_behavior_sankey(
    data = data,
    pre_col_str = cols[1],
    post_col_str = cols[2],
    behavior_label = b
  )
})

# 最后一张子图显示图例
sankey_plots[[length(sankey_plots)]] <- sankey_plots[[length(sankey_plots)]] +
  theme(legend.position = "right")

# 组合显示
combined_sankey <- Reduce(`+`, sankey_plots) +
  plot_layout(ncol = 2, guides = "collect")

print(combined_sankey)

