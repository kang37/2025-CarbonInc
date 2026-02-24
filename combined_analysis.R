# ==============================================================================
# 综合分析脚本
# 包含：桑基图、行为变化分析、三组对比、Q4/Q20感知分析、Q13/Q14/Q15多选题分析
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsankey)
library(patchwork)
library(knitr)

# ==============================================================================
# 全局配置
# ==============================================================================

# 行为变量映射（前后对比）
behavior_prepost <- list(
  "公共交通" = c(pre = "q16_pre_public_trans", post = "q18_change_public_trans"),
  "骑行或步行" = c(pre = "q16_pre_bike_walk", post = "q18_change_bike_walk"),
  "关闭电源" = c(pre = "q16_pre_turn_off_power", post = "q18_change_turn_off_power"),
  "垃圾分类" = c(pre = "q16_pre_garbage_sort", post = "q18_change_garbage_sort"),
  "使用环保袋" = c(pre = "q16_pre_reusable_bag", post = "q18_change_reusable_bag"),
  "选择节能电器" = c(pre = "q16_pre_choose_energy_eff", post = "q18_change_choose_energy_eff")
)

# 行为变量映射（三组对比：使用前、使用后、非用户）
behavior_three <- list(
  "公共交通" = list(pre = "q16_pre_public_trans", post = "q18_change_public_trans", non = "q17_usual_public_trans"),
  "骑行/步行" = list(pre = "q16_pre_bike_walk", post = "q18_change_bike_walk", non = "q17_usual_bike_walk"),
  "关闭电源" = list(pre = "q16_pre_turn_off_power", post = "q18_change_turn_off_power", non = "q17_usual_turn_off_power"),
  "垃圾分类" = list(pre = "q16_pre_garbage_sort", post = "q18_change_garbage_sort", non = "q17_usual_garbage_sort"),
  "使用环保袋" = list(pre = "q16_pre_reusable_bag", post = "q18_change_reusable_bag", non = "q17_usual_reusable_bag"),
  "选择节能产品" = list(pre = "q16_pre_choose_energy_eff", post = "q18_change_choose_energy_eff", non = "q17_usual_choose_energy_eff")
)

# 人口学分组变量
demo_groups <- c(
  "gender" = "性别",
  "age" = "年龄组",
  "marital_status" = "婚姻状况",
  "education" = "教育水平"
)

demo_groups_app <- c(demo_groups, "q1_used_app" = "APP使用情况")

# Q4/Q20 感知变量
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

q20_vars <- c(
  "q20_ranking_motivation" = "11.排行成就激励",
  "q20_image_inspire" = "12.图像动画更有趣",
  "q20_cumulative_inspire" = "13.累计成效激励",
  "q20_celeb_endorsement" = "9.明星代言更关注",
  "q20_video_intro" = "10.视频介绍更吸引"
)

q4q20_all <- c(q4_vars, q20_vars)

# Q13/Q14/Q15 多选题变量
q13_attract <- c(
  "q13_attract_points" = "碳积分兑换商品",
  "q13_attract_credit" = "绿色信用/政策优惠",
  "q13_attract_daily_task" = "每日打卡/任务",
  "q13_attract_ranking" = "排行榜/成就徽章",
  "q13_attract_game_fun" = "抽奖等娱乐玩法",
  "q13_attract_viz_carbon" = "可视化碳足迹"
)

q14_reward <- "q14_desired_reward"

q15_barrier <- c(
  "q15_barrier_trouble" = "觉得操作麻烦",
  "q15_barrier_privacy" = "担心信息不安全",
  "q15_barrier_low_reward" = "积分奖励太少",
  "q15_barrier_unknown" = "没听说过此类APP"
)


# ##############################################################################
# PART 1: 桑基图 - APP使用前后行为变化流向
# ##############################################################################

cat("\n", strrep("#", 80), "\n")
cat("PART 1: 桑基图 - APP使用前后行为变化流向\n")
cat(strrep("#", 80), "\n")

plot_sankey <- function(data, pre_col, post_col, label) {
  d <- data %>%
    select(Pre = all_of(pre_col), Post = all_of(post_col)) %>%
    mutate(Pre = as.factor(as.numeric(Pre)), Post = as.factor(as.numeric(Post))) %>%
    filter(!is.na(Pre) & !is.na(Post)) %>%
    count(Pre, Post, name = "Count") %>%
    make_long(Pre, Post, value = Count)

  d$score <- gsub(".*(\\d)$", "\\1", d$node)

  ggplot(d, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = score, value = value)) +
    geom_sankey(flow.alpha = 0.5, node.color = "gray30", width = 0.1) +
    scale_fill_manual(values = c("1"="#D32F2F","2"="#F57C00","3"="#FDD835","4"="#66BB6A","5"="#2E7D32"),
                      name = "得分", labels = c("1(很少)","2","3","4","5(频繁)")) +
    scale_x_discrete(labels = c("Pre" = "使用前", "Post" = "使用后")) +
    labs(title = label, x = NULL, y = NULL) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
          legend.position = "none", axis.text.y = element_blank(), panel.grid = element_blank())
}

sankey_list <- lapply(names(behavior_prepost), function(b) {
  plot_sankey(data, behavior_prepost[[b]][["pre"]], behavior_prepost[[b]][["post"]], b)
})
sankey_list[[length(sankey_list)]] <- sankey_list[[length(sankey_list)]] + theme(legend.position = "right")

print(Reduce(`+`, sankey_list) + plot_layout(ncol = 2, guides = "collect"))


# ##############################################################################
# PART 2: APP使用前后行为变化分析（整体+分群体）
# ##############################################################################

cat("\n\n", strrep("#", 80), "\n")
cat("PART 2: APP使用前后行为变化分析\n")
cat(strrep("#", 80), "\n")

app_users <- data %>% filter(q1_used_app == 1)

# 2.1 整体描述性统计
cat("\n--- 2.1 整体描述性统计 ---\n")
lapply(names(behavior_prepost), function(b) {
  app_users %>%
    select(Pre = all_of(behavior_prepost[[b]][["pre"]]), Post = all_of(behavior_prepost[[b]][["post"]])) %>%
    mutate(across(everything(), as.numeric)) %>%
    pivot_longer(everything(), names_to = "时期", values_to = "得分") %>%
    filter(!is.na(得分)) %>%
    group_by(时期) %>%
    summarise(N = n(), 平均分 = mean(得分), SD = sd(得分), Q1 = quantile(得分, 0.25),
              中位数 = median(得分), Q3 = quantile(得分, 0.75), .groups = "drop") %>%
    mutate(行为 = b, 时期 = factor(ifelse(时期 == "Pre", "使用前", "使用后"), c("使用前", "使用后")))
}) %>% bind_rows() %>% arrange(行为, 时期) %>%
  select(行为, 时期, N, 平均分, SD, Q1, 中位数, Q3) %>% kable(digits = 2, format = "simple") %>% print()

# 2.2 整体Wilcoxon检验
cat("\n--- 2.2 整体行为变化检验 ---\n")
lapply(names(behavior_prepost), function(b) {
  d <- app_users %>%
    select(Pre = all_of(behavior_prepost[[b]][["pre"]]), Post = all_of(behavior_prepost[[b]][["post"]])) %>%
    mutate(across(everything(), as.numeric)) %>% filter(!is.na(Pre) & !is.na(Post)) %>%
    mutate(Diff = Post - Pre)
  test <- wilcox.test(d$Post, d$Pre, paired = TRUE, alternative = "greater", exact = FALSE)
  data.frame(行为 = b, N = nrow(d), 使用前 = round(mean(d$Pre), 2), 使用后 = round(mean(d$Post), 2),
             差异 = round(mean(d$Diff), 3), Cohen_d = round(mean(d$Diff)/sd(d$Diff), 3),
             P值 = test$p.value, 显著 = ifelse(test$p.value < 0.001, "***", ifelse(test$p.value < 0.01, "**", ifelse(test$p.value < 0.05, "*", "ns"))))
}) %>% bind_rows() %>% mutate(P值 = format.pval(P值, 3)) %>% kable(format = "simple") %>% print()

# 2.3 分群体分析
cat("\n--- 2.3 分群体行为变化分析 ---\n")

# 预处理：合并教育分组
data_edu <- data %>%
  mutate(education_merged = case_when(
    education %in% c("大专", "高中及以下") ~ "大专及以下", TRUE ~ as.character(education)))

demo_groups_edu <- c("gender" = "性别", "age" = "年龄组", "marital_status" = "婚姻状况", "education_merged" = "教育水平")

behavior_by_group <- lapply(names(behavior_prepost), function(b) {
  lapply(names(demo_groups_edu), function(gvar) {
    d <- data_edu %>%
      filter(q1_used_app == 1 & !is.na(.data[[gvar]])) %>%
      select(grp = all_of(gvar), Pre = all_of(behavior_prepost[[b]][["pre"]]), Post = all_of(behavior_prepost[[b]][["post"]])) %>%
      mutate(across(c(Pre, Post), as.numeric)) %>% filter(!is.na(Pre) & !is.na(Post)) %>%
      mutate(Diff = Post - Pre)

    stats <- d %>% group_by(grp) %>%
      summarise(N = n(), 前均值 = round(mean(Pre), 2), 前SD = round(sd(Pre), 2),
                后均值 = round(mean(Post), 2), 后SD = round(sd(Post), 2), 差异 = round(mean(Diff), 3),
                Q1 = quantile(Diff, 0.25), 中位数 = median(Diff), Q3 = quantile(Diff, 0.75),
                p_within = tryCatch(wilcox.test(Post, Pre, paired = TRUE, alternative = "greater", exact = FALSE)$p.value, error = function(e) NA_real_),
                .groups = "drop") %>%
      mutate(组内 = case_when(is.na(p_within) ~ "NA", p_within < 0.001 ~ "***", p_within < 0.01 ~ "**", p_within < 0.05 ~ "*", TRUE ~ "ns"))

    n_grp <- n_distinct(d$grp)
    p_btw <- if (n_grp == 2) tryCatch(wilcox.test(Diff ~ grp, data = d, exact = FALSE)$p.value, error = function(e) NA)
             else if (n_grp > 2) tryCatch(kruskal.test(Diff ~ grp, data = d)$p.value, error = function(e) NA) else NA

    stats %>% mutate(行为 = b, 维度 = demo_groups_edu[gvar], p_between = p_btw,
                     组间 = case_when(is.na(p_btw) ~ "NA", p_btw < 0.001 ~ "***", p_btw < 0.01 ~ "**", p_btw < 0.05 ~ "*", TRUE ~ "ns"))
  }) %>% bind_rows()
}) %>% bind_rows()

for (dim in unique(behavior_by_group$维度)) {
  cat("\n  [", dim, "]\n", sep = "")
  behavior_by_group %>% filter(维度 == dim) %>%
    select(行为, 群体 = grp, N, 前均值, 前SD, 后均值, 后SD, 差异, Q1, 中位数, Q3, 组内, 组间) %>%
    kable(digits = 2, format = "simple") %>% print()
}

# 2.4 热图
behavior_by_group %>%
  mutate(行为 = factor(行为, levels = names(behavior_prepost))) %>%
  ggplot(aes(x = 行为, y = paste(维度, grp, sep = " - "), fill = (组内 != "ns" & 组内 != "NA"))) +
  geom_tile(color = "white") + geom_text(aes(label = 组内), size = 3) +
  facet_grid(维度 ~ ., scales = "free_y", space = "free_y") +
  labs(title = "分群体行为变化显著性（组内配对检验）", x = "行为", y = "") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1), strip.text.y = element_text(angle = 0), panel.grid = element_blank(), legend.position = "none") -> p1
print(p1)
ggsave("data_proc/behavior_change_within_group.png", p1, width = 11, height = 10, dpi = 300, bg = "white")


# ##############################################################################
# PART 3: 三组对比（使用前 vs 使用后 vs 非用户）
# ##############################################################################

cat("\n\n", strrep("#", 80), "\n")
cat("PART 3: 三组对比（使用前 vs 使用后 vs 非用户）\n")
cat(strrep("#", 80), "\n")

three_levels <- c("使用前", "使用后", "非用户")

# 3.1 描述性统计
cat("\n--- 3.1 三组描述性统计 ---\n")
three_desc <- lapply(names(behavior_three), function(b) {
  cols <- behavior_three[[b]]
  bind_rows(
    data.frame(群体 = "使用前", score = as.numeric(data$q1_used_app == 1) * as.numeric(data[[cols$pre]])) %>% filter(score > 0),
    data.frame(群体 = "使用后", score = as.numeric(data$q1_used_app == 1) * as.numeric(data[[cols$post]])) %>% filter(score > 0),
    data.frame(群体 = "非用户", score = as.numeric(data$q1_used_app != 1) * as.numeric(data[[cols$non]])) %>% filter(score > 0)
  ) %>% group_by(群体) %>%
    summarise(N = n(), 平均分 = mean(score), SD = sd(score), Q1 = quantile(score, 0.25),
              中位数 = median(score), Q3 = quantile(score, 0.75), .groups = "drop") %>%
    mutate(行为 = b, 群体 = factor(群体, three_levels))
}) %>% bind_rows() %>% arrange(行为, 群体)

three_desc %>% select(行为, 群体, N, 平均分, SD, Q1, 中位数, Q3) %>% kable(digits = 2, format = "simple") %>% print()

# 3.2 两两比较
cat("\n--- 3.2 两两比较 ---\n")
three_pairwise <- lapply(names(behavior_three), function(b) {
  cols <- behavior_three[[b]]
  scores <- list(
    "使用前" = na.omit(as.numeric(data[[cols$pre]][data$q1_used_app == 1])),
    "使用后" = na.omit(as.numeric(data[[cols$post]][data$q1_used_app == 1])),
    "非用户" = na.omit(as.numeric(data[[cols$non]][data$q1_used_app != 1]))
  )
  lapply(list(c("使用前","使用后"), c("使用前","非用户"), c("使用后","非用户")), function(pair) {
    p <- tryCatch(wilcox.test(scores[[pair[1]]], scores[[pair[2]]], exact = FALSE)$p.value, error = function(e) NA)
    p_adj <- min(p * 3, 1)
    data.frame(行为 = b, 比较 = paste(pair, collapse = " vs "), p_raw = p, p_adj = p_adj,
               显著 = case_when(is.na(p_adj) ~ "NA", p_adj < 0.001 ~ "***", p_adj < 0.01 ~ "**", p_adj < 0.05 ~ "*", TRUE ~ "ns"),
               是否显著 = !is.na(p_adj) & p_adj < 0.05)
  }) %>% bind_rows()
}) %>% bind_rows()

three_pairwise %>% mutate(p_raw = format.pval(p_raw, 3), p_adj = format.pval(p_adj, 3)) %>%
  select(行为, 比较, p_raw, p_adj, 显著) %>% kable(format = "simple") %>% print()

# 3.3 热图
three_pairwise %>%
  mutate(行为 = factor(行为, names(behavior_three)), 比较 = factor(比较, c("使用前 vs 使用后", "使用前 vs 非用户", "使用后 vs 非用户"))) %>%
  ggplot(aes(x = 行为, y = 比较, fill = 是否显著)) +
  geom_tile(color = "white", linewidth = 0.8) + geom_text(aes(label = 显著), size = 5, fontface = "bold") +
  scale_fill_manual(values = c("TRUE" = "#4CAF50", "FALSE" = "#EEEEEE")) +
  labs(title = "三组行为得分两两比较显著性", x = "行为", y = "比较") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1), panel.grid = element_blank(), legend.position = "none") -> p2
print(p2)
ggsave("data_proc/three_group_significance.png", p2, width = 10, height = 5, dpi = 300, bg = "white")


# ##############################################################################
# PART 4: Q4/Q20 APP感知与使用因素分析
# ##############################################################################

cat("\n\n", strrep("#", 80), "\n")
cat("PART 4: Q4/Q20 APP感知与使用因素分析\n")
cat(strrep("#", 80), "\n")

# 4.1 整体描述性统计
cat("\n--- 4.1 整体描述性统计 ---\n")
data %>% select(all_of(names(q4q20_all))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "score") %>%
  mutate(score = as.numeric(score)) %>% filter(!is.na(score)) %>%
  group_by(var) %>%
  summarise(N = n(), 平均分 = mean(score), SD = sd(score), Q1 = quantile(score, 0.25),
            中位数 = median(score), Q3 = quantile(score, 0.75), .groups = "drop") %>%
  mutate(题目 = q4q20_all[var]) %>% arrange(match(var, names(q4q20_all))) %>%
  select(题目, N, 平均分, SD, 中位数, Q1, Q3) %>% kable(digits = 2, format = "simple") %>% print()

# 4.2 分群体统计
cat("\n--- 4.2 分群体统计 ---\n")
for (i in seq_along(demo_groups_app)) {
  gvar <- names(demo_groups_app)[i]
  cat("\n  [", demo_groups_app[i], "]\n", sep = "")
  data %>% select(grp = all_of(gvar), all_of(names(q4q20_all))) %>% filter(!is.na(grp)) %>%
    pivot_longer(-grp, names_to = "var", values_to = "score") %>% mutate(score = as.numeric(score)) %>% filter(!is.na(score)) %>%
    group_by(群体 = grp, var) %>%
    summarise(N = n(), 平均分 = mean(score), SD = sd(score), Q1 = quantile(score, 0.25),
              中位数 = median(score), Q3 = quantile(score, 0.75), .groups = "drop") %>%
    mutate(题目 = q4q20_all[var]) %>% arrange(match(var, names(q4q20_all)), 群体) %>%
    select(题目, 群体, N, 平均分, SD, Q1, 中位数, Q3) %>% kable(digits = 2, format = "simple") %>% print()
}

# 4.3 显著性热图
cat("\n--- 4.3 显著性检验热图 ---\n")
q4q20_tests <- expand.grid(gvar = names(demo_groups_app), qvar = names(q4q20_all), stringsAsFactors = FALSE) %>%
  rowwise() %>% mutate(p.value = {
    d <- data %>% select(g = all_of(gvar), s = all_of(qvar)) %>% filter(!is.na(g) & !is.na(s)) %>% mutate(s = as.numeric(s))
    n_grp <- n_distinct(d$g)
    if (n_grp < 2 || nrow(d) < 10) NA_real_
    else if (n_grp == 2) suppressWarnings(wilcox.test(s ~ g, data = d)$p.value)
    else suppressWarnings(kruskal.test(s ~ g, data = d)$p.value)
  }) %>% ungroup() %>%
  mutate(维度 = factor(demo_groups_app[gvar], demo_groups_app), 题目 = factor(q4q20_all[qvar], q4q20_all),
         显著 = case_when(is.na(p.value) ~ "NA", p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", p.value < 0.05 ~ "*", TRUE ~ "ns"))

q4q20_tests %>%
  ggplot(aes(x = 题目, y = 维度, fill = (显著 != "ns" & 显著 != "NA"))) +
  geom_tile(color = "white") + geom_text(aes(label = 显著), size = 4) +
  labs(title = "Q4/Q20 各人口学维度群体差异显著性", x = "问题", y = "维度") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank(), legend.position = "none") -> p3
print(p3)
ggsave("data_proc/q4_q20_group_diff_heatmap.png", p3, width = 14, height = 5, dpi = 300, bg = "white")


# ##############################################################################
# PART 5: Q13/Q14/Q15 多选题分析
# ##############################################################################

cat("\n\n", strrep("#", 80), "\n")
cat("PART 5: Q13/Q14/Q15 多选题分析\n")
cat(strrep("#", 80), "\n")

# 辅助函数
calc_multi_freq <- function(data, labels) {
  data %>% select(all_of(names(labels))) %>%
    summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "var", values_to = "选择人数") %>%
    mutate(选项 = labels[var], 总人数 = nrow(data), 比例 = 选择人数 / 总人数) %>%
    arrange(desc(选择人数)) %>% select(选项, 选择人数, 总人数, 比例)
}

calc_multi_by_group <- function(data, labels, gvar) {
  data %>% filter(!is.na(.data[[gvar]])) %>% group_by(grp = .data[[gvar]]) %>%
    summarise(n = n(), across(all_of(names(labels)), ~ sum(. == 1, na.rm = TRUE) / n()), .groups = "drop") %>%
    pivot_longer(-c(grp, n), names_to = "var", values_to = "比例") %>%
    mutate(选项 = labels[var], grp = as.character(grp))
}

test_multi_by_group <- function(data, labels, gvar) {
  lapply(names(labels), function(v) {
    d <- data %>% filter(!is.na(.data[[gvar]]) & !is.na(.data[[v]])) %>%
      mutate(sel = as.factor(.data[[v]]), grp = as.factor(.data[[gvar]]))
    if (n_distinct(d$grp) < 2 || n_distinct(d$sel) < 2) return(data.frame(var = v, p.value = NA_real_))
    tryCatch({ data.frame(var = v, p.value = chisq.test(table(d$grp, d$sel))$p.value) },
             error = function(e) { tryCatch({ data.frame(var = v, p.value = fisher.test(table(d$grp, d$sel), simulate.p.value = TRUE)$p.value) },
                                            error = function(e2) { data.frame(var = v, p.value = NA_real_) }) })
  }) %>% bind_rows() %>% mutate(选项 = labels[var])
}

cochran_q <- function(data, vars) {
  mat <- data %>% select(all_of(vars)) %>% mutate(across(everything(), ~ as.numeric(. == 1))) %>% na.omit() %>% as.matrix()
  if (nrow(mat) < 10) return(list(Q = NA, p = NA, n = nrow(mat), df = NA))
  k <- ncol(mat); t_tot <- sum(mat)
  q_stat <- (k - 1) * (k * sum(colSums(mat)^2) - t_tot^2) / (k * t_tot - sum(rowSums(mat)^2))
  list(Q = q_stat, p = 1 - pchisq(q_stat, k - 1), n = nrow(mat), df = k - 1)
}

# 5.1 Q13 吸引点
cat("\n--- 5.1 Q13 吸引点 ---\n")
calc_multi_freq(data, q13_attract) %>% mutate(比例 = sprintf("%.1f%%", 比例 * 100)) %>% kable(format = "simple") %>% print()

q13_coch <- cochran_q(data, names(q13_attract))
cat(sprintf("Cochran's Q = %.2f, df = %d, p = %.4f, n = %d\n", q13_coch$Q, q13_coch$df, q13_coch$p, q13_coch$n))

q13_by_group <- lapply(names(demo_groups_app), function(gvar) {
  calc_multi_by_group(data, q13_attract, gvar) %>%
    left_join(test_multi_by_group(data, q13_attract, gvar) %>% select(var, p.value), by = "var") %>%
    mutate(维度 = demo_groups_app[gvar])
}) %>% bind_rows()

q13_by_group %>% select(维度, 选项, p.value) %>% distinct() %>%
  mutate(显著 = case_when(is.na(p.value) ~ "NA", p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", p.value < 0.05 ~ "*", TRUE ~ "ns"),
         维度 = factor(维度, demo_groups_app), 选项 = factor(选项, q13_attract)) %>%
  ggplot(aes(x = 选项, y = 维度, fill = (显著 != "ns" & 显著 != "NA"))) +
  geom_tile(color = "white") + geom_text(aes(label = 显著), size = 4) +
  labs(title = "Q13 各维度群体差异显著性", x = "吸引点", y = "维度") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1), panel.grid = element_blank(), legend.position = "none") -> p4
print(p4)
ggsave("data_proc/q13_attract_significance.png", p4, width = 10, height = 5, dpi = 300, bg = "white")

# 5.2 Q14 期望奖励
cat("\n--- 5.2 Q14 期望奖励 ---\n")
data %>% filter(!is.na(.data[[q14_reward]])) %>% count(选项 = .data[[q14_reward]], name = "人数") %>%
  mutate(比例 = sprintf("%.1f%%", 人数 / sum(人数) * 100)) %>% arrange(desc(人数)) %>% kable(format = "simple") %>% print()

q14_tests <- lapply(names(demo_groups_app), function(gvar) {
  d <- data %>% filter(!is.na(.data[[gvar]]) & !is.na(.data[[q14_reward]]))
  p <- tryCatch(chisq.test(table(d[[gvar]], d[[q14_reward]]))$p.value,
                error = function(e) tryCatch(fisher.test(table(d[[gvar]], d[[q14_reward]]), simulate.p.value = TRUE)$p.value, error = function(e2) NA))
  data.frame(维度 = demo_groups_app[gvar], p.value = p)
}) %>% bind_rows() %>%
  mutate(显著 = case_when(is.na(p.value) ~ "NA", p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", p.value < 0.05 ~ "*", TRUE ~ "ns"))
q14_tests %>% kable(digits = 4, format = "simple") %>% print()

# 5.3 Q15 使用障碍
cat("\n--- 5.3 Q15 使用障碍 ---\n")
calc_multi_freq(data, q15_barrier) %>% mutate(比例 = sprintf("%.1f%%", 比例 * 100)) %>% kable(format = "simple") %>% print()

q15_coch <- cochran_q(data, names(q15_barrier))
cat(sprintf("Cochran's Q = %.2f, df = %d, p = %.4f, n = %d\n", q15_coch$Q, q15_coch$df, q15_coch$p, q15_coch$n))

q15_by_group <- lapply(names(demo_groups_app), function(gvar) {
  calc_multi_by_group(data, q15_barrier, gvar) %>%
    left_join(test_multi_by_group(data, q15_barrier, gvar) %>% select(var, p.value), by = "var") %>%
    mutate(维度 = demo_groups_app[gvar])
}) %>% bind_rows()

q15_by_group %>% select(维度, 选项, p.value) %>% distinct() %>%
  mutate(显著 = case_when(is.na(p.value) ~ "NA", p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", p.value < 0.05 ~ "*", TRUE ~ "ns"),
         维度 = factor(维度, demo_groups_app), 选项 = factor(选项, q15_barrier)) %>%
  ggplot(aes(x = 选项, y = 维度, fill = (显著 != "ns" & 显著 != "NA"))) +
  geom_tile(color = "white") + geom_text(aes(label = 显著), size = 4) +
  labs(title = "Q15 各维度群体差异显著性", x = "使用障碍", y = "维度") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1), panel.grid = element_blank(), legend.position = "none") -> p5
print(p5)
ggsave("data_proc/q15_barrier_significance.png", p5, width = 9, height = 5, dpi = 300, bg = "white")


# ==============================================================================
cat("\n\n", strrep("=", 80), "\n")
cat("分析完成。输出文件保存于 data_proc/ 目录。\n")
cat(strrep("=", 80), "\n")
