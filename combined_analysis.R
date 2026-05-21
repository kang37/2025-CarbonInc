# 综合分析脚本
# 包含：桑基图、行为变化分析、三组对比、Q4/Q20感知分析、Q13/Q14/Q15多选题分析

# 载入包。
pacman::p_load(dplyr, tidyr, ggplot2, ggsankey, patchwork, knitr, showtext, stringr, readxl)
showtext_auto()

question_map <- c(
  "id" = "序号",
  "submit_time" = "提交答卷时间",
  "time_spent" = "所用时间",
  "source" = "来源",
  "source_detail" = "来源详情",
  "ip_address" = "来自IP",
  "total_score" = "总分",
  "gender" = "性别",
  "age" = "年龄",
  "education" = "教育",
  "marital_status" = "婚姻状况",
  "housing_sqm" = "住房面积（平方米）",
  "monthly_income_personal" = "个人月收入（元）",
  "monthly_income_family" = "家庭每月可支配收入（元）",
  "family_other_pop" = "家庭同住其他人口数量",
  "car_ownership" = "汽车拥有量",
  "new_energy_car_num" = "(1)其中新能源汽车___辆",
  "car_freq" = "汽车使用频率",
  "residence_area" = "居住区域",
  "residence_community" = "并且填写居住小区:",
  "q1_used_app" = "1 您是否使用过低碳减排APP?",
  "q2_how_know_app" = "2 您是如何了解到这个低碳减排APP的:",
  "q3_why_start_app" = "3 您为何开始使用这个低碳减排APP:",
  "q4_ui_simple" = "请选择最符合您想法或行为频率的选项。—1 我觉得低碳减排APP的界面应当简洁明了，方便浏览和操作。",
  "q4_integrate_platform" = "2 如果低碳减排APP与我常用的平台（如支付宝或微信）打通，我会更愿意使用它。",
  "q4_auto_record" = "3 如果APP能自动记录我的出行、用电等数据，我会觉得使用起来更方便。",
  "q4_clear_guidance" = "4 如果APP能清晰地引导我如何参与低碳活动，并且提供具体建议，我会更愿意持续使用它。",
  "q4_raise_awareness" = "5 我认为使用低碳减排APP能有效提高我的低碳行为意识。",
  "q4_info_feedback_useful" = "6 该APP提供的信息和反馈对我来说是实用的。",
  "q4_quicker_green_choice" = "7 使用该APP能让我更快地做出环保的生活选择。",
  "q4_know_carbon_credit" = "8 我了解“碳普惠”机制。",
  "q5_info_source_1" = "9 对于您来说，了解“碳普惠”或“低碳减排”方面信息的主要来源是？（提示：例如电视、报纸、网络、广告、政府文件、学校、社区管理员、志愿者、家人朋友等，也可以填写更具体的渠道）请填写最重要的1-3项，并且按照信息可靠程度进行排序。—1",
  "q5_info_source_2" = "2",
  "q5_info_source_3" = "3",
  "q6_know_garbage_sort" = "—10 我了解生活垃圾分类，包括分类的必要性、现状、分类方法、垃圾去向。",
  "q7_friend_recommend" = "11 我的朋友中有人已经使用或推荐过低碳减排APP。",
  "q8_social_media_influence" = "12 社交平台上的环保内容（如视频、话题、朋友圈动态）让我更关注自己的碳行为。",
  "q9_share_participate" = "13 如果我使用低碳减排APP，我希望朋友们知道我参与了这个计划（如转发、展示成绩）。",
  "q10_app_freq" = "(1)14 您最近使用该APP的频率大约是每月___次。",
  "q11_app_main_use" = "15 您主要使用该APP来做什么:",
  "q12_incentive_points" = "16 在使用过程中，您觉得实际上哪些方面最能激励您持续参与?（最多选择3项）(积分兑奖)",
  "q12_incentive_viz" = "16(图示减排成果（如碳足迹排名或减排量）)",
  "q12_incentive_ui" = "16(简洁、易操作的界面)",
  "q12_incentive_social" = "16(朋友参与和社交互动)",
  "q12_incentive_game" = "16(有趣的任务和游戏)",
  "q12_incentive_expert_advice" = "16(官方或专家推荐的减碳建议)",
  "q12_incentive_data_life" = "16(与生活紧密结合的数据记录（如出行、用电）)",
  "q12_incentive_other" = "16(其他（请注明:）)",
  "q13_attract_points" = "17 以下功能中，哪些最能吸引您使用一个低碳减排APP?(碳积分兑换商品或代金券)",
  "q13_attract_credit" = "17(提供绿色信用积分或政策优惠（如积分换公共服务）)",
  "q13_attract_daily_task" = "17(每日打卡/完成任务以获得积分)",
  "q13_attract_ranking" = "17(朋友排行榜、成就徽章展示)",
  "q13_attract_game_fun" = "17(抽奖、幸运转盘等娱乐性玩法)",
  "q13_attract_viz_carbon" = "17(可视化我的碳足迹与节能贡献)",
  "q13_attract_other" = "17(其他（请注明:）)",
  "q14_desired_reward" = "18 在以下APP提供的奖励中，我更希望获得:",
  "q15_barrier_trouble" = "19 您认为目前阻碍人们使用低碳减排APP的主要原因是什么?(觉得低碳减排APP的使用操作可能比较麻烦)",
  "q15_barrier_privacy" = "19(担心在使用低碳减排APP时个人信息不安全)",
  "q15_barrier_low_reward" = "19(积分奖励太少，对我没有吸引力)",
  "q15_barrier_unknown" = "19(很多人没有听说过这类APP)",
  "q15_barrier_other" = "19(其他:)",
  "q16_pre_public_trans" = "20 请您依据使用APP前的行为频率进行选择。—搭乘公共交通",
  "q16_pre_bike_walk" = "骑行或步行出行...63",
  "q16_pre_turn_off_power" = "关闭不用电器的电源...64",
  "q16_pre_garbage_sort" = "垃圾分类与回收...65",
  "q16_pre_reusable_bag" = "使用可重复使用的购物袋...66",
  "q16_pre_choose_energy_eff" = "选择节能但是售价更贵的电器...67",
  "q17_usual_public_trans" = "20 请您依据平时的行为频率进行选择。—搭乘公共交通",
  "q17_usual_bike_walk" = "骑行或步行出行...69",
  "q17_usual_turn_off_power" = "关闭不用电器的电源...70",
  "q17_usual_garbage_sort" = "垃圾分类与回收...71",
  "q17_usual_reusable_bag" = "使用可重复使用的购物袋...72",
  "q17_usual_choose_energy_eff" = "选择节能但是售价更贵的电器...73",
  "q18_change_public_trans" = "21 请问上述行为在使用该APP之后是否发生了改变?—搭乘公共交通",
  "q18_change_bike_walk" = "骑行或步行出行...75",
  "q18_change_turn_off_power" = "关闭不用电器的电源...76",
  "q18_change_garbage_sort" = "垃圾分类与回收...77",
  "q18_change_reusable_bag" = "使用可重复使用的购物袋...78",
  "q18_change_choose_energy_eff" = "选择节能但是售价更贵的电器...79",
  "q19_desired_feature" = "22 如果该APP可以添加一个功能或服务，您最希望是什么?",
  "q20_celeb_endorsement" = "—23 如果APP有明星或公众人物代言，我会更关注这个APP。",
  "q20_video_intro" = "24 如果APP有视频/短片介绍，我更愿意点击了解内容。",
  "q20_focus_env_vs_self" = "25 当APP介绍的是“环保意义”而不是“个人利益”（如积分、优惠券）时，我反而不太有兴趣继续使用。",
  "q20_ranking_motivation" = "26 如果APP提供“碳足迹排行”或“减排成就”，我会更有动力坚持使用该APP。",
  "q20_image_inspire" = "27 我觉得相比文字，APP界面上的图像或动画更能激发我使用APP的兴趣。",
  "q20_cumulative_inspire" = "28 当我看到累计的碳减排成效，我更愿意维持低碳行为。",
  "q21_suggestion" = "29 感谢您对本次调查的支持，您可以在下方写下您对低碳减排APP或者碳普惠的想法或建议。"
)

# 定义需要生成饼图的变量名（与数据框中的列名对应）
variables <- c(
  "gender", "age", "education", "marital_status", "housing_sqm",
  "monthly_income_personal", "monthly_income_family", "family_other_pop",
  "car_ownership"
)

# 构建数据。
combine_col_id <- 23
data <- left_join(
  # 个人属性等问题取文本。
  read_excel(
    "data_raw/320419112_按文本_2025年低碳减排问卷调查_1108_1067.xlsx", 
    range = cell_cols(1:combine_col_id)
  ) %>% 
    rename(!!!head(question_map, combine_col_id)) %>% 
    select(
      -submit_time, -time_spent, -source, -source_detail, -ip_address, 
      -total_score
    ) %>% 
    mutate(id = as.numeric(id)), 
  # 调查主体问题取序号。
  read_excel(
    "data_raw/320419112_按序号_2025年低碳减排问卷调查_1108_1067.xlsx"
  ) %>% 
    rename(!!!question_map) %>% 
    select(1, all_of(c(combine_col_id + 1):last_col())), 
  by = "id"
) %>% 
  mutate(
    gender = case_when(gender == "女" ~ "female", gender == "男" ~ "male"), 
    age = case_when(
      age %in% c("<18岁", "18–25岁", "26–30岁") ~ "≤30", 
      age %in% c("31–40岁", "41–50岁") ~ "31-50", 
      age %in% c("≥51岁") ~ "≥51"
    ), 
    education = case_when(
      education %in% c("初中及以下", "高中/技校", "大专") ~ "Below Bachelor", 
      education == "本科" ~ "Bachelor", 
      education == "硕士或博士" ~ "Master's or Doctoral"
    ), 
    marital_status = case_when(
      marital_status %in% c("其他〖分居〗", "离婚", "未婚") ~ 
        "Single/Divorced/Widowed", 
      marital_status == "已婚" ~ "Married"
    ), 
    monthly_income_personal = case_when(
      monthly_income_personal %in% c("≤2000", "2000–4000", "4000–6000") ~ 
        "<6000", 
      monthly_income_personal %in% c("6000–8000", "8000–10000") ~ 
        "6000-10000", 
      monthly_income_personal %in% c("10000–20000") ~ 
        "10000-20000", 
      monthly_income_personal %in% c("20000–30000", "≥30000") ~ ">20000"
    ), 
    # 1. 确定除数（总人数 = 其他成员 + 受访者本人）
    # 如果 other_pop 为 "≥5"，按 5 计算，总人数为 6
    div_n = case_when(
      family_other_pop == "≥5" ~ 6,
      TRUE ~ as.numeric(as.character(family_other_pop)) + 1
    ),
    
    # 2. 动态计算并生成字符串标签
    mon_income_family_pc = case_when(
      # 场景一：上限型 (≤4000)
      monthly_income_family == "≤4000" ~ 
        paste0("≤", round(4000 / div_n, 0)),
      
      # 场景二：区间型 (4000-6000)
      monthly_income_family == "4000-6000" ~ 
        paste0(round(4000 / div_n, 0), "-", round(6000 / div_n, 0)),
      
      monthly_income_family == "6000-8000" ~ 
        paste0(round(6000 / div_n, 0), "-", round(8000 / div_n, 0)),
      
      monthly_income_family == "8000-10000" ~ 
        paste0(round(8000 / div_n, 0), "-", round(10000 / div_n, 0)),
      
      monthly_income_family == "10000-20000" ~ 
        paste0(round(10000 / div_n, 0), "-", round(20000 / div_n, 0)),
      
      monthly_income_family == "20000-30000" ~ 
        paste0(round(20000 / div_n, 0), "-", round(30000 / div_n, 0)),
      # 场景三：下限型 (≥30,000)
      # 先去掉逗号再计算
      monthly_income_family == "≥30,000" | monthly_income_family == "≥30000" ~ 
        paste0("≥", round(30000 / div_n, 0))
    ), 
    # 为了分类，我们需要提取标签中的“上限”数值进行逻辑判断
    upper_bound = case_when(
      str_detect(mon_income_family_pc, "≤") ~ 
        as.numeric(str_extract(mon_income_family_pc, "\\d+")),
      str_detect(mon_income_family_pc, "≥") ~ 
        as.numeric(str_extract(mon_income_family_pc, "\\d+")),
      str_detect(mon_income_family_pc, "-") ~ 
        as.numeric(str_extract(mon_income_family_pc, "(?<=-)\\d+")),
      TRUE ~ NA_real_
    ),
    # 进一步整理为高中低收入
    mon_income_family_pc_lvl = case_when(
      # 1. 最高档：只要上限或下限超过 8000
      (str_detect(mon_income_family_pc, "≥") & upper_bound >= 9000) | 
        upper_bound > 9000 ~ "≥9000",
      # 2. 中高档：剩下的里面，上限超过 6000 的（即 6001-8000）
      (str_detect(mon_income_family_pc, "≥") & upper_bound >= 6000) | 
        upper_bound > 6000 ~ "6000-9000",
      # 3. 中等档：剩下的里面，上限超过 4000 的（即 4001-6000）
      (str_detect(mon_income_family_pc, "≥") & upper_bound >= 3000) | 
        upper_bound > 3000 ~ "3000-6000",
      # 4. 低收入：剩下的所有情况（即上限 <= 4000 的所有人群）
      TRUE ~ "≤3000"
    ), 
    mon_income_family_pc_lvl = factor(mon_income_family_pc_lvl, levels = c(
      "≤3000", "3000-6000", "6000-9000", "≥9000"
    )),
    car_ownership = case_when(
      car_ownership %in% c("2", "≥3") ~ "≥2", 
      TRUE ~ car_ownership
    ), 
    q1_used_app = grepl("使用过", q1_used_app)
  ) %>% 
  # 移除中间变量。
  select(-div_n, -upper_bound)

# Bug: data来自report.r
app_users <- data %>% 
  filter(q1_used_app == 1) %>% 
  mutate(education = case_when(
    education %in% c("大专", "高中及以下") ~ "大专或以下", 
    TRUE ~ education
  ))

# 全局配置 ----
# 行为变量映射（前后对比）
behavior_prepost <- list(
  "Public Transportation" = c(pre = "q16_pre_public_trans", post = "q18_change_public_trans"),
  "Cycling or walking" = c(pre = "q16_pre_bike_walk", post = "q18_change_bike_walk"),
  "Turn Off Power" = c(pre = "q16_pre_turn_off_power", post = "q18_change_turn_off_power"),
  "Garbage Sorting" = c(pre = "q16_pre_garbage_sort", post = "q18_change_garbage_sort"),
  "Reusable Bags" = c(pre = "q16_pre_reusable_bag", post = "q18_change_reusable_bag"),
  "Energy-Efficient Products" = c(pre = "q16_pre_choose_energy_eff", post = "q18_change_choose_energy_eff")
)

# 行为变量映射（三组对比：使用前、使用后、非用户）
behavior_three <- list(
  "Public Transportation" = list(pre = "q16_pre_public_trans", post = "q18_change_public_trans", non = "q17_usual_public_trans"),
  "Cycling or walking" = list(pre = "q16_pre_bike_walk", post = "q18_change_bike_walk", non = "q17_usual_bike_walk"),
  "Turn Off Power" = list(pre = "q16_pre_turn_off_power", post = "q18_change_turn_off_power", non = "q17_usual_turn_off_power"),
  "Garbage Sorting" = list(pre = "q16_pre_garbage_sort", post = "q18_change_garbage_sort", non = "q17_usual_garbage_sort"),
  "Reusable Bags" = list(pre = "q16_pre_reusable_bag", post = "q18_change_reusable_bag", non = "q17_usual_reusable_bag"),
  "Energy-Efficient Products" = list(pre = "q16_pre_choose_energy_eff", post = "q18_change_choose_energy_eff", non = "q17_usual_choose_energy_eff")
)

# 人口学分组变量
demo_groups <- c(
  "gender" = "Gender",
  "age" = "Age",
  "marital_status" = "Marital",
  "education" = "Education"
)

demo_groups_app <- c(demo_groups, "q1_used_app" = "APP Usage")

# Q4/Q20 感知变量
q4_vars <- c(
  "q4_ui_simple" = "1. Simple Interface",
  "q4_integrate_platform" = "2. Platform Integration",
  "q4_auto_record" = "3. Auto Data Recording",
  "q4_clear_guidance" = "4. Clear Guidance",
  "q4_raise_awareness" = "5. Raise Awareness",
  "q4_info_feedback_useful" = "6. Useful Feedback",
  "q4_quicker_green_choice" = "7. Faster Green Choices",
  "q4_know_carbon_credit" = "8. Understand Carbon Credit"
)

q20_vars <- c(
  "q20_ranking_motivation" = "11. Ranking Motivation",
  "q20_image_inspire" = "12. Visual Engagement",
  "q20_cumulative_inspire" = "13. Cumulative Progress",
  "q20_celeb_endorsement" = "9. Celebrity Endorsement",
  "q20_video_intro" = "10. Video Introduction"
)

q4q20_all <- c(q4_vars, q20_vars)

# Q13/Q14/Q15 多选题变量
q13_attract <- c(
  "q13_attract_points" = "Points Exchange",
  "q13_attract_credit" = "Green Credit",
  "q13_attract_daily_task" = "Daily Tasks",
  "q13_attract_ranking" = "Rankings/Badges",
  "q13_attract_game_fun" = "Entertainment",
  "q13_attract_viz_carbon" = "Carbon Visualization"
)

q14_reward <- "q14_desired_reward"

q15_barrier <- c(
  "q15_barrier_trouble" = "Complex Operation",
  "q15_barrier_privacy" = "Privacy Concerns",
  "q15_barrier_low_reward" = "Low Rewards",
  "q15_barrier_unknown" = "Unaware of APP"
)

# 作图设置。
pub_theme <- theme_bw(base_size = 18, base_family = "sans") +
  theme(
    plot.title = element_text(size = rel(0.8)),
    axis.title = element_text(face = "bold", size = rel(0.9)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey92"),
    legend.title = element_text(face = "bold", size = rel(0.8)),
    legend.text = element_text(size = 13),
    legend.key.size = unit(0.5, "cm"),
    strip.background = element_rect(fill = "grey95"),
    strip.text = element_text(face = "bold", size = rel(0.8))
  )

# PART 1: 桑基图 - APP使用前后行为变化流向
cat("PART 1: Sankey Diagram - Behavior Change Flow\n")

plot_sankey <- function(data, pre_col, post_col, label, print_table = FALSE) {
  # 统一因子水平 1-5
  lvls <- as.character(1:5)

  d <- data %>%
    select(Pre = all_of(pre_col), Post = all_of(post_col)) %>%
    # 显式转换为带统一水平的因子，确保排序一致
    mutate(
      Pre = factor(as.character(as.numeric(Pre)), levels = lvls),
      Post = factor(as.character(as.numeric(Post)), levels = lvls)
    ) %>%
    filter(!is.na(Pre) & !is.na(Post)) %>%
    count(Pre, Post, name = "Count") %>%
    make_long(Pre, Post, value = Count)

  # 打印节点百分比表格
  if (print_table) {
    node_pct <- d %>%
      filter(!is.na(node)) %>%
      group_by(x, node) %>%
      summarise(total = sum(value), .groups = "drop") %>%
      group_by(x) %>%
      mutate(pct = sprintf("%.1f%%", total / sum(total) * 100)) %>%
      select(Stage = x, Score = node, N = total, Percent = pct) %>%
      arrange(Stage, Score)
    cat("\n", label, "\n")
    print(knitr::kable(node_pct, format = "simple"))
  }

  # 提取分数用于着色
  d$score <- factor(d$node, levels = lvls)

  ggplot(d, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = score, value = value)) +
    geom_sankey(flow.alpha = 0.5, node.color = "gray30", width = 0.35, linewidth = 0.1) +
    scale_fill_manual(values = c("1"="#D32F2F","2"="#F57C00","3"="#FDD835","4"="#66BB6A","5"="#2E7D32"),
                      name = "Score", labels = lvls) +
    scale_x_discrete(labels = c("Pre" = "Before", "Post" = "After"), expand = c(0.05, 0.05)) +
    scale_y_continuous(expand = c(0.02, 0.02)) +
    labs(title = label, x = NULL, y = NULL) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 10),
      axis.text.x = element_text(size = 9),
      plot.margin = margin(2, 2, 2, 2)
    )
}

sankey_list <- lapply(seq_along(names(behavior_prepost)), function(i) {
  b <- names(behavior_prepost)[i]
  label <- paste0("(", letters[i], ")")
  plot_sankey(app_users, behavior_prepost[[b]][["pre"]], behavior_prepost[[b]][["post"]], label, print_table = TRUE)
})

cat("\n--- Sankey Diagram Legend ---\n")
cat(paste(sprintf("(%s) %s", letters[seq_along(names(behavior_prepost))], names(behavior_prepost)), collapse = "\n"), "\n")

sankey_list[[length(sankey_list)]] <- sankey_list[[length(sankey_list)]] + theme(legend.position = "right")

p_sankey <- Reduce(`+`, sankey_list) + plot_layout(ncol = 2, guides = "collect")
print(p_sankey)
png(
  "data_proc/sankey_behavior_change.png",
  width = 16, height = 20, units = "cm", res = 300
)
p_sankey
dev.off()

# PART 1.5: 详细的Sankey流向分析 ----
cat("\n\n", strrep("#", 80), "\n")
cat("PART 1.5: Detailed Sankey Flow Analysis\n")
cat("(Left distribution, Right distribution, Flow-out %, Flow-in %)\n")
cat(strrep("#", 80), "\n")

# 函数：计算详细的流向统计
calc_sankey_detailed <- function(data, pre_col, post_col, behavior_name) {

  # 1. 清理数据
  lvls <- as.character(1:5)
  d <- data %>%
    select(Pre = all_of(pre_col), Post = all_of(post_col)) %>%
    mutate(
      Pre = factor(as.character(as.numeric(Pre)), levels = lvls),
      Post = factor(as.character(as.numeric(Post)), levels = lvls)
    ) %>%
    filter(!is.na(Pre) & !is.na(Post)) %>%
    mutate(Pre = as.numeric(as.character(Pre)), Post = as.numeric(as.character(Post)))

  # 2. 左边各节点占比
  pre_dist <- d %>%
    group_by(Pre) %>%
    summarise(Count = n(), .groups = "drop") %>%
    mutate(
      Total = sum(Count),
      Proportion = Count / Total,
      Percent = sprintf("%.2f%%", Proportion * 100)
    ) %>%
    rename("Score" = Pre, "Pre_Count" = Count) %>%
    select(Score, Pre_Count, Total, Proportion, Percent)

  # 3. 右边各节点占比
  post_dist <- d %>%
    group_by(Post) %>%
    summarise(Count = n(), .groups = "drop") %>%
    mutate(
      Total = sum(Count),
      Proportion = Count / Total,
      Percent = sprintf("%.2f%%", Proportion * 100)
    ) %>%
    rename("Score" = Post, "Post_Count" = Count) %>%
    select(Score, Post_Count, Total, Proportion, Percent)

  # 4. 流出百分比：左边每个节点流向右边各节点的比例
  flow_out <- d %>%
    group_by(Pre, Post) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(Pre) %>%
    mutate(
      Total_Pre = sum(Count),
      Flow_Proportion = Count / Total_Pre,
      Flow_Percent = sprintf("%.2f%%", Flow_Proportion * 100)
    ) %>%
    rename("From_Score" = Pre, "To_Score" = Post) %>%
    select(From_Score, To_Score, Count, Total_Pre, Flow_Proportion, Flow_Percent) %>%
    arrange(From_Score, To_Score)

  # 创建流出矩阵（5x5矩阵，行=From，列=To）
  flow_out_matrix <- d %>%
    group_by(Pre, Post) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(Pre) %>%
    mutate(Flow_Percent = sprintf("%.2f%%", Count / sum(Count) * 100)) %>%
    select(Pre, Post, Flow_Percent) %>%
    pivot_wider(names_from = Post, values_from = Flow_Percent, values_fill = "0.00%") %>%
    rename("From" = Pre) %>%
    # 确保所有分数都存在（1-5）
    {
      all_scores <- 1:5
      missing_rows <- setdiff(all_scores, .$From)
      if (length(missing_rows) > 0) {
        missing_data <- data.frame(From = missing_rows)
        for (col in setdiff(names(.), "From")) {
          missing_data[[col]] <- "0.00%"
        }
        bind_rows(., missing_data)
      } else {
        .
      }
    } %>%
    arrange(From) %>%
    # 确保列的顺序是1-5
    {
      expected_cols <- c("From", as.character(1:5))
      existing_cols <- names(.)
      cols_to_keep <- intersect(expected_cols, existing_cols)
      select(., all_of(cols_to_keep))
    } %>%
    as.data.frame()

  # 5. 流入百分比：右边每个节点来自左边各节点的比例
  flow_in <- d %>%
    group_by(Pre, Post) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(Post) %>%
    mutate(
      Total_Post = sum(Count),
      Flow_Proportion = Count / Total_Post,
      Flow_Percent = sprintf("%.2f%%", Flow_Proportion * 100)
    ) %>%
    rename("From_Score" = Pre, "To_Score" = Post) %>%
    select(From_Score, To_Score, Count, Total_Post, Flow_Proportion, Flow_Percent) %>%
    arrange(To_Score, From_Score)

  # 创建流入矩阵（5x5矩阵，行=To，列=From）
  flow_in_matrix <- d %>%
    group_by(Pre, Post) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(Post) %>%
    mutate(Flow_Percent = sprintf("%.2f%%", Count / sum(Count) * 100)) %>%
    select(Pre, Post, Flow_Percent) %>%
    pivot_wider(names_from = Pre, values_from = Flow_Percent, values_fill = "0.00%") %>%
    rename("To" = Post) %>%
    # 确保所有分数都存在（1-5）
    {
      all_scores <- 1:5
      missing_rows <- setdiff(all_scores, .$To)
      if (length(missing_rows) > 0) {
        missing_data <- data.frame(To = missing_rows)
        for (col in setdiff(names(.), "To")) {
          missing_data[[col]] <- "0.00%"
        }
        bind_rows(., missing_data)
      } else {
        .
      }
    } %>%
    arrange(To) %>%
    # 确保列的顺序是1-5
    {
      expected_cols <- c("To", as.character(1:5))
      existing_cols <- names(.)
      cols_to_keep <- intersect(expected_cols, existing_cols)
      select(., all_of(cols_to_keep))
    } %>%
    as.data.frame()

  # 计算从低频(1-3)转化到高频(4-5)的人数
  conversion_count <- sum((d$Pre <= 3 & d$Post >= 4))

  # 计算高频用户占比的变化
  pre_high_pct <- sum(d$Pre >= 4) / nrow(d)
  post_high_pct <- sum(d$Post >= 4) / nrow(d)
  high_pct_change <- post_high_pct - pre_high_pct

  # 计算从分数3转化到4或5的比例
  mid_users <- sum(d$Pre == 3)
  if (mid_users > 0) {
    mid_to_high_conversion <- sum((d$Pre == 3 & d$Post >= 4)) / mid_users
  } else {
    mid_to_high_conversion <- NA
  }

  # 计算从分数1或2转化到3、4或5的比例
  low_users <- sum(d$Pre <= 2)
  if (low_users > 0) {
    low_to_mid_high_conversion <- sum((d$Pre <= 2 & d$Post >= 3)) / low_users
  } else {
    low_to_mid_high_conversion <- NA
  }

  # 返回包含所有统计的列表
  return(list(
    behavior = behavior_name,
    n_total = nrow(d),
    conversion_low_to_high = conversion_count,
    pre_high_pct = pre_high_pct,
    post_high_pct = post_high_pct,
    high_pct_change = high_pct_change,
    mid_to_high_conversion = mid_to_high_conversion,
    low_to_mid_high_conversion = low_to_mid_high_conversion,
    pre_distribution = pre_dist,
    post_distribution = post_dist,
    flow_out_table = flow_out,
    flow_out_matrix = flow_out_matrix,
    flow_in_table = flow_in,
    flow_in_matrix = flow_in_matrix
  ))
}

# 为所有行为变量计算详细统计
cat("\nCalculating detailed Sankey analysis for all behaviors...\n")
sankey_detailed_list <- lapply(names(behavior_prepost), function(b) {
  cat("  -", b, "\n")
  calc_sankey_detailed(
    app_users,
    behavior_prepost[[b]][["pre"]],
    behavior_prepost[[b]][["post"]],
    b
  )
})

# 给列表添加名称，方便按行为名称访问
names(sankey_detailed_list) <- names(behavior_prepost)

cat("\n✓ Detailed analysis complete!\n")
cat("\nAccess results using: sankey_detailed_list$'Behavior Name'\n")

# 打印每个行为的摘要
print_sankey_summary <- function(result) {
  cat("\n", strrep("-", 80), "\n")
  cat("Behavior:", result$behavior, "\n")
  cat("Total respondents (with valid pre & post):", result$n_total, "\n")
  cat(strrep("-", 80), "\n")

  cat("\n[1] LEFT SIDE (BEFORE) DISTRIBUTION:\n")
  print(knitr::kable(result$pre_distribution, format = "simple"))

  cat("\n[2] RIGHT SIDE (AFTER) DISTRIBUTION:\n")
  print(knitr::kable(result$post_distribution, format = "simple"))

  cat("\n[3] FLOW OUT MATRIX (% of 'From' that flows to each 'To'):\n")
  print(knitr::kable(result$flow_out_matrix, format = "simple"))

  cat("\n[4] FLOW IN MATRIX (% of 'To' that comes from each 'From'):\n")
  print(knitr::kable(result$flow_in_matrix, format = "simple"))
}

# 打印所有行为的详细摘要
invisible(lapply(sankey_detailed_list, print_sankey_summary))

## APP使用前后行为变化 ----
### 整体分析 ----
# 函数：导出表格并打印。
exp_print_tbl <- function(tbl_x, exp_tbl_title_x) {
  # 打印表格。
  kable(tbl_x, digits = 2, format = "simple") %>% print()
  # 导出结果。
  write.csv(tbl_x, paste0("data_proc/", exp_tbl_title_x))
}

# 描述性统计。
exp_print_tbl(
  lapply(names(behavior_prepost), function(b) {
    app_users %>%
      select(Pre = all_of(behavior_prepost[[b]][["pre"]]), Post = all_of(behavior_prepost[[b]][["post"]])) %>%
      mutate(across(everything(), as.numeric)) %>%
      pivot_longer(everything(), names_to = "Period", values_to = "Score") %>%
      filter(!is.na(Score)) %>%
      group_by(Period) %>%
      summarise(N = n(), Mean = mean(Score), SD = sd(Score), Q1 = quantile(Score, 0.25),
                Median = median(Score), Q3 = quantile(Score, 0.75), .groups = "drop") %>%
      mutate(Behavior = b, Period = factor(ifelse(Period == "Pre", "Before", "After"), c("Before", "After")))
  }) %>% bind_rows() %>% arrange(Behavior, Period) %>%
    select(Behavior, Period, N, Mean, SD, Q1, Median, Q3), 
  "使用前后行为描述性分析.csv"
)

# 整体Wilcoxon检验。
exp_print_tbl(
  lapply(names(behavior_prepost), function(b) {
    d <- app_users %>%
      select(Pre = all_of(behavior_prepost[[b]][["pre"]]), Post = all_of(behavior_prepost[[b]][["post"]])) %>%
      mutate(across(everything(), as.numeric)) %>% filter(!is.na(Pre) & !is.na(Post)) %>%
      mutate(Diff = Post - Pre)
    test <- wilcox.test(d$Post, d$Pre, paired = TRUE, alternative = "greater", exact = FALSE)
    data.frame(Behavior = b, N = nrow(d), Before = round(mean(d$Pre), 2), After = round(mean(d$Post), 2),
               Diff = round(mean(d$Diff), 3), Cohen_d = round(mean(d$Diff)/sd(d$Diff), 3),
               P_value = test$p.value, Sig = ifelse(test$p.value < 0.001, "***", ifelse(test$p.value < 0.01, "**", ifelse(test$p.value < 0.05, "*", "ns"))))
  }) %>% bind_rows() %>% mutate(P_value = format.pval(P_value, 3)), 
  "行为变化_整体.csv"
)


### 分群体分析 ----
# 预处理：合并教育分组
# 教育分组英文映射
edu_map <- c("大专或以下" = "Below Bachelor", "本科" = "Bachelor", "硕士或博士" = "Master's or Doctoral")
gender_map <- c("男" = "Male", "女" = "Female")
marital_map <- c("已婚" = "Married", "独居" = "Single/Divorced/Widowed")
demo_groups_edu <- c("gender" = "Gender", "age" = "Age", "marital_status" = "Marital", "education" = "Education")

behavior_by_group <- lapply(names(behavior_prepost), function(b) {
  lapply(names(demo_groups_edu), function(gvar) {
    d <- app_users %>%
      filter(!is.na(.data[[gvar]])) %>%
      select(
        grp = all_of(gvar), 
        Pre = all_of(behavior_prepost[[b]][["pre"]]), 
        Post = all_of(behavior_prepost[[b]][["post"]])
      ) %>%
      mutate(across(c(Pre, Post), as.numeric)) %>% 
      filter(!is.na(Pre) & !is.na(Post)) %>%
      mutate(Diff = Post - Pre)

    stats <- d %>% group_by(grp) %>%
      summarise(
        N = n(), 
        Pre_Mean = round(mean(Pre), 2), Pre_SD = round(sd(Pre), 2),
        Post_Mean = round(mean(Post), 2), Post_SD = round(sd(Post), 2), 
        Diff_Mean = round(mean(Diff), 3),
        # 四分位数。
        Q1 = quantile(Diff, 0.25), 
        Median = median(Diff), 
        Q3 = quantile(Diff, 0.75),
        p_within = tryCatch(wilcox.test(Post, Pre, paired = TRUE, alternative = "greater", exact = FALSE)$p.value, error = function(e) NA_real_),
        V_stat = tryCatch(wilcox.test(Post, Pre, paired = TRUE, alternative = "greater", exact = FALSE)$statistic, error = function(e) NA_real_),
        .groups = "drop"
      ) %>%
      mutate(Within = case_when(is.na(p_within) ~ "NA", p_within < 0.001 ~ "***", p_within < 0.01 ~ "**", p_within < 0.05 ~ "*", TRUE ~ "ns"))

    n_grp <- n_distinct(d$grp)
    p_btw <- if (n_grp == 2) tryCatch(wilcox.test(Diff ~ grp, data = d, exact = FALSE)$p.value, error = function(e) NA)
             else if (n_grp > 2) tryCatch(kruskal.test(Diff ~ grp, data = d)$p.value, error = function(e) NA) else NA

    stats %>% mutate(Behavior = b, Dimension = demo_groups_edu[gvar], p_between = p_btw,
                     Between = case_when(is.na(p_btw) ~ "NA", p_btw < 0.001 ~ "***", p_btw < 0.01 ~ "**", p_btw < 0.05 ~ "*", TRUE ~ "ns"))
  }) %>% bind_rows()
}) %>% 
  bind_rows()

# 翻译群体名称
behavior_by_group <- behavior_by_group %>%
  mutate(grp_en = case_when(
    Dimension == "Gender" ~ gender_map[grp],
    Dimension == "Marital" ~ marital_map[grp],
    Dimension == "Education" ~ edu_map[grp],
    TRUE ~ grp
  ))

for (dim in unique(behavior_by_group$Dimension)) {
  cat("\n  [", dim, "]\n", sep = "")
  behavior_by_group %>% filter(Dimension == dim) %>%
    select(Behavior, Group = grp_en, N, Pre_Mean, Pre_SD, Post_Mean, Post_SD, Diff_Mean, Q1, Median, Q3, Within, Between) %>%
    kable(digits = 2, format = "simple") %>% print()
}

# 2.4 热图
behavior_by_group <- behavior_by_group %>%
  mutate(label_text = paste0("V=", round(V_stat, 0), "\n", Within))

# 函数：打印并保存图片。
exp_print_fig <- function(fig_x, file_name_x, ...) {
  # 打印图片。
  print(fig_x)
  # 保存图片。
  png(
    paste0("data_proc/", file_name_x), 
    units = "cm", res = 300, ...
  )
  print(fig_x)
  dev.off()
}
# 分群体统计比较结果热力图。
exp_print_fig(
  behavior_by_group %>%
    mutate(label_text = paste0(round(V_stat, 0), "\n", Within)) %>%
    mutate(Behavior = factor(Behavior, levels = names(behavior_prepost)),
           Dimension = factor(Dimension, levels = c("Gender", "Age", "Marital", "Education"))) %>%
    ggplot(aes(x = Behavior, y = grp_en, fill = (Within != "ns" & Within != "NA"))) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = label_text), size = 3.5, lineheight = 0.85) +
    facet_grid(Dimension ~ ., scales = "free_y", space = "free_y") +
    scale_fill_manual(values = c("TRUE" = "#4CAF50", "FALSE" = "#E0E0E0"), guide = "none") +
    labs(x = "Behavior", y = "Demographic Group") +
    pub_theme +
    theme(axis.text.x = element_text(hjust = 1, angle = 30, size = rel(0.8)),
          strip.text = element_text(face = "bold"),
          panel.grid = element_blank()),
  "行为变化_分群体_热力图.png",
  width = 18, height = 14
)

# 新增：分群体行为均值热力图
exp_print_fig(
  behavior_by_group %>%
    mutate(Behavior = factor(Behavior, levels = names(behavior_prepost)),
           Dimension = factor(Dimension, levels = c("Gender", "Age", "Marital", "Education"))) %>%
    ggplot(aes(x = Behavior, y = grp_en, fill = Post_Mean)) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = round(Post_Mean, 2)), size = 3, color = "black") +
    facet_wrap(~ Dimension, scales = "free_y", ncol = 1) +
    scale_fill_viridis_c(
      name = "Mean Score",
      breaks = c(1, 2, 3, 4, 5),
      limits = c(1, 5),
      option = "plasma"
    ) +
    labs(x = "Behavior", y = "Demographic Group", title = "Behavior Change by Demographic Group (Post-Adoption Mean Scores)") +
    pub_theme +
    theme(axis.text.x = element_text(hjust = 1, angle = 30, size = rel(0.8)),
          strip.text = element_text(face = "bold"),
          panel.grid = element_blank()),
  "行为变化_分群体_均值热力图.png",
  width = 18, height = 16
)

# PART 3: 三组对比（使用前 vs 使用后 vs 非用户）
three_levels <- c("Before", "After", "Non-User")

# 3.1 描述性统计
three_desc <- lapply(names(behavior_three), function(b) {
  cols <- behavior_three[[b]]
  bind_rows(
    data.frame(Group = "Before", score = as.numeric(data$q1_used_app == 1) * as.numeric(data[[cols$pre]])) %>% filter(score > 0),
    data.frame(Group = "After", score = as.numeric(data$q1_used_app == 1) * as.numeric(data[[cols$post]])) %>% filter(score > 0),
    data.frame(Group = "Non-User", score = as.numeric(data$q1_used_app != 1) * as.numeric(data[[cols$non]])) %>% filter(score > 0)
  ) %>% group_by(Group) %>%
    summarise(N = n(), Mean = mean(score), SD = sd(score), Q1 = quantile(score, 0.25),
              Median = median(score), Q3 = quantile(score, 0.75), .groups = "drop") %>%
    mutate(Behavior = b, Group = factor(Group, three_levels))
}) %>% bind_rows() %>% arrange(Behavior, Group)

three_desc %>% select(Behavior, Group, N, Mean, SD, Q1, Median, Q3) %>% kable(digits = 2, format = "simple") %>% print()

# 3.2 两两比较
three_pairwise <- lapply(names(behavior_three), function(b) {
  cols <- behavior_three[[b]]
  scores <- list(
    "Before" = na.omit(as.numeric(data[[cols$pre]][data$q1_used_app == 1])),
    "After" = na.omit(as.numeric(data[[cols$post]][data$q1_used_app == 1])),
    "Non-User" = na.omit(as.numeric(data[[cols$non]][data$q1_used_app != 1]))
  )
  lapply(list(c("Before","After"), c("Before","Non-User"), c("After","Non-User")), function(pair) {
    test <- tryCatch(wilcox.test(scores[[pair[1]]], scores[[pair[2]]], exact = FALSE), error = function(e) NULL)
    p <- if (!is.null(test)) test$p.value else NA
    W <- if (!is.null(test)) test$statistic else NA
    p_adj <- min(p * 3, 1)
    data.frame(Behavior = b, Comparison = paste(pair, collapse = " vs "), W_stat = W,
               p_raw = p, p_adj = p_adj,
               Sig = case_when(is.na(p_adj) ~ "NA", p_adj < 0.001 ~ "***", p_adj < 0.01 ~ "**", p_adj < 0.05 ~ "*", TRUE ~ "ns"),
               Is_Sig = !is.na(p_adj) & p_adj < 0.05)
  }) %>% bind_rows()
}) %>% bind_rows()

three_pairwise %>% mutate(p_raw = format.pval(p_raw, 3), p_adj = format.pval(p_adj, 3), W_stat = round(W_stat, 0)) %>%
  select(Behavior, Comparison, W_stat, p_raw, p_adj, Sig) %>% kable(format = "simple") %>% print()

# 保存表格
write.csv(three_pairwise %>%
            mutate(
              # 行为名称大小写转换
              Behavior = case_when(
                Behavior == "Public Transportation" ~ "Public transportation",
                Behavior == "Cycling or walking" ~ "Cycling or walking",
                Behavior == "Turn Off Power" ~ "Turn off power",
                Behavior == "Garbage Sorting" ~ "Garbage sorting",
                Behavior == "Reusable Bags" ~ "Reusable bags",
                Behavior == "Energy-Efficient Products" ~ "Energy-efficient products",
                TRUE ~ Behavior
              ),
              # Comparison中的"Non-User"改为"Non-user"
              Comparison = str_replace(Comparison, "Non-User", "Non-user"),
              # 数值转换
              p_raw = format.pval(p_raw, 3),
              p_adj = format.pval(p_adj, 3),
              W_stat = round(W_stat, 0),
              # 创建新列：W_stat_Sig（例如"15271**"）
              W_stat_Sig = paste0(W_stat, Sig)
            ) %>%
            select(Behavior, Comparison, W_stat, Sig, W_stat_Sig, p_raw, p_adj),
          "data_proc/three_group_comparison_table.csv", row.names = FALSE)

# 创建article版本的三组对比表格（每行一个行为）
three_pairwise_article <- three_pairwise %>%
  mutate(
    # 行为名称标准化
    Behavior = case_when(
      Behavior == "Public Transportation" ~ "Public transportation",
      Behavior == "Cycling or walking" ~ "Cycling or walking",
      Behavior == "Turn Off Power" ~ "Turn off power",
      Behavior == "Garbage Sorting" ~ "Garbage sorting",
      Behavior == "Reusable Bags" ~ "Reusable bags",
      Behavior == "Energy-Efficient Products" ~ "Energy-efficient products",
      TRUE ~ Behavior
    ),
    # Comparison标准化
    Comparison = str_replace(Comparison, "Non-User", "Non-user"),
    # W_stat_Sig列
    W_stat_Sig = paste0(round(W_stat, 0),
                        case_when(is.na(p_adj) ~ "NA",
                                  p_adj < 0.001 ~ "***",
                                  p_adj < 0.01 ~ "**",
                                  p_adj < 0.05 ~ "*",
                                  TRUE ~ "ns")),
    # 行为因子化以保持顺序
    Behavior = factor(Behavior, levels = c("Public transportation", "Cycling or walking",
                                           "Turn off power", "Garbage sorting",
                                           "Reusable bags", "Energy-efficient products"))
  ) %>%
  select(Behavior, Comparison, W_stat_Sig) %>%
  pivot_wider(
    names_from = Comparison,
    values_from = W_stat_Sig,
    names_sort = FALSE
  ) %>%
  # 重新排列列的顺序
  select(Behavior, `Before vs After`, `Before vs Non-user`, `After vs Non-user`) %>%
  # 转换Behavior为字符（保持因子排序）
  mutate(Behavior = as.character(Behavior)) %>%
  arrange(match(Behavior, c("Public transportation", "Cycling or walking",
                            "Turn off power", "Garbage sorting",
                            "Reusable bags", "Energy-efficient products")))

write.csv(three_pairwise_article, "data_proc/three_group_comparison_table_article.csv", row.names = FALSE)

# 3.3 热图
exp_print_fig(
  three_pairwise %>%
    mutate(Behavior = factor(Behavior, names(behavior_three)),
           Comparison = factor(
             Comparison,
             c("Before vs After", "Before vs Non-User", "After vs Non-User")
           ),
           label_text = paste0(round(W_stat, 0), "\n", Sig)) %>%
    ggplot(aes(x = Behavior, y = Comparison, fill = Is_Sig)) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = label_text), size = 3.5, lineheight = 0.85) +
    scale_fill_manual(
      values = c("TRUE" = "#4CAF50", "FALSE" = "#EEEEEE"), guide = "none"
    ) +
    labs(x = "Behavior", y = "Comparison") +
    pub_theme +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),
      panel.grid = element_blank()
    ),
  "用户使用前后和非用户对比.png",
  width = 14, height = 8
)


# PART 4: Q4/Q20 APP感知与使用因素分析
cat("\n\n", strrep("#", 80), "\n")
cat("PART 4: Q4/Q20 APP Perception Analysis\n")
cat(strrep("#", 80), "\n")

# 4.1 整体描述性统计
cat("\n--- 4.1 Descriptive Statistics (Overall) ---\n")
data %>% select(all_of(names(q4q20_all))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "score") %>%
  mutate(score = as.numeric(score)) %>% filter(!is.na(score)) %>%
  group_by(var) %>%
  summarise(N = n(), Mean = mean(score), SD = sd(score), Q1 = quantile(score, 0.25),
            Median = median(score), Q3 = quantile(score, 0.75), .groups = "drop") %>%
  mutate(Item = q4q20_all[var]) %>% arrange(match(var, names(q4q20_all))) %>%
  select(Item, N, Mean, SD, Median, Q1, Q3) %>% kable(digits = 2, format = "simple") %>% print()

# 4.2 分群体统计
cat("\n--- 4.2 Statistics by Groups ---\n")
for (i in seq_along(demo_groups_app)) {
  gvar <- names(demo_groups_app)[i]
  cat("\n  [", demo_groups_app[i], "]\n", sep = "")
  data %>% select(grp = all_of(gvar), all_of(names(q4q20_all))) %>% filter(!is.na(grp)) %>%
    pivot_longer(-grp, names_to = "var", values_to = "score") %>% mutate(score = as.numeric(score)) %>% filter(!is.na(score)) %>%
    group_by(Group = grp, var) %>%
    summarise(N = n(), Mean = mean(score), SD = sd(score), Q1 = quantile(score, 0.25),
              Median = median(score), Q3 = quantile(score, 0.75), .groups = "drop") %>%
    mutate(Item = q4q20_all[var]) %>% arrange(match(var, names(q4q20_all)), Group) %>%
    select(Item, Group, N, Mean, SD, Q1, Median, Q3) %>% kable(digits = 2, format = "simple") %>% print()
}

# 4.3 显著性热图
cat("\n--- 4.3 Significance Heatmap ---\n")
q4q20_tests <- expand.grid(gvar = names(demo_groups_app), qvar = names(q4q20_all), stringsAsFactors = FALSE) %>%
  rowwise() %>% mutate(
    result = list({
      d <- data %>% select(g = all_of(gvar), s = all_of(qvar)) %>% filter(!is.na(g) & !is.na(s)) %>% mutate(s = as.numeric(s))
      n_grp <- n_distinct(d$g)
      if (n_grp < 2 || nrow(d) < 10) {
        list(p = NA_real_, stat = NA_real_, test_type = "NA")
      } else if (n_grp == 2) {
        test <- suppressWarnings(wilcox.test(s ~ g, data = d))
        list(p = test$p.value, stat = test$statistic, test_type = "W")
      } else {
        test <- suppressWarnings(kruskal.test(s ~ g, data = d))
        list(p = test$p.value, stat = test$statistic, test_type = "H")
      }
    }),
    p.value = result$p,
    stat_value = result$stat,
    test_type = result$test_type
  ) %>% ungroup() %>%
  mutate(Dimension = factor(demo_groups_app[gvar], demo_groups_app),
         Item = factor(q4q20_all[qvar], q4q20_all),
         Sig = case_when(is.na(p.value) ~ "NA", p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", p.value < 0.05 ~ "*", TRUE ~ "ns"),
         label_text = ifelse(is.na(stat_value), "NA", paste0(test_type, "=", round(stat_value, 1), "\n", Sig)))

p3 <- q4q20_tests %>%
  ggplot(aes(x = Item, y = Dimension, fill = (Sig != "ns" & Sig != "NA"))) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = label_text), size = 3, lineheight = 0.85) +
  scale_fill_manual(values = c("TRUE" = "#4CAF50", "FALSE" = "#E0E0E0"), guide = "none") +
  labs(title = "(i)", x = "Perception Item", y = "Dimension") +
  pub_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())
print(p3)
ggsave("data_proc/q4_q20_group_diff_heatmap.png", p3, width = 32, height = 12, units = "cm", dpi = 300, bg = "white")


# ##############################################################################
# PART 5: Q13/Q14/Q15 多选题分析
# ##############################################################################

cat("\n\n", strrep("#", 80), "\n")
cat("PART 5: Q13/Q14/Q15 Multi-Select Analysis\n")
cat(strrep("#", 80), "\n")

# 辅助函数
calc_multi_freq <- function(data, labels) {
  data %>% select(all_of(names(labels))) %>%
    summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "var", values_to = "Count") %>%
    mutate(Option = labels[var], Total = nrow(data), Proportion = Count / Total) %>%
    arrange(desc(Count)) %>% select(Option, Count, Total, Proportion)
}

calc_multi_by_group <- function(data, labels, gvar) {
  data %>% filter(!is.na(.data[[gvar]])) %>% group_by(grp = .data[[gvar]]) %>%
    summarise(n = n(), across(all_of(names(labels)), ~ sum(. == 1, na.rm = TRUE) / n()), .groups = "drop") %>%
    pivot_longer(-c(grp, n), names_to = "var", values_to = "Proportion") %>%
    mutate(Option = labels[var], grp = as.character(grp))
}

test_multi_by_group <- function(data, labels, gvar) {
  lapply(names(labels), function(v) {
    d <- data %>% filter(!is.na(.data[[gvar]]) & !is.na(.data[[v]])) %>%
      mutate(sel = as.factor(.data[[v]]), grp = as.factor(.data[[gvar]]))
    if (n_distinct(d$grp) < 2 || n_distinct(d$sel) < 2) return(data.frame(var = v, p.value = NA_real_, stat = NA_real_))
    tryCatch({
      test <- chisq.test(table(d$grp, d$sel))
      data.frame(var = v, p.value = test$p.value, stat = test$statistic)
    }, error = function(e) {
      tryCatch({
        test <- fisher.test(table(d$grp, d$sel), simulate.p.value = TRUE)
        data.frame(var = v, p.value = test$p.value, stat = NA_real_)
      }, error = function(e2) { data.frame(var = v, p.value = NA_real_, stat = NA_real_) })
    })
  }) %>% bind_rows() %>% mutate(Option = labels[var])
}

cochran_q <- function(data, vars) {
  mat <- data %>% select(all_of(vars)) %>% mutate(across(everything(), ~ as.numeric(. == 1))) %>% na.omit() %>% as.matrix()
  if (nrow(mat) < 10) return(list(Q = NA, p = NA, n = nrow(mat), df = NA))
  k <- ncol(mat); t_tot <- sum(mat)
  q_stat <- (k - 1) * (k * sum(colSums(mat)^2) - t_tot^2) / (k * t_tot - sum(rowSums(mat)^2))
  list(Q = q_stat, p = 1 - pchisq(q_stat, k - 1), n = nrow(mat), df = k - 1)
}

# 5.1 Q13 吸引点
cat("\n--- 5.1 Q13 Attraction Factors ---\n")
calc_multi_freq(data, q13_attract) %>% mutate(Proportion = sprintf("%.1f%%", Proportion * 100)) %>% kable(format = "simple") %>% print()

q13_coch <- cochran_q(data, names(q13_attract))
cat(sprintf("Cochran's Q = %.2f, df = %d, p = %.4f, n = %d\n", q13_coch$Q, q13_coch$df, q13_coch$p, q13_coch$n))

q13_by_group <- lapply(names(demo_groups_app), function(gvar) {
  calc_multi_by_group(data, q13_attract, gvar) %>%
    left_join(test_multi_by_group(data, q13_attract, gvar) %>% select(var, p.value, stat), by = "var") %>%
    mutate(Dimension = demo_groups_app[gvar])
}) %>% 
  bind_rows()

q13_tests <- q13_by_group %>% select(Dimension, Option, p.value, stat) %>% distinct() %>%
  mutate(Sig = case_when(is.na(p.value) ~ "NA", p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", p.value < 0.05 ~ "*", TRUE ~ "ns"),
         Dimension = factor(Dimension, demo_groups_app), Option = factor(Option, q13_attract),
         label_text = ifelse(is.na(stat), Sig, paste0("X2=", round(stat, 1), "\n", Sig)))

p_q13_sig <- q13_tests %>%
  ggplot(aes(x = Option, y = Dimension, fill = (Sig != "ns" & Sig != "NA"))) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = label_text), size = 15) +
  scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "#E0E0E0"), guide = "none") +
  labs(title = "(a)", x = NULL, y = "Dimension") +
  pub_theme + 
  theme(
    text = element_text(size = 60), 
    panel.grid = element_blank(), 
    axis.text.x = element_text(angle = 90)
  ) 
  

# Q13 比例热图 - 按群体分组
q13_prop_by_group <- q13_by_group %>%
  select(Option, Dimension, grp, Proportion) %>%
  distinct() %>%
  mutate(
    Option = factor(Option, q13_attract),
    Dimension = factor(Dimension, demo_groups_app),
    grp = factor(grp, levels = sort(unique(grp)))  # 按字母顺序排列群体
  )

p_q13_prop <- q13_prop_by_group %>%
  ggplot(aes(x = Option, y = grp, fill = Proportion)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", Proportion * 100)), size = 15, color = "white", fontface = "bold") +
  facet_wrap(~ Dimension, scales = "free_y", ncol = 1) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen", name = "Proportion") +
  labs(title = "(b)", x = NULL, y = "Group") +
  pub_theme +
  theme(
    text = element_text(size = 80), 
    panel.grid = element_blank(), 
    axis.text.x = element_text(angle = 90)
  )


# 5.2 Q14 期望奖励
cat("\n--- 5.2 Q14 Desired Rewards ---\n")

# 奖励类型英文映射
reward_map <- c(
  "现金红包/购物券等实际货币奖励" = "Cash/Vouchers",
  "碳积分可兑换的实用商品或优惠" = "Redeemable Goods",
  "公共服务优先权或折扣" = "Public Service Priority",
  "精神荣誉奖励" = "Honorary Rewards",
  "社交分享或公益捐赠奖励" = "Social/Charity Rewards"
)

data %>% filter(!is.na(.data[[q14_reward]])) %>% count(Option = .data[[q14_reward]], name = "Count") %>%
  mutate(Option_En = reward_map[Option], Proportion = sprintf("%.1f%%", Count / sum(Count) * 100)) %>%
  arrange(desc(Count)) %>% select(Option_En, Count, Proportion) %>% kable(format = "simple") %>% print()

q14_tests <- lapply(names(demo_groups_app), function(gvar) {
  d <- data %>% filter(!is.na(.data[[gvar]]) & !is.na(.data[[q14_reward]]))
  test <- tryCatch(chisq.test(table(d[[gvar]], d[[q14_reward]])),
                   error = function(e) tryCatch(fisher.test(table(d[[gvar]], d[[q14_reward]]), simulate.p.value = TRUE), error = function(e2) NULL))
  p <- if (!is.null(test)) test$p.value else NA
  stat <- if (!is.null(test) && "statistic" %in% names(test)) test$statistic else NA
  data.frame(Dimension = demo_groups_app[gvar], p.value = p, stat = stat)
}) %>% bind_rows() %>%
  mutate(Sig = case_when(is.na(p.value) ~ "NA", p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", p.value < 0.05 ~ "*", TRUE ~ "ns"),
         Dimension = factor(Dimension, demo_groups_app),
         label_text = ifelse(is.na(stat), Sig, paste0("X2=", round(stat, 1), "\n", Sig)))
q14_tests %>% select(Dimension, p.value, Sig) %>% kable(digits = 4, format = "simple") %>% print()

p_q14_sig <- q14_tests %>%
  ggplot(aes(x = 1, y = Dimension, fill = (Sig != "ns" & Sig != "NA"))) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = label_text), size = 3.5, lineheight = 0.85) +
  scale_fill_manual(values = c("TRUE" = "#4CAF50", "FALSE" = "#E0E0E0"), guide = "none") +
  labs(title = "(l)", x = NULL, y = "Dimension") +
  pub_theme +
  theme(axis.text.x = element_blank(), panel.grid = element_blank())

# Q14 分布热图（绿色）
q14_freq <- data %>%
  filter(!is.na(.data[[q14_reward]])) %>%
  count(Reward = .data[[q14_reward]], name = "Count") %>%
  mutate(Proportion = Count / sum(Count),
         Reward_En = reward_map[Reward],
         Reward_En = factor(Reward_En, levels = reward_map))

p_q14_dist <- q14_freq %>%
  ggplot(aes(x = Reward_En, y = 1, fill = Proportion)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", Proportion * 100)), size = 4.5, color = "white", fontface = "bold") +
  scale_fill_gradient(low = "#A5D6A7", high = "#2E7D32", name = "Proportion") +
  labs(title = "(m)", x = NULL, y = NULL) +
  pub_theme +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid = element_blank(), legend.key.height = unit(0.8, "cm"))


# 5.3 Q15 使用障碍
cat("\n--- 5.3 Q15 Usage Barriers ---\n")
calc_multi_freq(data, q15_barrier) %>% mutate(Proportion = sprintf("%.1f%%", Proportion * 100)) %>% kable(format = "simple") %>% print()

q15_coch <- cochran_q(data, names(q15_barrier))
cat(sprintf("Cochran's Q = %.2f, df = %d, p = %.4f, n = %d\n", q15_coch$Q, q15_coch$df, q15_coch$p, q15_coch$n))

q15_by_group <- lapply(names(demo_groups_app), function(gvar) {
  calc_multi_by_group(data, q15_barrier, gvar) %>%
    left_join(test_multi_by_group(data, q15_barrier, gvar) %>% select(var, p.value, stat), by = "var") %>%
    mutate(Dimension = demo_groups_app[gvar])
}) %>% bind_rows()

q15_tests <- q15_by_group %>% select(Dimension, Option, p.value, stat) %>% distinct() %>%
  mutate(Sig = case_when(is.na(p.value) ~ "NA", p.value < 0.001 ~ "***", p.value < 0.01 ~ "**", p.value < 0.05 ~ "*", TRUE ~ "ns"),
         Dimension = factor(Dimension, demo_groups_app), Option = factor(Option, q15_barrier),
         label_text = ifelse(is.na(stat), Sig, paste0("X2=", round(stat, 1), "\n", Sig)))

p_q15_sig <- q15_tests %>%
  ggplot(aes(x = Option, y = Dimension, fill = (Sig != "ns" & Sig != "NA"))) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = label_text), size = 3.5, lineheight = 0.85) +
  scale_fill_manual(values = c("TRUE" = "#FF7043", "FALSE" = "#E0E0E0"), guide = "none") +
  labs(title = "(n)", x = NULL, y = "Dimension") +
  pub_theme +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), panel.grid = element_blank())

# Q15 比例热图
q15_freq <- calc_multi_freq(data, q15_barrier) %>%
  mutate(Option = factor(Option, q15_barrier))

p_q15_prop <- q15_freq %>%
  ggplot(aes(x = Option, y = 1, fill = Proportion)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", Proportion * 100)), size = 4.5, color = "white", fontface = "bold") +
  scale_fill_gradient(low = "#FFCCBC", high = "#D84315", name = "Proportion") +
  labs(title = "(o)", x = NULL, y = NULL) +
  pub_theme +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid = element_blank(), legend.key.height = unit(0.8, "cm"))


# 5.4 单独导出Q13图（Significance + Heatmap）
p_q13_combined <- (p_q13_sig / p_q13_prop) +
  plot_layout(heights = c(1, 2.5)) +
  plot_annotation(title = "Q13: Attraction Factors")

ggsave("data_proc/q13_attract_significance_heatmap.png", p_q13_combined, width = 30, height = 40, units = "cm", dpi = 300, bg = "white")

# 5.5 Q13/Q14/Q15 组合图 (3x2)
p_combined_q13q14q15 <- (p_q13_sig + p_q13_prop) / (p_q14_sig + p_q14_dist) / (p_q15_sig + p_q15_prop) +
  plot_layout(heights = c(1.2, 1.2, 1.2)) +
  plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = rel(1.1))))

print(p_combined_q13q14q15)
ggsave("data_proc/q13_q14_q15_combined.png", p_combined_q13q14q15, width = 28, height = 24, units = "cm", dpi = 300, bg = "white")


# PART 6: 导出Sankey详细分析为Excel ----
cat("\n\n", strrep("#", 80), "\n")
cat("PART 6: Exporting Detailed Sankey Analysis to Excel\n")
cat(strrep("#", 80), "\n")

# 导出到Excel的函数
export_sankey_to_excel <- function(sankey_list, filename = "data_proc/sankey_detailed_analysis.xlsx") {

  pacman::p_load(openxlsx)

  # 创建工作簿
  wb <- createWorkbook()

  # 为每个行为创建一个sheet
  for (behavior_name in names(sankey_list)) {
    result <- sankey_list[[behavior_name]]

    # 创建sheet
    sheet_name <- substr(behavior_name, 1, 31)  # Excel sheet名称最多31个字符
    addWorksheet(wb, sheet_name)
    print(sheet_name)

    # 当前行位置
    current_row <- 1

    # 写入标题
    writeData(wb, sheet_name, "Sankey Detailed Analysis", startRow = current_row)
    current_row <- current_row + 1

    writeData(wb, sheet_name, paste("Behavior:", behavior_name), startRow = current_row)
    current_row <- current_row + 1

    writeData(wb, sheet_name, paste("Total respondents (with valid pre & post):", result$n_total), startRow = current_row)
    current_row <- current_row + 2

    # 表格1：左边分布
    writeData(wb, sheet_name, "[1] LEFT SIDE (BEFORE) DISTRIBUTION:", startRow = current_row)
    current_row <- current_row + 1
    writeData(wb, sheet_name, result$pre_distribution, startRow = current_row, rowNames = FALSE)
    current_row <- current_row + nrow(result$pre_distribution) + 3

    # 表格2：右边分布
    writeData(wb, sheet_name, "[2] RIGHT SIDE (AFTER) DISTRIBUTION:", startRow = current_row)
    current_row <- current_row + 1
    writeData(wb, sheet_name, result$post_distribution, startRow = current_row, rowNames = FALSE)
    current_row <- current_row + nrow(result$post_distribution) + 3

    # 表格3：流出矩阵
    writeData(wb, sheet_name, "[3] FLOW OUT MATRIX (% of 'From' that flows to each 'To'):", startRow = current_row)
    current_row <- current_row + 1
    writeData(wb, sheet_name, result$flow_out_matrix, startRow = current_row, rowNames = FALSE)
    current_row <- current_row + nrow(result$flow_out_matrix) + 3

    # 表格4：流入矩阵
    writeData(wb, sheet_name, "[4] FLOW IN MATRIX (% of 'To' that comes from each 'From'):", startRow = current_row)
    current_row <- current_row + 1
    writeData(wb, sheet_name, result$flow_in_matrix, startRow = current_row, rowNames = FALSE)

    # 设置列宽
    setColWidths(wb, sheet_name, cols = 1:20, widths = "auto")
  }

  # 创建汇总sheet
  addWorksheet(wb, "Summary")
  summary_data <- data.frame(
    Behavior = names(sankey_list),
    N_Total = sapply(sankey_list, function(x) x$n_total),
    Converted_1_3_to_4_5 = sapply(
      sankey_list, function(x) x$conversion_low_to_high
    ),
    Before_High_Freq_Pct = sprintf("%.2f%%", sapply(sankey_list, function(x) x$pre_high_pct * 100)),
    After_High_Freq_Pct = sprintf("%.2f%%", sapply(sankey_list, function(x) x$post_high_pct * 100)),
    High_Freq_Change_Pct = sprintf("%+.2f%%", sapply(sankey_list, function(x) x$high_pct_change * 100)),
    Score3_to_4_5_Rate = sprintf("%.2f%%", sapply(sankey_list, function(x) ifelse(is.na(x$mid_to_high_conversion), 0, x$mid_to_high_conversion) * 100)),
    Score1_2_to_3_5_Rate = sprintf("%.2f%%", sapply(sankey_list, function(x) ifelse(is.na(x$low_to_mid_high_conversion), 0, x$low_to_mid_high_conversion) * 100)),
    stringsAsFactors = FALSE
  )
  writeData(wb, "Summary", "Sankey Analysis Summary", startRow = 1)
  writeData(wb, "Summary", summary_data, startRow = 3, rowNames = FALSE)
  setColWidths(wb, "Summary", cols = 1:8, widths = "auto")

  # 保存文件
  saveWorkbook(wb, filename, overwrite = TRUE)
  cat("✓ Excel file exported successfully!\n")
  cat("  Location:", filename, "\n")
  cat("  Sheets created:", length(names(sankey_list)) + 1, "\n")
  cat("  - Summary sheet + ", length(names(sankey_list)), " behavior sheets\n", sep = "")
}

# 执行导出
export_sankey_to_excel(sankey_detailed_list)

# ==============================================================================
cat("\n\n", strrep("=", 80), "\n")
cat("Analysis complete. Output files saved to data_proc/ directory:\n")
cat("  - sankey_behavior_change.png\n")
cat("  - sankey_detailed_analysis.xlsx (NEW!)\n")
cat("  - behavior_change_within_group.png\n")
cat("  - three_group_comparison_table.csv\n")
cat("  - three_group_significance.png\n")
cat("  - q4_q20_group_diff_heatmap.png\n")
cat("  - q13_q14_q15_combined.png\n")
cat(strrep("=", 80), "\n")
