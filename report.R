# 加载必要库
library(readxl)
library(ggplot2)
library(dplyr)
library(showtext)
showtext_auto()

# 读取数据（如果你尚未读取）
data <- read_excel("data_raw/320419112_按序号_2025年低碳减排问卷调查_1108_1067.xlsx")

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
# 重命名变量
data <- data %>% rename(!!!question_map)

# 基本情况饼图 ----
# 定义需要生成饼图的变量名（与数据框中的列名对应）
variables <- c(
  "gender", "age", "education", "marital_status", "housing_sqm",
  "monthly_income_personal", "monthly_income_family", "family_other_pop",
  "car_ownership"
)

# Bug: 转化数据类型。
data <- data %>% mutate(across(all_of(variables), factor))

# 定义对应的图表标题
titles <- c(
  "性别比例", "年龄段比例", "教育程度比例", "婚姻状况比例", "住房面积比例",
  "个人月收入比例", "家庭月收入比例", "家庭同住其他人口数量比例",
  "汽车拥有量比例", "居住区域比例"
)

# 目标：保留最大比例的 N 个类别，其余合并为 "其他"
N_KEEP <- 4

# 函数：绘制单个饼图，将比例信息放入图例中
create_pie_chart_academic <- function(data, variable_name_str, title, n_to_keep = N_KEEP) {
  
  # 将字符串转换为符号，用于 dplyr/ggplot 的非标准评估
  var_sym <- sym(variable_name_str)
  
  # 1. 计算比例并合并小类别 (保持不变)
  plot_data <- data %>%
    # 转换为因子以确保所有级别都被计算，并删除NA
    filter(!is.na(!!var_sym)) %>%
    count(!!var_sym, name = "Count") %>%
    mutate(Proportion = Count / sum(Count)) %>%
    arrange(desc(Proportion))
  
  # 2. 确定要保留的行和 "其他" 行
  if (nrow(plot_data) > n_to_keep) {
    top_n_data <- plot_data[1:n_to_keep, ]
    other_data <- plot_data[(n_to_keep + 1):nrow(plot_data), ] %>%
      summarise(
        Count = sum(Count),
        Proportion = sum(Proportion)
      ) %>%
      mutate(!!var_sym := factor("其他"))
    
    plot_data <- bind_rows(top_n_data, other_data) %>%
      arrange(desc(Proportion))
  }
  
  # 3. 计算饼图标签位置 **(移除百分比计算，改为图例文本)**
  plot_data <- plot_data %>%
    rename(Category_Raw = !!var_sym) %>% # 备份原始类别
    mutate(
      Percentage = paste0(round(Proportion * 100, 1), "%"),
      
      # ***关键修改：将百分比和类别合并成新的图例文本***
      Category = paste0(Category_Raw, " (", Percentage, ")"),
      
      Cum_Prop = cumsum(Proportion),
      Mid_Prop = Cum_Prop - Proportion / 2
    )
  
  # 4. 绘制饼图 (主要修改部分)
  p <- ggplot(plot_data, aes(x = "", y = Proportion, fill = Category)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    # **应用学术配色：使用 'Set1', 'Pastel1', 或 'Dark2' 等**
    scale_fill_brewer(palette = "Set2") +
    
    # 移除不必要的元素
    labs(title = title, fill = title) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
      legend.title = element_text(size = 8, face = "bold"), # 标题加粗
      legend.text = element_text(size = 6),
      # 增加图例行距，改善外观
      legend.key.height = unit(0.5, "cm")
    )
  
  return(p)
}


variables <- c(
  "gender", "age", "education", "marital_status", "housing_sqm",
  "monthly_income_personal", "monthly_income_family", "family_other_pop",
  "car_ownership", "residence_area"
)

titles <- c(
  "性别比例", "年龄段比例", "教育程度比例", "婚姻状况比例", "住房面积比例",
  "个人月收入比例", "家庭月收入比例", "家庭同住其他人口数量比例",
  "汽车拥有量比例", "居住区域比例"
)
data <- data %>% mutate(across(all_of(variables), factor))

# 批量生成饼图 (使用 lapply)
pie_charts_list_academic <- lapply(
  seq_along(variables),
  function(i) {
    var_name <- variables[i]
    plot_title <- titles[i]
    # **使用新的函数**
    create_pie_chart_academic(data, var_name, plot_title, n_to_keep = N_KEEP) 
  }
)

# 组合图形
# 确保已加载 patchwork 库
# library(patchwork) 
combined_plots_academic <- Reduce(`+`, pie_charts_list_academic)
final_plot_academic <- combined_plots_academic + plot_layout(ncol = 3)

# 打印最终组合图
print(final_plot_academic)

# 其他列条形图 ----
# 画出data的residence_area的分布柱状图。
#' 创建分类计数条形图 (Bar Chart)
#' 
#' @param data 原始数据框。
#' @param col_x 要作图的分类列名（字符串，例如 "gender"）。
#' @param title 图表标题（字符串）。
#' @param sort_by_count 是否按计数大小降序排序。默认 FALSE (不排序)。
#' @return 一个 ggplot 对象。
plot_bar_chart <- function(data, col_x, title = "分类计数分布", sort_by_count = FALSE) {
  
  # ***关键修正：将字符串转换为符号***
  col_sym <- sym(col_x)
  
  # 1. 数据准备：过滤 NA，并计算计数
  plot_data <- data %>%
    filter(!is.na(!!col_sym)) %>%
    count(!!col_sym, name = "Count") 
  
  # ***新增逻辑：根据 sort_by_count 参数决定是否排序***
  if (sort_by_count) {
    plot_data <- plot_data %>% 
      arrange(desc(Count))
  }
  
  # 2. 绘图
  plot <- plot_data %>%
    ggplot() +
    geom_bar(
      # ***关键修改：使用 if/else 决定 x 轴的排序方式***
      aes(
        # 如果 sort_by_count 为 TRUE，则按 Count 排序
        x = if (sort_by_count) { reorder(!!col_sym, -Count) } else { !!col_sym },
        y = Count
      ), 
      stat = "identity", 
      fill = "steelblue"
    ) +
    
    # 添加标签和主题
    labs(
      title = title,
      x = col_x, # x轴标签直接使用输入的字符串
      y = "计数"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  
  return(plot)
}

# 函数：根据起始列和结束列批量生成条形图。
bar_plot_list <- function(start_col, end_col) {
  # 1. 提取所有目标列名
  # 使用 select() 配合 names() 获取连续的列名向量
  question_cols <- data %>%
    select(!!start_col:!!end_col) %>%
    names()
  
  # 2. 构造对应的标题 (这部分仍然需要手动创建或根据列名生成)
  # 由于标题无法自动生成，我们仍需一个与 question_cols 长度匹配的向量
  # 假设我们使用列名作为简化标题
  question_titles <- question_cols # 直接使用列名作为标题
  
  # 3. 批量生成图表 (使用之前定义的 plot_bar_chart 函数)
  bar_charts_list <- lapply(
    seq_along(question_cols),
    function(i) {
      var_name <- question_cols[i]
      plot_title <- question_titles[i]
      
      # 假设您的数据框名称是 data
      p <- plot_bar_chart(data, col_x = var_name, title = plot_title)
      return(p)
    }
  )
  
  return(bar_charts_list)
}

# 和使用体验有关的改进建议。
bar_ls_q4 <- bar_plot_list("q4_ui_simple", "q4_clear_guidance")
Reduce(`+`, bar_ls_q4)

# 和实用性有关的。
bar_ls_q4_2 <- bar_plot_list("q4_raise_awareness", "q4_quicker_green_choice")
Reduce(`+`, bar_ls_q4_2)

# 了解“碳普惠”或“低碳减排”方面信息的主要来源。
c(data$q5_info_source_1, data$q5_info_source_2, data$q5_info_source_3) %>% 
  table() %>% 
  sort(decreasing = TRUE) %>% 
  head(10)

# 吸引点 ----
# 当前功能中。
bar_ls_q12 <- bar_plot_list("q12_incentive_points", "q12_incentive_data_life")
Reduce(`+`, bar_ls_q12)

# 未来功能。
bar_ls_q13 <- bar_plot_list("q13_attract_points", "q13_attract_viz_carbon")
Reduce(`+`, bar_ls_q13)
# 一些其他方面的促进。
bar_ls_q20 <- bar_plot_list("q20_celeb_endorsement", "q20_cumulative_inspire")
Reduce(`+`, bar_ls_q20)

# 希望获得的具体奖励。
plot_bar_chart(data, "q14_desired_reward")

# 障碍 ----
bar_ls_q15 <- bar_plot_list("q15_barrier_trouble", "q15_barrier_unknown")
Reduce(`+`, bar_ls_q15)

# 行为改变 ----
#' 分析低碳APP使用前后行为差异
#' 
#' @param data 原始数据框。
#' @param pre_col_str 使用前行为的列名字符串。
#' @param post_col_str 使用后行为的列名字符串。
#' @param behavior_label 行为的描述标签。
#' @return 包含配对t检验结果和可视化图表的列表。
analyze_behavior_change <- function(data, pre_col_str, post_col_str, behavior_label) {
  
  # 将字符串列名转换为符号
  pre_sym <- sym(pre_col_str)
  post_sym <- sym(post_col_str)
  
  # 1. 数据准备与清洗
  analysis_data <- data %>%
    select(!!pre_sym, !!post_sym) %>%
    # 确保列是数值型，如果原始数据是因子，需要转换为数值（假设 1=极少, 5=频繁）
    mutate(
      Pre = as.numeric(!!pre_sym),
      Post = as.numeric(!!post_sym),
      # 计算差异
      Difference = Post - Pre
    ) %>%
    filter(!is.na(Pre) & !is.na(Post))
  
  # 2. 配对 T 检验
  # 检验 H0: 均值差异 = 0 (APP无效) vs Ha: 均值差异 > 0 (APP有效)
  t_result <- t.test(
    analysis_data$Post, 
    analysis_data$Pre, 
    paired = TRUE, 
    alternative = "greater" 
  )
  
  # 3. 数据可视化准备
  plot_data <- analysis_data %>%
    # 转换为长格式以便ggplot绘图
    pivot_longer(
      cols = c(Pre, Post), 
      names_to = "Period", 
      values_to = "Score"
    ) %>%
    group_by(Period) %>%
    summarise(Mean_Score = mean(Score, na.rm = TRUE), .groups = 'drop')
  
  # 4. 可视化：条形图展示均值差异
  p <- ggplot(plot_data, aes(x = Period, y = Mean_Score, fill = Period)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    geom_text(aes(label = round(Mean_Score, 2)), vjust = -0.5, size = 4) +
    scale_x_discrete(labels = c("Pre" = "使用前", "Post" = "使用后")) +
    labs(
      title = paste0(behavior_label),
      y = "行为平均分",
      x = "时期",
      fill = "时期"
    ) +
    lims(y = c(0, 6)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  return(list(
    label = behavior_label,
    t_test = t_result,
    plot = p
  ))
}

# ----------------------------------------------------------------------
# 批量分析与结果汇总
# ----------------------------------------------------------------------

# 行为映射列表 (假设问卷中 Q16 和 Q18 的选项顺序和内容完全一致)
behavior_map <- list(
  "公共交通" = c("q16_pre_public_trans", "q18_change_public_trans"),
  "骑行或步行" = c("q16_pre_bike_walk", "q18_change_bike_walk"),
  "关闭电源" = c("q16_pre_turn_off_power", "q18_change_turn_off_power"),
  "垃圾分类" = c("q16_pre_garbage_sort", "q18_change_garbage_sort"),
  "使用环保袋" = c("q16_pre_reusable_bag", "q18_change_reusable_bag"),
  "选择节能电器" = c("q16_pre_choose_energy_eff", "q18_change_choose_energy_eff")
)

# 批量执行分析。
analysis_results <- lapply(names(behavior_map), function(b) {
  cols <- behavior_map[[b]]
  analyze_behavior_change(
    data = data,
    pre_col_str = cols[1],
    post_col_str = cols[2],
    behavior_label = b
  )
})

# # 汇总统计结果（打印 P 值和均值差异）
for (res in analysis_results) {
  cat("----------------------------------\n")
  cat("行为:", res$label, "\n")
  # cat("使用后均值:", mean(res$t_test), "\n")
  # cat("使用前均值:", mean(res$t_test$data$Pre), "\n")
  cat("均值差异:", res$t_test$estimate, "\n")
  cat("配对 t 检验 P 值:", format.pval(res$t_test$p.value, digits = 4), "\n")
}
# 
# # 组合所有图表 (使用 patchwork)
Reduce(`+`, lapply(analysis_results, function(x) x$plot))

# 其他 ----
# 未来功能
plot_bar_chart(data, "q19_desired_feature")

# 环保意识。
plot_bar_chart(data, "q4_know_carbon_credit")

