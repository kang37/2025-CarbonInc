# 加载必要库
library(readxl)
library(ggplot2)
library(dplyr)
library(showtext)
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

# 重命名变量
data <- 
  read_excel(
    "data_raw/320419112_按序号_2025年低碳减排问卷调查_1108_1067.xlsx"
  ) %>% 
  rename(!!!question_map) %>% 
  mutate(across(all_of(variables), factor)) %>% 
  mutate(
    # --- 请根据您的数据字典（问卷选项）修改 "1" = "???" 的映射 ---
    
    # 1. 性别 (假设 1=男, 2=女)
    gender = recode_factor(gender,
                           "1" = "男",
                           "2" = "女",
                           .default = "其他/未填" # .default 会处理 NA 或未在上面列出的值
    ),
    
    # 2. 婚姻状况 (假设 1=未婚, 2=已婚, 3=...)
    marital_status = recode_factor(marital_status,
                                   "1" = "未婚",
                                   "2" = "已婚",
                                   "3" = "离异/丧偶",
                                   .default = "其他/未填"
    ),
    
    # 3. 汽车拥有量 (假设 0=0辆, 1=1辆, 2=2辆, 3=3辆及以上)
    car_ownership = recode_factor(car_ownership,
                                  "0" = "0辆",
                                  "1" = "1辆",
                                  "2" = "2辆",
                                  "3" = "3辆及以上",
                                  .default = "其他/未填"
    ),
    
    # 4. 家庭同住人口 (假设 0=0人, 1=1人 ...)
    family_other_pop = recode_factor(family_other_pop,
                                     "0" = "0人",
                                     "1" = "1人",
                                     "2" = "2人",
                                     "3" = "3人",
                                     "4" = "4人及以上",
                                     .default = "其他/未填"
    ),
    
    # --- 对于已经是描述性文本的变量 (如 年龄段)，直接转为因子 ---
    
    age = factor(age),
    education = factor(education),
    housing_sqm = factor(housing_sqm),
    monthly_income_personal = factor(monthly_income_personal),
    monthly_income_family = factor(monthly_income_family),
    residence_area = factor(residence_area)
  )

# 基本情况饼图 ----
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
    labs(fill = title) +
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
# --- 替换：更新 plot_bar_chart 函数 ---
#' 创建分类计数条形图 (Bar Chart)
#' 
#' @param data 原始数据框。
#' @param col_x 要作图的分类列名（字符串，例如 "gender"）。
#' @param title 图表标题（字符串）。
#' @param x_lab X轴标签（字符串）。默认为 "受访者选项"。
#' @param sort_by_count 是否按计数大小降序排序。默认 FALSE (不排序)。
#' @return 一个 ggplot 对象。
plot_bar_chart <- function(data, col_x, title = "分类计数分布", x_lab = "受访者选项", sort_by_count = FALSE) {
  
  col_sym <- sym(col_x)
  
  # 1. 数据准备：过滤 NA，并计算计数
  plot_data <- data %>%
    filter(!is.na(!!col_sym)) %>%
    count(!!col_sym, name = "Count") 
  
  if (sort_by_count) {
    plot_data <- plot_data %>% 
      arrange(desc(Count))
  }
  
  # 2. 绘图
  plot <- plot_data %>%
    ggplot() +
    geom_bar(
      aes(
        x = if (sort_by_count) { reorder(!!col_sym, -Count) } else { !!col_sym },
        y = Count
      ), 
      stat = "identity", 
      fill = "steelblue"
    ) +
    
    # ***关键修改：使用 x_lab 参数***
    labs(
      title = title,
      x = x_lab, # 使用传入的 x_lab
      y = "计数"
    ) +
    lims(y = c(0, 600)) + 
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  
  return(plot)
}

# 函数：根据起始列和结束列批量生成条形图。
# 新增 title_map 参数：一个命名向量, e.g. c("col_name" = "美观的标题")
# 新增 x_axis_label 参数：传递给 plot_bar_chart 的 X 轴标签
bar_plot_list <- function(start_col, end_col, title_map = NULL, x_axis_label = "受访者选项") {
  
  # 1. 提取所有目标列名
  question_cols <- data %>%
    select(!!start_col:!!end_col) %>%
    names()
  
  # 2. 批量生成图表
  bar_charts_list <- lapply(
    seq_along(question_cols),
    function(i) {
      var_name <- question_cols[i]
      
      # ***关键修改：使用 title_map 来获取美观的标题***
      if (!is.null(title_map) && var_name %in% names(title_map)) {
        plot_title <- title_map[var_name]
      } else {
        plot_title <- var_name # 如果映射中没有，则退回使用原始列名
      }
      
      # ***关键修改：传递 x_lab 和 title***
      p <- plot_bar_chart(
        data, 
        col_x = var_name, 
        title = plot_title, 
        x_lab = x_axis_label # 传递 x 轴标签
      )
      return(p)
    }
  )
  
  return(bar_charts_list)
}

# 和使用体验有关的改进建议。
# 为Q4系列问题定义更美观的标题
q4_titles <- c(
  "q4_ui_simple" = "界面应简洁明了",
  "q4_integrate_platform" = "希望与常用平台打通",
  "q4_auto_record" = "希望APP能自动记录数据",
  "q4_clear_guidance" = "希望APP提供清晰的低碳引导"
)
bar_ls_q4 <- bar_plot_list(
  "q4_ui_simple", 
  "q4_clear_guidance", 
  title_map = q4_titles,
  x_axis_label = "同意程度" # 假设这是李克特量表, 否则可改为 "受访者选项"
)
Reduce(`+`, bar_ls_q4) + plot_layout(nrow = 1)

# 和实用性有关的。
q4_2_titles <- c(
  "q4_raise_awareness" = "APP能提高低碳意识",
  "q4_info_feedback_useful" = "APP提供的信息和反馈很实用",
  "q4_quicker_green_choice" = "APP能助我更快做环保选择"
)

# 调用 bar_plot_list，传入 title_map 和 x_axis_label
bar_ls_q4_2 <- bar_plot_list(
  "q4_raise_awareness", 
  "q4_quicker_green_choice",
  title_map = q4_2_titles,
  x_axis_label = "同意程度"
)

# 组合图表并排成一行 (3个图)
Reduce(`+`, bar_ls_q4_2) + plot_layout(nrow = 1)

# 了解“碳普惠”或“低碳减排”方面信息的主要来源。
c(data$q5_info_source_1, data$q5_info_source_2, data$q5_info_source_3) %>% 
  table() %>% 
  sort(decreasing = TRUE) %>% 
  head(10)

# 吸引点 ----
# 1. 定义 Q12 相关的列
q12_cols <- c(
  "q12_incentive_points",
  "q12_incentive_viz",
  "q12_incentive_ui",
  "q12_incentive_social",
  "q12_incentive_game",
  "q12_incentive_expert_advice",
  "q12_incentive_data_life"
)

# 2. 定义美观的标题映射 (确保名称与 q12_cols 中的完全一致)
q12_titles_map <- c(
  "q12_incentive_points" = "积分兑奖",
  "q12_incentive_viz" = "图示减排成果",
  "q12_incentive_ui" = "简洁、易操作的界面",
  "q12_incentive_social" = "朋友参与和社交互动",
  "q12_incentive_game" = "有趣的任务和游戏",
  "q12_incentive_expert_advice" = "官方或专家推荐",
  "q12_incentive_data_life" = "与生活紧密结合的数据"
)

# 3. 数据处理与汇总
q12_summary <- data %>%
  # 仅选择相关的列
  select(all_of(q12_cols)) %>%
  # 将所有列转换为数值型 (确保 "1" 被识别为数字)
  mutate(across(everything(), as.numeric)) %>% 
  # 转换为长格式
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  # 过滤：我们只关心被选中的 (假设 "1" = 选中)
  filter(Value == 1) %>%
  # 按变量名分组并计数
  group_by(Variable) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  # 映射到美观的标题
  # 注意：我们使用您定义的 q12_titles_map 来查找中文名
  mutate(Option = factor(q12_titles_map[Variable], levels = q12_titles_map))

# 4. 绘制汇总条形图 (水平)
q12_summary_plot <- ggplot(q12_summary, aes(x = reorder(Option, Count), y = Count, fill = Option)) +
  geom_bar(stat = "identity") +
  # 添加数据标签
  geom_text(aes(label = Count), hjust = -0.2, size = 3.5, color = "black") +
  # *** 翻转坐标轴，使长标签更易读 ***
  coord_flip() + 
  labs(
    title = "Q12: 最能激励持续参与的因素 (多选)",
    subtitle = "按选择人数排序",
    x = "激励因素",
    y = "选择人数 (计数)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none" # 选项已在Y轴显示，移除图例
  )

print(q12_summary_plot)

# Q12共现分析。
# 1. 准备 0/1 数据矩阵 (处理 NA)
# (确保 q12_cols 变量在您的环境中)
q12_matrix_data <- data %>%
  select(all_of(q12_cols)) %>%
  mutate(across(everything(), as.numeric)) %>%
  # 关键：将 NA 替换为 0 (未选中)，否则矩阵计算会失败
  replace(is.na(.), 0)

# 2. 转换为 R 矩阵
q12_matrix <- as.matrix(q12_matrix_data)

# 3. 计算共现矩阵 (矩阵乘法)
# t(M) %*% M 得到一个 K x K 矩阵，其中 [i, j] 是 i 和 j 共同出现的次数
co_occur_matrix <- t(q12_matrix) %*% q12_matrix

# 4. 转换为长数据框 (Tidy format) 以便 ggplot 绘图
# (确保 q12_titles_map 映射表在您的环境中)
co_occur_long <- as.data.frame(co_occur_matrix) %>%
  # 将行名 (变量名) 转换成一个新列
  tibble::rownames_to_column("Option1_Var") %>%
  # 转换为长格式
  pivot_longer(
    cols = -Option1_Var,
    names_to = "Option2_Var",
    values_to = "Count"
  ) %>%
  # 应用美观的标签
  mutate(
    Option1 = factor(q12_titles_map[Option1_Var], levels = q12_titles_map),
    Option2 = factor(q12_titles_map[Option2_Var], levels = q12_titles_map)
  ) %>%
  # 过滤掉NA (防止映射失败)
  filter(!is.na(Option1) & !is.na(Option2))

# 5. 准备绘图数据 (优化)
# 我们将对角线 (自己和自己的共现) 的值设为 NA，
# 这样它们在图上会显示为灰色，不干扰我们对交叉项的观察
q12_heatmap_data <- co_occur_long %>%
  mutate(Count_Label = Count) %>%
  mutate(Count = ifelse(Option1_Var == Option2_Var, NA, Count))

# 6. 绘制热力图
q12_heatmap <- ggplot(q12_heatmap_data, 
                      aes(x = Option1, y = Option2, fill = Count)) +
  # 绘制热力图格子，并添加白色边框
  geom_tile(color = "white") +
  # 在格子上添加共现次数的数字
  geom_text(aes(label = Count_Label), color = "black", size = 3) +
  # 定义一个从浅到深的填充色（na.value 用于填充对角线）
  scale_fill_gradient(low = "#FFF3E0", high = "#E65100", na.value = "grey90") +
  labs(
    title = "Q12: 激励因素共现热力图",
    subtitle = "数字表示同时选择两项的人数",
    x = "激励因素 (A)",
    y = "激励因素 (B)",
    fill = "共现次数"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    # 旋转X轴标签，防止重叠
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(q12_heatmap)

## Q13相关 ----
# 1. 定义 Q13 相关的列
q13_cols <- c(
  "q13_attract_points",
  "q13_attract_credit",
  "q13_attract_daily_task",
  "q13_attract_ranking",
  "q13_attract_game_fun",
  "q13_attract_viz_carbon"
)

# 2. 定义美观的标题映射 (使用您提供的 q13_titles)
q13_titles_map <- c(
  "q13_attract_points" = "吸引功能: 碳积分兑换商品",
  "q13_attract_credit" = "吸引功能: 绿色信用积分/政策优惠",
  "q13_attract_daily_task" = "吸引功能: 每日打卡/任务",
  "q13_attract_ranking" = "吸引功能: 朋友排行榜/成就徽章",
  "q13_attract_game_fun" = "吸引功能: 抽奖等娱乐性玩法",
  "q13_attract_viz_carbon" = "吸引功能: 可视化我的碳足迹"
)

# 3. 数据处理与汇总
q13_summary <- data %>%
  select(all_of(q13_cols)) %>%
  mutate(across(everything(), as.numeric)) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  filter(Value == 1) %>%
  group_by(Variable) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Option = factor(q13_titles_map[Variable], levels = q13_titles_map))

# 4. 绘制汇总条形图 (水平)
q13_summary_plot <- ggplot(q13_summary, aes(x = reorder(Option, Count), y = Count, fill = Option)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), hjust = -0.2, size = 3.5, color = "black") +
  coord_flip() + 
  labs(
    title = "Q13: 最能吸引您使用的功能 (多选)",
    subtitle = "按选择人数排序",
    x = "吸引因素",
    y = "选择人数 (计数)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )

print(q13_summary_plot)

# 1. 定义 Q13 相关的列
q13_cols <- c(
  "q13_attract_points",
  "q13_attract_credit",
  "q13_attract_daily_task",
  "q13_attract_ranking",
  "q13_attract_game_fun",
  "q13_attract_viz_carbon"
)

# 2. 定义美观的标题映射 (使用您提供的 q13_titles)
q13_titles_map <- c(
  "q13_attract_points" = "吸引功能: 碳积分兑换商品",
  "q13_attract_credit" = "吸引功能: 绿色信用积分/政策优惠",
  "q13_attract_daily_task" = "吸引功能: 每日打卡/任务",
  "q13_attract_ranking" = "吸引功能: 朋友排行榜/成就徽章",
  "q13_attract_game_fun" = "吸引功能: 抽奖等娱乐性玩法",
  "q13_attract_viz_carbon" = "吸引功能: 可视化我的碳足迹"
)

# 3. 数据处理与汇总
q13_summary <- data %>%
  select(all_of(q13_cols)) %>%
  mutate(across(everything(), as.numeric)) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  filter(Value == 1) %>%
  group_by(Variable) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Option = factor(q13_titles_map[Variable], levels = q13_titles_map))

# 4. 绘制汇总条形图 (水平)
q13_summary_plot <- ggplot(q13_summary, aes(x = reorder(Option, Count), y = Count, fill = Option)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), hjust = -0.2, size = 3.5, color = "black") +
  coord_flip() + 
  labs(
    title = "Q13: 最能吸引您使用的功能 (多选)",
    subtitle = "按选择人数排序",
    x = "吸引因素",
    y = "选择人数 (计数)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )

print(q13_summary_plot)

# 目标：分析哪些吸引功能经常被同时选中
# 1. 准备 0/1 数据矩阵 (处理 NA)
q13_matrix_data <- data %>%
  select(all_of(q13_cols)) %>%
  mutate(across(everything(), as.numeric)) %>%
  # 关键：将 NA 替换为 0 (未选中)
  replace(is.na(.), 0)

# 2. 转换为 R 矩阵
q13_matrix <- as.matrix(q13_matrix_data)

# 3. 计算共现矩阵
co_occur_matrix_q13 <- t(q13_matrix) %*% q13_matrix

# 4. 转换为长数据框 (Tidy format) 以便 ggplot 绘图
# (确保 q13_titles_map 映射表在您的环境中)
co_occur_long_q13 <- as.data.frame(co_occur_matrix_q13) %>%
  tibble::rownames_to_column("Option1_Var") %>%
  pivot_longer(
    cols = -Option1_Var,
    names_to = "Option2_Var",
    values_to = "Count"
  ) %>%
  # 应用美观的标签
  mutate(
    Option1 = factor(q13_titles_map[Option1_Var], levels = q13_titles_map),
    Option2 = factor(q13_titles_map[Option2_Var], levels = q13_titles_map)
  ) %>%
  # 过滤掉NA (防止映射失败)
  filter(!is.na(Option1) & !is.na(Option2))

# 5. 准备绘图数据 (优化，隐藏对角线)
q13_heatmap_data <- co_occur_long_q13 %>%
  mutate(Count_Label = Count) %>%
  mutate(Count = ifelse(Option1_Var == Option2_Var, NA, Count))

# 6. 绘制热力图
q13_heatmap <- ggplot(q13_heatmap_data, 
                      aes(x = Option1, y = Option2, fill = Count)) +
  geom_tile(color = "white") +
  # 在格子上添加共现次数的数字
  geom_text(aes(label = Count_Label), color = "black", size = 3) +
  # 定义一个从浅到深的填充色（na.value 用于填充对角线）
  scale_fill_gradient(low = "#E3F2FD", high = "#0D47A1", na.value = "grey90") + # 换个色系
  labs(
    title = "Q13: 吸引功能共现热力图",
    subtitle = "数字表示同时选择两项的人数",
    x = "吸引功能 (A)",
    y = "吸引功能 (B)",
    fill = "共现次数"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    # 旋转X轴标签，防止重叠
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(q13_heatmap)

## Q20 ----
# (新增) Q20 标题映射
q20_titles <- c(
  "q20_celeb_endorsement" = "明星代言会让我更关注",
  "q20_video_intro" = "视频介绍更吸引我",
  "q20_focus_env_vs_self" = "“环保意义”不如“个人利益”吸引我",
  "q20_ranking_motivation" = "排行/成就会激励我",
  "q20_image_inspire" = "图像/动画比文字更有趣",
  "q20_cumulative_inspire" = "累计成效激励我维持行为"
)

# (修改) 一些其他方面的促进。
bar_ls_q20 <- bar_plot_list(
  "q20_celeb_endorsement", 
  "q20_cumulative_inspire",
  title_map = q20_titles,
  x_axis_label = "同意程度" # 这些是李克特量表
)
# 6个图，设为3列
Reduce(`+`, bar_ls_q20) + plot_layout(ncol = 3)

# (修改) 希望获得的具体奖励。
# --- 改进：Q14 (最希望获得的奖励) ---
# (已根据截图修正：使用 1-6 数字代码进行重编码)

# 1. 数据处理与汇总
q14_summary <- data %>%
  # 筛选出 "q14_desired_reward" 列，并移除 NA
  filter(!is.na(q14_desired_reward)) %>%
  # 按奖励类型分组并计数
  count(q14_desired_reward, name = "Count") %>%
  
  # *** 关键改进：将原始列(可能是数字1,2,3)转换为字符 "1","2","3" ***
  # 这样 recode_factor 才能正确匹配
  mutate(Option_Raw = as.character(q14_desired_reward))

# 2. *** 关键修改：使用 "数字" = "短标签" 的方式进行映射 ***
q14_summary <- q14_summary %>%
  mutate(
    Option = recode_factor(Option_Raw,
                           # --- 映射关系基于您提供的截图 ---
                           "1" = "公交出行优惠券",
                           "2" = "环保主题用品",
                           "3" = "购物代金券",
                           "4" = "低碳徽章",
                           "5" = "IP周边",
                           "6" = "其他"
                           # -----------------------------------
                           , .default = Option_Raw # 如果有其他值，保留原样
    )
  )

# 3. 绘制汇总条形图 (水平) - (这部分代码与之前相同)
q14_plot <- ggplot(q14_summary, aes(x = reorder(Option, Count), y = Count)) +
  geom_bar(stat = "identity", aes(fill = Option), show.legend = FALSE) + 
  geom_text(aes(label = Count), hjust = -0.2, size = 3.5, color = "black") +
  coord_flip() + 
  labs(
    title = "Q14: 最希望获得的奖励类型 (单选)",
    subtitle = "按选择人数排序",
    x = "奖励类型 (优化后标签)",
    y = "选择人数 (计数)"
  ) +
  scale_y_continuous(limits = c(0, max(q14_summary$Count) * 1.15)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(q14_plot)


# 障碍 ----
# --- 针对 Q15 (多选题) 的最佳分析：汇总条形图 ---
# 1. 定义 Q15 相关的列
q15_cols <- c(
  "q15_barrier_trouble",
  "q15_barrier_privacy",
  "q15_barrier_low_reward",
  "q15_barrier_unknown"
)

# 2. 定义美观的标题映射 (使用您提供的 q15_titles)
q15_titles_map <- c(
  "q15_barrier_trouble" = "障碍: 觉得操作麻烦",
  "q15_barrier_privacy" = "障碍: 担心个人信息不安全",
  "q15_barrier_low_reward" = "障碍: 积分奖励太少",
  "q15_barrier_unknown" = "障碍: 很多人没听说过"
)

# 3. 数据处理与汇总
q15_summary <- data %>%
  select(all_of(q15_cols)) %>%
  mutate(across(everything(), as.numeric)) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  filter(Value == 1) %>%
  group_by(Variable) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  # 映射到美观的标题
  mutate(Option = factor(q15_titles_map[Variable], levels = q15_titles_map))

# 4. 绘制汇总条形图 (水平)
q15_summary_plot <- ggplot(q15_summary, aes(x = reorder(Option, Count), y = Count, fill = Option)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), hjust = -0.2, size = 3.5, color = "black") +
  coord_flip() + 
  labs(
    title = "Q15: 阻碍人们使用APP的主要原因 (多选)",
    subtitle = "按选择人数排序",
    x = "障碍因素",
    y = "选择人数 (计数)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none" # 选项已在Y轴显示，移除图例
  )

print(q15_summary_plot)


# --- Q15 共现分析 (Heatmap) ---
# 目标：分析哪些障碍因素经常被同时选中
# 1. 准备 0/1 数据矩阵 (处理 NA)
q15_matrix_data <- data %>%
  select(all_of(q15_cols)) %>%
  mutate(across(everything(), as.numeric)) %>%
  # 关键：将 NA 替换为 0 (未选中)
  replace(is.na(.), 0)

# 2. 转换为 R 矩阵
q15_matrix <- as.matrix(q15_matrix_data)

# 3. 计算共现矩阵
co_occur_matrix_q15 <- t(q15_matrix) %*% q15_matrix

# 4. 转换为长数据框 (Tidy format)
co_occur_long_q15 <- as.data.frame(co_occur_matrix_q15) %>%
  tibble::rownames_to_column("Option1_Var") %>%
  pivot_longer(
    cols = -Option1_Var,
    names_to = "Option2_Var",
    values_to = "Count"
  ) %>%
  # 应用美观的标签
  mutate(
    Option1 = factor(q15_titles_map[Option1_Var], levels = q15_titles_map),
    Option2 = factor(q15_titles_map[Option2_Var], levels = q15_titles_map)
  ) %>%
  filter(!is.na(Option1) & !is.na(Option2))

# 5. 准备绘图数据 (优化，隐藏对角线)
q15_heatmap_data <- co_occur_long_q15 %>%
  mutate(Count_Label = Count) %>%
  mutate(Count = ifelse(Option1_Var == Option2_Var, NA, Count))

# 6. 绘制热力图
q15_heatmap <- ggplot(q15_heatmap_data, 
                       aes(x = Option1, y = Option2, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count_Label), color = "black", size = 3.5) +
  # 定义一个 "障碍" 色系 (例如从浅黄到深红)
  scale_fill_gradient(low = "#FFF9C4", high = "#D32F2F", na.value = "grey90") +
  labs(
    title = "Q15: 障碍因素共现热力图",
    subtitle = "数字表示同时选择两项的人数",
    x = "障碍因素 (A)",
    y = "障碍因素 (B)",
    fill = "共现次数"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(q15_heatmap)

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

# 具体问题 ----
## 行为变化 ----
# APP使用前后行为变化。
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(knitr)
library(kableExtra)

# APP使用前后行为变化分析函数（改进版）
analyze_behavior_change_wilcoxon <- function(
    data, pre_col_str, post_col_str, behavior_label) {
  
  # 将字符串列名转换为符号
  pre_sym <- sym(pre_col_str)
  post_sym <- sym(post_col_str)
  
  # 1. 数据准备与清洗
  analysis_data <- data %>%
    select(!!pre_sym, !!post_sym) %>%
    mutate(
      Pre = as.numeric(!!pre_sym),
      Post = as.numeric(!!post_sym),
      Difference = Post - Pre
    ) %>%
    filter(!is.na(Pre) & !is.na(Post))
  
  # 样本量
  n <- nrow(analysis_data)
  
  # 2. Wilcoxon 符号秩检验
  wilcox_result <- wilcox.test(
    analysis_data$Post, 
    analysis_data$Pre, 
    paired = TRUE, 
    alternative = "greater",
    exact = FALSE,
    correct = TRUE
  )
  
  # 3. 计算效应量 (Cohen's d)
  mean_diff <- mean(analysis_data$Difference, na.rm = TRUE)
  sd_diff <- sd(analysis_data$Difference, na.rm = TRUE)
  cohen_d <- mean_diff / sd_diff
  
  # 效应量分类
  effect_size_label <- ifelse(abs(cohen_d) < 0.2, "忽略不计",
                              ifelse(abs(cohen_d) < 0.5, "小",
                                     ifelse(abs(cohen_d) < 0.8, "中", "大")))
  
  # 4. 数据可视化准备（添加标准误）
  plot_data <- analysis_data %>%
    pivot_longer(
      cols = c(Pre, Post), 
      names_to = "Period", 
      values_to = "Score"
    ) %>%
    group_by(Period) %>%
    summarise(
      Mean_Score = mean(Score, na.rm = TRUE),
      SE = sd(Score, na.rm = TRUE) / sqrt(n()),  # 标准误
      .groups = 'drop'
    )
  
  # 5. 显著性标记
  if (wilcox_result$p.value < 0.001) {
    sig_label <- "***"
  } else if (wilcox_result$p.value < 0.01) {
    sig_label <- "**"
  } else if (wilcox_result$p.value < 0.05) {
    sig_label <- "*"
  } else {
    sig_label <- "ns"
  }
  
  # 6. 改进的可视化
  p <- ggplot(plot_data, aes(x = Period, y = Mean_Score, fill = Period)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8, width = 0.6) +
    geom_errorbar(
      aes(ymin = Mean_Score - SE, ymax = Mean_Score + SE),
      width = 0.2, position = position_dodge(0.6)
    ) +
    geom_text(aes(label = round(Mean_Score, 2)), vjust = -1.5, size = 4) +
    scale_x_discrete(labels = c("Pre" = "使用前", "Post" = "使用后")) +
    scale_fill_manual(values = c("Pre" = "#93C5FD", "Post" = "#86EFAC")) +
    labs(
      title = behavior_label,
      subtitle = paste0("p ", ifelse(wilcox_result$p.value < 0.001, "< 0.001", 
                                     paste("=", round(wilcox_result$p.value, 3)))),
      y = "行为平均分 (± SE)",
      x = NULL
    ) +
    ylim(0, max(plot_data$Mean_Score + plot_data$SE) * 1.3) +  # 动态Y轴
    annotate(
      "text", 
      x = 1.5, 
      y = max(plot_data$Mean_Score + plot_data$SE) * 1.15,
      label = sig_label, 
      size = 8
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "none"  # 移除冗余图例
    )
  
  # 7. 估计差异
  median_diff <- median(analysis_data$Difference, na.rm = TRUE)
  
  return(list(
    label = behavior_label,
    n = n,
    test_result = wilcox_result,
    plot = p,
    median_difference = median_diff,
    mean_difference = mean_diff,
    mean_score_pre = plot_data$Mean_Score[plot_data$Period == "Pre"],
    mean_score_post = plot_data$Mean_Score[plot_data$Period == "Post"],
    cohen_d = cohen_d,
    effect_size_label = effect_size_label,
    significance = sig_label
  ))
}

# ----------------------------------------------------------------------
# 批量分析
analysis_results_wilcoxon <- lapply(names(behavior_map), function(b) {
  cols <- behavior_map[[b]]
  analyze_behavior_change_wilcoxon(
    data = data, 
    pre_col_str = cols[1],
    post_col_str = cols[2],
    behavior_label = b
  )
})

# ----------------------------------------------------------------------
# 改进5：使用表格汇总结果
results_table <- do.call(rbind, lapply(analysis_results_wilcoxon, function(res) {
  data.frame(
    行为 = res$label,
    样本量 = res$n,
    使用前均值 = round(res$mean_score_pre, 2),
    使用后均值 = round(res$mean_score_post, 2),
    均值差异 = round(res$mean_difference, 2),
    中位数差异 = round(res$median_difference, 2),
    P值 = format.pval(res$test_result$p.value, digits = 3),
    Cohen_d = round(res$cohen_d, 3),
    效应量 = res$effect_size_label,
    显著性 = res$significance,
    stringsAsFactors = FALSE
  )
}))

# 打印表格
kable(results_table, 
      format = "html",
      caption = "APP使用前后行为变化分析结果汇总",
      align = c('l', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c')) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center"
  ) %>%
  column_spec(10, bold = TRUE) %>%  # 显著性列加粗
  row_spec(which(results_table$显著性 %in% c("*", "**", "***")), 
           background = "#D4EDDA")  # 显著结果高亮

# ----------------------------------------------------------------------
# 组合所有图表 (使用 patchwork)
combined_plot <- Reduce(`+`, lapply(analysis_results_wilcoxon, function(x) x$plot))
print(combined_plot)

# 绘制前后行为打分比例。
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# 函数：绘制行为前后选项比例图（5级李克特量表）
plot_behavior_distribution <- function(
    data, pre_col_str, post_col_str, behavior_label) {
  
  # 将字符串列名转换为符号
  pre_sym <- sym(pre_col_str)
  post_sym <- sym(post_col_str)
  
  # 1. 数据准备
  analysis_data <- data %>%
    select(!!pre_sym, !!post_sym) %>%
    mutate(
      Pre = as.numeric(!!pre_sym),
      Post = as.numeric(!!post_sym)
    ) %>%
    filter(!is.na(Pre) & !is.na(Post))
  
  # 2. 计算各选项的频数和比例
  dist_data <- analysis_data %>%
    pivot_longer(
      cols = c(Pre, Post),
      names_to = "Period",
      values_to = "Response"
    ) %>%
    group_by(Period, Response) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    group_by(Period) %>%
    mutate(
      Proportion = Count / sum(Count) * 100,
      Label = paste0(round(Proportion, 1), "%")
    ) %>%
    ungroup() %>%
    mutate(
      Period = factor(Period, levels = c("Pre", "Post")),
      Response = factor(Response, levels = 1:5)
    )
  
  # 3. 定义颜色方案（从不同意到同意的渐变）
  color_palette <- c(
    "1" = "#D32F2F",  # 深红
    "2" = "#F57C00",  # 橙色
    "3" = "#FDD835",  # 黄色
    "4" = "#66BB6A",  # 绿色
    "5" = "#2E7D32"   # 深绿
  )
  
  # 4. 堆叠条形图
  p <- ggplot(dist_data, aes(x = Period, y = Proportion, fill = Response)) +
    geom_bar(stat = "identity", position = "stack", width = 0.6) +
    geom_text(
      aes(label = ifelse(Proportion > 5, Label, "")),  # 只显示>5%的标签
      position = position_stack(vjust = 0.5),
      size = 3.5,
      color = "white",
      fontface = "bold"
    ) +
    scale_fill_manual(
      values = color_palette,
      name = "评分",
      labels = c("1", "2", "3", "4", "5"),
      drop = FALSE
    ) +
    scale_x_discrete(labels = c("Pre" = "使用前", "Post" = "使用后")) +
    labs(
      title = behavior_label,
      y = "比例 (%)",
      x = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      legend.position = "right",
      legend.text = element_text(size = 9),
      axis.text = element_text(size = 10)
    )
  
  return(p)
}

# ----------------------------------------------------------------------
# 批量生成比例图
distribution_plots <- lapply(names(behavior_map), function(b) {
  cols <- behavior_map[[b]]
  plot_behavior_distribution(
    data = data,
    pre_col_str = cols[1],
    post_col_str = cols[2],
    behavior_label = b
  )
})

# 组合显示（使用 patchwork）
combined_plot <- Reduce(`+`, distribution_plots) +
  plot_layout(ncol = 2, guides = "collect")

print(combined_plot)

# 桑基图版本。
library(ggsankey)  

# 函数：绘制行为前后变化的桑基图
plot_behavior_sankey <- function(
    data, pre_col_str, post_col_str, behavior_label) {
  # 将字符串列名转换为符号
  pre_sym <- sym(pre_col_str)
  post_sym <- sym(post_col_str)
  
  # 1. 数据准备
  analysis_data <- data %>%
    select(!!pre_sym, !!post_sym) %>%
    mutate(
      Pre = as.factor(as.numeric(!!pre_sym)),
      Post = as.factor(as.numeric(!!post_sym))
    ) %>%
    filter(!is.na(Pre) & !is.na(Post))
  
  # 2. 计算流量（Pre -> Post 的转换）
  flow_data <- analysis_data %>%
    group_by(Pre, Post) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    mutate(
      Pre_label = paste0("使用前: ", Pre),
      Post_label = paste0("使用后: ", Post)
    )
  
  # 3. 转换为 ggsankey 格式
  sankey_data <- flow_data %>%
    make_long(Pre_label, Post_label, value = Count)
  
  # 4. 定义颜色方案
  color_palette <- c(
    "使用前: 1" = "#D32F2F", "使用后: 1" = "#D32F2F",
    "使用前: 2" = "#F57C00", "使用后: 2" = "#F57C00",
    "使用前: 3" = "#FDD835", "使用后: 3" = "#FDD835",
    "使用前: 4" = "#66BB6A", "使用后: 4" = "#66BB6A",
    "使用前: 5" = "#2E7D32", "使用后: 5" = "#2E7D32"
  )
  
  # 5. 绘制桑基图
  p <- ggplot(sankey_data, aes(
    x = x,
    next_x = next_x,
    node = node,
    next_node = next_node,
    fill = node,
    value = value,
    label = node
  )) +
    geom_sankey(flow.alpha = 0.5, node.color = "gray30", width = 0.1) +
    geom_sankey_label(size = 3, color = "black", fill = "white", alpha = 0.7) +
    scale_fill_manual(values = color_palette) +
    labs(
      title = behavior_label, x = NULL, y = "人数"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = 9),
      panel.grid = element_blank()
    )
  
  return(p)
}

# ----------------------------------------------------------------------
# 批量生成桑基图
sankey_plots <- lapply(names(behavior_map), function(b) {
  cols <- behavior_map[[b]]
  plot_behavior_sankey(
    data = data,
    pre_col_str = cols[1],
    post_col_str = cols[2],
    behavior_label = b
  )
})

# 组合显示（使用 patchwork）
combined_sankey <- Reduce(`+`, sankey_plots) +
  plot_layout(ncol = 2)

print(combined_sankey)


# 使用APP和不使用APP的人差异是？
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(patchwork)

# 1. 定义行为对应关系（三个时期）
behavior_three_groups_map <- list(
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

# 2. 函数：比较三组的行为差异
compare_three_groups <- function(data, pre_col, post_col, non_user_col, behavior_label) {
  
  # 提取使用过APP用户的使用前数据
  pre_data <- data %>%
    filter(q1_used_app == 1) %>%
    select(score = all_of(pre_col)) %>%
    mutate(score = as.numeric(score)) %>%
    filter(!is.na(score)) %>%
    mutate(group = "使用前(APP用户)")
  
  # 提取使用过APP用户的使用后数据
  post_data <- data %>%
    filter(q1_used_app == 1) %>%
    select(score = all_of(post_col)) %>%
    mutate(score = as.numeric(score)) %>%
    filter(!is.na(score)) %>%
    mutate(group = "使用后(APP用户)")
  
  # 提取未使用过APP用户的平时数据
  non_user_data <- data %>%
    filter(q1_used_app != 1) %>%
    select(score = all_of(non_user_col)) %>%
    mutate(score = as.numeric(score)) %>%
    filter(!is.na(score)) %>%
    mutate(group = "非APP用户")
  
  # 合并数据
  combined_data <- bind_rows(pre_data, post_data, non_user_data) %>%
    mutate(group = factor(group, levels = c("使用前(APP用户)", "使用后(APP用户)", "非APP用户")))
  
  # Kruskal-Wallis 检验（三组或以上的非参数检验）
  kruskal_result <- kruskal.test(score ~ group, data = combined_data)
  
  # 事后成对比较（Pairwise Wilcoxon检验，Bonferroni校正）
  if (kruskal_result$p.value < 0.05) {
    pairwise_result <- pairwise.wilcox.test(
      combined_data$score,
      combined_data$group,
      p.adjust.method = "bonferroni",
      exact = FALSE
    )
  } else {
    pairwise_result <- NULL
  }
  
  # 计算描述性统计
  summary_stats <- combined_data %>%
    group_by(group) %>%
    summarise(
      n = n(),
      mean = mean(score, na.rm = TRUE),
      sd = sd(score, na.rm = TRUE),
      median = median(score, na.rm = TRUE),
      se = sd / sqrt(n),
      .groups = 'drop'
    )
  
  # 计算效应量（使用前 vs 使用后 的 Cohen's d）
  if (nrow(pre_data) > 0 && nrow(post_data) > 0) {
    mean_pre <- summary_stats$mean[summary_stats$group == "使用前(APP用户)"]
    mean_post <- summary_stats$mean[summary_stats$group == "使用后(APP用户)"]
    sd_pre <- summary_stats$sd[summary_stats$group == "使用前(APP用户)"]
    sd_post <- summary_stats$sd[summary_stats$group == "使用后(APP用户)"]
    n_pre <- summary_stats$n[summary_stats$group == "使用前(APP用户)"]
    n_post <- summary_stats$n[summary_stats$group == "使用后(APP用户)"]
    
    pooled_sd <- sqrt(((n_pre - 1) * sd_pre^2 + (n_post - 1) * sd_post^2) / 
                        (n_pre + n_post - 2))
    cohen_d_pre_post <- (mean_post - mean_pre) / pooled_sd
    
    effect_label_pre_post <- ifelse(abs(cohen_d_pre_post) < 0.2, "忽略不计",
                                    ifelse(abs(cohen_d_pre_post) < 0.5, "小",
                                           ifelse(abs(cohen_d_pre_post) < 0.8, "中", "大")))
  } else {
    cohen_d_pre_post <- NA
    effect_label_pre_post <- NA
  }
  
  # 可视化：箱线图 + 散点
  p <- ggplot(combined_data, aes(x = group, y = score, fill = group)) +
    geom_boxplot(alpha = 0.7, width = 0.6, outlier.shape = NA) +
    geom_jitter(alpha = 0.3, width = 0.2, size = 0.8) +
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 23,
      size = 3,
      fill = "red",
      color = "black"
    ) +
    scale_fill_manual(values = c(
      "使用前(APP用户)" = "#FCA5A5",
      "使用后(APP用户)" = "#86EFAC",
      "非APP用户" = "#93C5FD"
    )) +
    labs(
      title = behavior_label,
      subtitle = paste0("Kruskal-Wallis p ", 
                        ifelse(kruskal_result$p.value < 0.001, "< 0.001",
                               paste("=", round(kruskal_result$p.value, 3)))),
      y = "行为频率评分",
      x = NULL
    ) +
    ylim(0.5, 5.5) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
      plot.subtitle = element_text(hjust = 0.5, size = 9),
      legend.position = "none",
      axis.text.x = element_text(angle = 20, hjust = 1, size = 8)
    )
  
  # 添加显著性标记
  if (kruskal_result$p.value < 0.001) {
    sig_label <- "***"
  } else if (kruskal_result$p.value < 0.01) {
    sig_label <- "**"
  } else if (kruskal_result$p.value < 0.05) {
    sig_label <- "*"
  } else {
    sig_label <- "ns"
  }
  
  p <- p + annotate("text", x = 2, y = 5.3, label = sig_label, size = 8)
  
  # 可视化：柱状图（均值 + 误差线）
  p_bar <- ggplot(summary_stats, aes(x = group, y = mean, fill = group)) +
    geom_bar(stat = "identity", alpha = 0.8, width = 0.6) +
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      width = 0.2
    ) +
    geom_text(aes(label = round(mean, 2)), vjust = -1.5, size = 3.5) +
    scale_fill_manual(values = c(
      "使用前(APP用户)" = "#FCA5A5",
      "使用后(APP用户)" = "#86EFAC",
      "非APP用户" = "#93C5FD"
    )) +
    labs(
      title = behavior_label,
      subtitle = paste0("p ", ifelse(kruskal_result$p.value < 0.001, "< 0.001",
                                     paste("=", round(kruskal_result$p.value, 3)))),
      y = "行为平均分 (± SE)",
      x = NULL
    ) +
    ylim(0, max(summary_stats$mean + summary_stats$se) * 1.3) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
      plot.subtitle = element_text(hjust = 0.5, size = 9),
      legend.position = "none",
      axis.text.x = element_text(angle = 20, hjust = 1, size = 8)
    )
  
  return(list(
    label = behavior_label,
    summary_stats = summary_stats,
    kruskal_result = kruskal_result,
    pairwise_result = pairwise_result,
    cohen_d_pre_post = cohen_d_pre_post,
    effect_label_pre_post = effect_label_pre_post,
    significance = sig_label,
    plot_box = p,
    plot_bar = p_bar,
    combined_data = combined_data
  ))
}

# 3. 批量分析
three_group_results <- lapply(names(behavior_three_groups_map), function(b) {
  cols <- behavior_three_groups_map[[b]]
  compare_three_groups(
    data = data,
    pre_col = cols$pre,
    post_col = cols$post,
    non_user_col = cols$non_user,
    behavior_label = b
  )
})

# 4. 生成汇总表格
three_group_table <- do.call(rbind, lapply(three_group_results, function(res) {
  stats <- res$summary_stats
  
  data.frame(
    行为 = res$label,
    使用前样本量 = stats$n[stats$group == "使用前(APP用户)"],
    使用前均值 = round(stats$mean[stats$group == "使用前(APP用户)"], 2),
    使用后样本量 = stats$n[stats$group == "使用后(APP用户)"],
    使用后均值 = round(stats$mean[stats$group == "使用后(APP用户)"], 2),
    非用户样本量 = stats$n[stats$group == "非APP用户"],
    非用户均值 = round(stats$mean[stats$group == "非APP用户"], 2),
    使用前后差异 = round(stats$mean[stats$group == "使用后(APP用户)"] -
                     stats$mean[stats$group == "使用前(APP用户)"], 2),
    Kruskal_p值 = format.pval(res$kruskal_result$p.value, digits = 3),
    Cohen_d前后 = round(res$cohen_d_pre_post, 3),
    效应量 = res$effect_label_pre_post,
    显著性 = res$significance,
    stringsAsFactors = FALSE
  )
}))

# 打印表格
kable(three_group_table,
      format = "html",
      caption = "三组行为对比：使用前 vs 使用后 vs 非APP用户",
      align = c('l', rep('c', 11))) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "center",
    font_size = 12
  ) %>%
  column_spec(12, bold = TRUE) %>%
  row_spec(which(three_group_table$显著性 %in% c("*", "**", "***")),
           background = "#D4EDDA")

# 5. 组合所有箱线图
combined_box_plot <- Reduce(`+`, lapply(three_group_results, function(x) x$plot_box)) +
  plot_layout(ncol = 2)

print(combined_box_plot)

# 6. 组合所有柱状图
combined_bar_plot <- Reduce(`+`, lapply(three_group_results, function(x) x$plot_bar)) +
  plot_layout(ncol = 2)

print(combined_bar_plot)

# 7. 打印成对比较结果（如果显著）
cat("\n\n## 事后成对比较结果 (Bonferroni校正)\n\n")
for (res in three_group_results) {
  if (!is.null(res$pairwise_result)) {
    cat("\n### ", res$label, "\n")
    print(res$pairwise_result)
    cat("\n")
  }
}

## 环保意识 ----
# 函数：比较APP用户和非用户的环保意识差异
compare_environmental_awareness <- function(data, awareness_vars) {
  
  results_list <- lapply(names(awareness_vars), function(var_name) {
    col_name <- awareness_vars[[var_name]]
    
    # 提取APP用户数据
    used_data <- data %>%
      filter(q1_used_app == 1) %>%
      select(score = all_of(col_name)) %>%
      mutate(score = as.numeric(score)) %>%
      filter(!is.na(score))
    
    # 提取非APP用户数据
    not_used_data <- data %>%
      filter(q1_used_app != 1) %>%
      select(score = all_of(col_name)) %>%
      mutate(score = as.numeric(score)) %>%
      filter(!is.na(score))
    
    # 合并数据用于检验
    combined_data <- bind_rows(
      used_data %>% mutate(group = "APP用户"),
      not_used_data %>% mutate(group = "非APP用户")
    )
    
    # Mann-Whitney U 检验
    wilcox_result <- wilcox.test(
      score ~ group,
      data = combined_data,
      alternative = "two.sided",
      exact = FALSE
    )
    
    # 计算描述性统计
    n_used <- nrow(used_data)
    n_not_used <- nrow(not_used_data)
    
    mean_used <- mean(used_data$score, na.rm = TRUE)
    mean_not_used <- mean(not_used_data$score, na.rm = TRUE)
    
    median_used <- median(used_data$score, na.rm = TRUE)
    median_not_used <- median(not_used_data$score, na.rm = TRUE)
    
    sd_used <- sd(used_data$score, na.rm = TRUE)
    sd_not_used <- sd(not_used_data$score, na.rm = TRUE)
    
    # 计算效应量 (Cohen's d)
    pooled_sd <- sqrt(((n_used - 1) * sd_used^2 + (n_not_used - 1) * sd_not_used^2) / 
                        (n_used + n_not_used - 2))
    cohen_d <- (mean_used - mean_not_used) / pooled_sd
    
    # 效应量分类
    effect_label <- ifelse(abs(cohen_d) < 0.2, "忽略不计",
                           ifelse(abs(cohen_d) < 0.5, "小",
                                  ifelse(abs(cohen_d) < 0.8, "中", "大")))
    
    # 显著性标记
    if (wilcox_result$p.value < 0.001) {
      sig_label <- "***"
    } else if (wilcox_result$p.value < 0.01) {
      sig_label <- "**"
    } else if (wilcox_result$p.value < 0.05) {
      sig_label <- "*"
    } else {
      sig_label <- "ns"
    }
    
    # 返回结果
    data.frame(
      环保意识维度 = var_name,
      APP用户样本量 = n_used,
      APP用户均值 = round(mean_used, 3),
      APP用户中位数 = round(median_used, 2),
      APP用户标准差 = round(sd_used, 3),
      非用户样本量 = n_not_used,
      非用户均值 = round(mean_not_used, 3),
      非用户中位数 = round(median_not_used, 2),
      非用户标准差 = round(sd_not_used, 3),
      均值差异 = round(mean_used - mean_not_used, 3),
      中位数差异 = round(median_used - median_not_used, 2),
      Mann_Whitney_U = round(wilcox_result$statistic, 2),
      P值 = format.pval(wilcox_result$p.value, digits = 3),
      Cohen_d = round(cohen_d, 3),
      效应量 = effect_label,
      显著性 = sig_label,
      stringsAsFactors = FALSE
    )
  })
  
  # 合并所有结果
  do.call(rbind, results_list)
}

# 定义环保意识相关变量
awareness_variables <- list(
  "碳积分知晓度" = "q4_know_carbon_credit",
  "垃圾分类知晓度" = "q6_know_garbage_sort",
  "社交媒体影响" = "q8_social_media_influence"
)

# 执行分析
awareness_comparison <- compare_environmental_awareness(
  data = data,
  awareness_vars = awareness_variables
)

# 打印美化表格
kable(awareness_comparison,
      format = "html",
      caption = "APP用户 vs 非用户的环保意识对比分析",
      align = c('l', rep('c', 15))) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = TRUE,
    position = "center",
    font_size = 11
  )

# 可视化：箱线图对比
plot_awareness_comparison <- function(data, col_name, var_label) {
  
  # 准备数据
  plot_data <- data %>%
    mutate(
      score = as.numeric(.data[[col_name]]),
      group = ifelse(q1_used_app == 1, "APP用户", "非APP用户")
    ) %>%
    filter(!is.na(score)) %>%
    mutate(group = factor(group, levels = c("APP用户", "非APP用户")))
  
  # 统计检验
  wilcox_result <- wilcox.test(score ~ group, data = plot_data, exact = FALSE)
  
  # 显著性标记
  if (wilcox_result$p.value < 0.001) {
    sig_label <- "***"
  } else if (wilcox_result$p.value < 0.01) {
    sig_label <- "**"
  } else if (wilcox_result$p.value < 0.05) {
    sig_label <- "*"
  } else {
    sig_label <- "ns"
  }
  
  # 绘图
  p <- ggplot(plot_data, aes(x = group, y = score, fill = group)) +
    geom_boxplot(alpha = 0.7, width = 0.5, outlier.shape = NA) +
    geom_jitter(alpha = 0.3, width = 0.2, size = 1) +
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 23,
      size = 3,
      fill = "red",
      color = "black"
    ) +
    scale_fill_manual(values = c("APP用户" = "#86EFAC", "非APP用户" = "#93C5FD")) +
    labs(
      title = var_label,
      subtitle = paste0("p ", ifelse(wilcox_result$p.value < 0.001, "< 0.001",
                                     paste("=", round(wilcox_result$p.value, 3)))),
      y = "评分",
      x = NULL
    ) +
    ylim(min(plot_data$score) - 0.5, max(plot_data$score) + 0.5) +
    annotate("text", x = 1.5, y = max(plot_data$score) + 0.3, 
             label = sig_label, size = 8) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "none"
    )
  
  return(p)
}

# 生成所有可视化
awareness_plots <- lapply(names(awareness_variables), function(var_name) {
  plot_awareness_comparison(
    data = data,
    col_name = awareness_variables[[var_name]],
    var_label = var_name
  )
})

# 组合图表
combined_awareness_plot <- Reduce(`+`, awareness_plots) +
  plot_layout(ncol = 3)

print(combined_awareness_plot)
