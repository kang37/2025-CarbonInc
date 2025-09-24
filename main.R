# Preparation ----
library(readxl)
library(dplyr)
library(labelled)
library(purrr)
library(tidyr)
library(stringr)
library(ggplot2)
library(showtext)
showtext_auto()

# 问卷数据列名和原问题的对应关系。
question_map <- c(
  "submit_time" = "提交时间",
  "district" = "所在区",
  "practice_zone" = "所在低碳发展实践区",
  "organization" = "所在单位",
  "gender" = "性别",
  "age" = "年龄",
  "info_source" = "从哪个渠道了解到碳普惠专区",
  "open_easy" = "个人碳账户开立的流程是否简单易懂",
  "open_barrier" = "开立个人碳账户时，您是否遇到了技术问题或障碍",
  "layout_satisfaction" = "碳普惠专区界面的整体布局是否满意",
  "operation_easy" = "对碳普惠专区界面的操作便捷性",
  "travel_mode" = "最常使用的出行方式",
  "scene_auth" = "是否授权了地面公交、轨道交通、互联网租赁自行车、纯电动乘用车场景",
  "auth_process" = "场景授权操作流程是否简单，是否遇到过问题",
  "no_auth_reason" = "没有进行场景授权的原因是",
  "nfc_used" = "是否使用过 NFC 手机虚拟交通卡绑卡功能",
  "nfc_bind_easy" = "NFC手机虚拟交通卡绑卡的操作流程是否简单",
  "nfc_exp" = "使用NFC 手机虚拟交通卡累计碳减排量的体验",
  "no_nfc_reason" = "没有使用NFC 手机虚拟交通卡累计碳减排量的原因是：",
  "know_point_rule" = "清楚碳积分的兑换规则，",
  "point_process" = "碳积分兑换时，操作流程是否顺畅",
  "point_delivery" = "对碳积分兑换的商品到账时间",
  "point_problem" = "权益兑换过程中，是否遇到问题",
  "product_variety" = "您认为碳积分商城的商品种类是否丰富",
  "suggest_product" = "在碳积分商城中增加哪些类型的商品",
  "product_quality" = "对碳积分商城中现有商品的质量如何评价",
  "less_use_reason" = "您是否会因为商品多样性不足而减少对碳积分商城的使用",
  "mall_layout" = "对碳积分商城商品的界面布局是否合理",
  "product_sort" = "碳积分商城商品按照什么方式进行排序",
  "recommend" = "是否会向他人推荐碳普惠专区",
  "suggestion" = "对碳普惠专区的建议"
)

# 读取问卷数据。
survey <- read_xlsx("data_raw/第一次问卷结果.xlsx")[, 1:31] %>% 
  rename(!!!question_map)

# 给各列添加标签：原本的问卷问题。
lapply(1:ncol(survey), function(x) var_label(survey)[x] <<- question_map[x])
# 检查各列列名和标签。
View(look_for(survey))

# 新增受访者编号。
survey <- mutate(survey, res_id = row_number(), .before = 1)

# 将info_source拆分成多列。
survey_sub_wide <- survey %>%
  select(res_id, info_source) %>% 
  # 多选题按顿号拆分成多行。
  separate_rows(info_source, sep = "，") %>%  
  # 提取选项字母。
  mutate(info_source_clean = str_extract(info_source, "^[A-Z]")) %>%
  # 将多选题分割成多个列。
  mutate(val = 1) %>%
  pivot_wider(
    id_cols = res_id,
    names_from = info_source_clean,
    names_prefix = "info_source_",
    values_from = val,
    values_fill = 0
  ) 

# 将拆分后的多选题列加到原数据中。
survey <- survey %>% 
  left_join(survey_sub_wide, by = "res_id") %>% 
  relocate(sort(names(survey_sub_wide)[-1]), .after = "info_source")

# Analysis ----
# 各问题情况。
lapply(
  1:ncol(survey), 
  function(x) plot(
    sort(table(survey[[x]]), decreasing = TRUE), 
    main = names(survey)[x], ylab = "受访者数", cex.axis = 0.6, las = 2
  )
)
# 只筛选前10个。
lapply(
  1:ncol(survey), 
  function(x) plot(
    sort(table(survey[[x]])[1:10], decreasing = TRUE), 
    main = names(survey)[x], ylab = "受访者数", cex.axis = 0.6, las = 2
  )
)

# 正态化测试 ----
# 假设你的数据框名为 data_frame，
# 请将 'your_data_frame' 替换为你实际的数据框名称。
# 并且确保 make_normal_with_test 函数已经在R环境中定义好。

# 步骤1：创建一个包含所有需要正态化变量名称的向量
variables_to_normalize <- c(
  "percep_ui", "percep_auto_record",
  "percep_awareness", "percep_info_use", "percep_faster_choice",
  "percep_guidance", "app_freq",
  "change_bus", "change_walk", "change_off", "change_recycle",
  "peer_recommend", "social_media_influence", "show_off"
)

# 步骤2：创建一个空列表来存储每个变量的正态化结果
normalization_results <- list()

# 步骤3：循环遍历每个变量
your_data_frame <- data_raw

for (var in variables_to_normalize) {
  
  # 对当前变量执行正态化，并将结果存储在一个临时变量中
  result <- make_normal(your_data_frame[[var]])
  
  # 将正态化后的数据存储回数据框中
  your_data_frame[[var]] <- result
}

model_tam <- '
  # 测量模型
  PEOU =~ percep_ui + percep_auto_record
  PU   =~ percep_awareness + percep_info_use + percep_faster_choice
  BI   =~ percep_guidance + app_freq
  BC   =~ change_bus + change_walk + change_off + change_recycle
  SI   =~ peer_recommend + social_media_influence + show_off

  # 结构路径
  PU ~ PEOU
  BI ~ PU + PEOU
  BC ~ BI + SI
'
fit <- sem(model_tam, data = data_raw, estimator = "MLR")

# 输出结果
summary(fit, fit.measures = TRUE, standardized = TRUE)

# 作图。
lavaanPlot(
  model = fit, # 显示路径系数。
  coefs = TRUE, 
  stand = TRUE, # 使用标准化系数。
  # covs = TRUE, # 显示协方差。 
  stars = "regress", # 显示显著性星号。
  graph_options = list(rankdir = "LR"), 
  sig = 0.05
)

