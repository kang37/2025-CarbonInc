# 加载数据包。
library(lavaan)
library(psych)
library(semTools)

# 读取数据。
data_raw <- read_excel(
  "data_raw/320419112_按序号_2025年低碳减排问卷调查_1108_1067.xlsx"
) %>% 
  rename(
    "id" = "序号",
    "submit_time" = "提交答卷时间",
    "duration_sec" = "所用时间",
    "source_cat" = "来源",
    "source_detail" = "来源详情",
    "ip_addr" = "来自IP",
    "total_score" = "总分",
    "gender" = "性别",
    "age" = "年龄",
    "education" = "教育",
    "marital" = "婚姻状况",
    "house_area_m2" = "住房面积（平方米）",
    "pers_income" = "个人月收入（元）",
    "hh_income" = "家庭每月可支配收入（元）",
    "hh_occupants" = "家庭同住其他人口数量",
    "car_total" = "汽车拥有量",
    "car_ev" = "(1)其中新能源汽车___辆",
    "car_use_freq" = "汽车使用频率",
    "res_area" = "居住区域",
    "res_community" = "并且填写居住小区:",
    "q1_app_used" = "1 您是否使用过低碳减排APP?",
    "q2_info_source" = "2 您是如何了解到这个低碳减排APP的:",
    "q3_start_reason" = "3 您为何开始使用这个低碳减排APP:",
    "percep_ui" = "请选择最符合您想法或行为频率的选项。—1 我觉得低碳减排APP的界面应当简洁明了，方便浏览和操作。",
    "percep_integration" = "2 如果低碳减排APP与我常用的平台（如支付宝或微信）打通，我会更愿意使用它。",
    "percep_auto_record" = "3 如果APP能自动记录我的出行、用电等数据，我会觉得使用起来更方便。",
    "percep_guidance" = "4 如果APP能清晰地引导我如何参与低碳活动，并且提供具体建议，我会更愿意持续使用它。",
    "percep_awareness" = "5 我认为使用低碳减排APP能有效提高我的低碳行为意识。",
    "percep_info_use" = "6 该APP提供的信息和反馈对我来说是实用的。",
    "percep_faster_choice" = "7 使用该APP能让我更快地做出环保的生活选择。",
    "know_carbonph" = "8 我了解“碳普惠”机制。",
    "info_source1" = "9 对于您来说，了解“碳普惠”或“低碳减排”方面信息的主要来源是？（提示：例如电视、报纸、网络、广告、政府文件、学校、社区管理员、志愿者、家人朋友等，也可以填写更具体的渠道）请填写最重要的1-3项，并且按照信息可靠程度进行排序。—1",
    "info_source2" = "2",
    "info_source3" = "3",
    "know_waste_sort" = "—10 我了解生活垃圾分类，包括分类的必要性、现状、分类方法、垃圾去向。",
    "peer_recommend" = "11 我的朋友中有人已经使用或推荐过低碳减排APP。",
    "social_media_influence" = "12 社交平台上的环保内容（如视频、话题、朋友圈动态）让我更关注自己的碳行为。",
    "show_off" = "13 如果我使用低碳减排APP，我希望朋友们知道我参与了这个计划（如转发、展示成绩）。",
    "app_freq" = "(1)14 您最近使用该APP的频率大约是每月___次。",
    "app_main_use" = "15 您主要使用该APP来做什么:",
    "motivation_points" = "16 在使用过程中，您觉得实际上哪些方面最能激励您持续参与?（最多选择3项）(积分兑奖)",
    "motivation_visual" = "16(图示减排成果（如碳足迹排名或减排量）)",
    "motivation_ui" = "16(简洁、易操作的界面)",
    "motivation_social" = "16(朋友参与和社交互动)",
    "motivation_game" = "16(有趣的任务和游戏)",
    "motivation_advice" = "16(官方或专家推荐的减碳建议)",
    "motivation_data" = "16(与生活紧密结合的数据记录（如出行、用电）)",
    "motivation_other" = "16(其他（请注明:）)",
    "feature_points" = "17 以下功能中，哪些最能吸引您使用一个低碳减排APP?(碳积分兑换商品或代金券)",
    "feature_policy" = "17(提供绿色信用积分或政策优惠（如积分换公共服务）)",
    "feature_daily" = "17(每日打卡/完成任务以获得积分)",
    "feature_rank" = "17(朋友排行榜、成就徽章展示)",
    "feature_lottery" = "17(抽奖、幸运转盘等娱乐性玩法)",
    "feature_visual_cf" = "17(可视化我的碳足迹与节能贡献)",
    "feature_other" = "17(其他（请注明:）)",
    "reward_pref" = "18 在以下APP提供的奖励中，我更希望获得:",
    "obstacle_complex" = "19 您认为目前阻碍人们使用低碳减排APP的主要原因是什么?(觉得低碳减排APP的使用操作可能比较麻烦)",
    "obstacle_privacy" = "19(担心在使用低碳减排APP时个人信息不安全)",
    "obstacle_few_points" = "19(积分奖励太少，对我没有吸引力)",
    "obstacle_unaware" = "19(很多人没有听说过这类APP)",
    "obstacle_other" = "19(其他:)",
    "pre_freq_bus" = "20 请您依据使用APP前的行为频率进行选择。—搭乘公共交通",
    "pre_freq_walk" = "骑行或步行出行...63",
    "pre_freq_off" = "关闭不用电器的电源...64",
    "pre_freq_recycle" = "垃圾分类与回收...65",
    "pre_freq_bag" = "使用可重复使用的购物袋...66",
    "pre_freq_energy_appl" = "选择节能但是售价更贵的电器...67",
    "now_freq_bus" = "20 请您依据平时的行为频率进行选择。—搭乘公共交通",
    "now_freq_walk" = "骑行或步行出行...69",
    "now_freq_off" = "关闭不用电器的电源...70",
    "now_freq_recycle" = "垃圾分类与回收...71",
    "now_freq_bag" = "使用可重复使用的购物袋...72",
    "now_freq_energy_appl" = "选择节能但是售价更贵的电器...73",
    "change_bus" = "21 请问上述行为在使用该APP之后是否发生了改变?—搭乘公共交通",
    "change_walk" = "骑行或步行出行...75",
    "change_off" = "关闭不用电器的电源...76",
    "change_recycle" = "垃圾分类与回收...77",
    "change_bag" = "使用可重复使用的购物袋...78",
    "change_energy_appl" = "选择节能但是售价更贵的电器...79",
    "feature_add" = "22 如果该APP可以添加一个功能或服务，您最希望是什么?",
    "endorsement_celeb" = "—23 如果APP有明星或公众人物代言，我会更关注这个APP。",
    "video_intro" = "24 如果APP有视频/短片介绍，我更愿意点击了解内容。",
    "env_vs_self" = "25 当APP介绍的是“环保意义”而不是“个人利益”（如积分、优惠券）时，我反而不太有兴趣继续使用。",
    "rank_motivate" = "26 如果APP提供“碳足迹排行”或“减排成就”，我会更有动力坚持使用该APP。",
    "image_motivate" = "27 我觉得相比文字，APP界面上的图像或动画更能激发我使用APP的兴趣。",
    "result_motivate" = "28 当我看到累计的碳减排成效，我更愿意维持低碳行为。",
    "open_comment" = "29 感谢您对本次调查的支持，您可以在下方写下您对低碳减排APP或者碳普惠的想法或建议。"
  ) %>% 
  mutate(app_freq = as.numeric(app_freq))

# 变量重命名：按模型变量名。
data <- data_raw %>%
  rename(
    Q1 = percep_ui,
    Q2 = percep_integration,
    Q3 = percep_auto_record,
    Q4 = percep_guidance,
    Q5 = percep_awareness,
    Q6 = percep_info_use,
    Q7 = percep_faster_choice,
    Q13 = show_off,
    Q24 = app_freq,
    B1 = change_bus,
    B2 = change_walk,
    B3 = change_off,
    B4 = change_recycle
  )

# 假设检验 ----
# 信度检查。
alpha(data[paste0("Q", 1:7)])
# Bug：另一种方式，但是结果不同？但是要先运行建模过程。
# reliability(fit)

# 正态性检查。
# Bug：无法通过？
apply(data[c("Q1", "Q2", "Q3", "Q4")], 2, shapiro.test)

# 测量模型 ----
model_cfa <- '
  PEOU =~ Q1 + Q2 + Q3 + Q4
  PU   =~ Q5 + Q6 + Q7
  BI   =~ Q13 + Q24
  BC   =~ B1 + B2 + B3 + B4
'

fit_cfa <- cfa(model_cfa, data = data, estimator = "MLR")
summary(fit_cfa, fit.measures = TRUE, standardized = TRUE)

# 修正模型 ----
modindices(fit_cfa, sort.=TRUE, minimum.value = 10)

# TAM模型 ----
model_tam <- '
  # 测量模型
  PEOU =~ Q1 + Q2 + Q3 + Q4
  PU   =~ Q5 + Q6 + Q7
  BI   =~ Q13 + Q24
  BC   =~ B1 + B2 + B3 + B4

  # 结构路径
  PU ~ PEOU
  BI ~ PEOU + PU
  BC ~ BI
'

# 拟合模型。
# Bug: 选择部分数据建模。
fit <- sem(model_tam, data = data %>% filter(age < 6), estimator = "MLR")

# 输出结果
summary(fit, fit.measures = TRUE, standardized = TRUE)

# 作图。
library(lavaanPlot)
lavaanPlot(
  model = fit, # 显示路径系数。
  coefs = TRUE, 
  stand = TRUE, # 使用标准化系数。
  covs = TRUE, # 显示协方差。 
  stars = "regress" # 显示显著性星号。
)




