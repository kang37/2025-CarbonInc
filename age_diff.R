# 检测不同年龄段的人在各题上的选择是否有差异。

# 性别相对均衡。
table(data_raw$age, data_raw$gender)

chisq.test(table(data_raw$age, data_raw$q1_app_used))
# 如果去除<18岁受访者。
chisq.test(table(data_raw$age, data_raw$q1_app_used)[-1, ])

kruskal.test(data_raw$percep_ui, data_raw$age)
dunn.test::dunn.test(data_raw$percep_ui, data_raw$age)


library(FSA) 

kw_dunn_age6 <- function(data, age_var, score_var, alpha  = 0.05) {
  ## 1. 提取并保证是因子 ---------------------------------------------
  df <- data %>%
    transmute(age  = .data[[age_var]], score = .data[[score_var]]) %>%
    mutate(age = as.factor(age)) %>% 
    filter(!is.na(age), !is.na(score))
  
  ## 2. Kruskal–Wallis -------------------------------------------------
  kw <- kruskal.test(score ~ age, data = df)
  
  ## 3. Dunn 两两比较 --------------------------------------------------
  dunn <- dunnTest(score ~ age, data = df, kw = FALSE)
  
  # 把 Pair 字段拆开成两列
  comp <- dunn$res %>%
    tidyr::separate(Comparison, into = c("g1", "g2"), sep = " - ") %>%
    mutate(g1 = as.numeric(g1),
           g2 = as.numeric(g2),
           adjP = .data[[paste0("P.adj")]])
  
  ## 4. 挑出涉及最大组 (age_max) 的显著差异 ---------------------------
  sig_vs_max <- comp %>%
    filter((g1 == "6" | g2 == "6") & adjP < alpha) %>%
    mutate(other_grp = ifelse(g1 == "6", g2, g1)) %>%
    arrange(adjP)
  
  if(kw$p.value >= alpha & nrow(sig_vs_max) == 0) {
    cat(sprintf("%s 年龄最大组与其他组之间均无显著差异 (α = %.3f)", score_var, alpha))
  } else {
    cat(sprintf("%s 年龄最大组与以下组差异显著 (α = %.3f, 校正)：\n", score_var, 
                alpha))
    print(sig_vs_max %>% select(other_grp, Z = Z, adjP))
  }
  
  invisible(list(kw = kw, dunn = dunn, sig_vs_max = sig_vs_max))
}

lapply(
  c(
    "percep_ui", "percep_integration", "percep_auto_record", "percep_guidance",
    "percep_awareness", "percep_info_use", "percep_faster_choice", 
    "know_carbonph", "know_waste_sort", "peer_recommend", 
    "social_media_influence", "show_off", "app_freq", "pre_freq_bus",
    "pre_freq_walk", "pre_freq_off", "pre_freq_recycle", "pre_freq_bag",
    "pre_freq_energy_appl", "now_freq_bus", "now_freq_walk", "now_freq_off",
    "now_freq_recycle", "now_freq_bag", "now_freq_energy_appl", "change_bus",
    "change_walk", "change_off", "change_recycle", "change_bag",
    "change_energy_appl", "endorsement_celeb", "video_intro", "env_vs_self",
    "rank_motivate", "image_motivate"
  ), 
  function(x) kw_dunn_age6(data = data_raw, age_var = "age", score_var = x)
)

kw_dunn_age6(data = data_raw, age_var = "age", score_var = "percep_integration")
kw_dunn_age6(data = data_raw, age_var = "age", score_var = "percep_auto_record")

for (i in c(
  "percep_ui", "percep_integration", "percep_auto_record", "percep_guidance",
  "percep_awareness", "percep_info_use", "percep_faster_choice", 
  "know_carbonph", "know_waste_sort", "peer_recommend", 
  "social_media_influence", "show_off", "app_freq", "pre_freq_bus",
  "pre_freq_walk", "pre_freq_off", "pre_freq_recycle", "pre_freq_bag",
  "pre_freq_energy_appl", "now_freq_bus", "now_freq_walk", "now_freq_off",
  "now_freq_recycle", "now_freq_bag", "now_freq_energy_appl", "change_bus",
  "change_walk", "change_off", "change_recycle", "change_bag",
  "change_energy_appl", "endorsement_celeb", "video_intro", "env_vs_self",
  "rank_motivate", "image_motivate"
)) {
  kw_dunn_age6(data = data_raw, age_var = "age", score_var = i)
}
