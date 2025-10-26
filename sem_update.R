library(lavaan)
library(dplyr)
library(tidyr)
library(DiagrammeR)

# 定义模型 ----
# 模型 1: M1_Base (基准五因子模型)
model_1_base_df <- data.frame(
  Latent_Variable = c(
    rep("PFV", 3),
    rep("PIV", 3),
    # rep("PB", 3),
    # rep("RD", 3),
    rep("BC", 3)
  ),
  Observed_Variable = c(
    "q4_ui_simple", "q4_auto_record", "q4_clear_guidance",
    "q13_attract_points", "q13_attract_credit", "q13_attract_daily_task",
    # "q15_barrier_unknown", "q15_barrier_privacy", "q15_barrier_low_reward",
    # "q20_cumulative_inspire", "q20_image_inspire", "q20_ranking_motivation",
    "q18_change_public_trans", "q18_change_bike_walk", "q18_change_turn_off_power"
  )
)

# 模型 2: M2_Value (价值-障碍-行为三因子模型)
model_2_value_df <- data.frame(
  Latent_Variable = c(
    rep("PV", 9),
    rep("PB", 3),
    rep("BC", 3)
  ),
  Observed_Variable = c(
    "q4_ui_simple", "q4_auto_record", "q4_clear_guidance",
    "q13_attract_points", "q13_attract_credit", "q13_attract_daily_task",
    "q20_cumulative_inspire", "q20_image_inspire", "q20_ranking_motivation",
    "q15_barrier_unknown", "q15_barrier_privacy", "q15_barrier_low_reward",
    "q18_change_public_trans", "q18_change_bike_walk", "q18_change_turn_off_power"
  )
)

# 模型 3: M3_TAM (技术接受模型四因子变体)
model_3_tam_df <- data.frame(
  Latent_Variable = c(
    rep("EOU", 2),
    rep("PR", 3),
    rep("PB", 3),
    rep("BC", 3)
  ),
  Observed_Variable = c(
    "q4_ui_simple", "q4_auto_record",
    "q13_attract_points", "q13_attract_daily_task", "q20_cumulative_inspire",
    "q15_barrier_unknown", "q15_barrier_privacy", "q15_barrier_low_reward",
    "q18_change_public_trans", "q18_change_bike_walk", "q18_change_turn_off_power"
  )
)

# 模型 4: M4_Simplified_BC (简化行为结果模型)
model_4_simplified_bc_df <- data.frame(
  Latent_Variable = c(
    rep("PFV", 3),
    rep("PIV", 3),
    rep("PB", 3),
    rep("BC_Lite", 2)
  ),
  Observed_Variable = c(
    "q4_ui_simple", "q4_auto_record", "q4_clear_guidance",
    "q13_attract_points", "q13_attract_credit", "q13_attract_daily_task",
    "q15_barrier_unknown", "q15_barrier_privacy", "q15_barrier_low_reward",
    "q18_change_bike_walk", "q18_change_turn_off_power"
  )
)

# 模型 5: M5_TPB_Attitude (态度焦点模型)
model_5_tpb_attitude_df <- data.frame(
  Latent_Variable = c(
    rep("ATT", 9),
    rep("PBC", 3),
    rep("BC", 3)
  ),
  Observed_Variable = c(
    "q4_ui_simple", "q4_auto_record", "q4_clear_guidance",
    "q13_attract_points", "q13_attract_credit", "q13_attract_daily_task",
    "q20_cumulative_inspire", "q20_image_inspire", "q20_ranking_motivation",
    "q15_barrier_unknown", "q15_barrier_privacy", "q15_barrier_low_reward",
    "q18_change_public_trans", "q18_change_bike_walk", "q18_change_turn_off_power"
  )
)

# 模型 6: M6_Two_Incentive (区分物质与非物质激励)
model_6_two_incentive_df <- data.frame(
  Latent_Variable = c(
    rep("PFV", 3),
    rep("ME", 2),
    rep("VE", 3),
    rep("PB", 3),
    rep("BC", 3)
  ),
  Observed_Variable = c(
    "q4_ui_simple", "q4_auto_record", "q4_clear_guidance",
    "q13_attract_points", "q13_attract_credit",
    "q20_cumulative_inspire", "q20_image_inspire", "q20_ranking_motivation",
    "q15_barrier_unknown", "q15_barrier_privacy", "q15_barrier_low_reward",
    "q18_change_public_trans", "q18_change_bike_walk", "q18_change_turn_off_power"
  )
)

# 关键函数 ----
# 函数：从测量模型数据框生成 dot 代码的各个部分。
generate_dot_components_from_df <- function(df) {
  # 1. 检查输入数据框是否包含必要的列
  if (!all(c("Latent_Variable", "Observed_Variable") %in% names(df))) {
    stop("输入数据框必须包含 'Latent_Variable' 和 'Observed_Variable' 两列。")
  }
  
  # 2. 提取并去重所有潜在变量名
  latent_vars <- unique(df$Latent_Variable)
  
  # 3. 构建第一个字符串：潜在变量列表
  #    例如: "ATT; PBC; BC;"
  lv_list_string <- paste0(paste(latent_vars, collapse = "; "), ";")
  
  # 4. 构建第二个字符串：测量路径
  #    按潜在变量分组观测变量
  model_by_lv <- split(df$Observed_Variable, df$Latent_Variable)
  
  measurement_paths_lines <- character()
  for (lv_name in names(model_by_lv)) {
    ovs <- model_by_lv[[lv_name]]
    ov_string <- paste(ovs, collapse = " ") # 观测变量之间用空格分隔
    path_line <- paste0(lv_name, " -> {", ov_string, "};")
    measurement_paths_lines <- c(measurement_paths_lines, path_line)
  }
  measurement_paths_string <- paste(measurement_paths_lines, collapse = "\n")
  
  # 5. 构建第三个字符串：潜在变量协方差路径
  #    为所有潜在变量对生成双向箭头
  covariance_paths_lines <- character()
  if (length(latent_vars) > 1) {
    for (i in 1:(length(latent_vars) - 1)) {
      for (j in (i + 1):length(latent_vars)) {
        path_line <- paste0(latent_vars[i], " -> ", latent_vars[j], ";")
        covariance_paths_lines <- c(covariance_paths_lines, path_line)
      }
    }
  }
  covariance_paths_string <- paste(covariance_paths_lines, collapse = "\n")
  
  # 6. 返回结果列表
  return(list(
    lv_list = lv_list_string,
    measurement_paths = measurement_paths_string,
    covariance_paths = covariance_paths_string
  ))
}

# 函数：基于潜变量和观测变量数据框，绘图。
plt_cfa_structure_from_df <- function(measure_model_df) {
  # 生成 dot 代码的各个部分
  dot_parts <- generate_dot_components_from_df(measure_model_df)
  
  # 绘图。
  dot_code <- paste0(
    "digraph CFA_Structure {\n",
    "  # Latent Variables (Ovals)\n",
    "  node [shape = oval, style = filled, fillcolor = '#ADD8E6'];\n",
    "  ", dot_parts$lv_list, "\n\n",
    
    "  # Observed Variables (Rectangles)\n",
    "  node [shape = box, style = filled, fillcolor = '#FFFFE0'];\n",
    "  ", dot_parts$measurement_paths, "\n\n",
    
    "  # CFA Assumption: Covariances (Latent <-> Latent)\n",
    "  edge [color = gray, dir = both, arrowhead = normal, arrowtail = normal];\n",
    "  ", dot_parts$covariance_paths, "\n",
    "}"
  )
  
  # 渲染图形
  # grViz(dot_code)
  return(dot_code)
}

# 函数：从数据框构建 lavaan CFA 模型语法
#' @title build_cfa_model_syntax_from_df
#' @description 从包含潜在变量和观测变量对应关系的数据框构建 lavaan CFA 模型语法字符串。
#'
#' @param df 一个数据框，必须包含两列：
#'   - Latent_Variable: 包含潜在变量名称的字符向量。
#'   - Observed_Variable: 包含观测变量名称的字符向量。
#'     每个观测变量名称必须是唯一的，且对应一个潜在变量。
#'
#' @return 一个字符向量，表示 lavaan CFA 模型语法。
#'         例如: "ATT =~ q1 + q2 + q3\nBC =~ q4 + q5"
#' @export
#' 
build_cfa_model_syntax_from_df <- function(df) {
  
  # 检查输入数据框是否包含必要的列
  if (!all(c("Latent_Variable", "Observed_Variable") %in% names(df))) {
    stop("输入数据框必须包含 'Latent_Variable' 和 'Observed_Variable' 两列。")
  }
  
  # 按 Latent_Variable 分组，将观测变量收集起来
  # 使用 aggregate 或 tapply 也可以，但 split + lapply 更直观
  model_by_lv <- split(df$Observed_Variable, df$Latent_Variable)
  
  cfa_formula_lines <- character()
  
  # 为每个潜在变量构建公式行
  for (lv_name in names(model_by_lv)) {
    ovs <- model_by_lv[[lv_name]]
    ov_string <- paste(ovs, collapse = " + ")
    formula_line <- paste0(lv_name, " =~ ", ov_string)
    cfa_formula_lines <- c(cfa_formula_lines, formula_line)
  }
  
  # 将所有公式行合并成一个完整的 lavaan 模型字符串，用换行符分隔
  constructed_cfa_model_string <- paste(cfa_formula_lines, collapse = "\n")
  
  return(constructed_cfa_model_string)
}

# 函数：拟合CFA模型并输出详细检测结果
fit_and_extract_cfa <- function(model_syntax, data, model_name, 
                                print_summary = TRUE, 
                                return_fit = FALSE) {
  # 尝试拟合模型,使用 tryCatch 来处理不收敛的错误
  fit_result <- tryCatch({
    # 使用 cfa() 函数进行拟合,默认潜在变量间自由相关
    cfa(model_syntax, data = data, estimator = "MLR", se = "robust") 
  }, error = function(e) {
    message(paste("模型拟合失败:", model_name, "- 错误:", e$message))
    return(NULL)
  })
  
  if (is.null(fit_result)) {
    cat("\n=================================================================\n")
    cat("模型:", model_name, "\n")
    cat("状态: 拟合失败 - 无法生成结果\n")
    cat("=================================================================\n\n")
    if (return_fit) {
      return(list(status = "Failed", fit = NULL))
    } else {
      return(NULL)
    }
  }
  
  # 检查模型是否收敛,并检查 Heywood Case (负方差)
  if (!lavInspect(fit_result, "converged")) {
    Status <- "未收敛 (No Convergence)"
  } else {
    # 检查潜在变量方差 (psi) 是否有负值 (Heywood Case)
    has_negative_variance <- tryCatch({
      any(lavInspect(fit_result, "est")$psi < 0)
    }, error = function(e) {
      FALSE
    })
    
    if (has_negative_variance) {
      Status <- "Heywood Case (存在负方差)"
    } else {
      Status <- "成功收敛"
    }
  }
  
  # 打印模型检测结果 - 无论收敛与否都输出
  if (print_summary) {
    cat("\n")
    cat("=================================================================\n")
    cat("模型:", model_name, "\n")
    cat("状态:", Status, "\n")
    cat("=================================================================\n\n")
    
    # 输出完整的CFA结果
    cat("--- 模型拟合摘要 (包含因子载荷、标准误、标准化系数) ---\n\n")
    print(summary(fit_result, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE))
    
    cat("\n--- 拟合指标详细信息 ---\n")
    print(fitMeasures(fit_result))
    
    # 尝试输出因子相关矩阵
    cat("\n--- 因子相关矩阵 ---\n")
    tryCatch({
      print(lavInspect(fit_result, "cor.lv"))
    }, error = function(e) {
      cat("无法提取因子相关矩阵:", e$message, "\n")
    })
    
    # 尝试输出残差协方差矩阵
    cat("\n--- 残差协方差矩阵 ---\n")
    tryCatch({
      print(residuals(fit_result, type = "cor")$cor)
    }, error = function(e) {
      cat("无法提取残差协方差矩阵:", e$message, "\n")
    })
    
    cat("\n")
  }
  
  # 根据参数决定返回内容
  if (return_fit) {
    return(list(status = Status, fit = fit_result))
  } else {
    return(Status)
  }
}

# 基于各个模型数据框，画图并输出模型CFA检测结果。
lapply(
  list(
    list(df = model_1_base_df, name = "M1_Base"),
    list(df = model_2_value_df, name = "M2_Value"),
    list(df = model_3_tam_df, name = "M3_TAM"),
    list(df = model_4_simplified_bc_df, name = "M4_Simplified_BC"),
    list(df = model_5_tpb_attitude_df, name = "M5_TPB_Attitude"),
    list(df = model_6_two_incentive_df, name = "M6_Two_Incentive")
  ),
  function(model_info) {
    print(plt_cfa_structure_from_df(model_info$df))
    model_syntax <- build_cfa_model_syntax_from_df(model_info$df)
    fit_and_extract_cfa(
      model_syntax = model_syntax,
      data = data,
      model_name = model_info$name,
      print_summary = TRUE,
      return_fit = FALSE
    )
  }
)

# 如果只画图。
lapply(
  list(
    list(df = model_1_base_df, name = "M1_Base"),
    list(df = model_2_value_df, name = "M2_Value"),
    list(df = model_3_tam_df, name = "M3_TAM"),
    list(df = model_4_simplified_bc_df, name = "M4_Simplified_BC"),
    list(df = model_5_tpb_attitude_df, name = "M5_TPB_Attitude"),
    list(df = model_6_two_incentive_df, name = "M6_Two_Incentive")
  ),
  function(model_info) {
    print(plt_cfa_structure_from_df(model_info$df))
  }
)




