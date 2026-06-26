###############################################################################
#
# Functions to format dataframes into Latex
#
###############################################################################



#' Clean Simulation and Test Statistic Strings for LaTeX
#' @description Fixes truncated p-values (e.g., "(0)" -> "(0.00)", "(1)" -> "(1.00)") 
#' to ensure proper alignment in academic tables.
clean_stat_string <- function(x) {
  x <- as.character(x)
  # Fix zero p-values: "(0)" or "(.0)" -> "(0.00)"
  x <- gsub("\\((0|\\.0+)\\)", "(0.00)", x)
  # Fix integer p-values: "(1)" or "(1.0)" -> "(1.00)"
  x <- gsub("\\((1|1\\.0+)\\)", "(1.00)", x)
  return(x)
}

export_eval_to_latex <- function(eval_df, output_path = NULL) {
  #Eextract metadata from the first row of the data frame (these are repeated over all lines)
  model_name  <- as.character(eval_df$Model[1])
  benchmark   <- as.character(eval_df$Benchmark[1])
  est_start   <- as.character(eval_df$Estimation_start[1])
  eval_period <- as.character(eval_df$Evaluation_period[1])
  scheme      <- as.character(eval_df$Estimation_scheme[1])
  
  # Construct the LaTeX code block
  latex_lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    sprintf("\\caption{Out-of-Sample Predictive Evaluation: %s vs. %s Benchmark}", model_name, benchmark),
    sprintf("\\label{tab:%s_evaluation}", lclog <- gsub(" ", "_", tolower(model_name))),
    "\\footnotesize",
    "\\begin{tabular}{l cccccccc}",
    "\\hline\\hline",
    "\\rule{0pt}{3ex}",
    "\\textbf{H} & \\textbf{MSFE} & \\textbf{DM MSFE} & \\textbf{MAFE} & \\textbf{DM MAFE} & \\textbf{MZ Test} & \\textbf{Unbias} & \\textbf{Strong Eff} & \\textbf{Weak Eff} \\\\[0.5ex]",
    "\\hline",
    "\\rule{0pt}{2.5ex}"
  )
  
  # Loop through the rows and format the values
  for(i in 1:nrow(eval_df)) {
    horizon   <- eval_df$Horizon[i]
    msfe_rat  <- sprintf("%.2f", as.numeric(eval_df$MSFE_ratio[i]))
    mafe_rat  <- sprintf("%.2f", as.numeric(eval_df$MAFE_ratio[i]))
    
    # Referencing the standalone global helper function
    dm_msfe   <- clean_stat_string(eval_df$DM_MSFE[i])
    dm_mafe   <- clean_stat_string(eval_df$DM_MAFE[i])
    mz_test   <- clean_stat_string(eval_df$MZ[i])
    unbias    <- clean_stat_string(eval_df$Unbias[i])
    strong    <- clean_stat_string(eval_df$Strong_Eff[i])
    weak      <- clean_stat_string(eval_df$Weak_Eff[i])
    
    row_line  <- sprintf("%d & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
                         horizon, msfe_rat, dm_msfe, mafe_rat, dm_mafe, mz_test, unbias, strong, weak)
    latex_lines <- c(latex_lines, row_line)
  }
  
  # 5. Dynamically construct the academic footnote using your required dates
  footnote <- sprintf(
    paste0(
      "\\hline\\hline\n",
      "\\end{tabular}%%\n",
      "%% \\\\\n",
      "\\par\\smallskip\n",
      "\\parbox{\\textwidth}{\\scriptsize \\textit{Notes:} This table reports rolling ",
      "pseudo-out-of-sample forecast evaluation metrics for the %s against an ",
      "unconstrained %s benchmark. The evaluation period spans %s using a %s ",
      "estimation scheme with a benchmark sample start date of %s. Columns 2 and 4 ",
      "present forecast error variance and absolute ratios (values $< 1.00$ denote ",
      "model outperformance). Tests are as follows: Diebold-Mariano (DM) statistics, ",
      "Mincer-Zarnowitz (MZ) forecast rationality properties, Systemic over or ",
      "underprediction (Unbias), if the forecast itself predicts forecast errors (Strong),",
      "and serial correlation in forecast errors (Weak). ",
      "All reported values display calculated test statistics with asymptotic ",
      "$p$-values provided in parentheses.}"
    ),
    model_name, benchmark, eval_period, scheme, est_start
  )
  
  latex_lines <- c(latex_lines, footnote, "\\end{table}")
  
  # 6. Combine the character vectors into a single cleanly spaced string block
  final_latex <- paste(latex_lines, collapse = "\n")
  
  # 7. Output handler: Write to file or dump straight to the R console
  if (!is.null(output_path)) {
    writeLines(final_latex, output_path)
    message(paste("Successfully saved LaTeX table to:", output_path))
  } else {
    cat(final_latex, "\n")
  }
  
  return(invisible(final_latex))
}



#' Generate and Export Parameter Specification LaTeX Tables
#'
#' @param manifest_source List. The nested list configuration structure.
#' @param model_name Character. Human-readable name of the target model for labels.
#' @param save_path Character. Clean system path destination where the .tex file will be written.
#'
#' @return Character string containing the raw LaTeX code block.
#' @export
generate_param_latex_table <- function(manifest_source, model_name, save_path = NULL) {
  
  # 1. Initialize LaTeX Table Header Architecture
  tex_lines <- c(
    "\\begin{table}[htbp]",
    "\\centering",
    sprintf("\\caption{%s State-Space Model: Parameter Guesses and Constraints}", model_name),
    sprintf("\\label{tab:%s_ssm_specs}", tolower(gsub(" ", "_", model_name))),
    "\\begin{tabular}{llcl}",
    "\\hline\\hline",
    "\\textbf{Parameter Category} & \\textbf{Variable Identifier} & \\textbf{Initial Value} & \\textbf{Estimation Restriction Schema} \\\\ \\hline"
  )
  
  # 2. Group the components by their category attribute
  categories <- unique(sapply(manifest_source, function(x) x$category))
  
  for (cat in categories) {
    # Add category sub-header row line
    tex_lines <- c(tex_lines, sprintf("\\textit{%s} &&& \\\\", cat))
    
    # Filter variables belonging strictly to this group
    cat_items <- manifest_source[sapply(manifest_source, function(x) x$category == cat)]
    
    for (item in cat_items) {
      # Decode optimization rules into human-readable text strings
      restriction_text <- switch(as.character(item$rule),
                                 "0" = "Unconstrained (Linear Mapping)",
                                 "1" = "Strictly Positive Exponential ($>0$)",
                                 "2" = "Logit Probability Range ($[0, 1]$)",
                                 "3" = sprintf("Bounded Logistic Range: $[%s, %s]$", 
                                               format(item$low, nsmall = 3), format(item$high, nsmall = 3)),
                                 "Unknown Mapping Setup"
      )
      
      # Format individual row strings escaping the underscores safely for LaTeX compiler
      escaped_name <- gsub("_", "\\_", item$name, fixed = TRUE)
      row_string <- sprintf("    & \\texttt{%s} & %s & %s \\\\", 
                            escaped_name, 
                            format(item$val, nsmall = 2), 
                            restriction_text)
      
      tex_lines <- c(tex_lines, row_string)
    }
    tex_lines <- c(tex_lines, "\\hline")
  }
  
  # 3. Add Matrix Initial Closures Footer
  tex_lines <- c(
    "\\hline\\hline",
    "\\end{tabular}",
    "\\end{table}"
  )
  
  full_tex_code <- paste(tex_lines, collapse = "\n")
  
  # 4. Handle Disk Export Saving Operations
  if (!is.null(save_path)) {
    dir_target <- dirname(save_path)
    if (!dir.exists(dir_target)) dir.create(dir_target, recursive = TRUE)
    
    writeLines(full_tex_code, con = save_path)
    message(sprintf("[SUCCESS] Exported %s parameter LaTeX matrix file to: %s", model_name, save_path))
  }
  
  return(full_tex_code)
}