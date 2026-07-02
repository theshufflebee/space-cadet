###############################################################################
#
# Functions to format dataframes into Latex
#
###############################################################################



#' Clean Strings for LaTeX
#' @description Fixes truncated p-values (e.g., "(0)" -> "(0.00)", "(1)" -> "(1.00)") 
#' to ensure proper alignment in academic tables.
#' 
#' @param x A character string of numbers or float.
#' 
#' @return The cleaned string 
#' 
clean_stat_string <- function(x) {
  x <- as.character(x)
  # Fix zero p-values: "(0)" or "(.0)" -> "(0.00)"
  x <- gsub("\\((0|\\.0+)\\)", "(0.00)", x)
  # Fix integer p-values: "(1)" or "(1.0)" -> "(1.00)"
  x <- gsub("\\((1|1\\.0+)\\)", "(1.00)", x)
  return(x)
}




#'Export Evaluation Table to Latex
#'
#'@description Transforms the outputs of the creaFcstEval Package into a latex table
#'
#'@param eval_df The creaFcstEval Df
#'@param output_path where to save the table. If NULL prints output instead
#'
#'@return Either saves a Latex table or prints it out
export_eval_to_latex <- function(eval_df, output_path = NULL) {
  #Extract metadata from the data frame
  # (these are repeated and are the same over all lines)
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
    
    # clean strings with helper
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
  
  # Construct the academic footnote inserting the needed information
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
  
  # Combine the character vectors into a single string block
  final_latex <- paste(latex_lines, collapse = "\n")
  
  # Output handling: Write to file or print out for Rmd
  if (!is.null(output_path)) {
    writeLines(final_latex, output_path)
    message(paste("Successfully saved LaTeX table to:", output_path))
  } else {
    cat(final_latex, "\n")
  }
  
  return(invisible(final_latex))
}