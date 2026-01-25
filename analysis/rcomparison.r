#load cocor package
library(cocor)
# Check if readxl is installed, if not, install it
if (!require("readxl")) install.packages("readxl")
library(readxl)

# 1. Read the CSV file
# df_en <- read_excel("analysis/En_all_models_results.xlsx")
df_nl <- read_excel("analysis/Nl_all_models_results.xlsx")

# 2. Define dimensions and mapping
# Dimensions list from Study 2
dimensions_en <- c('auditory', 'gustatory', 'haptic', 'interoceptive', 'olfactory', 'visual', 'foot', 'hand', 'head', 'mouth', 'torso')
dimensions_nl <- c("Horen", "Proeven", "Voelen", "Sensaties", "Ruiken", "Zien")


# Helper function to find column name ignoring case
get_actual_col <- function(expected, all_cols) {
  idx <- which(tolower(all_cols) == tolower(expected))
  if (length(idx) > 0) return(all_cols[idx[1]])
  return(NULL)
}

# Initialize results data frame
results_df <- data.frame(
  Dimension = character(),
  base_Spearman = numeric(),
  en_ft_Spearman = numeric(),
  nl_ft_Spearman = numeric(),
  #QA_ft_Spearman = numeric(),
  
  # Comparisons
  # Base vs en
  Z_base_vs_en = numeric(),
  p_base_vs_en = numeric(),

  # Base vs nl
  Z_base_vs_nl = numeric(),
  p_base_vs_nl = numeric(),

  # Base vs QA
  #Z_base_vs_QA = numeric(),
  #p_base_vs_QA = numeric(),

  # en vs nl
  Z_en_vs_nl = numeric(),
  p_en_vs_nl = numeric(),
  
  # en vs QA
  #Z_en_vs_QA = numeric(),
  #p_en_vs_QA = numeric(),

  # nl vs QA
  #Z_nl_vs_QA = numeric(),
  #p_nl_vs_QA = numeric(),

  stringsAsFactors = FALSE
)

# Helper function for Steiger's test
# r.jk: r(Human, Model A)
# r.jh: r(Human, Model B)
# r.kh: r(Model A, Model B)
calc_steiger <- function(r.jk, r.jh, r.kh, n) {
  if (n < 4) return(list(z = NA, p = NA))

  # Using cocor
  # cocor.dep.groups.overlap compares two correlations based on dependent groups with one variable in common.
  res <- cocor.dep.groups.overlap(
    r.jk = r.jk, r.jh = r.jh, r.kh = r.kh, n = n,
    alternative = "two.sided", alpha = 0.05, conf.level = 0.95, null.value = 0
  )
  
  # Extract Z and p from cocor result object (Steiger 1980)
  z_val <- res@steiger1980$statistic
  p_val <- res@steiger1980$p.value
  
  return(list(z = z_val, p = p_val))
}

# 3. Loop through dimensions
for (dim in dimensions_nl) {
  # Construct expected names (using the dimension name directly)
  expected_human <- paste0(dim, "")
  expected_base <- paste0(dim, "_base")
  expected_en <- paste0(dim, "_en_ft")
  expected_nl <- paste0(dim, "_nl_ft")
  #expected_qa <- paste0(dim, "_QA_ft")

  
  # Find actual column names (case-insensitive)
  col_human <- get_actual_col(expected_human, names(df_nl))
  col_base <- get_actual_col(expected_base, names(df_nl))
  col_en <- get_actual_col(expected_en, names(df_nl))
  col_nl <- get_actual_col(expected_nl, names(df_nl))
  #col_qa <- get_actual_col(expected_qa, names(df))
  
  # Check if columns exist
  cols_found <- c(col_human, col_base, col_en, col_nl)
  if (any(sapply(cols_found, is.null))) {
    warning(paste("Missing columns for dimension:", dim))
    next
  }
  
  # Use the found column names
  cols_needed <- unlist(cols_found)
  
  # Subset data
  sub_data <- na.omit(df_nl[cols_needed])
  n <- nrow(sub_data)
  
  if (n < 3) {
    warning(paste("Not enough data for dimension:", dim))
    next
  }
  
  # Calculate Spearman correlations
  # Human vs Models
  r_h_base <- cor(sub_data[[col_human]], sub_data[[col_base]], method = "spearman")
  r_h_en <- cor(sub_data[[col_human]], sub_data[[col_en]], method = "spearman")
  r_h_nl <- cor(sub_data[[col_human]], sub_data[[col_nl]], method = "spearman")
  #r_h_qa <- cor(sub_data[[col_human]], sub_data[[col_qa]], method = "spearman")
  
  # Inter-model correlations (needed for Steiger's)
  r_base_en <- cor(sub_data[[col_base]], sub_data[[col_en]], method = "spearman")
  r_base_nl <- cor(sub_data[[col_base]], sub_data[[col_nl]], method = "spearman")
  r_en_nl <- cor(sub_data[[col_en]], sub_data[[col_nl]], method = "spearman")
  #r_base_qa <- cor(sub_data[[col_base]], sub_data[[col_qa]], method = "spearman")
  #r_en_qa <- cor(sub_data[[col_en]], sub_data[[col_qa]], method = "spearman")
  #r_nl_qa <- cor(sub_data[[col_nl]], sub_data[[col_qa]], method = "spearman")

  # Perform Tests
  
  #1. Base vs En
  test1 <- calc_steiger(r_h_base, r_h_en, r_base_en, n)
  
  #2. Base vs Nl
  test2 <- calc_steiger(r_h_base, r_h_nl, r_base_nl, n)

  #3. En vs Nl
  test3 <- calc_steiger(r_h_en, r_h_nl, r_en_nl, n)

  # 4. En vs QA
  #test4 <- calc_steiger(r_h_en, r_h_qa, r_en_qa, n)

  # 5. Nl vs QA
  #test5 <- calc_steiger(r_h_nl, r_h_qa, r_nl_qa, n) 

  # 6. Base vs QA
  #test0 <- calc_steiger(r_h_base, r_h_qa, r_base_qa, n)


  # Add to results
  new_row <- data.frame(
    Dimension = dim,
    base_Spearman = r_h_base,
    en_ft_Spearman = r_h_en,
    nl_ft_Spearman = r_h_nl,
    #QA_ft_Spearman = r_h_qa,
  
    
    # Comparisons
    # Base vs En
    Z_base_vs_en = test1$z,
    p_base_vs_en = test1$p,
    # Base vs Nl
    Z_base_vs_nl = test2$z,
    p_base_vs_nl = test2$p,
    # Base vs QA
    #Z_base_vs_QA = test0$z,
    #p_base_vs_QA = test0$p,
    # En vs Nl
    Z_en_vs_nl = test3$z,
    p_en_vs_nl = test3$p
    # En vs QA
    #Z_en_vs_qa = test4$z,
    #p_en_vs_qa = test4$p,
    # Nl vs QA
    #Z_nl_vs_qa = test5$z,
    #p_nl_vs_qa = test5$p
  )
  
  results_df <- rbind(results_df, new_row)
}

# 4. Save results
output_file <- "nl_correlation_comparison_results.csv"
write.csv(results_df, output_file, row.names = FALSE)

print('done')
cat("\nResults saved to", output_file, "\n")
