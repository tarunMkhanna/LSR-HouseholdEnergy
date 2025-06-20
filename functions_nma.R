# Function to calculate SUCRA
sucra = function(x, lower.is.better = FALSE) {
  rank.probability = x
  mat = as.matrix(rank.probability)
  a = ncol(mat)
  j = nrow(mat)
  names = rownames(mat)
  sucra = numeric()
  for (x in 1:j) {
    sucra[x] = sum(cumsum(mat[x, 1:(a - 1)]))/(a - 1)
  }
  if (lower.is.better == TRUE) {
    sucra = numeric()
    for (x in 1:j) {
      sucra[x] = 1 - sum(cumsum(mat[x, 1:(a - 1)]))/(a - 1)
    }
  }
  res = data.frame(Treatment = names, SUCRA = sucra)
  res = res[order(-res$SUCRA), ]
  rownames(res) = 1:j
  rownames(res) = res$Treatment
  res$Treatment = NULL
  class(res) = "sucra"
  invisible(res)
  res
}

# Intervention Mapping
mapping_vector <- c(
  "Control" = "Control",
  "Feedback0:Information1:SocialComparison0:Motivation0:MonetaryRewards0" = "Information",
  "Feedback0:Information1:SocialComparison1:Motivation0:MonetaryRewards0" = "Information_Social",
  "Feedback0:Information0:SocialComparison0:Motivation0:MonetaryRewards0" = "None",
  "Feedback0:Information0:SocialComparison0:Motivation0:MonetaryRewards1" = "Monetary",
  "Feedback0:Information1:SocialComparison1:Motivation1:MonetaryRewards0" = "Information_Social_Motivation", 
  "Feedback0:Information1:SocialComparison0:Motivation1:MonetaryRewards0" = "Information_Motivation",
  "Feedback0:Information0:SocialComparison1:Motivation0:MonetaryRewards0" = "Social",
  "Feedback0:Information1:SocialComparison0:Motivation0:MonetaryRewards1" = "Information_Monetary", 
  "Feedback0:Information0:SocialComparison0:Motivation1:MonetaryRewards0" = "Motivation", 
  "Feedback0:Information1:SocialComparison1:Motivation0:MonetaryRewards1" = "Information_Social_Monetary", 
  "Feedback0:Information0:SocialComparison1:Motivation1:MonetaryRewards0" = "Social_Motivation",
  "Feedback0:Information0:SocialComparison1:Motivation0:MonetaryRewards1" = "Social_Monetary",
  "Feedback0:Information0:SocialComparison0:Motivation1:MonetaryRewards1" = "Motivation_Monetary",
  "Feedback0:Information0:SocialComparison1:Motivation1:MonetaryRewards1" = "Social_Motivation_Monetary",
  "Feedback0:Information1:SocialComparison1:Motivation1:MonetaryRewards1" = "Information_Social_Motivation_Monetary",
  
  "Feedback1:Information1:SocialComparison0:Motivation0:MonetaryRewards0" = "Feedback_Information",
  "Feedback1:Information1:SocialComparison1:Motivation0:MonetaryRewards0" = "Feedback_Information_Social",
  "Feedback1:Information0:SocialComparison0:Motivation0:MonetaryRewards0" = "Feedback",
  "Feedback1:Information0:SocialComparison0:Motivation0:MonetaryRewards1" = "Feedback_Monetary",
  "Feedback1:Information1:SocialComparison1:Motivation1:MonetaryRewards0" = "Feedback_Information_Social_Motivation", 
  "Feedback1:Information1:SocialComparison0:Motivation1:MonetaryRewards0" = "Feedback_Information_Motivation",
  "Feedback1:Information0:SocialComparison1:Motivation0:MonetaryRewards0" = "Feedback_Social",
  "Feedback1:Information1:SocialComparison0:Motivation0:MonetaryRewards1" = "Feedback_Information_Monetary", 
  "Feedback1:Information0:SocialComparison0:Motivation1:MonetaryRewards0" = "Feedback_Motivation", 
  "Feedback1:Information1:SocialComparison1:Motivation0:MonetaryRewards1" = "Feedback_Information_Social_Monetary", 
  "Feedback1:Information0:SocialComparison1:Motivation1:MonetaryRewards0" = "Feedback_Social_Motivation",
  "Feedback1:Information0:SocialComparison1:Motivation0:MonetaryRewards1" = "Feedback_Social_Monetary",
  "Feedback1:Information1:SocialComparison0:Motivation1:MonetaryRewards1" = "Feedback_Information_Motivation_Monetary",
  
  "Feedback0:Information1:SocialComparison0:Motivation0:MonetaryRewards0" = "Information",
  "Feedback0:Information1:SocialComparison1:Motivation0:MonetaryRewards0" = "Information_Social",
  "Feedback0:Information0:SocialComparison0:Motivation0:MonetaryRewards0" = "DynamicPricing",
  "Feedback0:Information0:SocialComparison0:Motivation0:MonetaryRewards1" = "Monetary",
  "Feedback0:Information1:SocialComparison1:Motivation1:MonetaryRewards0" = "Information_Social_Motivation", 
  "Feedback0:Information1:SocialComparison0:Motivation1:MonetaryRewards0" = "Information_Motivation",
  "Feedback0:Information0:SocialComparison1:Motivation0:MonetaryRewards0" = "Social",
  "Feedback0:Information1:SocialComparison0:Motivation0:MonetaryRewards1" = "Information_Monetary", 
  "Feedback0:Information0:SocialComparison0:Motivation1:MonetaryRewards0" = "Motivation", 
  "Feedback0:Information1:SocialComparison1:Motivation0:MonetaryRewards1" = "Information_Social_Monetary", 
  "Feedback0:Information0:SocialComparison1:Motivation1:MonetaryRewards0" = "Social_Motivation",
  "Feedback0:Information0:SocialComparison1:Motivation0:MonetaryRewards1" = "Social_Monetary",
  "Feedback0:Information0:SocialComparison0:Motivation1:MonetaryRewards1" = "Motivation_Monetary",
  "Feedback0:Information0:SocialComparison1:Motivation1:MonetaryRewards1" = "Social_Motivation_Monetary",
  "Feedback0:Information1:SocialComparison1:Motivation1:MonetaryRewards1" = "Information_Social_Motivation_Monetary",
  
  "Feedback1:Information1:SocialComparison0:Motivation0:MonetaryRewards0" = "Feedback_Information",
  "Feedback1:Information1:SocialComparison1:Motivation0:MonetaryRewards0" = "Feedback_Information_Social",
  "Feedback1:Information0:SocialComparison0:Motivation0:MonetaryRewards0" = "Feedback",
  "Feedback1:Information0:SocialComparison0:Motivation0:MonetaryRewards1" = "Feedback_Monetary",
  "Feedback1:Information1:SocialComparison1:Motivation1:MonetaryRewards0" = "Feedback_Information_Social_Motivation", 
  "Feedback1:Information1:SocialComparison0:Motivation1:MonetaryRewards0" = "Feedback_Information_Motivation",
  "Feedback1:Information0:SocialComparison1:Motivation0:MonetaryRewards0" = "Feedback_Social",
  "Feedback1:Information1:SocialComparison0:Motivation0:MonetaryRewards1" = "Feedback_Information_Monetary", 
  "Feedback1:Information0:SocialComparison0:Motivation1:MonetaryRewards0" = "Feedback_Motivation", 
  "Feedback1:Information1:SocialComparison1:Motivation0:MonetaryRewards1" = "Feedback_Information_Social_Monetary", 
  "Feedback1:Information0:SocialComparison1:Motivation1:MonetaryRewards0" = "Feedback_Social_Motivation",
  "Feedback1:Information0:SocialComparison1:Motivation0:MonetaryRewards1" = "Feedback_Social_Monetary",
  "Feedback1:Information1:SocialComparison0:Motivation1:MonetaryRewards1" = "Feedback_Information_Motivation_Monetary",
  "Feedback0:Information1:SocialComparison0:Motivation1:MonetaryRewards1" = "Information_Motivation_Monetary"
)

mapping_vector_audit <- c(
  "Control" = "Control",
  "Feedback1:Information0:HomeAudit0:SocialComparison1:Motivation0:MonetaryIncentives0" = "Feedback_Social",
  "Feedback0:Information1:HomeAudit0:SocialComparison0:Motivation1:MonetaryIncentives0" = "Information_Motivation",
  "Feedback0:Information0:HomeAudit0:SocialComparison0:Motivation1:MonetaryIncentives1" = "Motivation_Monetary",
  "Feedback0:Information0:HomeAudit0:SocialComparison1:Motivation0:MonetaryIncentives0" = "Social",
  "Feedback0:Information1:HomeAudit0:SocialComparison0:Motivation0:MonetaryIncentives0" = "Information",
  "Feedback1:Information1:HomeAudit0:SocialComparison0:Motivation0:MonetaryIncentives0" = "Feedback_Information",
  "Feedback1:Information0:HomeAudit0:SocialComparison0:Motivation0:MonetaryIncentives0" = "Feedback",
  "Feedback1:Information0:HomeAudit0:SocialComparison1:Motivation1:MonetaryIncentives0" = "Feedback_Social_Motivation",
  "Feedback1:Information0:HomeAudit0:SocialComparison0:Motivation1:MonetaryIncentives0" = "Feedback_Motivation",
  "Feedback1:Information1:HomeAudit0:SocialComparison0:Motivation1:MonetaryIncentives0" = "Feedback_Information_Motivation",
  "Feedback1:Information0:HomeAudit0:SocialComparison0:Motivation0:MonetaryIncentives1" = "Feedback_Monetary",
  "Feedback0:Information0:HomeAudit0:SocialComparison0:Motivation0:MonetaryIncentives1" = "Monetary",
  "Feedback0:Information1:HomeAudit0:SocialComparison1:Motivation1:MonetaryIncentives0" = "Information_Social_Motivation",
  "Feedback0:Information1:HomeAudit0:SocialComparison1:Motivation0:MonetaryIncentives0" = "Information_Social",
  "Feedback0:Information0:HomeAudit0:SocialComparison0:Motivation1:MonetaryIncentives0" = "Motivation",
  "Feedback1:Information1:HomeAudit0:SocialComparison1:Motivation0:MonetaryIncentives0" = "Feedback_Information_Social",
  "Feedback1:Information0:HomeAudit0:SocialComparison1:Motivation0:MonetaryIncentives1" = "Feedback_Social_Monetary",
  "Feedback0:Information1:HomeAudit0:SocialComparison1:Motivation1:MonetaryIncentives1" = "Information_Social_Motivation",
  "Feedback1:Information1:HomeAudit0:SocialComparison1:Motivation1:MonetaryIncentives0" = "Feedback_Information_Social_Motivation",
  "Feedback0:Information0:HomeAudit1:SocialComparison0:Motivation0:MonetaryIncentives0" = "Audit",
  "Feedback1:Information1:HomeAudit0:SocialComparison1:Motivation0:MonetaryIncentives1" = "Feedback_Information_Social_Monetary",
  "Feedback0:Information0:HomeAudit0:SocialComparison1:Motivation0:MonetaryIncentives1" = "Social_Monetary",
  "Feedback0:Information1:HomeAudit0:SocialComparison0:Motivation0:MonetaryIncentives1" = "Information_Monetary",
  "Feedback1:Information1:HomeAudit0:SocialComparison0:Motivation1:MonetaryIncentives1" = "Feedback_Information_Monetary",
  "Feedback1:Information1:HomeAudit0:SocialComparison0:Motivation0:MonetaryIncentives1" = "Feedback_Information_Monetary",
  "Feedback0:Information1:HomeAudit1:SocialComparison0:Motivation0:MonetaryIncentives0" = "Information_Audit",
  "Feedback1:Information0:HomeAudit1:SocialComparison0:Motivation0:MonetaryIncentives0" = "Feedback_Audit",
  "Feedback0:Information0:HomeAudit0:SocialComparison1:Motivation1:MonetaryIncentives1" = "Social_Motivation_Monetary"
)


# Network Meta Analysis Function
network_meta_analysis <- function(df, adapt=10000, iter=100000, thin=10) {
  # dev.new(width=10, height=6)
  # Create the network meta-analysis model
  network <- mtc.network(data.re = df)
  
  # Summarize and plot the network
  print(summary(network))
  # x11()
  plot(network)
  
  # Set up the model
  model <- mtc.model(
    network = network,
    linearModel = "random",
    n.chain = 4
  )
  
  # Run the MCMC simulation
  set.seed(0)
  mc_results <- mtc.run(
    model = model,
    n.adapt = adapt,
    n.iter = iter,
    thin = thin
  )
  print(summary(mc_results))
  
  # Check convergence using Gelman-Rubin diagnostic
  # x11()
  gelman.plot(mc_results)
  print(gelman.diag(mc_results)$mpsrf)
  
  # Extract and format the results
  result_mat <- summary(mc_results)$summaries$statistics
  results_df <- data.frame(
    Treatment = rownames(result_mat),
    Mean = result_mat[, "Mean"],
    SD = result_mat[, "SD"],
    `95% CrI` = paste0("(", round(result_mat[, "Mean"] - 1.96 * result_mat[, "SD"], 2), ", ", round(result_mat[, "Mean"] + 1.96 * result_mat[, "SD"], 2), ")")
  )
  print(results_df)
  
  # Generate ranking probabilities, forest plots, and relative effect league table
  rank_all <- rank.probability(mc_results, preferredDirection = 1)
  # x11()
  plot(rank_all, beside = TRUE, cex.names = 0.5)
  # x11()
  
  #forest(relative.effect(mc_results, t1 = "Control")) # this line is currently causing an error
  league_tbl <- relative.effect.table(mc_results)
  # dev.off()
  
  # Calculate and plot SUCRA
  sucra_results <- sucra(rank.probability(mc_results), lower.is.better = FALSE)
  sucra_df <- data.frame(
    Treatment = attr(sucra_results, "row.names"),
    SUCRA = round(sucra_results$SUCRA, 3)
  ) %>% 
    arrange(desc(SUCRA))
  
  sucra_df$Treatment <- factor(sucra_df$Treatment, levels = sucra_df$Treatment)
  
  # x11()
  p<-ggplot(data = sucra_df, aes(x = Treatment, y = SUCRA)) +
    geom_point() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Treatment", y = "SUCRA", title = "Scatter Plot of Treatments vs. SUCRA Scores")
  print(p)
  
  # Generate creible interval of rankings
  sucra_qtl_df <- as.data.frame(rank.quantiles(rank.probability(mc_results)))
  
  colnames(sucra_qtl_df) <- c("CI_Lower", "Median", "CI_Upper")
  sucra_qtl_df$Treatment <- rownames(sucra_qtl_df)
  sucra_qtl_df <- sucra_qtl_df[, c("Treatment", "CI_Lower", "Median", "CI_Upper")]
  sucra_qtl_df <- sucra_qtl_df[order(sucra_qtl_df$Median),]
  print(sucra_qtl_df)
  
  # Return the results as a list
  list(
    Gelman_Rubin_Diagnostics = gelman.diag(mc_results)$mpsrf,
    Summary_Results = results_df,
    League_Table = league_tbl,
    SUCRA_Table = sucra_df, 
    SUCRA_Quantiles = rank.quantiles(rank.probability(mc_results))
  )
}

# Network Meta Regression Function
prep_mcr_df <- function(data, column, value, new_col_name) {
  data %>%
    filter(treatment != "Control") %>%
    mutate(!!sym(new_col_name) := ifelse(is.na(!!sym(column)) | !!sym(column) != value, 0, 1)) %>%
    select(-c(study, StudyDesign, Region, OptedIn, StatsMethod, Randomisation, Medium, Geographic_Scope)) %>%
    rename(study = study_indp)
}

# Network Meta Regression Function
network_meta_regression <- function(dataset, df, variable_name) {
  # Define network and regressor
  network.mr <- mtc.network(data.re = dataset, studies = df)
  
  regressor <- list(coefficient = 'shared', variable = variable_name, control = 'Control')
  
  # Model Compilation
  model.mr <- mtc.model(network.mr, type = "regression", regressor = regressor)
  
  mcr <- mtc.run(model.mr, n.adapt = 10000, n.iter = 100000, thin = 10)
  
  # Summary and DIC
  summary_output <- summary(mcr)
  print(summary_output)
  dic_value <- summary_output$DIC['DIC']
  
  # Forest plots for covariate = 1 and 0
  # forest(relative.effect(mcr, t1 = "Control", covariate = 1))
  # forest(relative.effect(mcr, t1 = "Control", covariate = 0))
  
  # Return DIC and summary
  return(data.frame(Variable = variable_name, DIC = dic_value))
}



























