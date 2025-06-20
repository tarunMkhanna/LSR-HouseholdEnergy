# Function to extract information from model runs into a dataframe

# Simple unrestricted random effects 
tidy2 <- function (x, ...) {
  dum <- data.frame(model = rownames(coef(summary(x))), model1 = rownames(coef(summary(x))), beta = coef(summary(x))$estimate, se = coef(summary(x))$se, pvalues = coef(summary(x))$pval, 
                    ci.up = coef(summary(x))$ci.ub, ci.low = coef(summary(x))$ci.lb,M = summary(x)$k, I2 = summary(x)$I2)
  return(dum)
}  

# multilevel model 
tidy3 <- function (x, ...) {
  dum <- data.frame(model = rownames(coef(summary(x))), model1 = rownames(coef(summary(x))), beta = coef(summary(x))$estimate, se = coef(summary(x))$se, pvalues = coef(summary(x))$pval, 
                    upper_lim = coef(summary(x))$ci.ub, lower_lim = coef(summary(x))$ci.lb,M = summary(x)$k)
  return(dum)
}  

# Restricted variance estimation to take into account multicollinearity
tidy4 <-  function (x, ...) {
  dum <- data.frame(model = "avg", beta = x$b.r, SE = x$reg_table$SE, upper_lim = x$reg_table$CI.U, lower_lim = x$reg_table$CI.L, 
                    prob = x$reg_table$prob, M = x$M, dfs = x$dfs)
  
  return(dum)
}

##########################

#Functions used to supply information into texreg

#function to extract information from the summary of rma output
tidy2.rma <- function (x, ...) {
  ret <- createTexreg(coef.names = rownames(coef(summary(x))), 
                      coef = coef(summary(x))$estimate,
                      se = coef(summary(x))$se,
                      pvalues = coef(summary(x))$pval, 
                      ci.up = coef(summary(x))$ci.ub, 
                      ci.low = coef(summary(x))$ci.lb,
                      gof.names = c("No. of Effects", "I2", "CR.lb", "CR.ub"), 
                      gof =  c(summary(x)$k, summary(x)$I2, predict(x)$cr.lb, predict(x)$cr.ub), #summary(x)$R2, 
  )
  
  return(ret)
}

tidy2.reg <- function (x, ...) {
  ret <- createTexreg(coef.names = rownames(coef(summary(x))), 
                      coef = coef(summary(x))$estimate,
                      se = coef(summary(x))$se,
                      pvalues = coef(summary(x))$pval, 
                      # ci.up = coef(summary(x))$ci.ub, 
                      # ci.low = coef(summary(x))$ci.lb,
                      gof.names = c("No. of Effects", "I2", "R2"), 
                      gof =  c(summary(x)$k, summary(x)$I2, summary(x)$R2), #summary(x)$R2, 
  )
  
  return(ret)
}

#function to extract information from the summary of rma.mv output
tidy3.rma <- function (x, ...) {
  ret <- createTexreg(coef.names = rownames(coef(summary(x))), 
                      coef = coef(summary(x))$estimate,
                      se = coef(summary(x))$se,
                      pvalues = coef(summary(x))$pval, 
                      ci.up = coef(summary(x))$ci.ub, 
                      ci.low = coef(summary(x))$ci.lb,
                      gof.names = c("No. of Effects", "CR.lb", "CR.ub"),
                      gof =  c(summary(x)$k, predict(x)$cr.lb, predict(x)$cr.ub),
  )
  
  return(ret)
}

tidy3.reg <- function (x, ...) {
  ret <- createTexreg(coef.names = rownames(coef(summary(x))), 
                      coef = coef(summary(x))$estimate,
                      se = coef(summary(x))$se,
                      pvalues = coef(summary(x))$pval, 
                      # ci.up = coef(summary(x))$ci.ub, 
                      # ci.low = coef(summary(x))$ci.lb,
                      gof.names = c("No. of Effects"),
                      gof =  c(summary(x)$k),
  )
  
  return(ret)
}

#function to extract information from robumeta output
tidy4.robu <-  function (x, ...) {
  
  dum <- data.frame(model = x$reg_table$labels, beta = x$b.r, SE = x$reg_table$SE, prob = x$reg_table$prob)
  ret <- createTexreg(coef.names = as.character(dum$model),
                      coef = dum$beta,
                      se = dum$SE,
                      pvalues = dum$prob, 
                      gof.names = c("No. of Effects", "dfs", "I2"),
                      gof =  c(x$M, x$dfs, x$mod_info$I.2),
  )
  return(ret)
}

# function to compute model diagnostics

compute_I2 <- function(model) {
  # weight matrix
  W <- diag(1 / model$vi)
  
  # design matrix
  X <- model.matrix(model)
  
  # projection matrix
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  
  # total variance (denominator)
  denom <- sum(model$sigma2) + (model$k - model$p) / sum(diag(P))
  
  # I2 statistics
  I2_total   <- 100 * sum(model$sigma2)     / denom
  I2_between <- 100 * model$sigma2[1]       / denom
  I2_within  <- 100 * model$sigma2[2]       / denom
  
  return(list(
    I2_total = I2_total,
    I2_between = I2_between,
    I2_within = I2_within
  ))
}


# egger'S test for rma.mv object
egger_test_mv <- function(model) {
  # check if required fields exist
  if (is.null(model$yi) || is.null(model$vi)) {
    stop("The model must contain yi (effect sizes) and vi (sampling variances).")
  }
  
  # calculate standard errors and precision
  sei <- sqrt(model$vi)
  precision <- 1 / sei
  std_effects <- model$yi / sei
  
  # run Egger's regression
  egger_lm <- lm(std_effects ~ precision)
  egger_summary <- summary(egger_lm)
  
  # extract key results
  intercept <- coef(egger_lm)[1]
  p_value <- coef(egger_summary)[1, 4]
  se_intercept <- coef(egger_summary)[1, 2]
  t_value <- coef(egger_summary)[1, 3]
  
  return(list(
    intercept = intercept, # estimated deviation from funnel plot symmetry (a non-zero intercept suggests asymmetry)
    std_error = se_intercept,
    t_value = t_value,
    p_value = p_value,
    regression_model = egger_lm
  ))
}

recode_binary_predictors <- function(df) {
  df <- df %>%
    mutate(
      Feedback = as.numeric(recode(Feedback, "Feedback1" = 1, "Feedback0" = 0)),
      MonetaryRewards = as.numeric(recode(MonetaryRewards, "MonetaryRewards1" = 1, "MonetaryRewards0" = 0)),
      Motivation = as.numeric(recode(Motivation, "Motivation1" = 1, "Motivation0" = 0)),
      Information = as.numeric(recode(Information, "Information1" = 1, "Information0" = 0)),
      SocialComparison = as.numeric(recode(SocialComparison, "SocialComparison1" = 1, "SocialComparison0" = 0)),
      All = 1
    )
  return(df)
}

process_order_label <- function(df) {
  
  predictors <- c("SocialComparison", "Feedback", "MonetaryRewards", 
                  "Motivation", "Information")
  
  df <- df %>%
    mutate(
      order = rowSums(across(all_of(predictors))),
      label = recode(model1, !!!mapping_vector)
    )
  
  return(df)
}

prep_num_label <- function(df) {
  df %>%
    mutate(significance = case_when(
      !is.na(pvalues) & pvalues < 0.001 ~ paste0(round(beta, 2), "***  (", round(lower_lim, 2), ",", round(upper_lim, 2), ")"),
      !is.na(pvalues) & pvalues < 0.01 ~ paste0(round(beta, 2), "**  (", round(lower_lim, 2), ",", round(upper_lim, 2), ")"),
      !is.na(pvalues) & pvalues < 0.05  ~ paste0(round(beta, 2), "*  (", round(lower_lim, 2), ",", round(upper_lim, 2), ")"),
      TRUE ~ paste0(round(beta, 2), "  (", round(lower_lim, 2), ",", round(upper_lim, 2), ")")
    ))
}
