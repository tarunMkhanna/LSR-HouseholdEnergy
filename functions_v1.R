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