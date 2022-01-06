not_sel <- "Not Selected"

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# 1. Numerical Variable ----

numerical_stat <- function(data_input, variable){
  
  colm <- data_input[, get(variable)]
  Statistics <- c("Numerical Variable:","Mean", "Median", "Std. dev.", "Min", 
                  "Max", "1st Quartile", "3rd Quartile", "5th percentile", "95th percentile")
  Values <- c(variable, round(mean(colm),2), round(median(colm),2), round(sd(colm),2), min(colm), 
              max(colm), quantile(colm, 0.25), quantile(colm, 0.75), quantile(colm, 0.05), quantile(colm, 0.95))
  #data.table(Statistics, Values)
  df <- data.frame("Statistics" = Statistics, "Values" = Values)
  return(df)
  
}

# 2. Factor Variable ----

factor_stat <- function(data_input, variable){
  
  # colm <- data_input[,get(variable)]
  # table1 <- data.frame(table(colm))
  # Statistics <- c("Factor Variable:", "Number of levels", "Mode", as.character(table1$colm))
  # Values <- c(variable, length(unique(colm)), Mode(colm), table1$Freq)
  colm <- data_input[, get(variable)]
  table1 <- data.frame(table(colm))
  Statistics <- c("Factor Variable:", "Number of levels", "Mode")
  Values <- c(variable, length(unique(colm)), Mode(colm))
  df <- data.frame("Statistics" = Statistics, "Values" = Values)
  return(df)
  
}

# 3. Num Var One Stat ----

num_var_only_summary_table <- function(data_input, num_var_1, num_var_2){
  
  if(num_var_1 != not_sel & num_var_2 == not_sel){variable <- num_var_1}
  if(num_var_1 == not_sel & num_var_2 != not_sel){variable <- num_var_2}
  result <- numerical_stat(data_input, variable)
  return(result)
  
}

# 4. Theme

kablanje <- function(data){
  stajl <- kable(data[-1,], align = "lc", row.names = FALSE,
                 caption = paste(data[1,1], data[1,2])) %>% 
    # row_spec(seq(1,nrow(data),2), background='#fcba03') %>% 
    kable_styling(latex_options = 'striped', full_width = FALSE)
  return(stajl)
}  








