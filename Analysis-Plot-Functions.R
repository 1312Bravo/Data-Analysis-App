not_sel <- "Not Selected"

# 1. Num var one plot ----

## 1.1 Histogram ----

draw_num_var_one_hist <- function(data_input, num_var_1, num_var_2){
  
  if(num_var_1 != not_sel & num_var_2 == not_sel){
    p <- ggplot(data = data_input, aes_string(x = num_var_1))
  }
  
  if(num_var_1 == not_sel & num_var_2 != not_sel){
    p <- ggplot(data = data_input, aes_string(x = num_var_2))
  }
  
  p <- p + geom_histogram(col = blackColor, fill = fillColor) + theme1 + labs(y="Count")
  print(p)
}

## 1.2 Density plot ----

draw_num_var_one_density <- function(data_input, num_var_1, num_var_2){
  
  if(num_var_1 != not_sel & num_var_2 == not_sel){
    p <- ggplot(data = data_input, aes_string(x = num_var_1, fill = num_var_1)) 
  }
  
  if(num_var_1 == not_sel & num_var_2 != not_sel){
    p <- ggplot(data = data_input, aes_string(x = num_var_2, fill = num_var_2)) 
  }
  
  p <- p + geom_density(col = blackColor, fill = fillColor, lwd = 1.5) + theme1 + labs(y="Count")
  print(p)
}

## 1.3 Box plot ----

draw_num_var_one_box <- function(data_input, num_var_1, num_var_2){
  
  if(num_var_1 != not_sel & num_var_2 == not_sel){
    p <- ggplot(data = data_input, aes_string(x = num_var_1)) 
  }
  
  if(num_var_1 == not_sel & num_var_2 != not_sel){
    p <- ggplot(data = data_input, aes_string(x = num_var_2)) 
  }
  
  p <- p + geom_boxplot(col=blackColor, fill = fillColor, lwd = 1.5) + theme1
  print(p)
}

# 2. Num var two plot ----

## 2.1 Scatter plot ----

draw_num_var_two_scatter <- function(data_input, num_var_1, num_var_2){
  
  ggplot(data = data_input, aes_string(x = num_var_1, y = num_var_2)) + 
    geom_point(col=blackColor, fill = fillColor, size = 2.5, pch = 16) + theme1 
  
}

## 2.2 Density plot ----

draw_num_var_two_density <- function(data_input, num_var_1, num_var_2){
  
  p <- ggplot(data = data_input, aes_string(x = num_var_1, y = num_var_2)) + 
    geom_point(col=blackColor, fill = fillColor, size = 2.5, pch = 16) + theme1 
  ggMarginal(p, type = "density", lwd= 1.5, fill = fillColor)
  
}

## 2.3 Box plot ----

draw_num_var_two_box <- function(data_input, num_var_1, num_var_2){
  
  p <- ggplot(data = data_input, aes_string(x = num_var_1, y = num_var_2)) + 
    geom_point(col=blackColor, fill = fillColor, size = 2.5, pch = 16) + theme1 
  ggMarginal(p, type = "boxplot", lwd= 1.5, fill = fillColor)
  
}

# 3. Num var & Fact var plot ----

## 3.1 Histogram ----

draw_num_var_fact_var_hist <- function(data_input, num_var_1, num_var_2, fact_var){

  if(num_var_1 != not_sel & num_var_2 == not_sel){
    p <- ggplot(data = data_input, aes_string(x = num_var_1, fill = fact_var)) 
  }
  
  if(num_var_1 == not_sel & num_var_2 != not_sel){
    p <- ggplot(data = data_input, aes_string(x = num_var_2, fill = fact_var)) 
  }
  
  p <- p + geom_histogram(col=blackColor, alpha = 0.7, position = 'identity') + theme1 + labs(y = "Count")
  print(p)
}


## 3.2 Density plot ----

draw_num_var_fact_var_density <- function(data_input, num_var_1, num_var_2, fact_var){
  
  if(num_var_1 != not_sel & num_var_2 == not_sel){
    p <- ggplot(data = data_input, aes_string(x = num_var_1, fill = fact_var)) 
  }
  
  if(num_var_1 == not_sel & num_var_2 != not_sel){
    p <- ggplot(data = data_input, aes_string(x = num_var_2, fill = fact_var)) 
  }
  
  p <- p + geom_density(col=blackColor, alpha = 0.7) + theme1 + labs(y = "Count")
  print(p)
}

## 3.3 Box plot ----

draw_num_var_fact_var_box <- function(data_input, num_var_1, num_var_2, fact_var){
  
  if(num_var_1 != not_sel & num_var_2 == not_sel){
    p <- ggplot(data = data_input, aes_string(x = num_var_1, fill = fact_var)) 
  }
  if(num_var_1 == not_sel & num_var_2 != not_sel){
    p <- ggplot(data = data_input, aes_string(x = num_var_2, fill = fact_var)) 
  }
  
  p <- p + geom_boxplot(col=blackColor) + theme1 
  print(p)
}

# 4. Fact var ----

## 4.1 Barplot ----

draw_only_fact_var_bar <- function(data_input, fact_var){
  
  ggplot(data = data_input, aes_string(x = fact_var, fill = fact_var)) +
    geom_bar(col=blackColor) + theme1 + theme(legend.position = "none")
  
}

## 4.2 Pieplot ----

draw_only_fact_var_pie <- function(data_input, fact_var){
  
  ggplot(data = data_input, aes_string(x = fact_var, fill = fact_var)) +
    geom_bar(col=blackColor) + coord_polar() + 
    theme1 + labs(y = "Count") + theme(legend.position = "none")
  
}

# 5. Two num var & Fact var ----

## 5.1 Scatter plot ----

draw_two_num_var_fact_var_scatter <- function(data_input, num_var_1, num_var_2, fact_var){
  
  ggplot(data = data_input, aes_string(x = num_var_1, y = num_var_2)) +
    geom_point(aes_string(color = fact_var), size = 2.5, pch = 16) +
    theme1
  
}

## 5.2 Density plot ----

draw_two_num_var_fact_var_density <- function(data_input, num_var_1, num_var_2, fact_var){
  
  # p <- ggMarginal(p, type="density", lwd= 1.5, groupFill = TRUE)
  # print(p)
  
  p <- ggplot(data = data_input, aes_string(x = num_var_1, y = num_var_2)) +
    geom_point(aes_string(color = fact_var), size = 2.5, pch = 16) +
    theme1
  
  ggMarginal(p, type = "density", groupFill = TRUE)
  
}


## 5.3 Box plot ----

draw_two_num_var_fact_var_box <- function(data_input, num_var_1, num_var_2, fact_var){
  
  # p <- ggMarginal(p, type="density", lwd= 1.5, groupFill = TRUE)
  # print(p)
  
  p <- ggplot(data = data_input, aes_string(x = num_var_1, y = num_var_2)) +
    geom_point(aes_string(color = fact_var), size = 2.5, pch = 16) +
    theme1
  
  ggMarginal(p, type = "boxplot", groupFill = TRUE)
  
}

# 6. Themes ----

theme1 <- theme(panel.background = element_rect(fill = whiteColor),
        plot.background = element_rect(fill = whiteColor),
        text=element_text(color=blackColor),
        axis.text=element_text(color=blackColor),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size=22),
        axis.line = element_line(colour = "black"),
        panel.grid.minor = element_line(colour = "black"),
        legend.position = "bottom",
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14)) 








