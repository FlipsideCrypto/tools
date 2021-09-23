library(data.table)
library(shiny)
library(ggplot2)
library(scales)
library(shinyWidgets)
library(plotly)
library(bslib)
require(reshape2)
require(dplyr)
require(RPostgreSQL)
library(RJSONIO)
library(stringr)
library(showtext)
font_add_google(name = "Roboto Condensed", family = "roboto-condensed")
font_add_google(name = "Roboto Mono", family = "roboto-mono")
showtext_auto()


swap.sd.constant <- 6

smartRound <- function(x) {
  
  if(x == 0) return(0)
  
  if(x < 5 & x > 0.1) {
    return(round(x, 4))
  } else {
    
    return(round(x, which(x * (10^(-2:20)) > 1)[1] - 2))
  }
  
  
}
round_any <- function(x, accuracy, f=round){f(x/ accuracy) * accuracy}


# liquidity funcitons, translated from from the uniswap nf position manager contract:
getLiquidityForAmount0 <- function(amount0, lower, upper) {
  
  amount0 * (sqrt(upper) * sqrt(lower)) / (sqrt(upper) - sqrt(lower))
  
}


getLiquidityForAmount1 <- function(amount1, lower, upper) {
  
  amount1 / (sqrt(upper) - sqrt(lower))
  
}

getLiquidityForAmounts <- function(amount0, amount1, cprice, lower, upper) {
  
  sqrtRatioX96 <- cprice
  sqrtRatioAX96 <- lower
  sqrtRatioBX96 <- upper
  
  # if the current price is below the lower bound:
  #if (sqrtRatioX96 <= sqrtRatioAX96) {
  if(cprice <= lower) {
    liquidity <- getLiquidityForAmount0(lower, upper, amount0)
    
  } else if (sqrtRatioX96 < sqrtRatioBX96) {
    liquidity0 = getLiquidityForAmount0(amount0, sqrtRatioX96, sqrtRatioBX96)
    liquidity1 = getLiquidityForAmount1(amount1, sqrtRatioAX96, sqrtRatioX96)
    
    liquidity = ifelse(liquidity0 < liquidity1, liquidity0, liquidity1)
  } else {
    liquidity = getLiquidityForAmount1(amount1, sqrtRatioAX96, sqrtRatioBX96);
  }
  
  return(liquidity)
}



#-------------- ggplot THEMEs:
theme_univ3 <- function(base_size = 14,
                        bgcolor.dark = "white",
                        bgcolor.light = "white",
                        text.color = "#637381",
                        title.color = "#212B36",
                        lines.color = "#EDEDF3") {
  half_line <- base_size/2
  theme(text = element_text(family = 'roboto-mono',
                            face = "plain",
                            colour = text.color, size = base_size,
                            lineheight = 0.9,  hjust = 0.5,
                            vjust = 0.5, angle = 0, 
                            margin = margin(), debug = FALSE),
        
        plot.background = element_rect(color = NA, fill = "transparent"), 
        plot.title = element_text(size = rel(1.2),
                                  color = title.color,
                                  margin = margin(b = half_line/2)),
        
        strip.background = element_rect(fill = bgcolor.dark, colour = NA),
        strip.text = element_text(colour = text.color, size = rel(0.8)),
        strip.text.x = element_text(margin = margin(t = half_line/2,
                                                    b = half_line/2)), 
        strip.text.y = element_text(angle = -90, 
                                    margin = margin(l = half_line/2, 
                                                    r = half_line/2)),
        
        axis.line = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(), 
        
        axis.text = element_text(color = text.color, size = base_size - 2),
        
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank(),
        panel.grid.major.x = element_line(colour = 'white', linetype = 1, size = 0.7), 
        panel.grid.major.y = element_line(colour = 'white', linetype = 1, size = 0.7), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        
        legend.background = element_rect(colour = NA, fill = "transparent"), 
        legend.key = element_rect(colour = NA, fill = "transparent"))
}

# same theme but smaller
theme_univ3_small <- function(base_size = 12,
                        bgcolor.dark = "white",
                        bgcolor.light = "white",
                        text.color = "#637381",
                        title.color = "#212B36",
                        lines.color = "#EDEDF3") {
  half_line <- base_size/2
  theme(text = element_text(family = 'roboto-mono',
                            face = "plain",
                            colour = text.color, size = base_size,
                            lineheight = 0.9,  hjust = 0.5,
                            vjust = 0.5, angle = 0, 
                            margin = margin(), debug = FALSE),
        
        plot.background = element_rect(color = NA, fill = "transparent"), 
        plot.title = element_text(size = rel(1.2),
                                  color = title.color,
                                  margin = margin(b = half_line/2)),
        
        strip.background = element_rect(fill = bgcolor.dark, colour = NA),
        strip.text = element_text(colour = text.color, size = rel(0.8)),
        strip.text.x = element_text(margin = margin(t = half_line/2,
                                                    b = half_line/2)), 
        strip.text.y = element_text(angle = -90, 
                                    margin = margin(l = half_line/2, 
                                                    r = half_line/2)),
        
        axis.line = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(), 
        
        axis.text = element_text(color = text.color, size = base_size - 2),
        
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank(),
        panel.grid.major.x = element_line(colour = 'white', linetype = 1, size = 0.7), 
        panel.grid.major.y = element_line(colour = 'white', linetype = 1, size = 0.7), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        
        legend.background = element_rect(colour = NA, fill = "transparent"), 
        legend.key = element_rect(colour = NA, fill = "transparent"),
        
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank())
}
