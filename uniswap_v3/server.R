server <- function(input, output, session) {
  load("data.RData")
  
  output$poolselect <- renderUI({
    selectInput(inputId = 'poolname', label = "Select Pool",
                selected = "USDC-USDT 3000 60",
                choices = pool.choices)
  })
  
  # reverse token prices depending on which token price is larger
  revTokes <- reactive({
    ifelse(pool.stats[pool_name == input$poolname][1]$price_0_1 <
             pool.stats[pool_name == input$poolname][1]$price_1_0,
           TRUE, FALSE)
  })
  
  poolToken1 <- reactive({
    ifelse(revTokes(),
           strsplit(strsplit(input$poolname, " ", fixed = TRUE)[[1]][1], "-", fixed = TRUE)[[1]][1],
           strsplit(strsplit(input$poolname, " ", fixed = TRUE)[[1]][1], "-", fixed = TRUE)[[1]][2])
  })
  
  poolToken2 <- reactive({
    ifelse(revTokes(),
           strsplit(strsplit(input$poolname, " ", fixed = TRUE)[[1]][1], "-", fixed = TRUE)[[1]][2],
           strsplit(strsplit(input$poolname, " ", fixed = TRUE)[[1]][1], "-", fixed = TRUE)[[1]][1])
    
  })
  
  output$n_open_positions <- renderText({
    paste0(nrow(current.positions[pool_name == input$poolname]), " Open Positions")
  })
  
  output$poolfee <- renderText({
    paste0(current.positions[pool_name == input$poolname]$fee[1] / 10^6 * 100, "%")
  })
  
  output$moveprice <- renderUI({
    
    if(revTokes()) {
      tmp.min <- swapSDs()$lower_bound_10
      tmp.max <- swapSDs()$upper_bound_10  
    } else {
      tmp.min <- swapSDs()$lower_bound_01
      tmp.max <- swapSDs()$upper_bound_01
    }
    sliderInput(
      inputId = "moveprice", "Move Active Price Assumption",
      min   = ifelse(tmp.min > 10, floor(tmp.min), smartRound(tmp.min)),
      max   = ifelse(tmp.max > 10, ceiling(tmp.max), smartRound(tmp.max)),
      value = getCurrentPrice(),
      step  = smartRound(abs(tmp.max - tmp.min) / 1000)
    )
  })
  
  output$lcurveselect <- renderUI({
    
    if(revTokes()) {
      tmp.min2 <- swapSDs()$lower_2sd_10
      tmp.max2 <- swapSDs()$upper_2sd_10
      
      tmp.min <- swapSDs()$lower_bound_10
      tmp.max <- swapSDs()$upper_bound_10
      
    } else {
      tmp.min2 <- swapSDs()$lower_2sd_01
      tmp.max2 <- swapSDs()$upper_2sd_01
      
      tmp.min <- swapSDs()$lower_bound_01
      tmp.max <- swapSDs()$upper_bound_01
    }
    
    sliderInput(inputId = "lcurveslider", "Select Liquidity Bounds",
                min = ifelse(tmp.min > 10, floor(tmp.min), smartRound(tmp.min)),
                max = ifelse(tmp.max > 10, ceiling(tmp.max), smartRound(tmp.max)),
                value = c(ifelse(tmp.min > 10, floor(tmp.min2), smartRound(tmp.min)), 
                          ifelse(tmp.max > 10, ceiling(tmp.max2), smartRound(tmp.max))),
                step = smartRound(abs(tmp.max - tmp.min) / 1000))
    
  })
  
  output$dates <- renderUI({
    sliderInput("daterange",
                "Select Swaps Date Range",
                min = as.Date(min(swaps$block_timestamp),"%Y-%m-%d"),
                max = as.Date(max(swaps$block_timestamp),"%Y-%m-%d"),
                value = c(as.Date(min(swaps$block_timestamp),"%Y-%m-%d"),
                          max = as.Date(max(swaps$block_timestamp),"%Y-%m-%d")),
                timeFormat="%Y-%m-%d",
                dragRange = TRUE)
  })
  
  
  getCurrentPrice <- reactive({
    max.swap <- max(swaps[pool_name == input$poolname]$block_id)
    c.price <- ifelse(revTokes(),
                      swaps[pool_name == input$poolname & block_id == max.swap]$price_1_0[1],
                      swaps[pool_name == input$poolname & block_id == max.swap]$price_0_1[1])
    ifelse(c.price > 1, round(c.price, 2), smartRound(c.price))
    
    return(c.price)
  })
  
  output$current_price <- renderText({
    paste0("Current Price: ", getCurrentPrice(), " ", poolToken2(), " per ", poolToken1())
  })
  
  output$last_update <- renderText({
    paste0("Last Pool Update: ", gsub(x = gsub(x = as.character(max(pool.stats[pool_name == input$poolname]$block_timestamp)), "T", " ", fixed = T), "Z", "", fixed = TRUE), " UTC")
  })
  
  output$swapvol24h <- renderText({
    usd.last.day <- swaps[block_timestamp > max(formattedSwapData()$block_timestamp) - (60*60*24) & pool_name == input$poolname,
                          ifelse(amount0_usd < 0, abs(amount0_usd), abs(amount1_usd))]
    paste0("24 Hour Swap Volume: $", formatC(round(sum(abs(usd.last.day))), format="f", digits=0, big.mark=","))
    
  })
  
  output$swapvol24h2 <- renderText({
    usd.last.day <- formattedSwapData()[block_timestamp > max(formattedSwapData()$block_timestamp) - (60*60*24)]$amount0_usd
    formatC(round(sum(abs(usd.last.day))), format="f", digits=0, big.mark=",")
    
  })
  
  
  output$pool_token1 <- renderText({
    paste0("Token 1: ", poolToken1())
  })
  
  output$pool_token2 <- renderText({
    paste0("Token 2: ", poolToken2())
  })
  
  poolAddy <- reactive({
    current.positions[pool_name == input$poolname]$pool_address[1]
  })
  
  poolStats <- reactive({
    pool.stats[pool_name == input$poolname]
  })
  
  formattedSwapData <- reactive({
    
    swaps[pool_name == input$poolname & 
            block_timestamp >= as.POSIXct(input$daterange[1]) & 
            block_timestamp <= as.POSIXct(input$daterange[2] + 1)]
    
  })
  
  # get the boundaries for whatever sd's away from mean swaps:
  swapSDs <- reactive({
    data.table(lower_bound_01 = max(0.0000001, mean(swaps[pool_name == input$poolname]$price_0_1) + (-swap.sd.constant)*sd(swaps[pool_name == input$poolname]$price_0_1)),
               lower_2sd_01   = max(0.0000001, mean(swaps[pool_name == input$poolname]$price_0_1) + (-swap.sd.constant/2)*sd(swaps[pool_name == input$poolname]$price_0_1)),
               upper_bound_01 = mean(swaps[pool_name == input$poolname]$price_0_1) + (swap.sd.constant)*sd(swaps[pool_name == input$poolname]$price_0_1),
               upper_2sd_01 = mean(swaps[pool_name == input$poolname]$price_0_1) + (swap.sd.constant/2)*sd(swaps[pool_name == input$poolname]$price_0_1),
               
               lower_bound_10 = max(0.0000001, mean(swaps[pool_name == input$poolname]$price_1_0) + (-swap.sd.constant)*sd(swaps[pool_name == input$poolname]$price_1_0)),
               lower_2sd_10 = max(0.0000001, mean(swaps[pool_name == input$poolname]$price_1_0) + (-swap.sd.constant/2)*sd(swaps[pool_name == input$poolname]$price_1_0)),
               upper_bound_10 = mean(swaps[pool_name == input$poolname]$price_1_0) + (swap.sd.constant)*sd(swaps[pool_name == input$poolname]$price_1_0),
               upper_2sd_10 = mean(swaps[pool_name == input$poolname]$price_1_0) + (swap.sd.constant/2)*sd(swaps[pool_name == input$poolname]$price_1_0)
    )
  })
  
  # only keep positions that overlap with the avg last 3 days of trading +/- 5sd
  goodPoolPos <- reactive({
    
    if(revTokes()) {
      this.pool.good.pos <- current.positions[pool_name == input$poolname & liquidity_adjusted > 0 &
                                                (price_upper_1_0 >= swapSDs()$lower_bound_10 &
                                                   price_lower_1_0 <= swapSDs()$upper_bound_10)]  
    } else {
      this.pool.good.pos <- current.positions[pool_name == input$poolname & liquidity_adjusted > 0 &
                                                (price_upper_0_1 >= swapSDs()$lower_bound_01 &
                                                   price_lower_0_1 <= swapSDs()$upper_bound_01)]
    }
    return(this.pool.good.pos)
    
  })
  
  # make data to do the existing liquidity plot
  # we'll "spread out" each position over its ticks
  summarizeLiquidity <- reactive({
    
    n.price.bins <- swap.sd.constant * 20
    tick.dec.adj <- poolStats()[1]$pool_dec_adj
    
    if(revTokes()) {
      closest.lower.tick <- floor( log( swapSDs()$lower_bound_10 * (10^tick.dec.adj), 1.0001))
      closest.upper.tick <- ceiling( log( swapSDs()$upper_bound_10 * (10^tick.dec.adj), 1.0001) )
    } else {
      closest.lower.tick <- floor( log( 1 / swapSDs()$upper_bound_01 * (10^tick.dec.adj), 1.0001) )
      closest.upper.tick <- ceiling( log( 1 / swapSDs()$lower_bound_01 * (10^tick.dec.adj), 1.0001) )
    }
    
    
    unique.positions <- goodPoolPos()
    
    bin.size <- ifelse(revTokes(),
                       (swapSDs()$upper_bound_10 - swapSDs()$lower_bound_10) / (swap.sd.constant * 20),
                       (swapSDs()$upper_bound_01 - swapSDs()$lower_bound_01) / (swap.sd.constant * 20))
    
    unique.positions[, bin_size := bin.size]
    
    if(revTokes()) {
      unique.positions[, avg_chunk_liq := liquidity_adjusted / ((price_upper_1_0 - price_lower_1_0) / bin_size)]
    } else {
      unique.positions[, avg_chunk_liq := liquidity_adjusted / ((price_upper_0_1 - price_lower_0_1) / bin_size)]
    }
    
    unique.positions <- unique.positions[, list(liquidity_by_pos = sum(avg_chunk_liq),
                                                liquidity_max = sum(liquidity_adjusted)),
                                         by = "tick_lower,tick_upper,price_lower_0_1,price_upper_0_1"]
    
    # the tick to price, undo it and get the right dec adjustment:
    tick.spacing <- ceiling(abs((closest.upper.tick - closest.lower.tick)/n.price.bins))
    by.tick <- data.table(tick = seq(closest.lower.tick, closest.upper.tick, 
                                     by = tick.spacing))
    
    if(revTokes()) {
      extra.tick.price <- (((1.0001^(closest.upper.tick + tick.spacing)) / (10^tick.dec.adj)))
    } else {
      extra.tick.price <- 1 / (((1.0001^(ifelse(closest.lower.tick < 0, closest.upper.tick, closest.lower.tick) + tick.spacing)) / (10^tick.dec.adj)))
    }
    
    # merge trick:
    unique.positions[, m := 1]
    by.tick[, m := 1]
    
    by.tick <- merge(by.tick, unique.positions, by = "m", all = TRUE, allow.cartesian = TRUE)
    
    if(revTokes()) {
      by.tick[, tick_price := (((1.0001^tick) / (10^tick.dec.adj)))]  
    } else {
      by.tick[, tick_price := 1 / (((1.0001^tick) / (10^tick.dec.adj)))]
    }
    
    by.tick <- by.tick[ (tick >= tick_lower) & (tick <= tick_upper)]
    
    
    if(revTokes()) {
      liquidity.by.tick <- by.tick[, list(max_liquidity = sum(liquidity_max),
                                          avg_liquidity = sum(liquidity_by_pos, na.rm = TRUE)), 
                                   by = "tick,tick_price"] %>% 
        .[tick_price > swapSDs()$lower_bound_10 & tick_price < swapSDs()$upper_bound_10]
      
      liquidity.by.tick[, tick_end := c(liquidity.by.tick$tick_price[2:length(liquidity.by.tick$tick_price)], extra.tick.price)]
    } else {
      liquidity.by.tick <- by.tick[, list(max_liquidity = sum(liquidity_max),
                                          avg_liquidity = sum(liquidity_by_pos, na.rm = TRUE)), 
                                   by = "tick,tick_price"] %>% 
        .[tick_price > swapSDs()$lower_bound_01 & tick_price < swapSDs()$upper_bound_01]
      
      liquidity.by.tick[, tick_end := c(liquidity.by.tick$tick_price[2:length(liquidity.by.tick$tick_price)], extra.tick.price)]
    }
    
    liquidity.by.tick
    
  })
  
  
  output$lcurve_plot <- renderPlot({
    
    tmp.data <- formattedSwapData()
    
    if(revTokes()) {
      tmp.data[, covered := ifelse(price_1_0 >= input$lcurveslider[1] & price_1_0 <= input$lcurveslider[2],
                                   "covered", "out")]
      
      lcurve.plot <- ggplot() +
        theme_univ3() +
        geom_line(data = tmp.data,
                  aes(x = price_1_0, y = price_0_1, alpha = covered), size = 6, color = "#01c3b3") +
        geom_point(data = tmp.data,
                   aes(x = price_1_0, y = price_0_1)) +
        geom_point(data = data.table(price_1_0 = input$moveprice, price_0_1 = (1/input$moveprice), covered = 'covered'),
                   aes(x = price_1_0, y = price_0_1),
                   size = 5, color = 'white') +
        labs(y = paste0("\n", poolToken1(), " per ", poolToken2()),
             x = paste0(poolToken2(), " per ", poolToken1(), "\n")) +
        theme(legend.position = 'none') +
        scale_alpha_manual(values = c(1, 0))
      
    } else {
      tmp.data[, covered := ifelse(price_0_1 >= input$lcurveslider[1] & price_0_1 <= input$lcurveslider[2],
                                   "covered", "out")]
      
      lcurve.plot <- ggplot() +
        theme_univ3() +
        geom_line(data = tmp.data,
                  aes(x = price_0_1, y = price_1_0, alpha = covered), size = 6, color = "#01c3b3") +
        geom_point(data = tmp.data,
                   aes(x = price_0_1, y = price_1_0)) +
        geom_point(data = data.table(price_0_1 = input$moveprice, price_1_0 = (1/input$moveprice), covered = 'covered'),
                   aes(x = price_0_1, y = price_1_0),
                   size = 5, color = 'white') +
        labs(y = paste0("\n", poolToken1(), " per ", poolToken2()),
             x = paste0(poolToken2(), " per ", poolToken1(), "\n")) +
        theme(legend.position = 'none') +
        scale_alpha_manual(values = c(1, 0))
      
    }
    
    
    
    return(lcurve.plot)
    
  }, bg = "transparent")
  
  output$concentration_view_plot <- renderPlot({
    
    ggplot() +
      geom_rect(data = summarizeLiquidity(),
                aes(xmin = tick_price, xmax = tick_end, ymin = 0, ymax = avg_liquidity),
                fill = "grey80") +
      geom_rect(data = summarizeLiquidity()[tick_price >= input$lcurveslider[1] &
                                              tick_end <= input$lcurveslider[2]],
                aes(xmin = tick_price, xmax = tick_end, ymin = 0, ymax = avg_liquidity),
                fill = "#01c3b3") +
      geom_vline(xintercept = input$moveprice, color = "black", linetype = 2) +
      # geom_vline(xintercept = input$lcurveslider[1], color = "#F8ABA7") +
      # geom_vline(xintercept = input$lcurveslider[2], color = "#F8ABA7") +
      theme_univ3() +
      geom_text(data = data.table(x = input$moveprice, y = 0, 
                                  label = " active swap price", xmin = 1, xmax = 1, ymin = 1, ymax = 1),
                aes(x = x, y = y, label = label),
                hjust = 0, vjust = -1, family = "roboto-mono") +
      labs(x = paste0(poolToken2(), " per ", poolToken1(), "\n"), y = "Existing Liquidity")
    
    
    
    
  }, bg = "transparent")
  
  output$timepriceplot <- renderPlot({
    
    plot.data <- swaps[pool_name == input$poolname &
                         as.Date(block_timestamp) >= min(input$daterange) &
                         as.Date(block_timestamp) <= max(input$daterange) ]
    
    if(revTokes()) {
      
      min.max.data <- rbind(plot.data[which.min(plot.data$price_1_0), list(block_timestamp, price = smartRound(price_1_0))],
                            plot.data[which.max(plot.data$price_1_0), list(block_timestamp, price = smartRound(price_1_0))])
      
      plot.data$price_0_1 <- plot.data$price_1_0  
      
      .limits <- c(swapSDs()$lower_bound_10*.99, swapSDs()$upper_bound_10*1.01)
      
    } else {
      
      min.max.data <- rbind(plot.data[which.min(plot.data$price_0_1), list(block_timestamp, price = smartRound(price_0_1))],
                            plot.data[which.max(plot.data$price_0_1), list(block_timestamp, price = smartRound(price_0_1))])
      
      .limits <- c(swapSDs()$lower_bound_01*.99, swapSDs()$upper_bound_01*1.01)
    }
    
    ggplot() +
      geom_rect(data = data.table(xmin = min(plot.data$block_timestamp),
                                  xmax = max(plot.data$block_timestamp),
                                  ymin = input$lcurveslider[1],
                                  ymax = input$lcurveslider[2]),
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
                fill  = "#01c3b3", alpha = 0.5) +
      geom_point(data = plot.data,
                 aes(x = block_timestamp, y = price_0_1), size = 0.6) +
      geom_label(data = min.max.data,
                 aes(x = block_timestamp, y = price, label = price),
                 color = "black", fill = "#e7d3f2", family = "roboto-mono", hjust = 0, vjust = 1, size = 6) +
      theme_univ3_small() +
      scale_y_continuous(limits = .limits) +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank()) +
      scale_x_datetime(expand = c(0, 0))
    
  },
  height = 130, 
  bg = "transparent")
  
  
  output$swaps5days <- renderPlot({
    
    plot.data <- swaps[pool_name == input$poolname &
                         swap_date >= Sys.Date() - 4 &
                         swap_date <= Sys.Date() &
                         !is.na(amount0_usd) &
                         !is.na(amount1_usd),
                       sum(ifelse(amount0_usd < 0, abs(amount0_usd), abs(amount1_usd))), 
                       by = swap_date]
    plot.data[, label := round(V1/1000000, 2)]
    
    tmp.plot <- ggplot(plot.data,
                       aes(x = swap_date, y = V1)) +
      geom_bar(stat = "identity", fill  = "#01c3b3") +
      geom_text(aes(x = swap_date, y = V1, label = label), 
                color = "black", family = "roboto-mono", size = 7, vjust = 1.25, color = "white") +
      labs(y = NULL, x = NULL) +
      theme_univ3_small()
    
    return(tmp.plot)
    
  },
  height = 130, 
  bg = "transparent")
  
  output$lcurve_prop_covered <- renderText({
    
    if(revTokes()) {
      prop <- round( (sum(abs(formattedSwapData()[price_1_0 >= input$lcurveslider[1] & price_1_0 <= input$lcurveslider[2]]$amount1_adjusted)) /
                        sum(abs(formattedSwapData()$amount1_adjusted))) *100)  
    } else {
      prop <- round( (sum(abs(formattedSwapData()[price_0_1 >= input$lcurveslider[1] & price_0_1 <= input$lcurveslider[2]]$amount0_adjusted)) /
                        sum(abs(formattedSwapData()$amount0_adjusted))) *100)
    }
    paste(prop, "% Swap Volume Covered")
    
  })
  
  # figure out the token amounts
  # this is also taken from the position manager contract
  tokenAmounts <- reactive({
    
    token0.price <- poolStats()$token0_balance_usd / poolStats()$token0_balance_adjusted
    token1.price <- poolStats()$token1_balance_usd / poolStats()$token1_balance_adjusted
    
    if(!revTokes()) {
      price.sqrt <- sqrt(1 / input$moveprice)
      price.upper.sqrt <- sqrt(1 / input$lcurveslider[1])
      price.lower.sqrt <- sqrt(1 / input$lcurveslider[2])
    } else {
      price.sqrt <- sqrt(input$moveprice)
      price.lower.sqrt <- sqrt(input$lcurveslider[1])
      price.upper.sqrt <- sqrt(input$lcurveslider[2])
    }
    
    relation <- (price.sqrt - price.lower.sqrt) / ( (1 / price.sqrt) - (1 / price.upper.sqrt) )     
    
    token0_amt <- input$lpamt / (token0.price + relation * token1.price)
    token1_amt <- token0_amt * relation
    
    tmp <- data.table(token0.price = token0.price,
                      token1.price = token1.price,
                      token0_amt = token0_amt, 
                      token1_amt = token1_amt)
    
    print(tmp)
    print(token0_amt * token0.price + token1_amt * token1.price)
    
    return(tmp)
  })
  
  # total liquidity for the selected position
  posTotalVL <- reactive({
    
    if(!revTokes()) {
      pos.total.vl <- getLiquidityForAmounts(tokenAmounts()$token1_amt, tokenAmounts()$token0_amt, input$moveprice, input$lcurveslider[1], input$lcurveslider[2])
    } else {
      pos.total.vl <- getLiquidityForAmounts(tokenAmounts()$token0_amt, tokenAmounts()$token1_amt, input$moveprice, input$lcurveslider[1], input$lcurveslider[2])
    }
    
    return(pos.total.vl)
    
  })
  
  findLPexisting <- reactive({
    
    if(revTokes()) {
      in.positions <- current.positions[pool_name == input$poolname & price_lower_1_0 <= input$moveprice & price_upper_1_0 >= input$moveprice]
    } else {
      in.positions <- current.positions[pool_name == input$poolname & price_lower_0_1 <= input$moveprice & price_upper_0_1 >= input$moveprice]
    }
    #in.positions <- current.positions[pool_name == input$poolname]
    
    return(sum(in.positions$liquidity_adjusted))
    
  })
  
  output$investamt <- renderUI({
    shinyWidgets::numericInputIcon("lpamt", 
                                   NULL, 
                                   value = 777,
                                   step= 0.01)
  })
  
  output$simpledailyfee <- renderText({
    
    if(input$lcurveslider[1] > input$moveprice | 
       input$lcurveslider[2] < input$moveprice) {
      tmp <- "$0 (0%)"
      
    } else {
      
      tmp.vol.usd <- swaps[pool_name == input$poolname & block_timestamp > max(swaps[pool_name == input$poolname]$block_timestamp) - (60*60*24)] %>%
        .[, ifelse(amount0_usd > 0, abs(amount1_usd), abs(amount0_usd))] %>%
        sum()
      pool.fee <- poolStats()$fee[1] / 10^6
      est.fee <- round(( posTotalVL() / (posTotalVL() + findLPexisting()) ) * tmp.vol.usd * pool.fee, 2)
      annual.prop <- round((est.fee*365 / input$lpamt) * 100, 2)
      
      tmp <- paste0("$", est.fee, " (", annual.prop, "%)")
    }
    
    
    
    tmp
  })
  
  output$simpledailyfee2 <- renderText({
    
    if(input$lcurveslider[1] > input$moveprice | 
       input$lcurveslider[2] < input$moveprice) {
      tmp <- "$0 (0%)"
      
    } else {
      
      tmp.vol.usd <- formattedSwapData()[block_timestamp > max(formattedSwapData()$block_timestamp) - (60*60*24)] %>%
        .[, ifelse(amount0_usd > 0, abs(amount1_usd), abs(amount0_usd))] %>%
        sum()
      pool.fee <- poolStats()$fee[1] / 10^6
      est.fee <- round(( posTotalVL() / (posTotalVL() + findLPexisting()) ) * tmp.vol.usd * pool.fee, 2)
      annual.prop <- round((est.fee*365 / input$lpamt) * 100, 2)
      
      tmp <- paste0("$", est.fee, " (", annual.prop, "%)")
    }
    
    
    
    tmp
  })
  
  output$posVLProp <- renderText({
    paste0(round(posTotalVL() / (posTotalVL() + findLPexisting())*100, 5), "% of Total Virtual Liquidity")
  })
  
  output$yourVL <- renderText({
    posTotalVL()
  })
  
  output$alltickVL <- renderText({
    findLPexisting()
  })
  
  output$token0amt <- renderText({
    paste0(round(tokenAmounts()$token0_amt, 2), " ", ifelse(!revTokes(), poolToken2(), poolToken1()), 
           " ($", round(tokenAmounts()$token0_amt * tokenAmounts()$token0.price, 2), ")")
  })
  
  output$token1amt <- renderText({
    paste0(round(tokenAmounts()$token1_amt, 2), " ", ifelse(!revTokes(), poolToken1(), poolToken2()), 
           " ($", round(tokenAmounts()$token1_amt * tokenAmounts()$token1.price, 2), ")")
  })
  
  output$link2uniswap <- renderUI({ 
    # copy this link:
    #https://app.uniswap.org/#/add/0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48/0xc02aaa39b223fe8d0a0e5c4f27ead9083c756cc2/3000
    HTML(paste0('<a href = "', 
                paste0("https://info.uniswap.org/#/pools/", poolAddy()), 
                '" target = "_blank"> 🦄 </a>'))
    
  })
}




