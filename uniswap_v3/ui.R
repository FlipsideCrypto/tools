
fluidPage(
  title = "Flipside: Uniswap V3 Fee Calculator",
  theme = bs_theme(
    version = version_default(),
    bootswatch = NULL,
    bg = "#f2defe", fg = "#110403", primary = "#01c3b3", secondary = "#f2defe",
    base_font = font_google("Roboto Mono"),
    code_font = font_google("Roboto Mono"),
    heading_font = font_google("Roboto Mono")
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "shinycss.css"),
    tags$link(rel = "icon", href = "fliptrans.png")
  ),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  fluidRow(
    column(6,
           div(h2(a(href = "http://www.flipsidecrypto.com", target = "_blank", img(src = 'fliptrans.png', width = '50px')), "Uniswap V3 Fee Calculator"))),
    column(
      6,
      div(h5(
        a(href = "http://app.flipsidecrypto.com/", 
          target = "_blank", "Data from Velocity by Flipside Crypto"), 
        a(href = "https://twitter.com/flipsidecrypto", target = "_blank",
          img(src = 'twitter.png', width = '30px')),
        a(href = "https://flipsidecrypto.com/discord", target = "_blank",
          img(src = 'discord.png', width = '35px')),
      style = "font-style: italic; font-weight: 150; text-align: right;"),
      style = "padding-top: 21px;")
    ),
    column(12,div("change the inputs on the left, see current swaps & liqudity + estimate fee income on the right, read disclaimer at the bottom")
    ) #close column
  ), # close intro row
  hr(),
  fluidRow(
    column(4, class = 'leftinputs',
           a(target = "_blank", uiOutput("poolselect")),
           div(id = 'lildate',
               p("To search, select the dropdown, hit backspace/delete and type"),
               textOutput('last_update')), 
           div(class = 'smallstats', textOutput('n_open_positions')), 
           div(class = 'smallstats', textOutput('swapvol24h')), 
           div(class = 'smallstats', textOutput('current_price')), 
           hr(),
           a(target = "_blank", uiOutput("moveprice")),
           hr(),
           a(target = "_blank", uiOutput("lcurveselect")),
           hr(),
           uiOutput("dates")
           
    ), # close inputs column
    
    # this is the results / graphs column
    column(8,
           fluidRow(h4("Simple Daily Fee Estimate:")),
           fluidRow(class = 'feedesc', 
                    column(4,class = 'feedesc', "Investment $ Here"),
                    column(4,class = 'feedesc', "24h Fee $ (annual %)"),
                    column(2, "GO ↯")
           ),
           fluidRow(class = 'feedesc', 
                    column(4, class = 'centerit', 
                           a(target = "_blank",
                             uiOutput('investamt'),
                    )), 
                    column(4, div(id = "fee", textOutput('simpledailyfee'))),
                    column(2, div(id = 'uni', uiOutput("link2uniswap")))
           ),
           hr(),
           fluidRow(
             column(6,h4("Swap Curve"),
                    textOutput("lcurve_prop_covered")),
             column(6,
                    h4("Liquidity Positions"),
                    textOutput("posVLProp"))
           ),
           fluidRow(
             column(6,plotOutput("lcurve_plot", height = '350px')),
             column(6,plotOutput("concentration_view_plot",height = '350px'))
           ),
           fluidRow(
             column(6,
                    h5("Prices Last 5 Days"),
                    plotOutput("timepriceplot", height = "130px")),
             column(6,
                    h5("5 Day Swap Volume ($MM)"),
                    plotOutput("swaps5days", height = "130px"))
           ) 
           
    ) # results columns
    
    
  ), # close inputs/results row
  fluidRow(
    column(12,
           hr(),
           div(
             id = 'disclaimer',
             "This is not investment advice, use it at your own risk. ", 
             a(href = "http://velocity-app.flipsidecrypto.com", 
               target = "_blank",
               "DYOR Here with Velocity"), 
             br(),
             "Questions? Join our ", 
             a(href = "https://flipsidecrypto.com/discord", "discord", target = "_blank"), " and let's chat.",
             hr(),
             "This tool presents a simple point-in-time estimate of how much you could 
             potentially earn in fees for providing liquidity in Uniswap V3. 
             It assumes no changes to swap price, swap volumes or liquidity positions for 24 hours,
             which is not realistic. It does not account for Impermanent Loss.",
             br(),
             "Use this tool to get an idea of where you want to invest, 
             and directionally what your fee income could be. We strive to provide accurate information about the present and the past but make no guarantees about the future."
           ),
           hr()
    )
  ),
  fluidRow(
    column(
      6,
      div(h5("How Fee Income is Estimated:")),
      div("Basic Formula (L = liquidity): (L_you / L_others) * (24h_swap_volume * pool_fee_rate)"),
      div("Calculation Details for This Position"),
      div("Liquidity for This Position: ", textOutput("yourVL")),
      div("All Other Existing Liquidity (liquidity for all other positions crossing the current price tick): ", textOutput("alltickVL")),
      div("24 Hour Swap Volume (across all ticks):", textOutput("swapvol24h2")),
      div("Pool Fee: ", textOutput("poolfee")),
      div("Fee Income: ", textOutput('simpledailyfee2')),
      div("Token Amounts Needed:"),
      div(textOutput("token0amt")),
      div(textOutput("token1amt"))
    ),
    column(
      6,
      div(h5("More on Liquidity:")),
      div("The liquidity amount is calculated from the following numbers that describe a position: amount of token 0 (amt0), amount of token 1 (amt1), price (as x token 1's per token 0) at the upper limit of the position (upper), price at the lower limit of the position (lower) and the current swap price (cprice). Then liquidity for a position is calculated as follows:"),
      div("Case 1: cprice <= lower"),
      div("liquidity = amt0 * (sqrt(upper) * sqrt(lower)) / (sqrt(upper) - sqrt(lower))"),
      div("Case 2: lower < cprice <= upper"),
      div("liquidity is the min of the following two calculations:"),
      div("amt0 * (sqrt(upper) * sqrt(cprice)) / (sqrt(upper) - sqrt(cprice))"),
      div("amt1 / (sqrt(cprice) - sqrt(lower))"),
      div("Case 3: upper < cprice"),
      div("liquidity = amt1 / (sqrt(upper) - sqrt(lower))")
    ),
    br()
  )
) # close fluid page







