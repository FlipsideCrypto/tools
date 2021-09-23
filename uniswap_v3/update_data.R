
max.blocks <- c(QuerySnowflake("SELECT max(block_id) FROM uniswapv3.swaps"),
                QuerySnowflake("SELECT max(block_id) FROM uniswapv3.pool_stats"),
                QuerySnowflake("SELECT max(block_id) FROM uniswapv3.positions"))
max.pull.block <- min(unlist(max.blocks))


current.positions <- QuerySnowflake(paste0("
WITH positions AS (
  SELECT * FROM uniswapv3.positions WHERE block_id <= ", max.pull.block, "
),
                            
max_blocks AS (
  SELECT
  max(block_id) AS block_id,
  pool_address,
  liquidity_provider,
  nf_token_id

  FROM positions
  
  GROUP BY
  pool_address,
  liquidity_provider,
  nf_token_id
)


  SELECT
  blockchain,
  p.block_id,
  block_timestamp,
  tx_id,
  fee_percent,
  liquidity_adjusted,
  p.liquidity_provider,
  nf_position_manager_address,
  p.nf_token_id,
  p.pool_address,
  p.pool_name,
  p.tick_lower,
  p.tick_upper,
  p.price_lower_0_1,
  p.price_lower_0_1_usd,
  p.price_upper_0_1,
  p.price_upper_0_1_usd,
  p.price_lower_1_0,
  p.price_lower_1_0_usd,
  p.price_upper_1_0,
  p.price_upper_1_0_usd
                            
  FROM positions p
                            
  INNER JOIN max_blocks mb ON 
  mb.block_id = p.block_id AND
  mb.pool_address = p.pool_address AND
  mb.liquidity_provider = p.liquidity_provider AND
  mb.nf_token_id = p.nf_token_id

  WHERE liquidity_adjusted > 0
  
"))
current.positions[, fee := as.numeric(strsplit(pool_name, " ", fixed = TRUE)[[1]][2]), by = pool_name ]
current.positions[, tick_spacing := as.numeric(strsplit(pool_name, " ", fixed = TRUE)[[1]][3]), by = pool_name ]


pool.stats <- QuerySnowflake(paste0("
WITH max_blocks AS (
SELECT
max(block_id) AS block_id,
pool_address
FROM
uniswapv3.pool_stats
WHERE block_id <= ", max.pull.block, "
GROUP BY
pool_address)

SELECT
ps.block_id,
block_timestamp,
ps.pool_address,
pool_name,
tick,
price_0_1,
price_1_0,
virtual_liquidity_adjusted,
virtual_reserves_token0_adjusted,
virtual_reserves_token1_adjusted,
token0_balance_adjusted,
token1_balance_adjusted,
token0_balance_usd,
token1_balance_usd,
token0_balance,
token1_balance

FROM
uniswapv3.pool_stats ps

JOIN 
max_blocks mb 
ON 
mb.block_id = ps.block_id 
AND 
mb.pool_address = ps.pool_address
"))


#tx_id, block_timestamp, pool_address, pool_name, price_1_0, price_0_1, tick, amount0_adjusted, amount1_adjusted, amount0_usd, amount1_usd
swaps <- QuerySnowflake(paste0("SELECT 
                    *
                    FROM 
                    uniswapv3.swaps sw
                    WHERE 
                    block_timestamp > getdate() - interval '5 days' AND block_id <= ", max.pull.block))
swaps[, swap_date := as.Date(substr(block_timestamp, 1, 10))]

# what is the price
pool.stats[, token0_dec := log(token0_balance / token0_balance_adjusted, 10)]
pool.stats[, token1_dec := log(token1_balance / token1_balance_adjusted, 10)]

pool.stats[, fee := as.numeric(strsplit(pool_name, " ", fixed = TRUE)[[1]][2]), by = pool_name ]
pool.stats[, tick_spacing := as.numeric(strsplit(pool_name, " ", fixed = TRUE)[[1]][3])]


pool.stats[, pool_dec_adj := token1_dec - token0_dec]

pool.stats <- pool.stats[!is.na(pool_dec_adj)]

swaps <- merge(swaps,
               pool.stats[, list(pool_address, pool_dec_adj)],
               by = "pool_address", 
               all = FALSE)
               
#swaps[, price_0_1 := ifelse(tick >= 0, ((1.0001^tick) / (10^pool_dec_adj)), 1/((1.0001^tick) / (10^pool_dec_adj)))]
#swaps[, price_1_0 := ifelse(tick >= 0, 1 / ((1.0001^tick) / (10^pool_dec_adj)), ((1.0001^tick) / (10^pool_dec_adj)) )]

swaps[, price_0_1 := 1 / ((1.0001^tick) / (10^pool_dec_adj))]
swaps[, price_1_0 := ((1.0001^tick) / (10^pool_dec_adj))]

keep.pools <- swaps[, list(n_swaps = .N), by = pool_address][n_swaps > 50]$pool_address


current.positions <- current.positions[pool_address %in% keep.pools]
swaps <- swaps[pool_address %in% keep.pools]
pool.stats <- pool.stats[pool_address %in% keep.pools]

pool.choices <- swaps[, .N, by = pool_name][order(-N)]
pool.choices[,fancy_name := paste0(
  strsplit(pool_name, " ", fixed = TRUE)[[1]][1]," ",
  as.numeric(strsplit(pool_name, " ", fixed = TRUE)[[1]][2]) / 10000,
  "% fee",collapse = ""), by = pool_name ]
pool.choices <- {
  .vec <- unique(pool.choices$pool_name)
  names(.vec) <- unique(pool.choices$fancy_name)
  .vec
}

special.addresses <- QuerySnowflake("select address as pool_address,
       meta:token0 as token0,
       meta:token1 as token1
       from silver.ethereum_contracts where address in (select pool_address from uniswapv3.pools)")

save(pool.choices, current.positions, swaps, pool.stats, file = "shiny_location")


