#use flipside API endpoints to bring the requisite data into R
#shape it into JSON
#put that JSON into the bubbles plot on JS-Fiddle
#fun haz

library(jsonlite)
library(data.table)
library(ggplot2)
library(stringr)
library(lubridate)
library(shroomDK)

PullVelocityData <- function(endpoint.url){
  raw.results <- readLines(endpoint.url)
  to.return <- data.table(jsonlite::fromJSON(raw.results))
  setnames(to.return, tolower(names(to.return)))
  return(to.return)
}

bonk.xfers <- shroomDK::auto_paginate_query(
  query = paste0(
    "with airdropees as (
  select distinct tx_to as address from 
  solana.core.fact_transfers
  where tx_from in (
  '9AhKqLR67hwapvG8SA2JFXaCshXc9nALJjpKaHZrsbkw',
  '6JZoszTBzkGsskbheswiS6z2LRGckyFY4SpEGiLZqA9p')
  and mint = 'DezXAZ8z7PnrnRJjz3wXBoRgixCa6xjnB7YaB1pPB263'
  and block_timestamp > '2022-12-24'
)

SELECT
  --date_trunc('day',tx.block_timestamp) as date,
  time_slice(tx.block_timestamp, 12, 'HOUR') as date,
  case when tx.tx_to = '9AhKqLR67hwapvG8SA2JFXaCshXc9nALJjpKaHZrsbkw' then 'bonk airdrop address'
       when tx.tx_to = '6JZoszTBzkGsskbheswiS6z2LRGckyFY4SpEGiLZqA9p' then 'bonk new airdrop address'
       when tx.tx_to = 'BqnpCdDLPV2pFdAaLnVidmn3G93RP2p5oRdGEY2sJGez' then 'orca bonk-sol pool'
       when tx.tx_to = '5P6n5omLbLbP4kaPGL8etqQAHEx2UCkaUyvjLDnwV4EY' then 'orca bonk-usdc pool'
       when tx.tx_to = '2PFvRYt5h88ePdQXBrH3dyFmQqJHTNZYLztE847dHWYz' then 'dex bonk-usdc pool'
       when tx.tx_to = 'DBR2ZUvjZTcgy6R9US64t96pBEZMyr9DPW6G2scrctQK' then 'bonk dao wallet'
       when tx.tx_to = '4CUMsJG7neKqZuuLeoBoMuqufaNBc2wdwQiXnoH4aJcD' then 'bonk team wallet'
       when tx.tx_to = '2yBBKgCwGdVpo192D8WZeAtqyhyP8DkCMnmTLeVYfKtA' then 'bonk marketing wallet'
  else coalesce(lto.label,'unlabeled') end as to_label,
  coalesce(lto.label_type,'unlabeled') as to_label_type,
  case when tx.tx_to in (select address from airdropees) then 'airdrop recipient'
       else coalesce(lto.label_subtype,'unlabeled user') end as to_label_subtype,
  case when tx.tx_from = '9AhKqLR67hwapvG8SA2JFXaCshXc9nALJjpKaHZrsbkw' then 'bonk airdrop address'
       when tx.tx_from = '6JZoszTBzkGsskbheswiS6z2LRGckyFY4SpEGiLZqA9p' then 'bonk new airdrop address'
       when tx.tx_from = 'BqnpCdDLPV2pFdAaLnVidmn3G93RP2p5oRdGEY2sJGez' then 'orca bonk-sol pool'
       when tx.tx_from = '5P6n5omLbLbP4kaPGL8etqQAHEx2UCkaUyvjLDnwV4EY' then 'orca bonk-usdc pool'
       when tx.tx_from = '2PFvRYt5h88ePdQXBrH3dyFmQqJHTNZYLztE847dHWYz' then 'dex bonk-usdc pool'
       when tx.tx_from = 'DBR2ZUvjZTcgy6R9US64t96pBEZMyr9DPW6G2scrctQK' then 'bonk dao wallet'
       when tx.tx_from = '4CUMsJG7neKqZuuLeoBoMuqufaNBc2wdwQiXnoH4aJcD' then 'bonk team wallet'
       when tx.tx_from = '2yBBKgCwGdVpo192D8WZeAtqyhyP8DkCMnmTLeVYfKtA' then 'bonk marketing wallet'
  else coalesce(lfr.label,'unlabeled') end as from_label,
  coalesce(lfr.label_type,'unlabeled') as from_label_type,
  case when tx.tx_from in (select address from airdropees) then 'airdrop recipient'
  else coalesce(lfr.label_subtype,'unlabeled user') end as from_label_subtype,
  sum(tx.amount) as n_tokens
FROM
solana.core.fact_transfers tx
  left join solana.core.dim_labels lfr on lfr.address = tx.tx_from
  left join solana.core.dim_labels lto on lto.address = tx.tx_to
where tx.block_timestamp > '2022-12-24'
and tx.mint = 'DezXAZ8z7PnrnRJjz3wXBoRgixCa6xjnB7YaB1pPB263'
group by 1,2,3,4,5,6,7;"),
  api_key = readLines('shroomdk.key') #get free api key at sdk.flipsidecrypto.xyz
)
bonk.xfers <- as.data.table(bonk.xfers)
setnames(bonk.xfers, tolower(names(bonk.xfers)))

#have a look at the pairs and determine what looks interesting, compress
#to a meaningful but not overwhelming number of groups
bonk.xfers[,sum(n_tokens),
           by = "to_label,from_label,to_label_subtype,from_label_subtype"][
             order(V1,decreasing = T)]

#do some labeling finagling
#this part is slightly subjective, relies on combination of what is
#important for the project, what your own curiosity is, how complex vs
#simple the story you want to tell is
bonk.xfers[,from_group := from_label ]
bonk.xfers[,to_group := to_label ]
bonk.xfers[ from_label_type == 'cex', from_group := 'cex' ]
bonk.xfers[ to_label_type == 'cex', to_group := 'cex' ]
bonk.xfers[ from_label_type == 'dex' | 
              from_label %in% c('orca bonk-usdc pool',
                                'orca bonk-sol pool',
                                'dex bonk-usdc pool'),
            from_group := "dexes" ]
bonk.xfers[ to_label_type == 'dex' |
              to_label %in% c('orca bonk-usdc pool',
                              'orca bonk-sol pool',
                              'dex bonk-usdc pool'), to_group := "dexes" ]
bonk.xfers[ from_label == "unlabeled", from_group := 'users' ]
bonk.xfers[ to_label == "unlabeled", to_group := 'users' ]

#crucial not to over-label addresses as airdrop recipients
#so we do this only for otherwise unlabeled addresses
bonk.xfers[ from_label == "unlabeled" & from_label_subtype == 'airdrop recipient', from_group := 'airdrop recipient' ]
bonk.xfers[ to_label == "unlabeled" & to_label_subtype == 'airdrop recipient', to_group := 'airdrop recipient' ]

#this is marginal in terms of volume, but 
#noticed a few nft addresses receiving and sending BONK around
bonk.xfers[ from_label_type == "nft", from_group := 'nft_projects' ]
bonk.xfers[ to_label_type == "nft", to_group := 'nft_projects' ]


bubble.data <- bonk.xfers[
  from_group != to_group,
  list(token_value = sum(n_tokens,na.rm = T)),
  by = list(xfer_date = as.POSIXct(date),
            from_group,
            to_group )]

bubble.data[,sum(token_value),by = "from_group,to_group"][order(V1,decreasing = T)]

bubble.data[,from_to := paste(from_group,to_group,sep = "_X_"),by = "from_group,to_group"]

#optionally plot the to/from groups
# ggplot(bubble.data, aes(x = xfer_date, y = token_value, group = from_to, fill = from_to)) +
#   geom_bar(stat = "identity",position = "dodge") +
#   theme(legend.position = "none")


# This is both a sanity check
# and helpful for putting the proper group names into the viz later
c(bubble.data$from_group,bubble.data$to_group) %>% unique() #groups
setnames(bubble.data, "token_value", "xfer_volume")

bubble.data[ ,from_group := str_replace_all(from_group,' ','_') ]
bubble.data[ ,to_group := str_replace_all(to_group,' ','_') ]

#boring json conversion that looks simple but took forever json is annoying
daily.row.empty <- "{ 'src': '%s', 'dst': '%s', 'num': %s },"

bubble.data[, row_num := 1:.N]
bubble.data[, flow_json := sprintf(daily.row.empty,
                                   from_group, to_group, xfer_volume), by = row_num]

bubble.data <- bubble.data[!is.na(xfer_volume)]
setorder(bubble.data, xfer_date) #block_timestamp
changes.list <- lapply(unique(bubble.data[!is.na(xfer_date) & !is.na(xfer_volume)]$xfer_date), function(.date){
  sub.data <- bubble.data[ xfer_date == .date ]
  unbox(unlist(lapply(toJSON(list(
    timestamp = unbox(as.integer(as.POSIXct(sub.data$xfer_date[1]))),
    changes = lapply(sub.data$flow_json,function(x) unbox(substr(x,1,nchar(x) - 1)))
  )),str_remove_all,'"')))
})
bubbles.json <- str_remove_all(toJSON(changes.list, pretty = T),'"')

new.to.save <- paste0("const data = {
                        steps: ",
                      bubbles.json,
                      "}")

#save it!
writeLines(new.to.save,con = "~/bonk_bubbles.json")
bubble.data[ ,sum(xfer_volume), by = "to_group,from_group"]




