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

#we don't use this but you CAN use it to pull any last-run data from the flipside STUDIO
PullVelocityData <- function(endpoint.url){
  raw.results <- readLines(endpoint.url)
  to.return <- data.table(jsonlite::fromJSON(raw.results))
  setnames(to.return, tolower(names(to.return)))
  return(to.return)
}

options(scipen = 999)

crvusd.xfers <- shroomDK::auto_paginate_query(
  query = paste0(
    "with taggies as (
      select distinct address, 
  first_value(tag_name) over (partition by address order by weight desc) as tag_name from (
  select
  address,
  tag_name,
  case
    when tag_name = 'wallet millionaire' then 5
    when tag_name = 'nft transactor top 10%' then 4
    when tag_name = 'gnosis safe address' then 3
    when tag_name = 'contract address' then 2
    else 0
  end as weight
from
  crosschain.core.address_tags
where
  creator = 'flipside'
  and blockchain = 'ethereum'
  and (
    end_date IS NULL
    or end_date > current_date - 31
  )
  and tag_name in (
    'wallet millionaire',
    'nft transactor top 10%',
    'gnosis safe address',
    'contract address'
  )
))
    select 
  block_timestamp::date as date,
  xfers.from_address, xfers.to_address,
  coalesce(lf.label,tagsfr.tag_name,'unlabeled') as from_label,
  coalesce(lf.label_subtype,tagsfr.tag_name,'unlabeled') as from_subtype,
  coalesce(lf.label_type,tagsfr.tag_name,'unlabeled') as from_type,
  coalesce(lf.address_name,tagsfr.tag_name,'unlabeled') as from_name,
  coalesce(lt.label,tagsto.tag_name,'unlabeled') as to_label,
  coalesce(lt.label_subtype,tagsto.tag_name,'unlabeled') as to_subtype,
  coalesce(lt.label_type,tagsto.tag_name,'unlabeled') as to_type,
  coalesce(lt.address_name,tagsto.tag_name,'unlabeled') as to_name,
  sum(amount) as amount
FROM
ethereum.core.ez_token_transfers xfers
left join ethereum.core.dim_labels lf
on lf.address = xfers.from_address
left join ethereum.core.dim_labels lt
on lt.address = xfers.to_address
left join taggies as tagsfr on tagsfr.address = xfers.from_address
left join taggies as tagsto on tagsto.address = xfers.to_address
where block_timestamp > '2023-05-14'
and xfers.contract_address = lower('0xf939E0A03FB07F59A73314E73794Be0E57ac1b4E')
group by 1,2,3,4,5,6,7,8,9,10,11 -- crvUSD
;"),
  api_key = readLines('~/data_science/util/shroomdk.key')
)
crvusd.xfers <- as.data.table(crvusd.xfers)
setnames(crvusd.xfers, tolower(names(crvusd.xfers)))

#do some labeling finagling
crvusd.xfers[ to_label %in% c('curve','curve fi'), to_label := to_name ]
crvusd.xfers[ from_label %in% c('curve','curve fi'), from_label := from_name ]

crvusd.xfers[,sum(amount),by = "to_label,from_label,to_type,from_type"][order(V1,decreasing = T)]

crvusd.xfers[,from_group := from_label ]
crvusd.xfers[,to_group := to_label ]

#couple of mis-flagged CEX deposit addresses
crvusd.xfers[ from_type %in% c('unlabeled','cex') , from_group := 'other users' ]
crvusd.xfers[ to_type %in% c('unlabeled','cex') , to_group := 'other users' ]

crvusd.xfers[ from_group %in% c('gnosis safe','contract address') , from_group := 'other contracts' ]
crvusd.xfers[ to_group %in% c('gnosis safe','contract address') , to_group := 'other contracts' ]

crvusd.xfers[ from_group %in% c('1inch','stacker ventures','frax finance','1inch network'), from_group := 'other protocols' ]
crvusd.xfers[ to_group %in% c('1inch','stacker ventures','frax finance','1inch network'), to_group := 'other protocols' ]

crvusd.xfers[ from_address == '0x99a58482bd75cbab83b27ec03ca68ff489b5788f', from_group := 'curve fi: general contract']
crvusd.xfers[ to_address == '0x99a58482bd75cbab83b27ec03ca68ff489b5788f', to_group := 'curve fi: general contract']

crvusd.xfers[ from_group %in% c('curve.fi: deployer 2',"curve fi: general contract",'curve.fi: pool owner'), from_group := "other curve contracts" ]
crvusd.xfers[ to_group %in% c('curve.fi: deployer 2',"curve fi: general contract",'curve.fi: pool owner'), to_group := "other curve contracts" ]

crvusd.xfers[ from_group == 'nft transactor top 10%', from_group := 'nft transactor top 10' ]
crvusd.xfers[ to_group == 'nft transactor top 10%', to_group := 'nft transactor top 10' ]

crvusd.xfers[ from_group == "curve.fi factory plain pool: crvusd/usdt", from_group := 'crvusd usdt' ]
crvusd.xfers[ to_group == "curve.fi factory plain pool: crvusd/usdt", to_group := 'crvusd usdt' ]

crvusd.xfers[ from_group == "curve.fi factory plain pool: crvusd/usdc", from_group := 'crvusd usdc' ]
crvusd.xfers[ to_group == "curve.fi factory plain pool: crvusd/usdc", to_group := 'crvusd usdc' ]

crvusd.xfers[ from_group == "curve.fi factory plain pool: crvusd/tusd", from_group := 'crvusd tusd' ]
crvusd.xfers[ to_group == "curve.fi factory plain pool: crvusd/tusd", to_group := 'crvusd tusd' ]

crvusd.xfers[ from_group == "curve.fi factory plain pool: crvusd/usdp", from_group := 'crvusd usdp' ]
crvusd.xfers[ to_group == "curve.fi factory plain pool: crvusd/usdp", to_group := 'crvusd usdp' ]

crvusd.xfers[ from_address == tolower('0x8472A9A7632b173c8Cf3a86D3afec50c35548e76'), from_group := 'crvusd controller' ]
crvusd.xfers[ to_address == tolower('0x8472A9A7632b173c8Cf3a86D3afec50c35548e76'), to_group := 'crvusd controller' ]

crvusd.xfers[ from_address == tolower('0x136e783846ef68C8Bd00a3369F787dF8d683a696'), from_group := 'sfrxETH LLAMMA' ]
crvusd.xfers[ to_address == tolower('0x136e783846ef68C8Bd00a3369F787dF8d683a696'), to_group := 'sfrxETH LLAMMA' ]

#peg-keepers for each pool
crvusd.xfers[ from_address %in% c(
  tolower('0xE7cd2b4EB1d98CD6a4A48B6071D46401Ac7DC5C8'),
  tolower('0x6b765d07cf966c745b340adca67749fe75b5c345'),
  tolower('0xaa346781ddd7009caa644a4980f044c50cd2ae22'),
  tolower('0x1ef89ed0edd93d1ec09e4c07373f69c49f4dccae')
), from_group := 'peg keeper' ]
crvusd.xfers[ to_address %in% c(
  tolower('0xE7cd2b4EB1d98CD6a4A48B6071D46401Ac7DC5C8'),
  tolower('0x6b765d07cf966c745b340adca67749fe75b5c345'),
  tolower('0xaa346781ddd7009caa644a4980f044c50cd2ae22'),
  tolower('0x1ef89ed0edd93d1ec09e4c07373f69c49f4dccae')
), to_group := 'peg keeper' ]

crvusd.xfers[ from_address == tolower('0x99a58482BD75cbab83b27EC03CA68fF489b5788f'), from_group := 'swap router' ]
crvusd.xfers[ to_address == tolower('0x99a58482BD75cbab83b27EC03CA68fF489b5788f'), to_group := 'swap router' ]

crvusd.xfers[,sum(amount),by = 'from_group,to_group'][ order(V1,decreasing = T)]

bubble.data <- crvusd.xfers[
  from_group != to_group,
  list(token_value = sum(amount,na.rm = T)),
  by = list(xfer_date = date,
            from_group,
            to_group )]

bubble.data[,from_to := paste(from_group,to_group,sep = "_X_"),by = "from_group,to_group"]
# ggplot(bubble.data, aes(x = xfer_date, y = token_value, group = from_to, fill = from_to)) +
#   geom_bar(stat = "identity",position = "dodge") +
#   theme(legend.position = "none")


# Sanity Check
c(bubble.data$from_group,bubble.data$to_group) %>% unique() #groups
setnames(bubble.data, "token_value", "xfer_volume")

bubble.data[ ,from_group := str_replace_all(from_group,' ','_') ]
bubble.data[ ,to_group := str_replace_all(to_group,' ','_') ]

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
writeLines(new.to.save,con = "crvusd_bubbles.json")
bubble.data[ ,sum(xfer_volume), by = "to_group,from_group"]




