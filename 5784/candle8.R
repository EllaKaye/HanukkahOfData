library(tidyverse)

customers <- read_csv(here::here("5784", "data", "noahs-customers.csv"))
orders_items <- read_csv(here::here("5784", "data", "noahs-orders_items.csv"))
orders <- read_csv(here::here("5784", "data", "noahs-orders.csv"))
products <- read_csv(here::here("5784", "data", "noahs-products.csv"))

# Looking for someone who has the entire set of Noah's collectibles.
# Not sure whether colour is relevant.
# Try without first
# The person with the most Noah's products
all_noah <- customers |> 
	left_join(orders, by = "customerid") |> 
	left_join(orders_items, by = "orderid") |> 
	left_join(products, by = "sku") |> 
	filter(str_detect(desc, "Noah"))

all_noah |> 
	select(phone, desc) |> 
	distinct() |> 
	count(phone, sort = TRUE) |> # unique phones, so equivalent to customer id
	slice_max(n) |> 
	pull(phone)
# the top has 89 items, next most has 35.
# "212-547-3518"

# how many Noah's products? 101
products |> 
	filter(str_detect(desc, "Noah")) 


candle8 <- function(customers, orders, orders_items, products) {
	customers |> 
		left_join(orders, by = "customerid") |> 
		left_join(orders_items, by = "orderid") |> 
		left_join(products, by = "sku") |> 
		filter(str_detect(desc, "Noah")) |> 
		count(phone, sort = TRUE) |> # unique phones, so equivalent to customer id
		slice_max(n) |> 
		pull(phone)
}

collector <- candle8(customers, orders, orders_items, products)
collector	

# post-speedrun refactor
# this ran fine, just change left to inner join

candle8 <- function(customers, orders, orders_items, products) {
	customers |> 
		inner_join(orders, by = "customerid") |> 
		inner_join(orders_items, by = "orderid") |> 
		inner_join(products, by = "sku") |> 
		filter(str_detect(desc, "Noah")) |> 
		count(phone, sort = TRUE) |> # unique phones, so equivalent to customer id
		slice_max(n) |> 
		pull(phone)
}
collector <- candle8(customers, orders, orders_items, products)
collector	
# "212-547-3518"

customers_speed <- read_csv(here::here("5784", "speedrun", "noahs-customers.csv"))
orders_speed <- read_csv(here::here("5784", "speedrun", "noahs-orders.csv"))
orders_items_speed <- read_csv(here::here("5784", "speedrun", "noahs-orders_items.csv"))
products_speed <- read_csv(here::here("5784", "speedrun", "noahs-products.csv"))

collector_speed <- candle8(customers_speed, orders_speed, orders_items_speed, products_speed) 
collector_speed
# "516-638-9966"