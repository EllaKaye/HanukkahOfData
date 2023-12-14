library(tidyverse)

customers <- read_csv(here::here("5784", "data", "noahs-customers.csv"))
orders_items <- read_csv(here::here("5784", "data", "noahs-orders_items.csv"))
orders <- read_csv(here::here("5784", "data", "noahs-orders.csv"))
products <- read_csv(here::here("5784", "data", "noahs-products.csv"))

# Looking for someone who has the entire set of Noah's collectibles.
# Not sure whether colour is relevant.
# Try without first

all_noah <- customers |> 
	left_join(orders, by = "customerid") |> 
	left_join(orders_items, by = "orderid") |> 
	left_join(products, by = "sku") |> 
	filter(str_detect(desc, "Noah"))

all_noah |> 
	count(phone, sort = TRUE) |> # unique phones, so equivalent to customer id
	slice_max(n) |> 
	pull(phone)
# the top has 115 items, next most has 41.

	