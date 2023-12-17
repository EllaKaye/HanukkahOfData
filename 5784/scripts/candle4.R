library(tidyverse)

customers <- read_csv(here::here("5784", "data", "noahs-customers.csv"))
orders <- read_csv(here::here("5784", "data", "noahs-orders.csv"))
orders_items <- read_csv(here::here("5784", "data", "noahs-orders_items.csv"))
products <- read_csv(here::here("5784", "data", "noahs-products.csv"))

# Find an order of pastries that were ordered before 5am
early_orders <- orders |> 
	#separate_wider_delim(ordered, " ", names = c("date", "time")) |> 
	separate_wider_delim(shipped, " ", names = c("date", "time")) |> 
	#select(-shipped, -date, -items, -total) |> 
	select(-ordered, -date, -items, -total) |> 
#	mutate(time = hms(time)) |> 
	mutate(early = time < "05:00:00" & time > "04:00:00") |> 
	filter(early) |> 
	select(-early)

left_join(early_orders, orders_items, by = "orderid") |> 
	left_join(products, by = "sku") |> 
#	count(desc) |> 
#	View()
	filter(str_detect(desc, "Cookie|Rugelach|Sufganiah|Babka")) |> 
	group_by(orderid) |> 
#	summarise(n_pastries = sum(qty))
#	count(orderid) |>  # Wll distinct orders. We're looking for plural pastries
	View()

# Let's back up and look at all products
products |> 
	arrange(desc) |> 
	filter(!str_detect(desc, "Cat|Dog|Ferret|Food|Automatic|Manual|Electric|Disney|Marvel|Lego|Mechanical|Handmade|Jigsaw")) |> 
	View()

# Maybe work backward from pastry orders?
# Which is relevant, ordered or shipped time?
# Need to find which of the foods are pastries!
# Forgot about the `qty` column!

# Let's make sure we've got all pastries.
pastries <- products |> 
	filter(str_detect(desc, "Poppyseed")) |> 
	separate_wider_delim(desc, " ", names =  c("ingredient", "pastry"), too_many = "merge") |> 
	pull(pastry) |> 
	paste(collapse = "|")
pastries
# This worked for this dataset, but turns out I'd missed some


left_join(early_orders, orders_items, by = "orderid") |> 
	left_join(products, by = "sku") |> 
	filter(str_detect(desc, pastries)) |> 
	group_by(orderid) |> 
	summarise(n_pastries = sum(qty)) |> 
	filter(n_pastries > 1) |> 
	pull(orderid)
# 49639
orders |> 
	filter(orderid == 49639) |> 
	pull(customerid)
# 2749

customers |> 
	filter(customerid == 2749) |> 
	pull(phone)
# 607-231-3605

# saw a hint afterwards to look at codes in 'sku'

products |> 
	filter(str_detect(sku, "BKY"))
# This picks up more than my pastry list above

products |> 
	filter(str_detect(desc, "Bagel"))

# earliest hour a pastry is shipped
orders |> 
#	filter(hour(shipped) %in% 3:4) |> 
	left_join(orders_items, by = "orderid") |> 
	filter(str_detect(sku, "BKY")) |> 
	separate_wider_delim(shipped, " ", names = c("shipped_date", "shipped_time")) |> 
	mutate(shipped_time = hms(shipped_time)) |> 
	filter(shipped_time > 2) |> 
	arrange(shipped_time) |> 
	View()

# final version, refactored to use `BKY` in sku
# also using `hour()` is so much neater than above - h/t Chad Allison
# https://github.com/chadallison/hanukkah_of_data_2023
# note that this time we have more than one customer with more than one pastry
# from hint "she liked to get up before dawn and claim the first pastries that came out of the oven"
# suggests she does this often, so from them select customer with the most orders at this time
# earliest sunrise in Manhattan is at 5:24am in mid-June.
# Probably sometime after 4am reasonable from clue, but could extend to 3am
candle4 <- function(customers, orders, orders_items) {
	orders |> 
		filter(hour(shipped) %in% 3:4) |> 
		left_join(orders_items, by = "orderid") |> 
		filter(str_detect(sku, "BKY")) |> 
		summarise(n_pastries = sum(qty), .by = "orderid") |> 
		filter(n_pastries > 1) |> 
		left_join(orders, by = "orderid") |> 
		count(customerid) |> 
		slice_max(n) |> 
		left_join(customers) |> 
		pull(phone)	
}

early_bird <- candle4(customers, orders, orders_items)
early_bird

# "607-231-3605"

# Speedtun ran fine, but use inner join instead
candle4 <- function(customers, orders, orders_items) {
	orders |> 
		filter(hour(shipped) %in% 3:4) |> 
		inner_join(orders_items, by = "orderid") |> 
		filter(str_detect(sku, "BKY")) |> 
		summarise(n_pastries = sum(qty), .by = "orderid") |> 
		filter(n_pastries > 1) |> 
		inner_join(orders, by = "orderid") |> 
		count(customerid) |> 
		slice_max(n) |> 
		inner_join(customers, by = "customerid") |> 
		pull(phone)	
}

early_bird <- candle4(customers, orders, orders_items)
early_bird
# # "607-231-3605"

customers_speed <- read_csv(here::here("5784", "speedrun", "noahs-customers.csv"))
orders_speed <- read_csv(here::here("5784", "speedrun", "noahs-orders.csv"))
orders_items_speed <- read_csv(here::here("5784", "speedrun", "noahs-orders_items.csv"))
early_bird_speed <- candle4(customers_speed, orders_speed, orders_items_speed)
early_bird_speed
# "716-789-4433"
