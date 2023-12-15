library(tidyverse)

customers <- read_csv(here::here("5784", "speedrun", "noahs-customers.csv"))
orders_items <- read_csv(here::here("5784", "speedrun", "noahs-orders_items.csv"))
orders <- read_csv(here::here("5784", "speedrun", "noahs-orders.csv"))
products <- read_csv(here::here("5784", "speedrun", "noahs-products.csv"))

# Candle 1 ----------------------------------------------------------------

candle1 <- function(customers) {
	customers |> 
		select(name, phone) |> 
		separate_wider_delim(name, delim = " ", names = c("first", "last"), too_many = "merge") |> 
		mutate(phone_numbers = str_remove_all(phone, "-")) |> 
		mutate(last_phone = str_replace_all(last, "[a-cA-C]", "2")) |> 
		mutate(last_phone = str_replace_all(last_phone, "[d-fD-F]", "3")) |> 
		mutate(last_phone = str_replace_all(last_phone, "[g-iG-I]", "4")) |> 	
		mutate(last_phone = str_replace_all(last_phone, "[j-lJ-L]", "5")) |> 	
		mutate(last_phone = str_replace_all(last_phone, "[m-oM-O]", "6")) |> 	
		mutate(last_phone = str_replace_all(last_phone, "[p-sP-S]", "7")) |> 	
		mutate(last_phone = str_replace_all(last_phone, "[t-vT-V]", "8")) |> 	
		mutate(last_phone = str_replace_all(last_phone, "[w-zW-Z]", "9")) |> 
		filter(phone_numbers == last_phone) |> 
		pull(phone)
}

investigator <- candle1(customers)
investigator

# This ran fine

# Candle 2 ----------------------------------------------------------------

candle2 <- function(customers, orders, orders_items, products) {
	customers |> 
		separate_wider_delim(name, delim = " ", names = c("first", "last"), too_many = "merge") |> 
		filter(str_detect(first, "^D")) |> 
		filter(str_detect(last, "^S")) |> 
		left_join(orders, by = "customerid") |> 
		filter(str_detect(ordered, "2017")) |> 
		left_join(orders_items, by = "orderid") |> 
		left_join(products, by = "sku") |> 
		select(customerid, phone, orderid, desc) |> 
		group_by(customerid, phone, orderid) |> 
		filter(any(str_detect(desc, "Coffee")) & any(str_detect(desc, "Bagel"))) |> 
		distinct(customerid) |> 
		pull(phone) |> 
		I()
}

contractor <- candle2(customers, orders, orders_items, products)
contractor

# Needed to change the initials 
# (this caught me out as I assumed the puzzle text was exactly the same)

# Candle 3 ----------------------------------------------------------------

candle3 <- function(customers, contractor) { 
	
	neighborhood <- customers |> 
		filter(phone == contractor) |> 
		pull(citystatezip)
	
	customers |> 
		filter(year(birthdate) %in% c(1931, 1943, 1955, 1967, 1979, 1991, 2003, 2015)) |> 
		filter(month(birthdate) == 9 & day(birthdate) >= 21 | month(birthdate) == 10 & day(birthdate) <= 22) |> 
		filter(citystatezip == neighborhood) |> 
		pull(phone)
	
}

neighbor <- candle3(customers, contractor)
neighbor

# Needed to change the astrological details (goat/libra instead rabbit/cancer)

# Candle 4 ----------------------------------------------------------------

# This ran fine

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

# Candle 5 ----------------------------------------------------------------

candle5 <- function(customers, orders, orders_items) {
	customers |> 
		#filter(str_detect(citystatezip, "Staten")) |> 
		left_join(orders, by = "customerid") |> 
		left_join(orders_items, by = "orderid") |> 
		left_join(products, by = "sku") |> 
		filter(str_detect(desc, "Senior Cat")) |> 
		summarise(qty_cat_food = sum(qty), .by = c(phone, orderid))|> 
		select(-orderid) |> 
		distinct() |> 
		slice_max(qty_cat_food) |> 
		pull(phone)
}
cat_lady <- candle5(customers, orders, orders_items)
cat_lady

# No mention this time of Staten Island, 
# so needed to comment that out to get the correct solution

# Candle 6 ----------------------------------------------------------------

candle6 <- function(customers, orders, orders_items, products) {
	left_join(orders_items, products, by = "sku") |> 
		mutate(shop_price = qty * unit_price) |> 
		mutate(wholesale_price = qty * wholesale_cost) |> 
		select(-sku, -unit_price, -desc, -wholesale_cost, -dims_cm) |> 
		summarise(order_shop_price = sum(shop_price), order_wholesale_price = sum(wholesale_price), .by = orderid) |> 
		filter(order_wholesale_price > order_shop_price) |> 
		left_join(orders, by = "orderid") |> 
		count(customerid) |> 
		slice_max(n) |> 
		left_join(customers) |> 
		pull(phone)
}
bargain_hunter <- candle6(customers, orders, orders_items, products)
bargain_hunter

# This ran fine

# Candle 7 ----------------------------------------------------------------

candle7 <- function(customers, orders, orders_items, products, bargain_hunter) {
	
	# get required info about the bargain hunter
	bargain_hunter_id <- customers |> 
		filter(phone == bargain_hunter) |> 
		pull(customerid)
	
	bargain_hunter_orders <- customers |> 
		filter(phone == bargain_hunter) |> 
		left_join(orders, by = "customerid") |> 
		select(-(customerid:long), -items, -total, -ordered) 
	
	bargain_hunter_orders_dates <- bargain_hunter_orders |> 
		mutate(date = date(shipped)) |> 
		distinct(date) |> 
		pull(date)
	
	# All orders on the same date as the bargain hunter orders
	# Keep track of which are the bargain hunter and which are potential ex
	same_date_orders <- orders |> 
		mutate(shipped_date = date(shipped)) |> 
		filter(shipped_date %in% bargain_hunter_orders_dates) |> 
		select(orderid, customerid, shipped, shipped_date) |> 
		mutate(is_bargain_hunter = if_else(customerid == bargain_hunter_id, TRUE, FALSE)) |> 
		left_join(orders_items, by = "orderid") |> 
		left_join(products, by = "sku") |> 
		select(-qty, -unit_price, -wholesale_cost, -dims_cm) 
	
	# Make regex for colours
	colours <- products |> 
		filter(str_detect(desc, "Poster")) |>  # 12 colours, all with COL in sku
		arrange(desc) |> 
		separate_wider_regex(desc, c(item = ".*", " ", colour = ".*")) |> 
		pull(colour) |> 
		paste(collapse = "|") |> 
		str_remove_all("\\(|\\)") 
	
	# Orders of items with a colour on the dates that the bargain hunter shopped
	# Where two or more people bought the same item on that day
	# And where one of the two was the bargain hunter
	same_products <- same_date_orders |> 
		filter(str_detect(desc, colours)) |> 
		separate_wider_regex(desc, c(item = ".*", " ", colour = ".*")) |> 
		group_by(shipped_date) |>
		add_count(item) |>
		filter(n > 1) |>
		mutate(has_bargain_hunter = any(is_bargain_hunter)) |>
		filter(has_bargain_hunter) |>
		ungroup()
	
	# separate in bargain hunter and possible meet cute
	bh <- same_products |> 
		filter(is_bargain_hunter) |> 
		select(customerid, shipped_date, shipped, item, colour)
	
	possible_meet_cute <- same_products |> 
		filter(!is_bargain_hunter) |> 
		select(customerid, shipped_date, shipped, item, colour)
	
	# Now join these together by item and date 
	# .x is bargain hunter, .y is possible meet cute
	# find for row where colour is different and time is closest
	# that's the meet cute, so join with customers and pull phone
	inner_join(bh, possible_meet_cute, by = c("shipped_date", "item")) |> 
		filter(colour.x != colour.y) |>
		mutate(time_diff = abs(shipped.x - shipped.y)) |>
		slice_min(time_diff) |>
		left_join(customers, join_by(customerid.y == customerid)) |>
		pull(phone)
}
meet_cute <- candle7(customers, orders, orders_items, products, bargain_hunter) 
meet_cute

# There was a bug due to an assumption I'd made.
# I originally used COL in 'sku' to filter for items with colours, 
# but these are COLlector items (hinted at in puzzle 8), not COLour one
# and I should have paid more attention to my own original working notes about this
# and done what I'd originally said I'd come back to, getting colours from a product!
# Had to add that to the function 
# (and will refactor my candle5 function accordingly when I write it up for the blog)

# Candle 8 ----------------------------------------------------------------

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

# This ran fine

# All answers -------------------------------------------------------------

cat("Investigator:", investigator) # 767-365-7269
cat("Contractor:", contractor) # 838-351-0370
cat("Neighbor:", neighbor) # 914-594-5535
cat("Early Bird:", early_bird) # 716-789-4433
cat("Cat Lady:", cat_lady) # 347-835-2358
cat("Bargain Hunter:", bargain_hunter) # 838-295-7143
cat("Meet Cute:", meet_cute) # 516-544-4187
cat("Collector:", collector) # 516-638-9966
