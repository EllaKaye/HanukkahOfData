library(tidyverse)

customers <- read_csv(here::here("5784", "data", "noahs-customers.csv"))
orders_items <- read_csv(here::here("5784", "data", "noahs-orders_items.csv"))
orders <- read_csv(here::here("5784", "data", "noahs-orders.csv"))
products <- read_csv(here::here("5784", "data", "noahs-products.csv"))

# Find someone who bought the same item as the bargain hunter on the same day
# but in a different colour

bargain_hunter_phone <- "585-838-9161" # from yesterday
bargain_hunter <- customers |> 
	filter(phone == bargain_hunter_phone) 

bargain_hunter_id <- bargain_hunter |> pull(customerid)

bargain_hunter_orders <- customers |> 
	filter(phone == bargain_hunter_phone) |> 
	left_join(orders) |> 
	select(-(customerid:long), -items, -total, -ordered) |> 
	separate_wider_delim(shipped, " ", names = c("shipped_date", "shipped_time")) # probably don't need time
# DO need time!

bargain_hunter_orders_dates <- bargain_hunter_orders |> pull(shipped_date)

# All orders on the same day as the bargain hunter orders
matched_orders <- orders |> 
	separate_wider_delim(shipped, " ", names = c("shipped_date", "shipped_time")) |> 
	filter(shipped_date %in% bargain_hunter_orders_dates) |> 
	select(orderid, customerid, shipped_date, shipped_time) |> 
	mutate(is_bargain_hunter = if_else(customerid == bargain_hunter_id, TRUE, FALSE)) |> 
	left_join(orders_items, by = "orderid") |> 
	left_join(products, by = "sku") |> 
	select(-qty, -unit_price, -wholesale_cost, -dims_cm) 

# Anything with a colour seems to end with the colour name in () at the end
products |> 
	filter(str_detect(desc, "red")) #seven items, each have "COL" in the sku
# Poster, Bobblehead, Jersey, Gift Box, Action Figure, Lunchbox, Jewelry
# Do all items that have a colour have the same set of colours?

products |> 
	#filter(str_detect(desc, "Poster")) # 12 colours, all with COL in sku
	#filter(str_detect(desc, "Bobblehead")) |>  # 12 colours, all with COL in sku
	filter(str_detect(desc, "Jersey")) |>  # 12 colours, all with COL in sku
	arrange(desc)
# amber, azure, blue, green, magenta, mauve, orange, puce, purple, red, white, yellow
# all seem to have the same 12 colours

products |> 
	filter(str_detect(sku, "COL")) |> # Hopefully 84 rows, 7 products * 12 colours 
	arrange(desc) |> 
	View()
# Actually 85 orders, # COL0041 is the addition.
# But only 1 of these, so will filter out when comparing for different colour

# Maybe safest to go by colour names
# But would need to pull these from the products. 
# But can I be sure that, e.g. Jersey will appear in the speed run set?
# Maybe come back to this

# dates where two people ordered the same item, on a date when the bh shopped
bh_ex_candidates <- matched_orders |> 
	filter(str_detect(sku, "COL")) |> 
	#filter(sku != "COL0041") |> # doesn't make a difference here 
	mutate(desc = str_remove(desc, "Noah's ")) |> 
	separate_wider_delim(desc, " ", names = c("item", "item_colour"), too_many = "merge") |> 
	separate_wider_delim(item_colour, " ", names = c("item2", "colour"), too_few = "align_end") |> 
	select(-item2) |> 
	group_by(shipped_date) |> 
	add_count(item) |> 
	filter(n > 1) |> 
	# get rid of ship dates that now don't include the bargain hunter
	mutate(has_bargain_hunter = any(is_bargain_hunter)) |> 
	filter(has_bargain_hunter) 
	
bh_order <- bh_ex_candidates |> 
	filter(is_bargain_hunter) |> 
	select(customerid, shipped_date, shipped_time, item, colour)
bh_order
# now join this back by item
possible_ex <- bh_ex_candidates |> 
	filter(!is_bargain_hunter) |> 
	select(customerid, shipped_date, shipped_time, item, colour)

ex_id <- inner_join(bh_order, possible_ex, by = c("shipped_date", "item")) |> 
	filter(colour.x != colour.y) |> 
# find shipped times close to each other
	mutate(shipped_time.x = strptime(shipped_time.x, format = "%H:%M:%S"), # adds date, but that doesn't matter
				 shipped_time.y = strptime(shipped_time.y, format = "%H:%M:%S")) |> 
	mutate(time_diff = abs(shipped_time.x - shipped_time.y)) |> 
	ungroup() |> 
	slice_min(time_diff) |> 
	pull(customerid.y)

customers |> 
	filter(customerid == ex_id) |> 
	pull(phone)
# Correct! "838-335-7157"

# note - if I keep `shipped` I can calculate the difference between those without needing the strptime conversion
# then can use shipped_date for grouping by date and shipped for time differences.
# e.g.
orders |> 
	head() |> 
	#mutate(shipped_date = str_extract(shipped, "\\d{4}-\\d{2}-\\d{2}"))
	mutate(shipped_date = str_extract(shipped, "^.{10}"))

# Better splitting of colours by regex
# see ?separate_wider_regex for an example of splitting by last delimiter,
# and why this regex works
products |> 
	filter(str_detect(sku, "COL")) |> 
	select(desc) |> 
	separate_wider_regex(desc, c(item = ".*", " ", colour = ".*"))



# refactor 
bargain_hunter <- "585-838-9161" # from yesterday
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
	
	# Orders of items with a colour on the dates that the bargain hunter shopped
	# Where two or more people bought the same item on that day
	# And where one of the two was the bargain hunter
	same_products <- same_date_orders |> 
		filter(str_detect(sku, "COL")) |> 
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
