library(tidyverse)
customers <- read_csv(here::here("5784", "data", "noahs-customers.csv"))

customers |> 
	summarise(min_year = min(birthdate), max_year = max(birthdate))

# Years of the Rabbit
browseURL("https://en.wikipedia.org/wiki/Rabbit_(zodiac)")

# Find Cancer star signs born in the Year of the Rabbit
rc_customers <- customers |> 
	filter(birthdate >= ymd("1939-06-21") & birthdate <= ymd("1939-07-22") |
				 birthdate >= ymd("1951-06-21") & birthdate <= ymd("1951-07-22") |
				 birthdate >= ymd("1963-06-21") & birthdate <= ymd("1963-07-22") |
				 birthdate >= ymd("1975-06-21") & birthdate <= ymd("1975-07-22") |
				 birthdate >= ymd("1987-06-21") & birthdate <= ymd("1987-07-22") |
				 birthdate >= ymd("1999-06-21") & birthdate <= ymd("1999-07-22") |
				 birthdate >= ymd("2011-06-21") & birthdate <= ymd("2011-07-22") 
	) 

# We know that the guy we're looking for lives in the same neighbourhood as the contractor.
# So where does the contractor live?
# From Day 2, contractor has ID 1475

neighborhood <- customers |> 
	filter(customerid == 1475) |> 
	pull(citystatezip)

rc_customers |> 
	filter(citystatezip == neighborhood) |> 
	pull(phone)
# correct: 917-288-9635

years_of_rabbit <- c(1939, 1951, 1963, 1975, 1987, 1999, 2011)


# refactor using lubridate functions 
# (h/t to Chad Allison, https://github.com/chadallison/hanukkah_of_data_2023)
candle3 <- function(customers, contractor) { 
	
	rabbit_cancer <- customers |> 
		filter(year(birthdate) %in% c(1939, 1951, 1963, 1975, 1987, 1999, 2011)) |> 
		filter(month(birthdate) == 6 & day(birthday) >= 21 | month(birthdate) == 7 & day(birthday) <= 22)
	
	neighborhood <- customers |> 
		filter(phone == contractor) |> 
		pull(citystatezip)
	
	rabbit_cancer |> 
		filter(citystatezip == neighborhood) |> 
		pull(phone)

}

# from Candle 2 (will need all days in same file for speedrun)
contractor <- "332-274-4185" 

candle3 <- function(customers, contractor) { 
	
	neighborhood <- customers |> 
		filter(phone == contractor) |> 
		pull(citystatezip)
	
	customers |> 
		filter(year(birthdate) %in% c(1939, 1951, 1963, 1975, 1987, 1999, 2011)) |> 
		filter(month(birthdate) == 6 & day(birthdate) >= 21 | month(birthdate) == 7 & day(birthdate) <= 22) |> 
		filter(citystatezip == neighborhood) |> 
		pull(phone)
	
}

neighbor <- candle3(customers, contractor)
neighbor
# "917-288-9635"
