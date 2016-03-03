library(tidyjson)   # this library
library(dplyr)      # for %>% and other dplyr functions

purch_json <- '
[
  {
  "name": "bob", 
  "purchases": [
  {
  "date": "2014/09/13",
  "items": [
  {"name": "shoes", "price": 187},
  {"name": "belt", "price": 35}
  ]
  }
  ]
  },
  {
  "name": "susan", 
  "purchases": [
  {
  "date": "2014/10/01",
  "items": [
  {"name": "dress", "price": 58},
  {"name": "bag", "price": 118}
  ]
  },
  {
  "date": "2015/01/03",
  "items": [
  {"name": "shoes", "price": 115}
  ]
  }
  ]
  }
  ]'
  
  purch_items <- purch_json %>%
    gather_array                                     # stack the users 
    
  
    
  purch_items <- purch_json %>%
    gather_array %>%                                     # stack the users 
    spread_values(person = jstring("name")) %>%          # extract the user name
    enter_object("purchases") %>% gather_array %>%       # stack the purchases
    spread_values(purchase.date = jstring("date")) %>%   # extract the purchase date
    enter_object("items") %>% gather_array %>%           # stack the items
    spread_values(                                       # extract item name and price
      item.name = jstring("name"),
      item.price = jnumber("price")
    ) %>%
    select(person, purchase.date, item.name, item.price) # select only what is needed