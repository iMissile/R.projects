library(rvest)
ami <- read_html("https://wiki.asterisk.org/wiki/display/AST/Asterisk+13+AMI+Events")

ref <- ami %>% 
  html_nodes(".child-display") %>%
  # html_text()
  # html_attrs("href")
  html_nodes("a") %>%
  html_attr("href")

# %>%
  # html_text()
