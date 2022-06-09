
data <- readRDS("data/Dashboard_Data.rds") %>%
  mutate(Dates = year(MPE_DATE)) %>%
  mutate(value = VALUE) %>%
  add.var.col(.) %>%
  add.sc.col(.) %>%
  add.ic.col(.) %>% 
  add.ic.fy.col(.) 

return(data)