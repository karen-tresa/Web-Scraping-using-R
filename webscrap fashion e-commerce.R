install.packages("rvest")
install.packages("dplyr")
install.packages("magrittr")
install.packages("stringr")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("tidytext")

#tshirts

library(stringr)
library(lubridate)
library(tidyverse)
library(tidytext)
library(magrittr) 
library(rvest)
library(dplyr)



get_fabric = function(product_link){
  product_page= read_html(product_link)
  product_fabric= product_page %>% html_nodes(".row:nth-child(4) ._2vZqPX") %>%
    html_text()%>% 
               {if(length(.) == 0) NA else .}
  return(product_fabric)
}



get_fit = function(product_link){
  product_page= read_html(product_link)
  product_fit= product_page %>% html_nodes(".row:nth-child(3) ._2vZqPX") %>%
    html_text()%>% 
              {if(length(.) == 0) NA else .}
  return(product_fit)
}

get_sleeve = function(product_link){
  product_page= read_html(product_link)
  product_sleeve= product_page %>% html_nodes(".X3BRps .row:nth-child(2) ._2vZqPX") %>%
    html_text()%>% 
    {if(length(.) == 0) NA else .}
  return(product_sleeve)
}

get_pattern = function(product_link){
  product_page= read_html(product_link)
  product_pattern= product_page %>% html_nodes(".row:nth-child(10) ._2vZqPX") %>%
    html_text()%>% 
      {if(length(.) == 0) NA else .}
  return(product_pattern)
}


get_category = function(product_link){
  product_page= read_html(product_link)
  product_category= product_page %>% html_nodes("._3GIHBu:nth-child(5) ._2whKao") %>%
    html_text()%>% 
               {if(length(.) == 0) NA else .}
  return(product_category)
}

get_collar = function(product_link){
  product_page= read_html(product_link)
  collar= product_page %>% html_nodes(".X3BRps .row:nth-child(1) ._2vZqPX") %>%
    html_text()%>% 
      {if(length(.) == 0) NA else .}
  return(collar)
}

get_price = function(product_link){
  product_page= read_html(product_link)
  price= product_page %>% html_nodes("._2p6lqe") %>%
    html_text()%>% 
    {if(length(.) == 0) NA else .}
  return(price)
}

get_color = function(product_link){
  product_page= read_html(product_link)
  color= product_page %>% html_nodes(".row:nth-child(6) ._2vZqPX") %>%
    html_text()%>% 
    {if(length(.) == 0) NA else .}
  return(color)
}

webscraptshirts = data.frame()

for (page_result in seq(from = 1, to = 9, by = 1)) {
  link = paste0("https://www.flipkart.com/mens-tshirts/pr?sid=clo%2Cash%2Cank%2Cedy&fm=neo%2Fmerchandising&iid=M_7fa50db4-a582-4bfd-bbf4-9437c551ffe6_2_372UD5BXDFYS_MC.IF56C41VGEYS&otracker=hp_rich_navigation_2_2.navigationCard.RICH_NAVIGATION_Fashion%7EMen%2527s%2BTop%2BWear%7EMen%2527s%2BT-Shirts_IF56C41VGEYS&otracker1=hp_rich_navigation_PINNED_neo%2Fmerchandising_NA_NAV_EXPANDABLE_navigationCard_cc_2_L2_view-all&cid=IF56C41VGEYS&p%5B%5D=facets.brand%255B%255D%3DPEOPLE&p%5B%5D=facets.brand%255B%255D%3DByford%2Bby%2BPantaloons&p%5B%5D=facets.brand%255B%255D%3DRoadster&p%5B%5D=facets.brand%255B%255D%3DUrban%2BRanger%2Bby%2BPantaloons&page=",page_result)
  page = read_html(link)
  
  brand = page %>% html_nodes("._2WkVRV") %>% 
    html_text()%>% 
      {if(length(.) == 0) NA else .}
  productdescription = page %>% html_nodes(".IRpwTa") %>% 
    html_text()%>% 
      {if(length(.) == 0) NA else .}
  productimage = page %>% html_nodes("._2r_T1I") %>% 
    html_text()%>% 
      {if(length(.) == 0) NA else .}
  product_links = page %>% html_nodes("._2UzuFa") %>% 
    html_attr("href") %>% paste("https://www.flipkart.com",., sep="")
  
  fabric= sapply(product_links, FUN = get_fabric, USE.NAMES = FALSE)
  fit= sapply(product_links, FUN = get_fit, USE.NAMES = FALSE)
  collar= sapply(product_links, FUN = get_collar, USE.NAMES = FALSE)
  pattern= sapply(product_links, FUN = get_pattern, USE.NAMES = FALSE)
  category= sapply(product_links, FUN = get_category, USE.NAMES = FALSE)
  price= sapply(product_links, FUN = get_price, USE.NAMES = FALSE)
  color= sapply(product_links, FUN = get_color, USE.NAMES = FALSE)
  sleeve= sapply(product_links, FUN = get_sleeve, USE.NAMES = FALSE)
 
   
  print(length(brand))
  print(length(productdescription))
  print(length(productimage))
  print(length(product_links))
  print(length(price))
  print(length(fabric))
  print(length(fit))
  print(length(category))
  print(length(pattern))
  
  webscraptshirts = rbind(webscraptshirts, data.frame(category, brand, productdescription, price, fabric, fit, color, collar, pattern, sleeve, productimage, stringsAsFactors = FALSE) )
  
 
  print(paste("Page:", page_result))
  
  
}




View (webscraptshirts)
write.csv(webscraptshirts,"webscraptshirts.csv")
