library(rvest)
library(stringr)
library(purrr)
library(tidyverse)
library(rlist)


#"https://www.rightmove.co.uk/property-to-rent/find.html?locationIdentifier=STATION%5E5792&minBedrooms=3&maxPrice=2250&minPrice=1000&radius=1.0&index=24&propertyTypes=&includeLetAgreed=false&mustHave=&dontShow=&furnishTypes=&keywords="
#"https://www.zoopla.co.uk/to-rent/property/camden-town/?beds_min=3&page_size=25&price_frequency=per_month&price_max=2750&q=Camden%20Town%2C%20London&radius=0&results_sort=newest_listings&search_source=refine"

get_results <- function(page, class) {
  page %>% html_node(class) %>% 
  html_text() %>% str_extract("\\d+") %>%
  as.numeric()
}

get_rightmove_results <- function(prop_per, url, results) {
  unlist(lapply(c(0:floor(results/prop_per)-1), function(x) {
    split_url <- unlist(str_split(url, "propertyTypes"))
    paste(split_url[1], "index=", x * prop_per, "&propertyTypes", split_url[2], sep = "")
  }))
}
get_zoopla_results <- function(prop_per, url, results) {
  n_pages <- floor(results/prop_per) + 1
  all_result_pages <- lapply(c(1:n_pages), function(x) {
    paste0(url, "&pn=", x)
  }) %>% unlist() %>% .[. != "https://www.zoopla.co.uk"]
}

get_props_from_results <- function(all_pages, website, class) {
  map(all_pages, function(x) {
    read_html(x) %>% html_nodes(class) %>% html_attr('href')
  }) %>% unlist() %>% unique() %>% .[. != ""] %>%
    paste(website, ., sep = "")
}

get_info <- function(url) {
  
  criteria <- c("Property", "Price", "Type", "Bedrooms", "Bathrooms", "Station", "URl")
  prop <- read_html(url)
  
  if (str_detect(url, "rightmove")) {
    price <- prop %>% html_node('._1gfnqJ3Vtd1z40MlC0MzXu') %>% html_text() %>% 
      str_replace(., ",", "") %>% str_extract(., "\\d+") %>% as.numeric()
    
    nearest_train <- prop %>% html_node(".mlEuHXZpfrrzJtwlRmwBe .cGDiWU3FlTjqSs-F1LwK4") %>% html_text()
    
    basics <- prop %>% html_nodes('._1u12RxIYGx3c84eaGxI6_b') %>% map(., function(x) {
      x %>% html_node('._3mqo4prndvEDFoh4cDJw_n') %>% html_text()
    })
    type <- basics[1]
    bedrooms <- basics[2] %>% str_extract("\\d") %>% as.numeric()
    bathrooms <- ifelse(length(basics) == 3, basics[3], "NULL")
    
    name <- prop %>% html_node('._2uQQ3SV0eMHL1P6t5ZDo2q') %>% html_text()
  }
  else {
    name <- prop %>% html_node('.evrk8bx4') %>% html_text()
    price <- prop %>% html_node(".evrk8bx10") %>% html_text() %>% str_replace(., ",", "") %>% 
      str_extract(., "\\d+") %>% as.numeric()

    nearest_train <- "NA"
    ammenities <- prop %>% html_nodes('.e1p8m6mm2')
    for (a in ammenities) {
      if (a %>% html_node('svg') %>% html_attr('data-testid') %>% str_detect("station")) {
        nearest_train <- a %>% html_node('.e1p8m6mm0') %>% html_text()
      }
    }
    
    bathrooms <- "NA"
    bedrooms <- "NA"
    
    for (item in prop %>% html_nodes('.evrk8bx2')) {
      if (str_detect(html_text(item), "bath")) {
        bathrooms <- html_text(item) %>% str_extract("\\d") %>% as.numeric()
      }
      if (str_detect(html_text(item), "bed")) {
        bedrooms <- html_text(item) %>% str_extract("\\d") %>% as.numeric()
      }
    }
    
    type <- prop %>% html_node('.evrk8bx5') %>% html_text() %>% str_extract(., pattern="flat|house|apartment|property|duplex|maisonette|detached")
    
  }
  
  r <- data.frame(name, price, type, bedrooms, bathrooms, nearest_train, url)
  names(r) <- criteria
  return(r)
  
}

get_urls <- function(area, bedrooms, pp_pw) {
  
  print("the hunt begins!")
  
  urls <- c()
  
  for (b in bedrooms[1]:bedrooms[2]) {
    max <- pp_pw[2] * 4 * b
    min <- pp_pw[1] * 4 * b
    if (area != "") {
      z_url <- paste0("https://www.zoopla.co.uk/to-rent/property/station/tube/", tolower(str_replace_all(area, " ", "-")), "/?beds_min=", b, "&page_size=25&price_frequency=per_month&price_max=", max, "&price_min=", min, "&q=", str_replace_all(area, " ", "%20"), "%2C%20London&radius=1&results_sort=newest_listings&search_source=refine")
      area_code <- read_html(paste0("https://www.bing.com/search?q=rightmove+", str_replace_all(area, " ", "+"), "+station+rent")) %>% html_nodes('#b_results h2 a') %>% 
        html_attr('href') %>% str_extract(., "%\\w+") %>% sub('.', '', .)
      area_code <- area_code[which(!is.na(area_code))]
      rm_url <- paste0("https://www.rightmove.co.uk/property-to-rent/find.html?locationIdentifier=STATION%", area_code, "&maxBedrooms=", b, "&minBedrooms=", b, "&maxPrice=", max, "&minPrice=", min, "&radius=1.0&propertyTypes=&includeLetAgreed=false&dontShow=houseShare%2Cretirement%2Cstudent&furnishTypes=&keywords=")
    }
    else {
      z_url <- paste0("https://www.zoopla.co.uk/to-rent/property/camden-town/?beds_min=", b,"&include_shared_accommodation=false&page_size=25&polyenc=oofyHl_b@x}@e|Fml@muDwYooGtQi`Fa[ajAggC{cBwmAdn@mhA`sC_SxtAwyAxnCk@`bD`[~eBkX`dC~FfqE`wAbjAvq@tIlxAr}Bb{@rVxiAucAlh@qpA&price_frequency=per_month&price_max=", max, "&price_min=", min, "&q=Camden%20Town%2C%20London&radius=0&rental_term=short&results_sort=newest_listings&search_source=facets")
      rm_url <- paste0("https://www.rightmove.co.uk/property-to-rent/find.html?locationIdentifier=USERDEFINEDAREA%5E%7B%22polylines%22%3A%22ynlyHnhh%40cgAe_%40u%5DijBm%7CBq_Bac%40%7Dx%40iDw%7DC%60%7B%40_fB~JeuC%3FcjAk%40w_Cn%7CBcqD%60gAuaBdcBmlBr%60CihCt%7C%40b%7BBspBpcLh%40hqElh%40boEk%60%40~%7BEkvBznEoaAjh%40%22%7D&minBedrooms=", b, "&maxPrice=", max, "&minPrice=", min, "&sortType=2&propertyTypes=&mustHave=garden&dontShow=houseShare%2Cstudent%2Cretirement&furnishTypes=&keywords=")
    }
    urls <- c(urls, z_url, rm_url)
  }
  urls
}

get_all_props <- function(urls) {
  
  all_props <- c()
  
  for (url in urls) {
    pg <- read_html(url)
    if (str_detect(url, "rightmove")) {
      all_pages <- get_rightmove_results(24, url, get_results(pg, '.searchHeader-title'))
      your_props<- get_props_from_results(all_pages, "https://www.rightmove.co.uk", '.propertyCard-details a')
      
      all_props <- c(all_props, your_props)
    }
    if (str_detect(url, "zoopla")) {
      all_pages <- get_zoopla_results(25, url, get_results(pg, '.egjkayq7'))
      your_props <- get_props_from_results(all_pages, "https://www.zoopla.co.uk", '.e2uk8e4')
     
      all_props <- unique(c(all_props, your_props))
    }
  }
  return(unique(all_props[!all_props %in% c("https://www.rightmove.co.uk", "https://www.zoopla.co.uk")]))
}

get_all_info <- function(all_props) {
  
  criteria <- c("Property", "Price", "Type", "Bedrooms", "Bathrooms", "Station", "URl")
  df <- data.frame(1,2,3,4,5,6, 7)
  names(df) <- criteria
  
  total <- length(all_props)
  i <- 1
  
  for (prop in all_props) {
    print(prop)
    df <- rbind(df, get_info(prop))
    Sys.sleep(sample(1:10)/10)
    print(paste0(i, " out of ", total, " scraped!"))
    print("- - - - - - - - - - - - - - - - - - - - - - -")
    i <- i + 1
  }
  return(df[-1, ])
}

get_line_selector <- function(line) {
  raw <- read_html(paste0("https://tfl.gov.uk/tube/route/", line, "/")) %>% html_nodes(".stop-link") %>% 
    html_text() %>% substr(., 2, str_locate(., "Underground")-2)
}

get_image <- function(link, ctr) {
  #href <- tbl$URl[i]
  if (str_detect(link, "zoopla")) {
    all_images <- read_html(link) %>% html_nodes('.eb6a2uw1 picture img') %>% html_attr('src')
  }
  else {
    read_html(link) %>% html_nodes('._2TqQt-Hr9MN0c0wH7p7Z5p ._30hgiLFzNTpFG4iV-9f6oK ._7dxmhc6j4MexHyNVWBvhZ meta') %>% html_attr('content')
  }
}

