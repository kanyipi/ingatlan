# install.packages(c("rvest", "data.table", "jsonlite"))
rm(list = ls())

library(rvest)
library(data.table)
library(tidyverse)

get_one_house <- function(url) {
  t_list <- list()

  t <- read_html(url)
  t_list[["address"]] <- t %>%
    html_nodes(".address") %>%
    html_text() %>%
    trimws()
  t_list[["price"]] <- t %>%
    html_nodes(".parameterTitleLink+ .parameterValues span") %>%
    html_text()
  t_list[["area"]] <- t %>%
    html_nodes(".parameter:nth-child(2) span") %>%
    html_text()
  t_list[["noroom"]] <- t %>%
    html_nodes(".parameter~ .parameter+ .parameter .parameterValues , .restricted") %>%
    html_text()
  t_list[["description"]] <- t %>%
    html_nodes(".longDescription") %>%
    html_text()

  keys <- t %>%
    html_nodes(".parameterName") %>%
    html_text() %>%
    trimws()
  values <- t %>%
    html_nodes(".parameterValue") %>%
    html_text() %>%
    trimws()
  if (length(keys) == length(values)) {
    for (i in 1:length(keys)) {
      t_list[[keys[i]]] <- values[i]
    }
  }
  return(t_list)
}

# test
# url <- 'https://ingatlan.com/xvi-ker/elado+lakas/tegla-epitesu-lakas/32395911'
# get_one_house(url)

get_links_on_page <- function(url) {
  t <- read_html(url)
  rel_links <- t %>%
    html_nodes(".listing__link") %>%
    html_attr("href")
  links <- paste0("https://ingatlan.com", rel_links)
  return(links)
}

# test
# url <- 'https://ingatlan.com/lista/elado+lakas+xvi-ker?page=2'
# get_links_on_page(url)

## TODO FUNCTION AND I FOR LINKS
get_read_property <- function() {
  for (i in 0:34) {
    print(paste("start", i))
    si <- ((i * 50) + 1)
    se <- (((i + 1) * 50) + 1)
    lastpageurl <- "https://ingatlan.com/lista/elado+lakas+budapest+ar-szerint"
    t <- read_html(lastpageurl)
    lastpage <- t %>%
      html_nodes(".pagination__page-number") %>%
      html_text() %>%
      trimws() %>%
      substr(5, 8) %>%
      strtoi()
    if (lastpage < se) {
      se <- lastpage
    }
    links_to_get_links <- paste0("https://ingatlan.com/lista/elado+lakas+budapest+ar-szerint?page=", si:se)
    link_list <- sapply(links_to_get_links, get_links_on_page)
    data_list <- list()
    k <- 0
    for (j in link_list) {
      tryCatch(
        {
          temphtml <- get_one_house(j)
          k <- k + 1
        },
        error = function(e) {
          print(e)
          print(j)
        }
      )
      if (length(temphtml) > 1) {
        data_list[[k]] <- temphtml
      }
      # print(length(data_list))
    }
    df <- rbindlist(data_list, fill = T)
    csvname <- paste0("budapest", i, ".csv")
    write.csv(df, csvname)
  }
}

# old read
# for (i in 1:34) {
#  links_to_get_links <- paste0("https://ingatlan.com/lista/elado+lakas+budapest+ar-szerint?page=", (i * 50):((i * 50) + 50))
#  link_list <- sapply(links_to_get_links, get_links_on_page)
#  data_list <- lapply(link_list, get_one_house)
#  df <- rbindlist(data_list, fill = T)
#  csvname <- paste0("budapest", i, ".csv")
#  write.csv(df, csvname)
# }

get_all_property <- function() {
  houses <- read.csv("budapest0.csv")
  df <- subset(houses, select = -Panelprogram)
  for (i in 1:34) {
    houses <- read.csv(paste0("budapest", i, ".csv"))
    if (ncol(houses) == 27) {
      houses <- subset(houses, select = -Panelprogram)
    }
    print(i)
    df <- rbind(df, houses)
  }
  return(df)
}

elementlist <- function(lst, n) {
  sapply(lst, `[`, n)
}

get_clean_all <- function(df) {
  df <- df %>% subset(select = -X)
  df <- df %>%
    group_by(description, area) %>%
    slice(1)
  df <- df %>% mutate(price_in_cur = elementlist(str_split(price, " "), 3))
  df <- df %>% mutate(price_in_cur = ifelse(price_in_cur != "Ft", "EUR",
    ifelse(price_in_cur == "Ft", "HUF", "other")
  ))
  options(digits=3)
  df <- df %>% mutate(price = as.double(elementlist(str_split(price, " "), 1)))
  conversiourl <- "https://www.xe.com/currencyconverter/convert/?Amount=1&From=EUR&To=HUF"
  t <- read_html(conversiourl)
  eur_to_huf <- t %>%
    html_nodes(".iGrAod") %>%
    html_text() %>%
    str_split(" ") %>%
    elementlist(1) %>%
    substr(1, 3) %>%
    strtoi()
  df <- df %>% mutate(price = ifelse(price_in_cur=="EUR",price*eur_to_huf/1000000,price))
  
  return(df)
}
summary(df)

setwd("~/GitHub/ECBS-5306-Coding-2-Web-Scraping-with-R/ingatlancom")
# get_read_property()
df <- get_all_property()
get_clean_all(df)
