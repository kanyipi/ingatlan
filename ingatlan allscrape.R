# install.packages(c("rvest", "data.table", "jsonlite"))
# rm(list = ls())

library(rvest)
library(data.table)
library(tidyverse)

get_one_property <- function(url) {
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

# get_one_property gets the data points for one house namely:
# the address, the price, the size, the number of rooms,
# the description and the list data values e.g.: the year of construction, the condition and so on

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

# get_links_on_page gets every relative link on a ingatlan.com search page
# and coverst them to an absolute link

# test
# url <- 'https://ingatlan.com/lista/elado+lakas+xvi-ker?page=2'
# get_links_on_page(url)


get_read_property <- function(nameofcsvs, linktoscrape, noofpagestoget = 0,
                              batchsize = 50, startbatchnumber = 0) {
  # nameofcsvs is the name of the folder and the csvs
  # linktoscrape is the search link to scrape,
  # noofpagestoget is how much pages we should get, if 0 or not set it will get all
  # batchsize is how many pages goes into a csv
  # startbatchnumber is to set the first batch, good for saving default=0
  
  #create the dir  
  dir.create(nameofcsvs)

  #get the last page number  
  lastpagetext <- read_html(linktoscrape) %>%
    html_nodes(".pagination__page-number") %>%
    html_text() %>%
    trimws()

  lastpageno <- lastpagetext %>%
    substr(2, nchar(lastpagetext)) %>%
    parse_number()

  #check whats smaller the last page or the noofpages to get
  if (noofpagestoget != 0) {
    if (noofpagestoget < lastpageno) {
      lastpageno <- noofpagestoget
    }
  }

  lastpagebatch <- floor(lastpageno / batchsize) + 1

  
  # iterate through the batches
  for (i in startbatchnumber:lastpagebatch) {
    print(paste("start", i))

    # si= start page index in the batch
    # ei= end page index in the batch
    si <- ((i * batchsize) + 1)
    ei <- (((i + 1) * batchsize))

    #check to see if ei is bigger than lastpageno
    if (lastpageno < ei) {
      ei <- lastpageno
    }

    #get all search pages in batch
    links_to_get_links <- paste0(linktoscrape, "?page=", si:ei)
    #get all property links from the search pages
    link_list <- sapply(links_to_get_links, get_links_on_page)

    #initiate
    data_list <- list()
    k <- 0
    
    #loop through pages with foor because I couldnt try catch a lapply
    for (j in link_list) {
      #try catch as http 503 was very common
      tryCatch(
        {
          oneproperty <- get_one_property(j)
          #only k if get_one_property(j) did not error
          k <- k + 1
        },
        error = function(e) {
          #show error and errored link
          print(e)
          print(j)
        }
      )
      #if list has data point insert it to data_list
      if (length(oneproperty) > 1) {
        data_list[[k]] <- oneproperty
      }
    }
    #rbind lists to data frame
    df <- rbindlist(data_list, fill = T)
    #save data frame
    csvname <- paste0(nameofcsvs, "/", nameofcsvs, i, ".csv")
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




get_all_property <- function(nameofcsv) {
  csv_list <- list.files(nameofcsv)
  max_file_no <- 0

  for (i in csv_list) {
    fileno <- parse_number(i)
    if (fileno > max_file_no) {
      max_file_no <- fileno
    }
  }

  houses <- read.csv(paste0(nameofcsv, "/", nameofcsv, "0.csv"))

  df <- subset(houses, select = -Panelprogram)

  for (i in 1:max_file_no) {
    houses <- read.csv(paste0(nameofcsv, "/", nameofcsv, i, ".csv"))
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
  df <- df %>% mutate(price = elementlist(str_split(price, " "), 1))
  df <- df %>% mutate(price = as.double(str_replace(price, ",", ".")))
  conversiourl <- "https://www.xe.com/currencyconverter/convert/?Amount=1&From=EUR&To=HUF"
  t <- read_html(conversiourl)
  eur_to_huf <- t %>%
    html_nodes(".iGrAod") %>%
    html_text() %>%
    str_split(" ") %>%
    elementlist(1) %>%
    substr(1, 7) %>%
    as.double()
  df <- df %>% mutate(price = ifelse(price_in_cur == "EUR", price * eur_to_huf / 1000000, price))

  return(df)
}

# some example runs

# scrapename <- "hungary"
# get_read_property(scrapename,"https://ingatlan.com/lista/elado+lakas")
# all hungarian properties on ingatlan.com

# scrapename <- "fiftycheapbud"
# get_read_property(scrapename,"https://ingatlan.com/szukites/elado+lakas+budapest+ar-szerint",50,25)
# first 50 pages of the cheapest properties in budapest

# scrapename <- "miskolc"
# get_read_property(scrapename,"https://ingatlan.com/szukites/elado+lakas+miskolc")
# all Miskolc properties on ingatlan.com

# some good naming convention would be to name them as hungary20211210
# for the date of the scrape but that would mess up my read back
# so maybe a better solution would be to tag the properties when read in with Sys.time()

scrapename <- "budapestall"
get_read_property(scrapename, "https://ingatlan.com/lista/elado+lakas+budapest")
df <- get_all_property(scrapename) %>% get_clean_all()

# some upgrades which could be done:
# update the last page while running, as it may change
# update the reading back code so files work with numbers
# tag the data, with the link it cam from and the date




##TODO rmarkdown and graphs