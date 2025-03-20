library(rvest)
library(tidyverse)
library(readxl)

if(!dir.exists("data/")) {dir.create("data/")}

# parse the webpage with files
url <- "https://md.gov.cz/Statistiky/Silnicni-doprava/Centralni-registr-vozidel/Mesicni-statistiky-2022,"
session <- session(url)

links <- session %>% 
    read_html() %>% 
    html_element("article.article") %>% 
    html_element("table") %>% 
    html_elements("a") %>% 
    html_attr("href")

links_regs <- links[grep("REG[0-9]", links)]

file_to_down <- tibble(
    url = paste0("https://md.gov.cz/", links_regs),
    filename = paste0(
        "data/",
        str_extract(links_regs, "REG[0-9][^\\.]+\\.[^\\.]+")
        )
    )

for(i in 1:nrow(file_to_down)) {
    
    download.file(file_to_down$url[i], file_to_down$filename[i])
    
    if(grepl("zip", file_to_down$filename[i])) {
        unzip(file_to_down$filename[i], exdir = "data/")
    }
    
    Sys.sleep(1)
}

files <- dir("data", full.names = TRUE, pattern = "xlsx$")

data <- tibble()


colnames <- c("pcv", "kategorie_vozidla", "vin", "cislo_tp", "stav",
              "registrace_cr", "registrace_svet", "hmotnost", "provozovatel",
              "leasing", "ico_provozovatel", "ico_vlastnik", "cislo_ztp",
              "okres", "misto", "barva_hlavni", "barva_doplnkova",
              "spis_prestavby", "znacka", "oznaceni", "verze")

for (file in files) {
    
    filename <- str_extract(file, "REG[^\\.]+")
    
    print(
        paste("Loading file", filename)
    )
    
    data_part <- tryCatch(
        error = function(cnd) {
            read_xlsx(
                file,
                col_names = c(colnames, "x"),
                col_types = "text")
        },
        
        read_xlsx(
            file,
            col_names = colnames,
            col_types = "text",
        )
    )
    
    data_part$month <- filename
    
    data <- bind_rows(data, data_part)
    
}

data_clean <- data %>% 
    mutate(
        znacka = if_else(is.na(znacka), oznaceni, znacka)
    ) %>% 
    mutate(
        across(c("pcv", "hmotnost"), as.integer),
        across(c("stav", "provozovatel", "leasing", "okres", "misto",
                 "barva_hlavni", "barva_doplnkova", "znacka", "oznaceni"),
               factor),
        across(c("registrace_cr", "registrace_svet"), ymd),
        month = ym(str_remove(str_extract(month, "REG[0-9]{4}"), "REG")),
        kategorie_hlavni = str_extract(kategorie_vozidla, "^.")
    )


cars <- data_clean %>% 
    filter(kategorie_hlavni == "M") %>% 
    select(-x, vin, cislo_tp, ico_provozovatel, ico_vlastnik)

write_csv(cars, "data/cars.gz")
