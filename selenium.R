library(playwrightr)
# library(tidyverse)

source("utils.R")

options(python_init = TRUE)


# install.packages("pacman")
pacman::p_load(
  reticulate,
  vroom,
  progress,
  janitor,
  tidyr,
  scales,
  tibble,
  dplyr,
  stringr,
  lubridate,
  purrr,
  glue,
  rvest,
  cli,
  digest,
  readr
)


conda_install(packages = "xvfbwrapper", pip = T)


print("installed xvfbwrapper")
system("pip install --only-binary :all: greenlet")
system("pip install greenlet --no-cache-dir")
conda_install(packages = "greenlet", pip = T)

conda_install(packages = "playwright", pip = T)

print("installed playwright")


# conda_install(packages = "fcntl", pip = T)
pw_init(use_xvfb = T) 
print("pw initted")
# Launch the browser
system("playwright install")


dir.create("data/ggl", recursive = T)


download.file(url = "https://storage.googleapis.com/political-csv/google-political-ads-transparency-bundle.zip", destfile = "data/ggl/ggl.zip", method = "curl")

unzip("data/ggl/ggl.zip", exdir = "data/ggl")

# google_political_ads_advertiser_stats %>%
#   filter(str_detect(Regions, "DE")) %>% View()

# wdman::gecko()

Sys.sleep(10)

gglstats <- vroom::vroom("data/ggl/google-political-ads-creative-stats.csv")

ggl_spend <- gglstats   %>%
  mutate(Date_Range_Start = lubridate::ymd(Date_Range_Start)) %>%
  filter(Date_Range_Start >= as.Date("2023-08-01")) %>%
  filter(str_detect(Regions, "DE")) %>%
  group_by(Advertiser_ID, Advertiser_Name) %>%
  summarize(Spend_Range_Min_EUR = sum(Spend_Range_Min_EUR)) %>%
  ungroup() %>%
  left_join(readRDS("data/ggladvers.rds"))  %>% 
  drop_na(party) %>%
  bind_rows(readRDS("data/ggladvers.rds")) %>% 
  distinct(Advertiser_ID, .keep_all = T) %>% 
  rename(party1 = party)


# openxlsx::read.xlsx("data/ggl.xlsx") %>%
#   filter(party != "xxx") %>%
#   saveRDS("data/ggladvers.rds")

# ggl_spend

# ggl_spend %>%
#   # janitor::clean_names() %>%
#   filter(str_detect(Advertiser_Name, "FDP|CSU|LINKE|Linke|Grün|GRÜN|SPD|AfD|Alternative für|Freie Wähler|FW")) %>%
#   bind_rows(ggl_spend ) %>%
#   distinct() %>%
#   arrange(desc(Spend_Range_Min_EUR)) %>%
# openxlsx::write.xlsx("data/ggl.xlsx")

saveRDS(ggl_spend, "data/ggl_spend.rds")

# ggl_spend <- readRDS("data/ggl_spend.rds")

print("Launch the browser")

browser_df <- browser_launch(
  headless = F,
  browser = "firefox",
  user_agent = NULL,
  user_data_dir = "out"
)

print("headlesss")
# Create a new page

# page_df <- new_page(browser_df)
page_df <- browser_df %>%
  glimpse


retrieve_spend_daily <- function(id, the_date, cntry = "DE") {
  
  # id <- "AR18091944865565769729"
  url <- glue::glue("https://adstransparency.google.com/advertiser/{id}?political&region={cntry}&start-date={the_date}&end-date={the_date}&topic=political&hl=en")
  
  page_df %>%
    goto(url)
  
  Sys.sleep(3)
  
  
  
  root3 <- "/html/body/div[3]"
  root5 <- "/html/body/div[5]"
  ending <- "/root/advertiser-page/political-tabs/div/material-tab-strip/div/tab-button[2]/material-ripple"
  
  page_df <- page_df %>% get_by_label("Insights")
  
  page_df %>% press("Enter") %>% click()
  
  root <- root3
  
  Sys.sleep(3)
  
  pp <- page_df %>% playwrightr::get_content()
  
  ending_eur <- "/root/advertiser-page/insights-grid/div/div/overview/widget/div[3]/div[1]/div"
  ending_ads <- "/root/advertiser-page/insights-grid/div/div/overview/widget/div[3]/div[3]/div"
  
  print("retrieve numbers")
  # try({
  eur_amount <- pp %>%
    html_elements(xpath = paste0(root, ending_eur)) %>%
    html_text()
  
  if(length(eur_amount)==0) root <- root5
  
  eur_amount <- pp %>%
    html_elements(xpath = paste0(root, ending_eur)) %>%
    html_text()
  
  
  num_ads <- pp %>%
    html_elements(xpath = paste0(root, ending_ads)) %>%
    html_text()
  
  # })
  
  fin <- tibble(advertiser_id = id, eur_amount, num_ads, date = the_date)
  
  print(fin)
  
  return(fin)
  
}

# debugonce(retrieve_spend_daily)
# retrieve_spend_daily <- possibly(retrieve_spend_daily, otherwise = NULL, quiet = F)

# daily_spending <- readRDS("data/daily_spending.rds")
# Apr 17, 2023 - May 16, 2023
# 13 February 2023
timelines <- seq.Date(as.Date("2023-08-01"), lubridate::today()-lubridate::days(1), by = "day")

try({
  daily_spending_old <- readRDS("data/ggl_daily_spending.rds")
})


if(exists("daily_spending_old")){
  daily_spending <- expand_grid(unique(ggl_spend$Advertiser_ID), timelines) %>%
    set_names(c("advertiser_id", "timelines")) %>%
    anti_join(daily_spending_old %>% select(advertiser_id, timelines = date)) %>%
    # slice(1) %>%
    split(1:nrow(.)) %>%
    map_dfr_progress(~{retrieve_spend_daily(.x$advertiser_id, .x$timelines)})

  saveRDS(daily_spending %>% bind_rows(daily_spending_old) %>% distinct(), file = "data/ggl_daily_spending.rds")

} else {
  daily_spending <- expand_grid(unique(ggl_spend$Advertiser_ID), timelines) %>%
    set_names(c("advertiser_id", "timelines")) %>%
    # anti_join(daily_spending_old %>% select(advertiser_id, timelines = date)) %>%
    # slice(1) %>%
    split(1:nrow(.)) %>%
    map_dfr_progress(~{retrieve_spend_daily(.x$advertiser_id, .x$timelines)})

  saveRDS(daily_spending, file = "data/ggl_daily_spending.rds")

}


# missings <- expand_grid(unique(ggl_spend$Advertiser_ID), timelines) %>%
#   set_names(c("advertiser_id", "timelines")) %>%
#   anti_join(daily_spending %>% rename(timelines = date))  %>%
#   split(1:nrow(.)) %>%
#   map_dfr_progress(~{retrieve_spend_daily(.x$advertiser_id, .x$timelines)})




dates <- read_csv("data/dates.csv")

retrieve_spend_custom <- function(id, from, to, cntry = "DE") {

  # id <- "AR18091944865565769729"
  url <- glue::glue("https://adstransparency.google.com/advertiser/{id}?political&region={cntry}&start-date={from}&end-date={to}&hl=en")
  # remDr$navigate(url)


  page_df %>%
    goto(url)

  Sys.sleep(1)



  root3 <- "/html/body/div[3]"
  root5 <- "/html/body/div[5]"
  ending <- "/root/advertiser-page/political-tabs/div/material-tab-strip/div/tab-button[2]/material-ripple"

  page_df <- page_df %>% get_by_label("Insights")

  page_df %>% press("Enter") %>% click()

  root <- root3

  Sys.sleep(3)

  pp <- page_df %>% playwrightr::get_content()

  ending_eur <- "/root/advertiser-page/insights-grid/div/div/overview/widget/div[3]/div[1]/div"
  ending_ads <- "/root/advertiser-page/insights-grid/div/div/overview/widget/div[3]/div[3]/div"

  print("retrieve numbers")
  # try({
  eur_amount <- pp %>%
    html_elements(xpath = paste0(root, ending_eur)) %>%
    html_text()

  if(length(eur_amount)==0) root <- root5

  num_ads <- pp %>%
    html_elements(xpath = paste0(root, ending_ads)) %>%
    html_text()

  # })


  ending_type <- "/root/advertiser-page/insights-grid/div/div/ad-formats/widget/div[4]"


  type_spend <<- pp %>%
    html_elements(xpath = paste0(root, ending_type)) %>%
    html_children() %>%
    html_text() %>%
    tibble(raww = .) %>%
    mutate(type = str_to_lower(str_extract(raww, "Video|Text|Image"))) %>%
    mutate(raww = str_remove_all(raww, "Video|Text|Image") %>% str_remove_all("%|\\(.*\\)") %>% as.numeric) %>%
    pivot_wider(names_from = type, values_from = raww)


  fin <- tibble(advertiser_id = id, eur_amount, num_ads, from, to)

  if(nrow(type_spend)!=0){
    fin <- fin %>%
      bind_cols(type_spend)
  }



  print(fin)

  return(fin %>% mutate_all(as.character))

}

retrieve_spend_custom <- possibly(retrieve_spend_custom, otherwise = NULL, quiet = F)

# ggl_sel_sp_old <- readRDS("data/ggl_sel_sp.rds")

# if(!any(dates$begin30 == ggl_sel_sp_old$from)){
# debugonce(retrieve_spend_custom)
  ggl_sel_sp <- unique(ggl_spend$Advertiser_ID) %>%
    # .[10] %>%
    map_dfr_progress(~{retrieve_spend_custom(.x, dates$begin30, dates$fin)})



  misssss <- ggl_sel_sp$advertiser_id %>% setdiff(unique(ggl_spend$Advertiser_ID), .)
  # filter(!(advertiser_id %in% unique(ggl_spend$Advertiser_ID)))

  # ggl_sel_sp <- ggl_sel_sp %>%
  # bind_rows(fvd) %>%
  # distinct(advertiser_id, .keep_all = T)

  # fvd <- retrieve_spend("AR03397262231409262593")
  fvd <- misssss %>%
    # .[22] %>%
    map_dfr_progress(~{retrieve_spend_custom(.x, dates$begin30, dates$fin)})

  ggl_sel_sp <- ggl_sel_sp %>%
    bind_rows(fvd) %>%
    distinct(advertiser_id, .keep_all = T)


  saveRDS(ggl_sel_sp, file = "data/ggl_sel_sp.rds")


  ggl_sel_sp7 <- unique(ggl_spend$Advertiser_ID) %>%
    # .[22] %>%
    map_dfr_progress(~{retrieve_spend_custom(.x, dates$begin7, dates$fin)})

  misssss7 <- ggl_sel_sp7$advertiser_id %>% setdiff(unique(ggl_spend$Advertiser_ID), .)

  # misss <- retrieve_spend_custom("AR14725485108811268097", dates$begin7, dates$fin)

  misss <- misssss7 %>%
    # .[22] %>%
    map_dfr_progress(~{retrieve_spend_custom(.x, dates$begin7, dates$fin)})


  saveRDS(ggl_sel_sp7 %>% bind_rows(misss)%>%
            distinct(advertiser_id, .keep_all = T), file = "data/ggl_sel_sp7.rds")


# }

unlink("data/ggl", recursive = T, force = T)
# file.remove("data/ggl/google-political-ads-creative-stats.csv")

try({
  
  
  
  # ggl_sel_sp <- readRDS("data/ggl_sel_sp.rds")
  
  ggl_sel_sp <- ggl_sel_sp %>%
    filter(num_ads != "0")
  
  # ggl_sel_sp <- readRDS("data/ggl_sel_sp.rds")
  
  c("Bayern", "Hessen") %>% 
    walk(~{
      
      
      tt_ads <- ggl_sel_sp %>%
        rename(Advertiser_ID = advertiser_id) %>%
        left_join(ggl_spend %>% distinct(Advertiser_ID, party1, state))  %>%
        filter(state == .x) %>% 
        # mutate(Date_Range_Start = lubridate::ymd(Date_Range_Start)) %>%
        # filter(Date_Range_Start >= as.Date("2023-02-05")) %>%
        group_by(party1) %>%
        summarize(total_num_ads = sum(as.numeric(num_ads))) %>%
        # count(party1, name = "total_num_ads") %>%
        mutate(total_num_ads = scales::comma(total_num_ads)) %>%
        pivot_wider(names_from = party1, values_from = total_num_ads) %>%
        mutate(party_col = "Number of Ads")
      
      
      ttl_spn <- ggl_sel_sp %>%
        rename(Advertiser_ID = advertiser_id) %>%
        left_join(ggl_spend %>% distinct(Advertiser_ID, party1, state)) %>%
        filter(state == .x) %>% 
        mutate(Spend_EUR = readr::parse_number(str_remove(eur_amount, "\\."))) %>%
        group_by(party1) %>%
        summarize(Spend_EUR = sum(Spend_EUR)) %>%
        arrange(desc(Spend_EUR)) %>%
        select(party = party1, spend = Spend_EUR) %>%
        mutate(spend = scales::comma(spend)) %>%
        mutate(spend = paste0("€", spend)) %>%
        drop_na() %>%
        pivot_wider(names_from = party, values_from = spend) %>%
        mutate(party_col = "Total Spend")
      
      
      
      tp_spnders <- ggl_sel_sp %>%
        rename(Advertiser_ID = advertiser_id) %>%
        left_join(ggl_spend %>% distinct(Advertiser_ID, party1, state, .keep_all = T)  %>%
                    filter(state == .x) %>%  select(Advertiser_ID, party1, Advertiser_Name)) %>% 
        mutate(Spend_EUR = readr::parse_number(str_remove(eur_amount, "\\.")))   %>%
        group_by(Advertiser_Name, party1) %>%
        summarize(Spend_EUR = sum(Spend_EUR)) %>%
        ungroup() %>%
        group_by(party1) %>%
        arrange(desc(Spend_EUR)) %>%
        slice(1:3) %>%
        mutate(Spend_EUR = scales::comma(Spend_EUR)) %>%
        mutate(n_words = str_count(Advertiser_Name, " ")) %>%
        # mutate(lab = paste0(word(str_remove(page_name, "-"), 1,ifelse(n_words>=2, 3, 2), sep=" "), "<br>(€", total_spend_formatted, ")")) %>%
        mutate(lab = paste0(Advertiser_Name, " (€", Spend_EUR, ")")) %>%
        select(party1, lab) %>%
        drop_na() %>%
        summarize(lab = paste0("<br>", 1:n(), ". ", lab, collapse = "")) %>%
        pivot_wider(names_from = party1, values_from = lab) %>%
        mutate(party_col = "Top Spenders")
      
      ggl_all <<- tt_ads %>%
        bind_rows(tp_spnders) %>%
        bind_rows(ttl_spn) %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column("Coalizione/Partito") %>%
        set_names(.[nrow(.),] %>% as.character()) %>%
        slice(1:(n()-1))
      
      
      saveRDS(ggl_all, file = paste0("data/ggl_all_", .x, ".rds"))
      
      
      
    })
  
  
  
  
  
  
  ggl_sel_sp7 <- ggl_sel_sp7 %>%
    filter(num_ads != "0")
  
  
  c("Bayern", "Hessen") %>% 
    walk(~{
      
      
      
      tt_ads <- ggl_sel_sp7 %>%
        rename(Advertiser_ID = advertiser_id) %>%
        left_join(ggl_spend %>% distinct(Advertiser_ID, party1, state))  %>%
        filter(state == .x) %>% 
        # mutate(Date_Range_Start = lubridate::ymd(Date_Range_Start)) %>%
        # filter(Date_Range_Start >= as.Date("2023-02-05")) %>%
        group_by(party1) %>%
        summarize(total_num_ads = sum(as.numeric(num_ads))) %>%
        # count(party1, name = "total_num_ads") %>%
        mutate(total_num_ads = scales::comma(total_num_ads)) %>%
        pivot_wider(names_from = party1, values_from = total_num_ads) %>%
        mutate(party_col = "Number of Ads")
      
      
      ttl_spn <- ggl_sel_sp7 %>%
        rename(Advertiser_ID = advertiser_id) %>%
        left_join(ggl_spend %>% distinct(Advertiser_ID, party1, state)) %>%
        filter(state == .x) %>% 
        mutate(Spend_EUR = readr::parse_number(str_remove(eur_amount, "\\."))) %>%
        group_by(party1) %>%
        summarize(Spend_EUR = sum(Spend_EUR)) %>%
        arrange(desc(Spend_EUR)) %>%
        select(party = party1, spend = Spend_EUR) %>%
        mutate(spend = scales::comma(spend)) %>%
        mutate(spend = paste0("€", spend)) %>%
        drop_na() %>%
        pivot_wider(names_from = party, values_from = spend) %>%
        mutate(party_col = "Total Spend")
      
      
      
      tp_spnders <- ggl_sel_sp7 %>%
        rename(Advertiser_ID = advertiser_id) %>%
        left_join(ggl_spend %>% distinct(Advertiser_ID, party1, state, .keep_all = T) %>%  filter(state == .x) %>% select(Advertiser_ID, party1, Advertiser_Name)) %>%
        mutate(Spend_EUR = readr::parse_number(str_remove(eur_amount, "\\.")))   %>%
        group_by(Advertiser_Name, party1) %>%
        summarize(Spend_EUR = sum(Spend_EUR)) %>%
        ungroup() %>%
        group_by(party1) %>%
        arrange(desc(Spend_EUR)) %>%
        slice(1:3) %>%
        mutate(Spend_EUR = scales::comma(Spend_EUR)) %>%
        mutate(n_words = str_count(Advertiser_Name, " ")) %>%
        # mutate(lab = paste0(word(str_remove(page_name, "-"), 1,ifelse(n_words>=2, 3, 2), sep=" "), "<br>(€", total_spend_formatted, ")")) %>%
        mutate(lab = paste0(Advertiser_Name, " (€", Spend_EUR, ")")) %>%
        select(party1, lab) %>%
        drop_na() %>%
        summarize(lab = paste0("<br>", 1:n(), ". ", lab, collapse = "")) %>%
        pivot_wider(names_from = party1, values_from = lab) %>%
        mutate(party_col = "Top Spenders")
      
      ggl_all7 <<- tt_ads %>%
        bind_rows(tp_spnders) %>%
        bind_rows(ttl_spn) %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column("Coalizione/Partito") %>%
        set_names(.[nrow(.),] %>% as.character()) %>%
        slice(1:(n()-1))
      
      
      saveRDS(ggl_all7, file = paste0("data/ggl_all7_",  .x,".rds"))
      
      
    })
  
  
})




unlink("node_modules", recursive = T, force = T)
unlink("out", recursive = T, force = T)
