library(shiny)
library(shinydashboard)
library(data.table)
library(DT)
library(tidyverse)
library(magrittr)
library(readxl)
library(data.table)
library(lubridate)
setDTthreads(3) #nproc is 4
set.seed(707)
library(naniar)


## data ####
jpd = names(which(RCurl::url.exists(paste0("https://api.covid19india.org/raw_data",1:10,".json"))==T)) %>% map(jsonlite::fromJSON) %>% map("raw_data") %>% rbindlist(.,use.names = T,fill=TRUE)
rpd = jsonlite::fromJSON("https://api.covid19india.org/data.json")

ipd <- jpd %>%
  mutate_all(na_if,"") %>%
  dplyr::select(Gender=gender,Age=agebracket) %>%
  dplyr::filter_all(any_vars(!is.na(.))) %>%
  mutate(Gender = fct_recode(str_to_upper(str_sub(Gender,1,1)),Female="F", Male="M"), Gender = ifelse(Gender=="Female"|Gender=="Male",as.character(Gender),NA)) %>%
  mutate(Age = as.numeric(ifelse(grepl("-",Age),mean(as.numeric(str_split(Age,pattern = "-",simplify = T))),Age))) %>%
  mutate_if(is.character,as.factor)

nstatus = type_convert(rpd$statewise) %>% dplyr::filter(state=="Total") %>% dplyr::select(-contains("delta")) %>% as.data.table()
nunassign = type_convert(rpd$statewise) %>% dplyr::filter(grepl("Unassigned|Total",state)) %>% pull(active)

owd.tests = fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-latest-data-source-details.csv", select=c("Date","Entity","Cumulative total", "Cumulative total per thousand")) %>% separate("Entity", c("Country", "rem"), sep=" - ") %>% dplyr::select(Country, Tests.total = `Cumulative total`, Tests.perp = `Cumulative total per thousand`) %>% group_by(Country) %>% dplyr::filter(Tests.total==max(Tests.total)) %>% ungroup %>% mutate(Country=ifelse(Country=="India","I N D I A",Country))

tested = type_convert(rpd$tested) %>% separate("updatetimestamp", c("date","time"),sep=" ", convert = T) %>% mutate(date = lubridate::dmy(date), time = lubridate::hour(lubridate::hms(time))) %>% group_by(date) %>% dplyr::filter(time==max(time)) %>% dplyr::select(-source, -time) %>% ungroup() %>% dplyr::select(totalpositivecases,totalsamplestested,totalindividualstested,date) %>% dplyr::filter_at(vars(totalpositivecases,totalsamplestested,totalindividualstested),any_vars(!is.na(.)))

stst = type_convert(jsonlite::fromJSON("https://api.covid19india.org/data.json")$statewise) %>% dplyr::filter(state!="Daman and Diu") %>% dplyr::select(state,statecode,recovered,confirmed,deaths,active) %>% mutate(recper=recovered/confirmed, decper=deaths/confirmed, survival=ifelse(deaths>0,recovered/deaths,NA)) %>% mutate(state=ifelse(grepl("Dadra",state),"DNHDD",state)) %>% as.data.table()

dailies = type_convert(rpd$cases_time_series) %>% mutate(date = as.Date(date, "%d %B"))
tested %<>% left_join(dailies)

pop = fread("state_population.csv") %>% mutate(state=ifelse(grepl("Dadra",state),"DNHDD",state)) %>% mutate(sized = case_when(population<1e7~"Small",population>5e7~"Large",between(population,1e7,5e7)~"Medium"))

tst = jsonlite::fromJSON("https://api.covid19india.org/state_test_data.json")[[1]] %>% dplyr::select(totaltested,state) %>% group_by(state) %>% summarize(tests = max(as.integer(totaltested),na.rm=T)) %>% arrange(desc(tests)) %>% mutate(state=ifelse(grepl("Dadra",state),"DNHDD",state)) %>% full_join(pop) %>% as.data.table()

tstd = type_convert(jsonlite::fromJSON("https://api.covid19india.org/state_test_data.json")[[1]]) %>% mutate(date = lubridate::dmy(updatedon)) %>% mutate(state=ifelse(grepl("Dadra",state),"DNHDD",state)) %>% full_join(pop) %>% as.data.table()

ststd = type_convert(jsonlite::fromJSON("https://api.covid19india.org/states_daily.json")$states_daily) %>% gather("statecode","num",-c(status,date)) %>% mutate(statecode = str_to_upper(statecode), status=str_to_lower(str_replace_all(status,"Deceased","Deaths")), date = lubridate::dmy(date)) %>% spread("status","num") %>%  mutate(recper=recovered/confirmed, decper=deaths/confirmed, survival=ifelse(deaths>0,recovered/deaths,NA)) %>% inner_join(stst[,c("state","statecode")]) %>% dplyr::filter(!grepl("Total|State",state)) %>% as.data.table()

statewise = type_convert(rpd$statewise) %>% dplyr::select(-contains("delta"),-statenotes)

dstt = type_convert(jsonlite::fromJSON("https://api.covid19india.org/v2/state_district_wise.json")) %>% unnest(districtData) %>% dplyr::select(-contains("delta"), -statecode, -notes) %>% distinct() %>% dplyr::filter(confirmed>0) %>% dplyr::rename(deaths=deceased) %>% mutate(survival=na_if(recovered/deaths,Inf)) %>% mutate(state=ifelse(grepl("Dadra",state),"DNHDD",state)) %>% as.data.table()

dstts = dstt %>% group_by(state) %>% summarise_at(vars(confirmed,active,recovered,deaths),sum)

dstt = dstt[!(district %in% c("Unknown","Other State","Foreign Evacuees","Italians","Unassigned"))]
