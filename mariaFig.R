library(tidyverse)
library(stringr)
library(lubridate)
library(ggforce)
library(pdftools)

# note this is heavily borrowed from the original paper Kishore et al. 2018. Mortality in Puerto Rico after Hurricane Maria.  N Engl J Med 2018; 379:162-170 DOI: 10.1056/NEJMsa1803972 https://www.nejm.org/doi/full/10.1056/NEJMsa1803972 

filename <- "https://github.com/c2-d2/pr_mort_official/raw/master/data/RD-Mortality-Report_2015-18-180531.pdf"
txt <- pdf_text(filename)
dat <- lapply(seq_along(txt), function(i){
  s <- str_replace_all(txt[i], "\\*.*", "") %>%
    str_replace_all("Fuente: Registro Demográfico - División de Calidad y Estadísticas Vitales", "") %>%
    str_replace_all("Y(201\\d)\\*?", "\\1") %>%
    str_replace("SEP", "9") %>%
    str_replace("OCT", "10") %>%
    str_replace("NOV", "11") %>%
    str_replace("DEC", "12") %>%
    str_replace("JAN", "1") %>%
    str_replace("FEB", "2") %>%
    str_replace("MAR", "3") %>%
    str_replace("APR", "4") %>%
    str_replace("MAY", "5") %>%
    str_replace("JUN", "6") %>%
    str_replace("JUL", "7") %>%
    str_replace("AGO", "8") %>%
    str_replace("Total", "@") 
  
  tmp <- str_split(s, "\n") %>% .[[1]] %>% str_trim %>% str_split_fixed("\\s+", 50) %>% .[,1:5] %>% as_tibble()
  colnames(tmp) <- tmp[2,]
  tmp <- tmp[-(1:2),]
  j <- which(tmp[,1]=="@")
  if(colnames(tmp)[1]=="2") { ## deal with february 29
    k <- which(tmp==29)
    the_number <- unlist(tmp[k,-1])
    the_number <- the_number[the_number!=""]
    tmp[k, colnames(tmp)!="2016" & colnames(tmp)!="2"] <- 0
    tmp[k, "2016"] <- the_number
  }
  tmp <- tmp %>% slice(1:(j-1)) %>% mutate_all(funs(as.numeric)) %>%
    filter(!is.na(`2015`) & !is.na(`2016`) & !is.na(`2017`)  & !is.na(`2017`))
  tmp <- mutate(tmp, month = as.numeric(names(tmp)[1]))
  names(tmp)[1] <- "day"
  tmp <- tmp[,c(6,1,2,3,4,5)]
  ones <- which(tmp$day==1) ##1 2 3 4 appears due to graph... let's take it out
  if(length(ones)>1) tmp <- tmp[-ones[-1],]
  if(any(diff(tmp$day)!=1)) stop(i) ## check days are ordered
  ##check if negative. this means a black was left and the diff between 2016 and 0 was used!
  tmp[tmp<0] <- NA
  gather(tmp, year, deaths, 3:6, convert = TRUE) 
})
official <- do.call(rbind, dat) %>% 
  arrange(year, month, day) %>% 
  filter(!(month==2 & day==29 & year != 2016))
## add date
official <-official %>% mutate(date = ymd(paste(year, month, day,"-")))


official <- official %>% 
  mutate(ymd = ymd(paste0("2000-",str_sub(as.character(official$date),-5)))) %>%
  mutate(deaths = ifelse(deaths != 0, deaths,NA))


official %>% 
  filter(year < 2018) %>%
  ggplot(aes(ymd, deaths, color = factor(year)) )+ 
  geom_point(alpha = 0.3) +
  geom_smooth(method="loess", span = .1, formula = y ~ x) +
  geom_vline(xintercept = make_date(2017,09,20), lty=2, col="grey") +
  scale_x_date(date_labels = "%b", date_breaks = "1 months")  + 
  theme_light() +
  annotate(geom = "text", x = ymd("2000-09-16"), y = 100, label = "deaths from maria")
