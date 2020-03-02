---
title: "Bind IAT"
author: "Stefan"
date: "9 4 2019"
output: html_document
---

# Setup

```r
pacman::p_load(tidyverse, limma, IATScore, magrittr, ggplot2)
```

# Participant of interest

```r
PARTICIPANT <- "17121534"
```

# Data

```r
path_to_data <- paste(getwd(), "/iat_data", sep = "")

temp <- list.files(path = path_to_data, pattern="*.csv")
temp <-paste(path_to_data, temp, sep = "/")

csv <- function(x){
  y <- read_csv2(x) %>%
    mutate(participant = as.character(participant))
  return(y)
}


iat_data <- lapply(temp, csv) %>%
  bind_rows()
# here we replace the participant id 1715364 with the participant id 17101993, 
# because of a data entry error. 
iat_data <- iat_data %>%
  filter(!is.na(Word)) %>%
  select(c("participant", names(iat_data))[!duplicated(c("participant", names(iat_data)))], -X17) %>%
  mutate(participant = ifelse(participant == "17101993", "17153164", participant), 
         participant = ifelse(participant == "07114689", "07114687", participant), 
         participant = ifelse(participant == "14134935", "14134936", participant), 
         )
```




## Get in shape


```r
iat_data %<>% 
  mutate(rt = 1000*as.numeric(key_resp_2.rt),
         incorrect = (as.numeric(key_resp_2.corr)-1)^2,
         participant = as_factor(participant))
iat_selected <- iat_data %>% 
  select(
    participant, session,  Block, trials.thisTrialN, Category, trials.thisIndex, incorrect, rt) %>%
  rename(X1 = Block, X2 = trials.thisTrialN, X3 = Category, X4 = trials.thisIndex, X5 = incorrect, X6 = rt)
names(iat_data)
```

```
##  [1] "participant"       "Block"             "Word"              "Category"          "CorrResp"          "trials.thisRepN"   "trials.thisTrialN"
##  [8] "trials.thisN"      "trials.thisIndex"  "key_resp_2.keys"   "key_resp_2.corr"   "key_resp_2.rt"     "session"           "date"             
## [15] "expName"           "frameRate"         "rt"                "incorrect"
```

```r
result <- data.frame(participant = levels(iat_selected$participant),
                     iat_t1 = NA,
                     iat_t2 = NA)
```


```r
n_participants <- length(unique(iat_data$participant))
source("iat_function.R")
result_long <- data.frame(participant = rep(levels(as.factor(iat_data$participant)), each = 2), time = rep(c("001", "002"), times = n_participants), iat = NA)


for(i in levels(as.factor(iat_data$participant))){
  for(j in c("001", "002")){
     result_long$iat[result_long$participant == i & result_long$time == j] <- iat(iat_data %>% filter(participant == i, session == j))
    print(j)
  }
  print(i)
}
```

```
## [1] "001"
## [1] "002"
## [1] "00119636"
## [1] "001"
## [1] "002"
## [1] "05949000"
## [1] "001"
## [1] "002"
## [1] "06218481"
## [1] "001"
## [1] "002"
## [1] "06643977"
## [1] "001"
## [1] "002"
## [1] "07114687"
## [1] "001"
## [1] "002"
## [1] "09214883"
## [1] "001"
## [1] "002"
## [1] "09562547"
## [1] "001"
## [1] "002"
## [1] "09733551"
## [1] "001"
## [1] "002"
## [1] "11110681"
## [1] "001"
## [1] "002"
## [1] "11111580"
## [1] "001"
## [1] "002"
## [1] "11114873"
## [1] "001"
## [1] "002"
## [1] "11121787"
## [1] "001"
## [1] "002"
## [1] "12113072"
## [1] "001"
## [1] "002"
## [1] "12614137"
## [1] "001"
## [1] "002"
## [1] "12921623"
## [1] "001"
## [1] "002"
## [1] "13101928"
## [1] "001"
## [1] "002"
## [1] "13102215"
## [1] "001"
## [1] "002"
## [1] "13108311"
## [1] "001"
## [1] "002"
## [1] "13109376"
## [1] "001"
## [1] "002"
## [1] "13111752"
## [1] "001"
## [1] "002"
## [1] "13112792"
## [1] "001"
## [1] "002"
## [1] "13115704"
## [1] "001"
## [1] "002"
## [1] "13123625"
## [1] "001"
## [1] "002"
## [1] "13279500"
## [1] "001"
## [1] "002"
## [1] "13719067"
## [1] "001"
## [1] "002"
## [1] "14113971"
## [1] "001"
## [1] "002"
## [1] "14118467"
## [1] "001"
## [1] "002"
## [1] "14120430"
## [1] "001"
## [1] "002"
## [1] "14127948"
## [1] "001"
## [1] "002"
## [1] "14131965"
## [1] "001"
## [1] "002"
## [1] "14132336"
## [1] "001"
## [1] "002"
## [1] "14134936"
## [1] "001"
## [1] "002"
## [1] "14250831"
## [1] "001"
## [1] "002"
## [1] "15102775"
## [1] "001"
## [1] "002"
## [1] "15110646"
## [1] "001"
## [1] "002"
## [1] "15119027"
## [1] "001"
## [1] "002"
## [1] "15119050"
## [1] "001"
## [1] "002"
## [1] "15123524"
## [1] "001"
## [1] "002"
## [1] "16103707"
## [1] "001"
## [1] "002"
## [1] "16117897"
## [1] "001"
## [1] "002"
## [1] "16120685"
## [1] "001"
## [1] "002"
## [1] "16725020"
## [1] "001"
## [1] "002"
## [1] "17153164"
## [1] "001"
## [1] "002"
## [1] "17104084"
## [1] "001"
## [1] "002"
## [1] "17108085"
## [1] "001"
## [1] "002"
## [1] "17118597"
## [1] "001"
## [1] "002"
## [1] "17118647"
## [1] "001"
## [1] "002"
## [1] "17118670"
## [1] "001"
## [1] "002"
## [1] "17118746"
## [1] "001"
## [1] "002"
## [1] "17121534"
## [1] "001"
## [1] "002"
## [1] "17124801"
## [1] "001"
## [1] "002"
## [1] "17126475"
## [1] "001"
## [1] "002"
## [1] "18100917"
## [1] "001"
## [1] "002"
## [1] "18101246"
## [1] "001"
## [1] "002"
## [1] "18101881"
## [1] "001"
## [1] "002"
## [1] "18108431"
## [1] "001"
## [1] "002"
## [1] "18108720"
## [1] "001"
## [1] "002"
## [1] "18113662"
## [1] "001"
## [1] "002"
## [1] "18119461"
## [1] "001"
## [1] "002"
## [1] "18119925"
## [1] "001"
## [1] "002"
## [1] "18874834"
## [1] "001"
## [1] "002"
## [1] "18874859"
## [1] "001"
## [1] "002"
## [1] "20111222"
## [1] "001"
## [1] "002"
## [1] "20200300"
## [1] "001"
## [1] "002"
## [1] "20222333"
## [1] "001"
## [1] "002"
## [1] "20456789"
## [1] "001"
## [1] "002"
## [1] "EftPx"
## [1] "001"
## [1] "002"
## [1] "kim"
## [1] "001"
## [1] "002"
## [1] "sutti"
```

```r
result_long
```




# Save as csv

```r
write_csv(result_long, "iat_result.csv")
```



