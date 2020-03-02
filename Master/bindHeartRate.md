---
title: "heartRate"
author: "Stefan P. Thoma"
date: "7/13/2019"
output: html_document
---
# Setup


# Load data


```r
# first, the excel transcript file with the hear rate information
info_raw <- read_excel("open/transcript.xlsx" ) 
info_raw <- info_raw %>% select(`participant nr.`, ID, condition, date, baseStart, baseEnd, vrStart, vrEnd, Frage1, Frage2, Frage3, starts_with("OQ")) %>%
  mutate(baseStart = as.hms(format(baseStart, format = "%H:%M:%S")), # these lines drop the dates and leave time only.
         baseEnd = as.hms(format(baseEnd, format = "%H:%M:%S")),
         vrStart = as.hms(format(vrStart, format = "%H:%M:%S")),
         vrEnd = as.hms(format(vrEnd, format = "%H:%M:%S")), 
         date = format(date, format = "%Y-%m-%d")) %>%
  rename(id = ID)
info <- info_raw %>% drop_na()
```


```r
# find all csv files that start with "Fitbit" and end in ".csv"
temp <- list.files(patter = ".csv", path = paste(getwd(), "/hr_data", sep = ""))
temp <- paste(getwd(), "/hr_data/", sep = "", temp[temp %>% startsWith("Fitbit")])

hr_data <- map(temp, read_csv) %>%
  bind_rows() %>%
  mutate(id = NA,
         t = NA
         )
```

```
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
## Parsed with column specification:
## cols(
##   time = col_time(format = ""),
##   value = col_double(),
##   Date = col_date(format = "")
## )
```
# Assign correct ID


```r
for(i in info$id){
    hr_data$id[hr_data$Date == info$date[info$id == i] & hr_data$time >= info$baseStart[info$id == i] & hr_data$time <= info$baseEnd[info$id == i]] <- i
    hr_data$t[hr_data$Date == info$date[info$id == i] & hr_data$time >= info$baseStart[info$id == i] & hr_data$time <= info$baseEnd[info$id == i]] <- 1
    hr_data$id[hr_data$Date == info$date[info$id == i] & hr_data$time >= info$vrStart[info$id == i] & hr_data$time <= info$vrEnd[info$id == i]] <- i
    hr_data$t[hr_data$Date == info$date[info$id == i] & hr_data$time >= info$vrStart[info$id == i] & hr_data$time <= info$vrEnd[info$id == i]] <- 2
}
hr_data_reduced <- hr_data %>% drop_na
```
# Get mean per person

```r
hr_mean <- hr_data_reduced %>% group_by(id, t) %>%
  mutate(hr_mean=mean(value)) %>%
  select(-value, -time) %>%
  distinct()
```


```r
for(i in hr_mean$id){
try(info_raw$hr_meant1[info_raw$id==i] <- hr_mean$hr_mean[hr_mean$t==1 & hr_mean$id==i])
try(info_raw$hr_meant2[info_raw$id==i] <- hr_mean$hr_mean[hr_mean$t==2 & hr_mean$id==i])
}
```

```
## Warning: Unknown or uninitialised column: 'hr_meant1'.
```

```
## Warning: Unknown or uninitialised column: 'hr_meant2'.

## Warning: Unknown or uninitialised column: 'hr_meant2'.
```

```
## Error in info_raw$hr_meant2[info_raw$id == i] <- hr_mean$hr_mean[hr_mean$t ==  : 
##   replacement has length zero
```

```r
data_hr <- info_raw %>% gather(key = time, value = hr_mean, hr_meant1:hr_meant2) %>%
  mutate(time = ifelse(time == "hr_meant1", 1, 2)) %>%
  arrange(id, time)
```


```r
data_hr$id[data_hr$id=="09547562"] <- "09562547"
data_hr$id <- trimws(data_hr$id, "right")



data_hr$id[data_hr$id=="14132336 "] <- "14132336"
```



```r
write.csv(data_hr, "hr_open_data.csv")
write.csv(hr_data_reduced, "hr_raw")
```

