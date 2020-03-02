---
title: "bind_qualtrics"
author: "Stefan P. Thoma"
date: "7/17/2019"
output: html_document
---

# Setup


# Load data

```r
vppp <- read_csv("vppool_participants.csv")
```

```
## Parsed with column specification:
## cols(
##   first_name = col_character(),
##   last_name = col_character(),
##   login_id = col_character(),
##   survey_id = col_double(),
##   email = col_character(),
##   phone = col_character(),
##   occur_date = col_character(),
##   date_granted = col_character(),
##   location = col_character(),
##   credit_type = col_character(),
##   show_credit = col_double()
## )
```


```r
# find all csv files
temp <- list.files(patter = ".csv", path = paste(getwd(), "/qualtrics", sep = ""))
temp_quest <- paste(getwd(), "/qualtrics/", sep = "", temp[temp %>% startsWith("Umfrage")])
temp_exp <- paste(getwd(), "/qualtrics/", sep = "", temp[temp %>% startsWith("Experiment")])

quest_data <- read_csv(temp_quest) 
```

```
## Parsed with column specification:
## cols(
##   .default = col_character()
## )
```

```
## See spec(...) for full column specifications.
```

```r
quest_data_questions <- quest_data[1,]
quest_data <- quest_data[-1:-2,] %>%
  mutate(t = 1)
  

exp_data <- read_csv(temp_exp)
```

```
## Parsed with column specification:
## cols(
##   .default = col_character()
## )
## See spec(...) for full column specifications.
```

```r
exp_data_questions <- exp_data[1,]

exp_data <- exp_data[-1:-2, ] %>%
  mutate(t = 2)
exp_data$age[exp_data$age==22] <- 1997
```


```r
# variables we want
var_we_want <- c("ID", "id", "t", "StartDate", "Duration (in seconds)", "Finished", "Progress") 
quest_data <- quest_data %>%
  select(var_we_want, starts_with("ccs"), starts_with("nr"), starts_with("nep"), starts_with("SES"))

exp_data <- exp_data %>% 
  select(-EndDate, -Status, -IPAddress, -`Duration (in seconds)`, -RecordedDate, -ResponseId, -starts_with("Recipient"), -starts_with("Location"), -UserLanguage, -sex_2_TEXT, -rem_ipq) %>%
  rename(pol = Q84,
         SES_1 = Q89_1,
         SES_2 = Q89_2,
         SES_3 = Q89_3,
         SES_4 = Q89_4,
         SES_5 = Q89_5,
         SES_6 = Q89_6,
         SES_7 = Q89_7,
         SES_8 = Q89_8,
         SES_9 = Q89_9,
         SES_10 = Q89_10,
         SES_11 = Q89_11,
         SES_12 = Q89_12,
         SES_13 = Q89_13,
         SES_14 = Q89_14,
         SES_15 = Q89_15,
         SES_16 = Q89_16,
         SES_17 = Q89_17
         ) %>% 
  mutate(yearOfBirth = substr(age, nchar(age)-4+1, nchar(age)),
         age = 2019-as.numeric(yearOfBirth))
```


```r
# The first four participants received a flawed version of the ipq where one question was repeated three times. 
# This next step deletes two of those answers, namely item 9 and 10 of the ipq
exp_data[["ipq10"]][as.Date(exp_data$StartDate)<=as.Date("2019-05-13")] <- NA
exp_data[["ipq9"]][as.Date(exp_data$StartDate)<=as.Date("2019-05-13")] <- NA
```


```r
exp_data <- exp_data %>% filter(DistributionChannel == "anonymous")

# Assigns the correct matriculation number to the participants by looking at the vp-pool-participants-sheet
for(i in vppp$survey_id){
  quest_data$ID[quest_data$id==i] <- vppp$login_id[vppp$survey_id==i]
}


quest_data$ID[quest_data$id=="sutti"] <- "sutti"
quest_data$ID[quest_data$id=="15119027"] <- "15119027"
quest_data$ID[quest_data$ID=="13-101-928"] <- "13101928"
quest_data$ID[quest_data$id=="EftPx"] <- "EftPx"
exp_data$id[exp_data$id == "09562547"] <- "09547562"

quest_data$ID[quest_data$ID=="14132936"] <- "14134936"
quest_data$ID[quest_data$id == "3811"] <- "16725020"



quest_data <- quest_data %>% filter(Finished == 1)
quest_data <- quest_data %>% filter(as.Date(StartDate) >= as.Date("2019-04-26"))

quest_data <- quest_data %>% filter(quest_data$ID %in% exp_data$id)
quest_data$id <- quest_data$ID
```


```r
exp_data$id
# Check the ids... 
data.frame(quest_data$ID,  quest_data$ID %in% exp_data$id)
quest_data


sort(exp_data$id)[!sort(exp_data$id)  %in% sort(quest_data$ID)]

quest_data$ID[quest_data$id == "3811"]
```


```r
qualtrix <- bind_rows(quest_data, exp_data) %>% 
  select(-ID, -`Duration (in seconds)`, -Finished, -Progress, -DistributionChannel, -ExternalReference) %>%
  arrange(id, t, StartDate) %>%
  group_by(id) %>%
  mutate(age = age[t==2], 
         edu = edu[t==2],
         sex = sex[t==2],
         pol = pol[t==2],
         yearOfBirth = yearOfBirth[t==2],
         ipq1 = ipq1[t==2], 
         ipq2 = ipq2[t==2], 
         ipq3 = ipq3[t==2], 
         ipq4 = ipq4[t==2], 
         ipq5 = ipq5[t==2], 
         ipq6 = ipq6[t==2], 
         ipq7 = ipq7[t==2],
         ipq8 = ipq8[t==2], 
         ipq9 = ipq9[t==2], 
         ipq10 = ipq10[t==2], 
         ipq11 = ipq11[t==2], 
         ipq12 = ipq12[t==2], 
         ipq13 = ipq13[t==2], 
         ipq14 = ipq14[t==2], 
         sod_1 = sod_1[t==2], 
         sod_2 = sod_2[t==2], 
         sod_3 = sod_3[t==2], 
         sod_4 = sod_4[t==2], 
         sod_5 = sod_5[t==2], 
         sod_6 = sod_6[t==2], 
         sod_7 = sod_7[t==2], 
         sod_8 = sod_8[t==2], 
         sod_9 = sod_9[t==2], 
         vr_exp = vr_exp[t==2], 
         vr_eval1 = vr_eval1[t==2], 
         vr_eval2 = vr_eval2[t==2], 
         vr_eval3 = vr_eval3[t==2], 
         vr_eval4 = vr_eval4[t==2], 
         vr_eval5 = vr_eval5[t==2], 
         span = span[t==2], 
         seen = seen[t==2],
         ) %>%
  ungroup()

# fix cases of mistaken identity...
qualtrix$id[qualtrix$id=="07114689"] <- "07114687"
qualtrix$id[qualtrix$id=="09547562"] <- "09562547"
```


```r
qualtrix <- qualtrix %>%
  rename(time = t)
write_csv(qualtrix, "qualtrix.csv")
```

