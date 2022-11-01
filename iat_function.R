## By Stefan P. Thoma
## Based on https://rpubs.com/dstorage/IAT

# iat function
iat <- function(data){
  
  
# get in shape
IAT <- data %>% 
  mutate(Block = Block,
         Trial = trials.thisIndex,
         Category = Category,
         Cat_Item = Word,
         Correct = abs(key_resp_2.corr-1),
         RT = as.numeric(key_resp_2.rt)*1000) %>%
  select(
    Block,
    Trial,
    Category,
    Cat_Item,
    Correct,
    RT)

# Step 1: Delete any reaction times > 10,000 ms ####
i <- nrow(IAT) # define i counting variable for while loop
while (i < 180) { # define while loop for Step 1 
  if (IAT$RT[i] > 10000) {IAT$RT[i] <- 0}
  i = i + 1
}
IAT2 <- subset(IAT, RT!=0)

# Step 2: Check for exclusion based on response speed (10% trials < 300 ms) ####
SpeedCount <- length(which(IAT2$RT<300)) # count number of RTs under 300
SpeedCount # display the number of RTs under 300 

SpeedProp <- SpeedCount/nrow(IAT2) # calculate proportion of RTs under 300
SpeedProp # display proportion of RTs under 300 

if (SpeedProp > 0.10) { 
  print("STOP ANALYZING AND EXCLUDE")
  Exclude <- "Yes"}

# Step 3: Compute means of correct trials in blocks 2, 3, 5, and 6 ####
Block3trials <- subset(IAT2, Block==3) # subset data frame for only Block 2 trials 
Block3correct <- subset(Block3trials, Correct==0) # subset data frame for only correct trials
Block3correctMean <- mean(Block3correct$RT) # mean of Block 2 correct trials 
Block4trials <- subset(IAT2, Block==4) # subset data frame for only Block 3 trials 
Block4correct <- subset(Block4trials, Correct==0) # subset data frame for only correct trials
Block4correctMean <- mean(Block4correct$RT) # mean of Block 3 correct trials 
Block6trials <- subset(IAT2, Block==6) # subset data frame for only Block 5 trials 
Block6correct <- subset(Block6trials, Correct==0) # subset data frame for only correct trials
Block6correctMean <- mean(Block6correct$RT) # mean of Block 5 correct trials 
Block7trials <- subset(IAT2, Block==7) # subset data frame for only Block 6 trials 
Block7correct <- subset(Block7trials, Correct==0) # subset data frame for only correct trials
Block7correctMean <- mean(Block7correct$RT) # mean of Block 6 correct trials 


# Step 4: Replace incorrect trials with average RT by block + 600 ####
newBlock3 <- Block3correctMean + 600
newBlock4 <- Block4correctMean + 600
newBlock6 <- Block6correctMean + 600
newBlock7 <- Block7correctMean + 600

i <- 1 # define i counting variable for while loop 
while (i < nrow(IAT2) + 1) { # create while loop for Block 2 incorrect trial replacement
  if (IAT2$Block[i] == 3 && IAT2$Correct[i] == 1) {
    IAT2$RT[i] <- newBlock3 }
  i <- i + 1
}

i <- 1 # define i counting variable for while loop 
while (i < nrow(IAT2) + 1) { # create while loop for Block 3 incorrect trial replacement
  if (IAT2$Block[i] == 4 && IAT2$Correct[i] == 1) {
    IAT2$RT[i] <- newBlock4 }
  i <- i + 1
}

i <- 1 # define i counting variable for while loop 
while (i < nrow(IAT2) + 1) { # create while loop for Block 5 incorrect trial replacement
  if (IAT2$Block[i] == 6 && IAT2$Correct[i] == 1) {
    IAT2$RT[i] <- newBlock6 }
  i <- i + 1
}

i <- 1 # define i counting variable for while loop 
while (i < nrow(IAT2) + 1) { # create while loop for Block 6 incorrect trial replacement
  if (IAT2$Block[i] == 7 && IAT2$Correct[i] == 1) {
    IAT2$RT[i] <- newBlock7 }
  i <- i + 1
}


# Step 5: Calculate stdevs for all trials in blocks 2&5 and 3&6 ####
IAT3 <- subset(IAT2, Block==3 | Block==6) # pool all trials in blocks 2&5
sd36 <- sd(IAT3$RT) # stdev of all trials in blocks 2&5
IAT4 <- subset(IAT2, Block==4 | Block==7) # pool all trials in blocks 3&6
sd47 <- sd(IAT4$RT) # stdev of all trials in blocks 3&6

IAT5 <- subset(IAT2, Block==3) # only Block 2 trials
block3mean <- mean(IAT5$RT) # block 2 mean
IAT6 <- subset(IAT2, Block==4) # only Block 3 trials
block4mean <- mean(IAT6$RT) # block 3 mean 
IAT7 <- subset(IAT2, Block==6) # only Block 5 trials
block6mean <- mean(IAT7$RT) # block 5 mean
IAT8 <- subset(IAT2, Block==7) # only Block 6 trials
block7mean <- mean(IAT8$RT) # block 6 mean


# Step 7: Compute mean differences in test trials #### 
meandiff1 <- block3mean - block6mean # first mean difference (blocks 5 - 2)
meandiff2 <- block4mean - block7mean # second mean difference (blocks 6 - 3)

value1 <- meandiff1/sd36
value2 <- meandiff2/sd47

# Step 9: Average these two values to get your IAT effect size ####
IATeffect <- (value1+value2)/2 # calculate IAT effect size 
return(IATeffect) # print IAT effect size 

}