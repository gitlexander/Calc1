# 1. Data cleaning

library(readr)
library(readxl)
library(ggplot2)
library(data.table)
library(tidyr)
library(car)
library(dplyr)
library(caret)
library(MASS)
library(lsa)
library(plumber)

#* Get the list of subtasks
#* @get /subtasks
function(nage, ngender, nmednum, nmedhisnum, ncog, nphy, nvis, nmotiv, nenv, nhear) {
  df <- read_excel("Data.xlsx")
  
  nid <- 81
  nage <- as.numeric(nage)
  ngender <- as.numeric(ngender)
  nmednum <- as.numeric(nmednum)
  nmedhisnum <- as.numeric(nmedhisnum)
  ncog <- as.numeric(ncog)
  nphy <- as.numeric(nphy)
  nvis <- as.numeric(nvis)
  nmotiv <- as.numeric(nmotiv)
  nenv <- as.numeric(nenv)
  nhear <- as.numeric(nhear)
  
  #modify df to include new participant's information in the last column
  
  # change column names for products
  colnames(df)[4] <- "PD1"
  colnames(df)[5] <- "PD2"
  colnames(df)[6] <- "PD3"
  colnames(df)[7] <- "PD4"
  colnames(df)[8] <- "PD5"
  colnames(df)[9] <- "PD6"
  colnames(df)[10] <- "PD7"
  colnames(df)[11] <- "PD8"
  colnames(df)[12] <- "PD9"
  colnames(df)[13] <- "PD10"
  colnames(df)[14] <- "PD11"
  colnames(df)[15] <- "PD12"
  colnames(df)[16] <- "PD13"
  
  
  # add whisper test percentage column
  df$`whisper test left`[df$`whisper test left` == 2] <- 0
  df$`whisper test right`[df$ `whisper test right` == 2] <- 0
  df$`whisper score percentage` <- (df$`whisper test left` + df$`whisper test right`)/2 * 100
  df$gender <- factor(ifelse(df$gender == 1, 0, 1))
  # add a column for total number of medical conditions
  df$totalmedhist <- rowSums(df[, c(23:43)])
  
  # create a new data frame that takes in only the important variables
  
  # columns for subtasks
  subtask1columns <- sort(c(seq(from = 85, to = 240, by = 5), seq(from = 88, to = 243, by = 5)))
  democolumns <- c(1, 17, 18, 22, 353) 
  #limitcolumns <- c(53, 59, 62, 74, 79, 352)
  limitcolumns <- c(52, 58, 61, 74, 79, 69)
  
  df2 <- df[,c(democolumns, limitcolumns, subtask1columns)]
  
  # rename the columns
  colnames(df2) <- c("ID", "Age", "Gender", "Mednum", "Medhisnum", "Cog", "Phy", "Vis", "Motiv", "Env", "Hear", "A1c", "A1f",
                     "A2c", "A2f", "A3c", "A3f", "A4c", "A4f", "A5c", "A5f", "A6c", "A6f", "B1c", "B1f", "B2c", "B2f", "B3c", "B3f", "B4c", "B4f",
                     "C1c", "C1f", "C2c", "C2f", "D1c", "D1f", "D2c", "D2f", "D3c", "D3f", "D4c", "D4f", "D5c", "D5f", "D6c", "D6f", "G1c", "G1f",
                     "G2c", "G2f", "G3c", "G3f", "H1c", "H1f", "I1c", "I1f", "I2c", "I2f", "I3c", "I3f", "M1c", "M1f", "P1c", "P1f", "R1c", "R1f", 
                     "R2c", "R2f", "T1c", "T1f", "T2c", "T2f", "U1c", "U1f")
  
  df3 <- df2 %>%
    group_by(ID) %>%
    summarise(
      Age = first(Age), Gender = first(Gender), Mednum = first(Mednum), Medhisnum = first(Medhisnum),
      Cog = first(Cog), Phy = first(Phy), Vis = first(Vis), Motiv = first(Motiv), Env = first(Env), Hear = first(Hear),
      A1 = sum(A1c),
      A1fail = sum(A1f),
      A2 = sum(A2c),
      A2fail = sum(A2f),
      A3 = sum(A3c),
      A3fail = sum(A3f),
      A4 = sum(A4c),
      A4fail = sum(A4f),
      A5 = sum(A5c),
      A5fail = sum(A5f),
      A6 = sum(A6c),
      A6fail = sum(A6f),
      B1 = sum(B1c),
      B1fail = sum(B1f),
      B2 = sum(B2c),
      B2fail = sum(B2f),
      B3 = sum(B3c),
      B3fail = sum(B3f),
      B4 = sum(B4c),
      B4fail = sum(B4f),
      C1 = sum(C1c),
      C1fail = sum(C1f),
      C2 = sum(C2c),
      C2fail = sum(C2f),
      D1 = sum(D1c),
      D1fail = sum(D1f),
      D2 = sum(D2c),
      D2fail = sum(D2f),
      D3 = sum(D3c),
      D3fail = sum(D3f),
      D4 = sum(D4c),
      D4fail = sum(D4f),
      D5 = sum(D5c),
      D5fail = sum(D5f),
      D6 = sum(D6c),
      D6fail = sum(D6f),
      G1 = sum(G1c),
      G1fail = sum(G1f),
      G2 = sum(G2c),
      G2fail = sum(G2f),
      G3 = sum(G3c),
      G3fail = sum(G3f),
      H1 = sum(H1c),
      H1fail = sum(H1f),
      I1 = sum(I1c),
      I1fail = sum(I1f),
      I2 = sum(I2c),
      I2fail = sum(I2f),
      I3 = sum(I3c),
      I3fail = sum(I3f),
      M1 = sum(M1c),
      M1fail = sum(M1f),
      P1 = sum(P1c),
      P1fail = sum(P1f),
      R1 = sum(R1c),
      R1fail = sum(R1f),
      R2 = sum(R2c),
      R2fail = sum(R2f),
      T1 = sum(T1c),
      T1fail = sum(T1f),
      T2 = sum(T2c),
      T2fail = sum(T2f),
      U1 = sum(U1c),
      U1fail = sum(U1f)
    )
  
  # New row with some data
  new_row <- c(ID=nid,Age=nage,Gender=ngender,Mednum=nmednum,Medhisnum=nmedhisnum,Cog=ncog,Phy=nphy,Vis=nvis,Motiv=nmotiv,Env=nenv,Hear=nhear)
  
  # Create a new row with all columns
  new_row_full <- setNames(as.list(rep(0, ncol(df3))), names(df3))
  
  # Update with the specified values
  new_row_full[names(new_row)] <- new_row
  
  # Convert to data frame
  new_row_df <- as.data.frame(new_row_full, stringsAsFactors = FALSE)
  
  # Add the new row to the existing data frame
  df3 <- rbind(df3, new_row_df)
  
  
  #reformat binary variables as factors
  df3$Motiv <- as.factor(df3$Motiv)
  df3$Env <- as.factor(df3$Env)
  df3$Hear <- as.factor(df3$Hear)
  df3$Vis <- as.numeric(df3$Vis)
  
  # standardizing continuous covariates
  
  df3$Age_std <- (df3$Age - mean(df3$Age))/sd(df3$Age)
  df3$Mednum_std <- (df3$Mednum - mean(df3$Mednum))/sd(df3$Mednum)
  df3$Medhisnum_std <- (df3$Medhisnum - mean(df3$Medhisnum))/sd(df3$Medhisnum)
  df3$Cog_std <- (df3$Cog - mean(df3$Cog))/sd(df3$Cog)
  df3$Phy_std <- (df3$Phy - mean(df3$Phy))/sd(df3$Phy)
  df3$Vis_std <- (df3$Vis - mean(df3$Vis))/sd(df3$Vis)
  
  #write.csv(df3, "df3.csv")
  
  #2. Splitting the data into training and predicting
  
  # Get important columns from df3
  
  PPMiden <- df3[c("ID", "Age_std", "Gender", "Mednum_std", "Medhisnum_std", "Cog_std", "Phy_std", "Vis_std", "Motiv", "Env", "Hear")]
  
  # randomly select 3 rows from PPMiden to demonstrate later how prediction works
  #set.seed(123)
  #remove <- sample(nrow(PPMiden), 3)
  #predicting <- PPMiden[c(remove),]
  #training <- PPMiden[-c(remove),]
  
  #select the newly added row for prediction
  # Find the index of the new row
  new_row_id <- df3$ID[nrow(df3)] # since the new row is the last one added
  
  # Create the new predicting dataset
  predicting <- PPMiden[PPMiden$ID == new_row_id, , drop = FALSE]
  training <- PPMiden[PPMiden$ID != new_row_id, , drop = FALSE]
  
  # perform cosine similarity to identify Mp% of the training data that is the most similar to each participant in `predicting`
  
  idenID <- function(predicting_i, training, Mp) {
    
    testingvecnoID <-as.numeric(predicting_i)[-1]
    testingvecnoID[c(2, 8, 9, 10)] <- testingvecnoID[c(2, 8, 9, 10)]-1 # since as.numeric added 1 to the binary variables
    
    # change training data into all numeric values
    trainingnoID <- training[-1]
    trainingnoID$Gender <- as.numeric(trainingnoID$Gender)-1
    trainingnoID$Motiv <- as.numeric(trainingnoID$Motiv) - 1
    trainingnoID$Env <- as.numeric(trainingnoID$Env) -1
    trainingnoID$Hear <- as.numeric(trainingnoID$Hear) -1
    
    # calculate cosine similarity metric of every value in the training data with one testing data and append to the training data
    training$csm <- apply(trainingnoID, 1, function(x) cosine(testingvecnoID, x))
    
    training <- training[order(training$csm, decreasing = TRUE),]
    
    # sort the training data by decreasing csm, identify the ID of participants in the top Mp%
    M <- round(length(training$ID)*Mp, 0)
    return(training$ID[1:M])
    
  }
  
  
  newperson1_simID <- idenID(predicting[1,], training, 0.80)
  
  predicting$ID
  
  #3. Identifying similar participants
  
  print("TrainingID for new participant:")
  newperson1_simID
  
  #4. Fitting the model
  # first, we do some more data cleaning to make it into a format that we want:
  
  df4 <- df3 %>% pivot_longer(cols = c("A1", "A2", "A3", "A4", "A5", "A6", 
                                       "B1", "B2", "B3", "B4", "C1", "C2", 
                                       "D1", "D2", "D3", "D4", "D5", "D6", 
                                       "G1", "G2", "G3", "H1", "I1", "I2", 
                                       "I3", "M1", "P1", "R1", "R2", "T1", 
                                       "T2", "U1"),
                              names_to = "Subtasks",
                              values_to = "Count")
  
  
  # create a new vector failcount of length 2560. Three things are needed to find the correct element to append to this vector: the subtask, the patient ID, and its failure moment for the corresponding subtask.
  
  # writing the above in a function:
  failvec <- c()
  for (i in 1:length(df4$ID)) {
    
    # to find the correct subtask:
    identify_st <- df4[i,]$Subtasks
    # change the vector name to be able to find the failure moment:
    identify_failcolumn <- paste(identify_st, "fail", sep = "")
    # identify the correct participant:
    identify_ID <- df4[i,]$ID
    # then the value to be appended to the ith element in the vector is:
    truefailval <- df3[df3$ID == identify_ID,][identify_failcolumn]
    
    failvec <- c(failvec, truefailval)
  }
  
  df4$Fail <- as.numeric(failvec)
  
  # create a new variable, proportion based on the count and fail columns
  df4$Proportion <- (df4$Count - df4$Fail)/df4$Count
  
  dfnoNA <- df4[df4$Count != 0,]
  
  dfnoNA$Age <- as.numeric(dfnoNA$Age)
  dfnoNA$Gender <- as.factor(dfnoNA$Gender) #0 for male, 1 for female, 2 for other
  dfnoNA$Mednum <- as.numeric(dfnoNA$Mednum)
  dfnoNA$Medhisnum <- as.numeric(dfnoNA$Medhisnum)
  dfnoNA$Cog <- as.numeric(dfnoNA$Cog)
  dfnoNA$Phy <- as.numeric(dfnoNA$Phy)
  dfnoNA$Vis <- as.numeric(dfnoNA$Vis)
  dfnoNA$Motiv <- as.factor(dfnoNA$Motiv)
  dfnoNA$Env <- as.factor(dfnoNA$Env)
  #dfnoNA$Hear <- as.numeric(dfnoNA$Hear)
  dfnoNA$Hear <- as.factor(dfnoNA$Hear)
  dfnoNA$Subtasks <- as.factor(dfnoNA$Subtasks)
  
  # It looks like some subtasks have no split in the data (i.e. everyone was successful). Let's check:
  # dfnoNA$Proportion[dfnoNA$Subtasks == "A3"]
  # dfnoNA$Proportion[dfnoNA$Subtasks == "D1"]
  # dfnoNA$Proportion[dfnoNA$Subtasks == "I2"]
  # dfnoNA$Proportion[dfnoNA$Subtasks == "U1"]
  
  # Should we remove these subtasks from the model? Let's create a new dataframe 
  dfnoNA2 <- dfnoNA[!dfnoNA$Subtasks %in% c("A3", "D1", "I2", "U1"), ]
  dfnoNA2$Subtasks <- droplevels(dfnoNA2$Subtasks)
  #unique(dfnoNA$Subtasks)
  #unique(dfnoNA2$Subtasks)
  #write.csv(dfnoNA2, "dfnoNA2.csv")
  
  # for newperson1
  training1 <- subset(dfnoNA2, ID %in% newperson1_simID)
  
  model1 <- glm(Proportion ~ Gender + Mednum_std + Medhisnum_std + Cog_std + Phy_std + Vis_std + Motiv + Env + Hear + Subtasks,
                data = training1,
                weights = Count,
                family = binomial(link = "logit"))
  # showing the output of the model
  summary(model1)
  
  
  #6. Making predictions
  makingpred <- function(newparticipant, subtask_list, fittedmodel) {
    
    repeated_rows <- rep(newparticipant, each = length(subtask_list))
    newdata <- as.data.frame(matrix(unlist(repeated_rows), nrow = length(subtask_list)))
    colnames(newdata) <- colnames(newparticipant)
    
    newdata$Subtasks <- subtask_list
    newdata$Subtasks <- as.factor(newdata$Subtasks)
    newdata$ID <- as.numeric(newdata$ID)
    newdata$Gender <- as.factor(newdata$Gender-1)
    newdata$Mednum_std <- as.numeric(newdata$Mednum_std)
    newdata$Medhisnum_std <- as.numeric(newdata$Medhisnum_std)
    newdata$Cog_std <- as.numeric(newdata$Cog_std)
    newdata$Phy_std <- as.numeric(newdata$Phy_std)
    newdata$Vis_std <- as.numeric(newdata$Vis_std)
    newdata$Motiv <- as.factor(newdata$Motiv-1)
    newdata$Env <- as.factor(newdata$Env-1)
    newdata$Hear <- as.factor(newdata$Hear-1)
    
    newdata$logodds <- as.numeric(predict(fittedmodel, newdata))
    newdata$prop <- exp(newdata$logodds)/(1+exp(newdata$logodds))
    
    return(newdata[order(newdata$prop, decreasing = TRUE),])
    
    
  }
  
  #newparticipant_ <- df3[df3$ID == predicting$ID[1],]
  #newparticipant <- newparticipant_[c("ID", "Age", "Gender", "Mednum", "Medhisnum", "Cog", "Phy", "Vis", "Motiv", "Env", "Hear")]
  newparticipant <- predicting[1,]
  
  
  subtask_list <- c("A1", "A2", "A4", "A5", "A6", "B1", "B2", "B3", "B4", 
                    "C1", "C2", "D2", "D3", "D4", "D5", "D6", "G1", "G2", 
                    "G3", "H1", "I1", "I3", "M1", "P1", "R1", "R2", "T1", "T2")
  
  pred1 <- makingpred(newparticipant, subtask_list, model1)
  
  pred1[1, c(3:11)]
  pred1[c(1, 12, 13, 14)]
  
  #7. Conclusion
  
result1 <- data.frame(subID=pred1$Subtasks, probability=round((pred1$prop)*100))
# Assuming you already have result1 and descriptions_df data frames

# Create the descriptions data frame
descriptions_df <- data.frame(
  subID = c("A1", "A2", "A4", "A5", "A6", "B1", "B2", "B3", "B4", 
            "C1", "C2", "D2", "D3", "D4", "D5", "D6", "G1", "G2", 
            "G3", "H1", "I1", "I3", "M1", "P1", "R1", "R2", "T1", "T2"),
  description = c(description = c(
    "Locate the battery/cartridge compartment/medication cavity",
    "Place/insert batteries correctly",
    "Slide in/out battery compartment door",
    "Slide a tab/button",
    "Check/ensure/verify the device is on or the lock is placed in position/Follow instructions/Ensure indicator light flashes",
    "Flip device",
    "Insert key and rotate",
    "Press and rotate a lid",
    "Open a lid by lifting",
    "Press and hold a button on a device",
    "Press a button on a device",
    "Open pill box or compartment or tray or door by sliding",
    "Pick up the correct pillbox/pill organizer/open correct compartment",
    "Insert/fill/place medication in compartment/pillbox/pill organizer",
    "Close lid",
    "Put stickers on pillbox dividers",
    "Remove the medication",
    "Grab/hold the device",
    "Place hand over open slot",
    "Rotate the carousel three days from today’s date",
    "Locate and touch on an icon/button on an app or screen",
    "Scroll through the options on a screen",
    "Align and insert cartridge into the designated slot",
    "Tear packaging",
    "Rotate retaining clips at each end of the device in an open/close position",
    "Align connectors to one another and gently push card into the device",
    "Pierce cavity barrier",
    "Pinch number printed on card and pull out"
  )
  ) 
)

# Merge result1 with descriptions_df to add descriptions
result1 <- merge(result1, descriptions_df, by.x = "subID", by.y = "subID")

# View the updated result1
  print(result1)
  
}
