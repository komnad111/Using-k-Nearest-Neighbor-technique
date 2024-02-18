###NORTHSTAR BANK kNN

#LOAD THE NECESSARY PACKAGES
library(dplyr) #Functions for editing data frames.
library(haven) #Lets R recognize other data file types besides csv.
library(mosaic) #Functions for common statistical tasks.
library(fastDummies) #Adds a function for making dummy variables.
library(FNN) #Functions for k Nearest Neighbor algorithms.


#DATA PREPROCESSING

#Load the master data file. 
    d_northstar <- read_spss("Northstar_Bank_data.sav") #haven
    
#Make dummy variables for multinomial variables.
  #Remove the first dummy to avoid having all included.
    d_northstar_dum1 <- dummy_cols(d_northstar, 
                               select_columns = c('Ed'), 
                               remove_first_dummy = TRUE) #fastDummies
  #Remove original multinomial variables.
    d_northstar_dum2 <- select(d_northstar_dum1, -Ed) #dplyr

#Rescale the scale variables to 0-1.
  #This prevents giving large-number variables too much influence.
    d_northstar_normdum <- mutate(d_northstar_dum2, 
        Age=(Age-min(Age))/(max(Age)-min(Age)),
        Employ = (Employ-min(Employ))/(max(Employ)-min(Employ)),
        Address = (Address-min(Address))/(max(Address)-min(Address)),
        Income = (Income-min(Income))/(max(Income)-min(Income)),
        Debtinc = (Debtinc-min(Debtinc))/(max(Debtinc)-min(Debtinc)),
        Creddebt = (Creddebt-min(Creddebt))/(max(Creddebt)-min(Creddebt)),
        Othdebt = (Othdebt-min(Othdebt))/(max(Othdebt)-min(Othdebt))) #dplyr 

#Make training and test data frames based on value of 'partition'.
  #Also, remove the 'partition' variable.
    d_northstar_train <- filter(d_northstar_normdum, partition == 'train') %>% 
        select(-partition) #dplyr
    d_northstar_test <- filter(d_northstar_normdum, partition == 'test') %>% 
        select(-partition) #dplyr

#The stepwise procedure showed that these variables are most influential:
#Debtinc, Employ, Creddebt, and Address.
    
#Create data frames with only the selected independent variables.
  #Use those selected by stepwise regression above.
  #The kNN algorithm requires the data frame include only the selected variables.
    d_northstar_knn_train <- select(d_northstar_train, Debtinc, 
                                Employ, Creddebt, Address) #dplyr
    d_northstar_knn_test <- select(d_northstar_test, Debtinc, 
                               Employ, Creddebt, Address) #dplyr


#MODEL TRAINING
    
#Make kNN predictions using 3 nearest neighbors
    v_knn3_train =  knn(d_northstar_knn_train, d_northstar_knn_train, 
                    d_northstar_train$Default, k = 3) #FNN    

#Make kNN predictions using 5 nearest neighbors
    v_knn5_train =  knn(d_northstar_knn_train, d_northstar_knn_train, 
                    d_northstar_train$Default, k = 5) #FNN    


#MODEL TUNING

#Evaluate fit of kNN model using k = 3 
  #Add the predicted values to the training data frame.
    d_pred_knn3_train <- mutate(d_northstar_train, Prediction = v_knn3_train) #dplyr
  #Compute/display the Percentage Correct in training data to evaluate fit.
    mean(~(Default == Prediction), data = d_pred_knn3_train) #mosaic
  
  #Create/display Classification Tables for the training data.
    #Classification Table of raw counts.
    tally(Default ~ Prediction, data=d_pred_knn3_train) %>% addmargins() #mosaic
    #Classification Table of percentages of training data.
    tally(Default ~ Prediction, data=d_pred_knn3_train) %>% 
      prop.table(margin=1) %>% round(2) #mosaic  


    #Evaluate fit of kNN model using k = 5 
  #Add the predicted values to the training data frame.
    d_pred_knn5_train <- mutate(d_northstar_train, Prediction = v_knn5_train) #dplyr
  #Compute/display the Percentage Correct in training data to evaluate fit.
    mean(~(Default == Prediction), data = d_pred_knn5_train) #mosaic

  #Create/display Classification Tables for the training data.
    #Classification Table of raw counts.
    tally(Default ~ Prediction, data=d_pred_knn5_train) %>% addmargins() #mosaic
    #Classification Table of percentages of training data.
    tally(Default ~ Prediction, data=d_pred_knn5_train) %>% 
      prop.table(margin=1) %>% round(2) #mosaic
    
#Conclusion
  #The k=5 model has a higher % correct in the training data.
  #So choose the k=5 model.


#MODEL TESTING

#Evaluate accuracy for the k=5 model
  #Get predictions of k=5 model for the test data.
    v_knn5_test =  knn(d_northstar_knn_train, d_northstar_knn_test, 
                       d_northstar_train$Default, k = 5) #FNN
  #Add the predicted values to the test data frame.
    d_pred_knn5_test <- mutate(d_northstar_test, Prediction = v_knn5_test) #dplyr
  #Compute/display the Percentage Correct in test data to evaluate accuracy.
    mean(~(Default == Prediction), data = d_pred_knn5_test) #mosaic
  
  #Create/display Classification Tables for the test data.
    #Classification Table of raw counts.
    tally(Default ~ Prediction, data=d_pred_knn5_test) %>% addmargins() #mosaic
    #Classification Table of percentages of test data.
    tally(Default ~ Prediction, data=d_pred_knn5_test) %>% 
      prop.table(margin=1) %>% round(2) #mosaic

