#This is a function for a "moving window regression": we're doing a linear regression on a window of 29 years,
#assigning the angular coefficient (sign and modulus) of the interpolating line as the value of the 15th year,
#then we're moving the window to the next year. Necessary condition to give a result is to have ~80% of the data
#for the year, i.e. 23 years on 29 years total.
#Note: you can change the dimension of the window's size changing the parameter window_size.
#Note2: assign an odd window size! Otherwise is not granted the correctness of results.
#Threshold is the min percentage of data to compute the regression (must be between 0 and 1).
#Alpha is the threshold for pvalue, must be between 0 and 1 (corresponding to 0 and 100%).
moving_window <- function(array, time_array, window_size, threshold, alpha){
  initial_array <- array #Assigning the array, typically the result of occorrenze_filtro().
  window_regr <- c()
  window_size <- odd_wsize(window_size) #Checking if window_size is odd
  check_parameter(threshold, 0, 1)  #Check if parameters are out of range
  check_parameter(alpha, 0, 1)
  i <- 1  
  
  while(((i+window_size)-1)<length(initial_array)){
    temp_array <- initial_array
    for(j in 1:length(temp_array)){       #Setting to NA all the elements outside of the window
      if(j<i | j>(i+window_size-1)) temp_array[j] <- NA
      if(j<=floor(window_size/2) | j>=(length(initial_array)-floor(window_size/2))) window_regr[j] <- NA
    }
    
    if(length(which(!is.na(temp_array)))>=(threshold*window_size)){ #Checking the existence of 80% of data
      #     par_lm <- linear_regr(temp_array)       #Doing linear regression on the window
      par_lm <- lm(temp_array ~ time_array)
      bool_p <- pvalue_control(par_lm, alpha)  #p-value should be <5%, else set to 0 (confirmed null hypothesis?)
      if(bool_p==TRUE) window_regr[(ceiling((window_size/2)))+i-1] <- par_lm$coefficients[2] #Assigning angular coefficient
      else window_regr[(ceiling((window_size/2)))+i-1] <- 0 
      #We want the element in the middle: thus we're dividing an odd 
      #number by 2 and approximating to the smallest
      #integer containing the result (e.g., 29/2=14.5 -> 15)  
    }
    
    else window_regr[(ceiling((window_size/2)))+i-1] <- NA
    i <- i+1
  }
  
  return(window_regr)
}