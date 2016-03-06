#Function to check if window_size in moving_window() is odd: if it's even, forces it to the biggest
#odd number contained in the actual window_size
odd_wsize <- function(wsize){
  if(wsize%%2==0){
    warning('Even window size! Size will be coerced to first odd integer contained in the actual size.')
    wsize <- (wsize -1)
  }
  return(wsize)
}
