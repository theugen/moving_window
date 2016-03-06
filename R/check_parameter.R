#Check if a parameter is in a range. If not, prints an error and stop execution
check_parameter <- function(parameter, vmin, vmax){
  if(parameter<vmin | parameter>vmax) stop(paste('Parameter ', parameter, ' out of range', sep=''))
}
