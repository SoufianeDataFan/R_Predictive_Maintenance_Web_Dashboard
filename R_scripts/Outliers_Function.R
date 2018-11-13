calcul.IQR <-  function(x) {
  iqrV <- IQR(x, na.rm=TRUE)
  iqrV }

uper.interval <- function(x,y) {
  up.inter <- mean(x, na.rm=TRUE)+6*(y) 
  up.inter}

lower.interval <- function(x,y) {
  low.inter <- mean(x, na.rm=TRUE)-6*(y)
  low.inter}

functionData <- function(x,h,l) {
  out <- ifelse(x > h, NA, ifelse(x < l, NA, x))
  out}


outlier.fun <- function(column1) {
  med_data <- median(column1, na.rm=TRUE)
  cal_IQR <- calcul.IQR(column1)
  up_data <- uper.interval(med_data, cal_IQR)
  low_data <- lower.interval(med_data, cal_IQR)
  column_without_outliers <- functionData(column1, up_data, low_data)
  
  return(column_without_outliers)
}

