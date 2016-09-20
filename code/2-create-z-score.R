## let's compute Z-score for sub-gov to better display on a map the differences between areas...

## First compute average proportion of "yes" for each questions per subgov

## list of subgov
subgov <- as.data.frame(unique(data$subgov))

for (i in 6:133 ) {
  i <- 10
  rm(variablename)
  variablename <- names(data.single)[i]
  ## compute ht e proportion table
  data.proptable <- ddply(data.single,.(subgov),function(x){prop.table(table(x[ , i]))})
  
  ## Rename the column with yes to the current variable name
  names(data.proptable$Yes) <- variablename
  
  ## Merge with the the list of sub gov
  subgov <- cbind (subgov,data.proptable )
  
}

## Then comput the Z-score for subgov and each Governorate
for (i in 84:161 ) {
  data.agreement[, i + 78 ] <- data.agreement[, i ]
  data.agreement[, i + 78 ] <- scale(data.agreement[, i], center = TRUE, scale = TRUE)
}

### Compute centroid per subgov based on average the X and Y coordinates
subgovcentroid <- aggregate(cbind(lat, long) ~ subgov, data = data, FUN = mean, na.rm = TRUE)

## testing a plot of coordinates
plot(subgovcentroid$long, subgovcentroid$lat )

## Now merge 