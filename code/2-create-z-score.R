## let's compute Z-score for sub-gov to better display on a map the differences between areas...

## First compute average proportion of "yes" for each questions per subgov

## list of subgov
subgov <- as.data.frame(unique(data$subgov))
names(subgov)[1] <- "subgov"

for (i in 32:135 ) {
  rm(variablename)
  variablename <- names(data.single)[i]
  ## compute ht e proportion table
  data.proptable <- ddply(data.single,.(subgov),function(x){prop.table(table(x[ , i]))})
  
  ## Rename the column with yes to the current variable name
  names(data.proptable)[names(data.proptable)=="Yes"] <- variablename
  column <- c("subgov", variablename)
  data.proptable <- data.proptable[ column]
  
  ## Merge with the the list of sub gov
  subgov <- join(x=subgov,y=data.proptable, by="subgov", type="left" )
  rm(data.proptable)
}

## Then comput the Z-score for subgov 
subgov.z <- subgov
for (i in 2:101 ) {
  subgov.z[, i  ] <- scale(subgov.z[, i], center = TRUE, scale = TRUE)
}

### Compute centroid per subgov based on average the X and Y coordinates
subgovcentroid <- aggregate(cbind(lat, long) ~ subgov, data = data, FUN = mean, na.rm = TRUE)

## testing a plot of coordinates
#plot(subgovcentroid$long, subgovcentroid$lat )

## Now merge 
subgovcentroid.z <- join(x=subgovcentroid,y=subgov.z, by="subgov", type="left" )

#### Labels are trimmed when subsetting so..
## Let's add again the label for the variable using our dictionnary datalabel
subgovcentroid.z.label <- as.data.frame(names(subgovcentroid.z))
names(subgovcentroid.z.label)[1] <- "name"
datalabel.map <- datalabel[ ,c(7,15)]
names(datalabel.map)[1] <- "name"
subgovcentroid.z.label <- join (x=subgovcentroid.z.label, y=datalabel.map, by="name", type="left" )
attributes(subgovcentroid.z)$variable.labels <- subgovcentroid.z.label$label

rm(datalabel.map, subgovcentroid.z.label, subgov.z, subgovcentroid)
