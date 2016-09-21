################################################################
### Uncomment to load the packages used in this analysis
#lab.packages <- c("lattice",  "car","ggplot2","extrafont","ggthemes","zoo","reshape2",
#"maptools","rgdal","rgeos","ggmap","sp","hexbin",")
#install.packages(pkgs=lab.packages)

##This should detect and install missing packages before loading them – 
packages <- c(
  "dplyr",  "data.table", "doBy", ## Data manipulation
  "reshape2", # package to easily melt data to long form
  #"Hmisc", # generate a detailled describtion of a given dataset 
  #"formatR", #,  used to format the code
  "ggplot2", ## advanced graphics
  "ggrepel", ## Add labels in ggplot2 scatterplot
  "ggthemes", ## load different cusotmised themes for ggplot2: excel, stata, economist, tufte, wall street journal...
  "grid", "gridExtra","scales", # package for elegant data visualization using the Grammar of Graphics
  #"vcd", # Visualisation of categorical data
  "RColorBrewer", # a package offering color palette from 
  #"extrafont", ##" load additional font
  "sp","maptools","rgdal","rgeos", ## standard Geo manipulation packages
  "ggmap", ## get background from webmapping API
  "hexbin", ## Hexagrid viz
  "raster","cartography", ## packages used for the maps --
  "classInt",  ## used for univariate classification
  "lubridate","date","gdata", ## playing with date
  #"lme4", "lmtest", "car", "caret",  ## used for regressions
 # "AER",  # interesting datasets
  #"lattice", # Visualisation
 #"ade4",  ## multivariate analysis
 # "survival", # survival analysis 
 # "sqldf", "RODBC",  ## Direct connection with databases
  "stringr", # manipulation of string data
  "XML",  ## Manipulation of xml
  "tm", ## text mining
  "rJava", "XLConnect", ## Read and write excel files
  "cluster", ## Cluster analysis
  #"foreign", ## read data from SPSS, SAS or Stata  
 # "parallel", "httr", "rjson", 
 # "MASS", "gvlma", "VGAM", "aod", "fields", 
 # "scatterplot3d",  "psych",  "ellipse",   "pastecs",
  "FactoMineR", ## Multiple Correspondance analysis
 # "rattle",
  "devtools", # package used to load packages hosted in github -- install CURL before and separately
  "xkcd" ## Style from the xkcd comics 
)

## identify packages not installed yet
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

rm(packages)

# loads packages into memory
#library(lattice)
#library(rattle)
#library(car)
library(plyr)
library(ggplot2) ## The grammar of graphics!
library(ggrepel) ## add labels in ggplot2 scatterplot
library(extrafont) ## Additional fonts
library(ggthemes) ## Additional themes for gplot2
library(zoo) ## Manage reformatting of date
library(reshape2) ## Restructure data between wide and long format before plotting them - melt and cast
library(maptools) ## Create maps
library(rgdal) ## Open geographic files
#library(rgeos)
library(ggmap) ## get background map from google map
library(sp) ## Spatial library
#library(raster) ## Managing raster dataset
library(RColorBrewer) ## Color palette
library(classInt) ## Classififcation
library(hexbin) ## Hexa binning

#gpclibPermit()
library(lubridate)
library(date)
library(gdata)
library(grid)
library(gridExtra)
library(scales)
#library(formatR)
#library(RGtk2)
#library(gWidgetsRGtk2)


library(readxl)
library(plyr)
#library(xlsx)

library(FactoMineR)

library(stringr)

## gui for Code reformatting
## tidy.gui('RGtk2')

#
format_si <- function(...) {
  # Format a vector of numeric values according
  # to the International System of Units.
  # http://en.wikipedia.org/wiki/SI_prefix
  #
  # Based on code by Ben Tupper
  # https://stat.ethz.ch/pipermail/r-help/2012-January/299804.html
  # Args:
  #   ...: Args passed to format()
  #
  # Returns:
  #   A function to format a vector of strings using
  #   SI prefix notation
  #
  
  function(x) {
    limits <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12,
                1e-9,  1e-6,  1e-3,  1e0,   1e3,
                1e6,   1e9,   1e12,  1e15,  1e18,
                1e21,  1e24)
    prefix <- c("y",   "z",   "a",   "f",   "p",
                "n",   "µ",   "m",   " ",   "k",
                "M",   "G",   "T",   "P",   "E",
                "Z",   "Y")
    
    # Vector with array indices according to position in intervals
    i <- findInterval(abs(x), limits)
    
    # Set prefix to " " for very small values < 1e-24
    i <- ifelse(i==0, which(limits == 1e0), i)
    
    paste(format(round(x/limits[i], 1),
                 trim=TRUE, scientific=FALSE, ...),
          prefix[i])
  }
}


# Function that will sum values even if we have NA
psum <- function(..., na.rm=FALSE) {
  x <- list(...)
  rowSums(matrix(unlist(x), ncol=length(x)), na.rm=na.rm)
}


### Customised theme
### http://docs.ggplot2.org/dev/vignettes/themes.html


