########################################################################
################# Trying ot get the form nicely with labels 
## https://gist.github.com/mrdwab/28c13a0537044aeb5dc0
########################################################################

#source("code/1-kobo_form_downloader.R")
#kobo_form_downloader (formid, user = usernamepasswordunhcr , api = "https://kobocat.unhcr.org/api/v1/")


## Load survey structure in XLS form

form_tmp <- "data/R_PMT_v1.xls"
survey <- read_excel(form_tmp, sheet = "survey")                        


## Avoid columns without names
survey <- survey[ ,c("type",   "name" ,  "label"# "label::English",
                     #"label::Arabic" ,"hint::Arabic",               
                     # "hint::English", "relevant",  "required", "constraint",   "constraint_message::Arabic", 
                     # "constraint_message::English", "default",  "appearance", "calculation",  "read_only"  ,                
                     # "repeat_count"
)]

## need to delete empty rows from the form
survey <- as.data.frame(survey[!is.na(survey$type), ])

#str(survey)
#levels(as.factor(survey$type))

### We can now extract the id of the list name to reconstruct the full label fo rthe question
survey$listname <- ""
## Extract for select_one 
survey$listname <- with(survey, ifelse(grepl("select_one", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type) ,                                        
                                                       paste0( substr(survey$type , (regexpr("select_one", survey$type , ignore.case=FALSE, fixed=TRUE))+10,250)),survey$listname))


survey$type <- with(survey, ifelse(grepl("select_one", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type), paste0("select_one"),survey$type))
       
## Extract for select multiple & clean type field
survey$listname <- with(survey,  ifelse(grepl("select_multiple", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type),
                           paste0( substr(survey$type , (regexpr("select_multiple", survey$type , ignore.case=FALSE, fixed=TRUE))+16,250)),survey$listname ))
survey$type <- with(survey, ifelse(grepl("select_multiple", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  survey$type), paste0("select_multiple_d"),survey$type))

## Remove space
survey$listname <- trim(survey$listname)
#str(survey)

write.csv (survey, "data/survey.csv")
survey_temp <- survey[ ,c("type",   "name" ,  "label")]




### Bind choices
choices <- read_excel(form_tmp, sheet = "choices")  
names(choices)[4] <- "listname"
choices <- choices[,c("listname",   "name" ,  "label")]
choices1 <- join(x=choices, y=survey, by="listname", type="left")
choices1$type <- with(choices1, ifelse(grepl("select_one", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  choices1$type), paste0("select_one_d"),choices1$type))
choices1$type <- with(choices1, ifelse(grepl("select_multiple_d", ignore.case = TRUE, fixed = FALSE, useBytes = FALSE,  choices1$type), paste0("select_mutiple"),choices1$type))

names(choices1)[5] <- "nameq"
names(choices1)[6] <- "labelq"
choices1$labelfull <- paste0(choices1$labelq, sep = ": ", choices1$label)

names(choices1)
choices2 <- choices1[,c("type",   "name" ,  "labelfull")]
names(choices2)[3] <- "label"

# Remove duplicates based on Sepal.Width columns
choices3 <- choices2[!duplicated(choices2$name), ]

survey_all <- rbind(survey_temp,choices3)

write.csv (survey_all, "data/surveyall.csv")

## get variable name from data
rm(datalabel)
datalabel <- as.data.frame( names(data))
names(datalabel)[1] <- "nameor"
datalabel$nameor <- as.character(datalabel$nameor)

## new variables name without /
datalabel$namenew <- str_replace_all(datalabel$nameor, "/", ".")
## let's recode the variable of the dataset using short label - column 3 of my reviewed labels
#names(data) <- datalabel[, 2]

## Extract the variable name as defined in the form
datalabel$length <- str_length(datalabel$namenew)
#str(datalabel)
## Find the next dot to parse the label 
datalabel$find <- regexpr(".", datalabel$namenew, fixed = TRUE, useBytes = TRUE)
#summary(datalabel$find)
datalabel$nameor2 <- substr(datalabel$namenew,datalabel$find+1, 200)
datalabel$find2 <- regexpr(".",datalabel$nameor2, fixed = TRUE, useBytes = TRUE)
datalabel$nameor3 <- substr(datalabel$nameor2,datalabel$find2 +1, 200)
datalabel$find3 <- regexpr(".",datalabel$nameor3, fixed = TRUE, useBytes = TRUE)
datalabel$nameor4 <- substr(datalabel$nameor3,datalabel$find3 +1, 200)
datalabel$find4 <- regexpr(".",datalabel$nameor4, fixed = TRUE, useBytes = TRUE)
datalabel$nameor5 <- substr(datalabel$nameor4,datalabel$find4 +1, 200)
datalabel$find5 <- regexpr(".",datalabel$nameor5, fixed = TRUE, useBytes = TRUE)
datalabel$nameor6 <- substr(datalabel$nameor5,datalabel$find5 +1, 200)

## backup
datalabel1 <- datalabel


## merging now with lables
datalabel <- datalabel1

names(datalabel)
names(datalabel)[13] <- "name"
datalabel <- join(x=datalabel, y=survey_all, by="name", type="left")
names(datalabel)
names(datalabel)[13]<- "nameor6"

##Check if duplicate
datalabel$dup <- duplicated(datalabel$nameor)

write.csv(datalabel, "data/datalabel.csv")

rm(choices, choices1, choices2, choices3, datalabel1, survey, survey_all, survey_temp, form_tmp)

