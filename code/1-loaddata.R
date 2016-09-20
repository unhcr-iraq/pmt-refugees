rm(list = ls())


################################################################
## Load all required packages
source("code/0-packages.R")


################################################################
## Load the data dump from the data collection server -- here we skip the first line
rm(data)
data <- read.csv("data/data.csv", encoding="UTF-8", na.strings="n/a", skip=1)


################################################################
## Special script to generate lables based on the data & form definition 
source("code/1-get-labels-from-form.R")

## Now we can apply better names & labels to the datframe
names(data) <- datalabel[ , 7]
attributes(data)$variable.labels <- datalabel[ , 15]

#names(data)

################################################################
####  extracting coordinates from dataset
data$geo <- as.character(data$GPS_location)
options(digits = 15)
data$lat <- as.numeric(substr(data$geo , 1,13))
data$long <- as.numeric(substr( data$geo, 15,27))

## Create a variable that sum up all subdistrict & camp
data$subgov <- paste0(data$Location.Erbil_districts, data$Location.Suli_districts, data$Location.Dahuk_districts, data$Location.camp_name)


################################################################
### Recognise date from format: "01-01-2009 03:41:25"
str(data$end)
data$start1 <- as.Date(as.character(data$start), "%d-%m-%Y %h:%m:%s")
data$end1 <- as.Date(as.character(data$end), format = "%d-%m-%Y %h:%m:%s")


#str(data)

################################################################
### Set up the correct order for the factor variables so that graphs are correctly displayed

################################################################
## extracting unique choice questions -- 
data.single.label <- as.data.frame(datalabel[datalabel$type %in% c("select_one", "select_mutiple") ,7])

## Export in CSV so that a manual Review of this file can be performed in order to get a list of selected variables to map
write.csv(data.single.label, "data/datasinglelabel.csv")
rm(data.single.label)

names(data)

data.single <- data[ , c("Location.Governorate", "subgov", 
                         #)] 
                         "lat", "long",
                         "Household_information.Family_Size",
                         ## Questions with multiple choices - put them here so that the grah generation in loop works well
                         
                         
                         "Location.assessment_camp_noncamp",
                         "Household_information.Sex_of_PA",
                         "Household_information.Marital_status",
                         
                         "Access_to_Health_Services.family_have_access_health_serv",
                         "Access_to_Health_Services.type_family_health_access",
                         "Access_to_Health_Services.Why_not_family_acess_health",
                         "Housing.Assessor_How_do_you_evaluate_",
                         
                         "Household_information.left_family_member_go_to",
                         "Housing.Type_of_residence",
                         "Housing.Camp_housing_type",
                         "Housing.Duration_of_stay_same_residenc",
                         "Housing.If_the_stay_is_less_than_12_mo",
                         "Household_information.Specify_location_unknown",
                        
                         
                         "Livelihood.Monthly_family_income",
                         "Livelihood.If_Remittances_specify_source",
                         "Specific_Needs.Categorization_of_household_vu",
                         
                         "Specific_Needs.Reason_risk_of_deportation",
                         "Specific_Needs.Reason_risk_of_deportation.other",
                         "Specific_Needs.Reason_for_member_detention",
                         "Specific_Needs.Reason_for_member_detention.other",
                         "Referral.Assessor_dose_the_case_require",
                         
                         "Communication.main_source_information",
                         "Communication.main_source_information.other",
                         "Communication.How_do_communicate_needs",
                         
                         ## From here we only have Yes / Na - No questions
                         "Consent_obtained",
                         "Household_information.number_members_changed",
                         "Household_information.Why_actual_size_different.newly_born",
                         "Household_information.Why_actual_size_different.death_in_the_f",
                         "Household_information.Why_actual_size_different.not_registered",
                         "Household_information.Why_actual_size_different.member_left",
                         "Household_information.Why_actual_size_different.different_regi",
                         "Household_information.Why_actual_size_different.separation_div",
                         "Household_information.Why_actual_size_different.location_unknown",
                         "Household_information.Has_any_family_member_left_Iraq",
                         "Housing.sharing_the_residence",
                         "Housing.What_assets_do_you_have_Selec.floor_mattress",
                         "Housing.What_assets_do_you_have_Selec.chairs_traditi",
                         "Housing.What_assets_do_you_have_Selec.kitchen_set",
                         "Housing.What_assets_do_you_have_Selec.computer",
                         "Housing.What_assets_do_you_have_Selec.internet",
                         "Housing.What_assets_do_you_have_Selec.blankets",
                         "Housing.What_assets_do_you_have_Selec.beds",
                         "Housing.What_assets_do_you_have_Selec.stove",
                         "Housing.What_assets_do_you_have_Selec.washing_machin",
                         "Housing.What_assets_do_you_have_Selec.cabinets",
                         "Housing.What_assets_do_you_have_Selec.fridge",
                         "Housing.What_assets_do_you_have_Selec.television",
                         "Housing.What_assets_do_you_have_Selec.water_heater",
                         "Housing.What_assets_do_you_have_Selec.water_cooler",
                         "Housing.What_assets_do_you_have_Selec.air_cooler",
                         "Housing.What_assets_do_you_have_Selec.heater",
                         "Housing.received_assistance_last12mont",
                         "Housing.Housing_key_requiremnets.running_water",
                         "Housing.Housing_key_requiremnets.electricity",
                         "Housing.Housing_key_requiremnets.sanitation_fac",
                         "Housing.Housing_key_requiremnets.seperate_cooki",
                         "Housing.Housing_key_requiremnets.windows",
                         "Housing.Housing_key_requiremnets.main_door",
                         "Housing.Housing_key_requiremnets.weather_sealed",
                         "Livelihood.member_under_18_working",
                         "Livelihood.Source_of_income.proceeds_from_work__monthly",
                         "Livelihood.Source_of_income.remittances",
                         "Livelihood.Source_of_income.finical_support_from_relative_",
                         "Livelihood.Do_you_have_any_debts",
                         "Livelihood.received_cash_assist_in12month",
                         "Livelihood.Have_received_food_12month",
                         "Documentation.members_lacking_documents.no_birth_certi_1",
                         "Documentation.members_lacking_documents.residency_docu_1",
                         "Documentation.members_lacking_documents.marriage_certi_1",
                         "Documentation.members_lacking_documents.valid_unhcr_ce",
                         "Documentation.members_lacking_documents.valid_unhcr_lo",
                         "Documentation.members_lacking_documents.national_passp_1",
                         "Documentation.members_lacking_documents.civil_id",
                         "Documentation.members_lacking_documents.syrian_identif",
                         "Documentation.members_lacking_documents.syrian_family_",
                         "Documentation.familiar_with_doc_procedures",
                         "Education.Why_not_all_children_go_school.1__financial_constraints__tran",
                         "Education.Why_not_all_children_go_school.2__not_feel_safe",
                         "Education.Why_not_all_children_go_school.3__not_accepted_at_the_school_",
                         "Education.Why_not_all_children_go_school.4__not_having_the_documents_re",
                         "Education.Why_not_all_children_go_school.5__disability_medical_conditio",
                         "Education.Why_not_all_children_go_school.6__the_child_is_working",
                         "Education.Why_not_all_children_go_school.7__lack_of_arabic_school",
                         "Education.Why_not_all_children_go_school.8__missed_the_registration_dea",
                         "Education.Why_not_all_children_go_school.other",
                         
                         "Access_to_Health_Services.Why_not_family_acess_health.other",
                         "Access_to_Health_Services.measles_vac",
                         "Access_to_Health_Services.polio_vac",
                         "Access_to_Health_Services.vaccination_card",
                         "Specific_Needs.Select_the_applicable_specific.serious_medical_condition",
                         "Specific_Needs.Select_the_applicable_specific.mental_phy_dis",
                         "Specific_Needs.Select_the_applicable_specific.child_or_adolescent_at_risk",
                         "Specific_Needs.Select_the_applicable_specific.single_parent",
                         "Specific_Needs.Select_the_applicable_specific.elderly_at_risk",
                         "Specific_Needs.Select_the_applicable_specific.woman_at_risk",
                         "Specific_Needs.Select_the_applicable_specific.specific_legal_and_physical_pr",
                         "Specific_Needs.Specify_the_serious_medical_co.serious_medica",
                         "Specific_Needs.Specify_the_serious_medical_co.serious_med_ot",
                         "Specific_Needs.Specify_the_serious_medical_co.psychological_",
                         "Specific_Needs.Specify_the_serious_medical_co.high_risk_preg",
                         "Specific_Needs.Specify_the_Mental_physical_d.hearing_impair",
                         "Specific_Needs.Specify_the_Mental_physical_d.speech_impairm",
                         "Specific_Needs.Specify_the_Mental_physical_d.sight_impairme",
                         "Specific_Needs.Specify_the_Mental_physical_d.mental_disabil",
                         "Specific_Needs.Specify_the_Mental_physical_d.mental_disab_s",
                         "Specific_Needs.Specify_the_Mental_physical_d.physical_dis_s",
                         "Specific_Needs.Specify_the_Mental_physical_d.physical_dis_m",
                         "Specific_Needs.Specify_the_child_or_adolescen.unaccompanied_",
                         "Specific_Needs.Specify_the_child_or_adolescen.separated_chil",
                         "Specific_Needs.Specify_the_child_or_adolescen.child_no_schoo",
                         "Specific_Needs.Specify_the_child_or_adolescen.child_edu_spe",
                         "Specific_Needs.Specify_the_child_or_adolescen.child_as_fight",
                         "Specific_Needs.Specify_the_child_or_adolescen.child_spouse",
                         "Specific_Needs.Specify_the_child_or_adolescen.worst_form_lab",
                         "Specific_Needs.Specific_legal_and_physical_pr.at_risk_of_dep",
                         "Specific_Needs.Specific_legal_and_physical_pr.detention",
                         "Specific_Needs.Specific_legal_and_physical_pr.is_the_family_",
                         "Specific_Needs.Specific_legal_and_physical_pr.physical_prote",
                         "Specific_Needs.Specific_legal_and_physical_pr.domestic_viole",
                         "Specific_Needs.Specific_legal_and_physical_pr.torture",
                         "Referral.Assessor_recom_assist_referral.winterization_",
                         "Referral.Assessor_recom_assist_referral.summarization_",
                         "Referral.Assessor_recom_assist_referral.multi_purpose_",
                         "Referral.Assessor_recom_assist_referral.cri",
                         "Referral.Please_refer_to_lawyer_unless_",
                         "Referral.auto_refer_registration.registration",
                         "Referral.Referral_to_change_registratio",
                         "Referral.understand_the_implicat_locati"
)]


## Remove variable where we get only NA
data.single <- data.single[,colSums(is.na(data.single))<nrow(data.single)]
#names(data.single)
## print the structure
#str(data.single, list.len=ncol(data.single))

#### Labels are trimmed when subsetting so..
## Let's add again the label for the variable using our dictionnary datalabel
data.single.label <- as.data.frame(names(data.single))
names(data.single.label)[1] <- "name"
datalabel.map <- datalabel[ ,c(7,15)]
names(datalabel.map)[1] <- "name"
data.single.label <- join (x=data.single.label, y=datalabel.map, by="name", type="left" )
attributes(data.single)$variable.labels <- data.single.label$label

rm(datalabel.map, data.single.label)

## Now for select_multiple questions, convert 1/NUL to Yes/No
## ### Now we recode all variables through a loop
#str(datasp)
for (i in 30:133 ) {
  ## convert to Factor to Revalue - 
  data.single[, i] <- as.factor(data.single[, i])
  data.single[, i] <- revalue(data.single[, i], c( "1"= "Yes", "yes"= "Yes",  "0"= "No",  "no"= "No"))
  
  ## convert to character to re-encode N|A response  
  data.single[, i] <- as.character(data.single[, i])
  # Rencode No Response
  data.single[, i][is.na(data.single[, i])] <- "No Response"
  ## Reconver to Factor
  data.single[, i] <- as.factor(data.single[, i])
}



################################################################
## check some aggregated figures for the assessment


## Build aggregation to be compared with Registration data
data.gov <- aggregate(cbind(Household_information.Family_Size) ~ Location.Governorate, data = data, FUN = sum, na.rm = TRUE)
data.subgov <- aggregate(cbind(Household_information.Family_Size) ~ subgov, data = data, FUN = sum, na.rm = TRUE)


