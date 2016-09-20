## Generation of a exploratory graphs

## Visual exploration of data will provide a quick overview 


##########################################################
## box plot to display interaction between variables

box1 <- ggplot(data, aes(x=Housing.Type_of_residence, y=Household_information.Family_Size, 
                         fill=Household_information.Marital_status) ) + 
  geom_boxplot() +
  coord_fixed() + ##used to maintain the adspect ratio of the plot when it needs to be saved
  theme(aspect.ratio=4/3,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = rel(1.2), face = "bold"))
ggsave("out/box12.png",box1, width=8, height=6, units="in", dpi=300)

box2 <- ggplot(data, aes(x=Housing.Duration_of_stay_same_residenc, y=Household_information.Family_Size, 
                         fill=Housing.Type_of_residence) ) + 
  geom_boxplot() + 
  coord_fixed() ##used to maintain the adspect ratio of the plot when it needs to be saved
ggsave("out/box2.png",box2, width=8, height=6, units="in", dpi=300)

box3 <- ggplot(data, aes(x=Housing.Duration_of_stay_same_residenc, y=Household_information.Family_Size, 
                         fill=Housing.Assessor_How_do_you_evaluate_) ) + 
  geom_boxplot() + 
  coord_fixed() ##used to maintain the adspect ratio of the plot when it needs to be saved
ggsave("out/box3.png",box3, width=8, height=6, units="in", dpi=300)

box4 <- ggplot(data, aes(x=Documentation.members_lacking_documents, y=Household_information.Family_Size, 
                         fill=Housing.Assessor_How_do_you_evaluate_) ) + 
  geom_boxplot() + 
  coord_fixed() ##used to maintain the adspect ratio of the plot when it needs to be saved
ggsave("out/box4.png",box4, width=10, height=5, units="in", dpi=300)


rm(box1,box1,box2,box3,box4 )

##########################################################
##  Line chart to see evolution over time
lineplot <- ggplot(data, aes(x = start, y = Household_information.Family_Size)) + 
  geom_line() +
  scale_x_date(labels = date_format("%b %Y")) +
  labs(x = "Time (months)", y = "# if Ind") +
  ggtitle("# Individual monitored") +
  theme_bw() + 
  coord_fixed() + ##used to maintain the adspect ratio of the plot when it needs to be saved
  theme(aspect.ratio=4/3,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = rel(1.2), face = "bold"))
lineplot
ggsave("out/lineplot.png",lineplot, width=10, height=5, units="in", dpi=300)





###### Testing a first graph with GGPlot2
#### Bar graph to show repartition for categories
bar.Housing.Assessor_How_do_you_evaluate_ <- ggplot(data=data, aes(x=reorder(Housing.Assessor_How_do_you_evaluate_,Housing.Assessor_How_do_you_evaluate_,
                                                 function(x)-length(x)) , y=Household_information.Family_Size)) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
  # geom_text(aes(label=variable), vjust=0) +
  guides(fill=FALSE) + 
  facet_wrap(~ Location.Governorate , ncol=3) +
  # coord_flip()+
  xlab("Housing.Assessor_How_do_you_evaluate_") + 
  ylab("# of Ind") +
  scale_y_continuous(labels=format_si())+
  coord_flip()+ 
  ggtitle("How Assessor evaluate Housing? # ind") +
  coord_fixed() + ##used to maintain the adspect ratio of the plot when it needs to be saved
  theme(aspect.ratio=4/3,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = rel(1.2), face = "bold"))
ggsave("out/barHousingevaluate2.png", bar.Housing.Assessor_How_do_you_evaluate_, width=8, height=6,units="in", dpi=300)


rm(bar.Housing.Assessor_How_do_you_evaluate_ )

#################################################
## Generate a series of barplot for each variable

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
                        "Consent_obtained",
                        "Location.assessment_camp_noncamp",
                        "Household_information.Sex_of_PA",
                        "Household_information.Marital_status",
                        "Household_information.number_members_changed",
                        "Household_information.Why_actual_size_different.newly_born",
                        "Household_information.Why_actual_size_different.death_in_the_f",
                        "Household_information.Why_actual_size_different.not_registered",
                        "Household_information.Why_actual_size_different.member_left",
                        "Household_information.Why_actual_size_different.different_regi",
                        "Household_information.Why_actual_size_different.separation_div",
                        "Household_information.Why_actual_size_different.location_unknown",
                        "Household_information.Specify_location_unknown",
                        "Household_information.Has_any_family_member_left_Iraq",
                        "Household_information.left_family_member_go_to",
                        "Housing.Type_of_residence",
                        "Housing.Camp_housing_type",
                        "Housing.sharing_the_residence",
                        "Housing.Duration_of_stay_same_residenc",
                        "Housing.If_the_stay_is_less_than_12_mo",
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
                        "Housing.Assessor_How_do_you_evaluate_",
                        "Livelihood.member_under_18_working",
                        "Livelihood.Monthly_family_income",
                        "Livelihood.Source_of_income.proceeds_from_work__monthly",
                        "Livelihood.Source_of_income.remittances",
                        "Livelihood.Source_of_income.finical_support_from_relative_",
                        "Livelihood.If_Remittances_specify_source",
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
                        "Access_to_Health_Services.family_have_access_health_serv",
                        "Access_to_Health_Services.type_family_health_access",
                        "Access_to_Health_Services.Why_not_family_acess_health",
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
                        "Specific_Needs.Reason_risk_of_deportation",
                        "Specific_Needs.Reason_risk_of_deportation.other",
                        "Specific_Needs.Reason_for_member_detention",
                        "Specific_Needs.Reason_for_member_detention.other",
                        "Specific_Needs.Categorization_of_household_vu",
                        "Referral.Assessor_dose_the_case_require",
                        "Referral.Assessor_recom_assist_referral.winterization_",
                        "Referral.Assessor_recom_assist_referral.summarization_",
                        "Referral.Assessor_recom_assist_referral.multi_purpose_",
                        "Referral.Assessor_recom_assist_referral.cri",
                        "Referral.Please_refer_to_lawyer_unless_",
                        "Referral.auto_refer_registration.registration",
                        "Referral.Referral_to_change_registratio",
                        "Referral.understand_the_implicat_locati",
                        "Communication.main_source_information",
                        "Communication.main_source_information.other",
                        "Communication.How_do_communicate_needs"
                        )]

## Remove variable where we get only NA
data.single <- data.single[,colSums(is.na(data.single))<nrow(data.single)]
#names(data.single)

## Let's add again the label for the variable
data.single.label <- as.data.frame(names(data.single))
names(data.single.label)[1] <- "name"
datalabel.map <- datalabel[ ,c(7,15)]
names(datalabel.map)[1] <- "name"
data.single.label <- join (x=data.single.label, y=datalabel.map, by="name", type="left" )
attributes(data.single)$variable.labels <- data.single.label$label

## Now for select_multiple questions, convert 1/NUL to Yes/No
## ### Now we recode all variables through a loop
#str(datasp)
for (i in 6:133 ) {
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

## Now we can start plotting in a loop
# the key is at the line here
# p + aes_string(x = names(mydata)[i])
# Use aes_string instead of aes, so that when you look at summary(ggplot_obj), 
# the mapping for x-values that are changing will be the actual string and not a variable i.

p <- ggplot(data.frame(data.single) , aes(y=data.single$Household_information.Family_Size)
            ) +
      ylab("# of Ind") +
      scale_y_continuous(labels=format_si())

for (i in 6:133 ) {
  rm(variablename)
  variablename <- names(data.single)[i]
  title <- attributes(data.single)$variable.labels[i]
  rm(plot)
  plot <- p + 
     aes_string(x = names(data.single)[i]) +
   # aes_string(x = reorder( names(data.single)[i], names(data.single)[i], function(x)-length(x)))  +
    xlab(colnames(data.single[i])) + 
    geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8")  +
    guides(fill=FALSE) + 
    # coord_flip()+
     xlab("") + 
    coord_flip() + 
    #coord_fixed() + ##used to maintain the adspect ratio of the plot when it needs to be saved
    ggtitle(title) +
    theme(plot.title=element_text(face="bold", size=9),
          plot.background = element_rect(fill = "transparent",colour = NA))
  #assign(paste("plot", variablename, sep=""), plot)
  ggsave(filename=paste("out/",i,"_",variablename,"_barplot.png",sep=""), plot=plot, width=8, height=6,units="in", dpi=300)

## Now generating the same series of graph with "facet" by Governorate 
  rm(plot)
  plot <- p + 
    aes_string(x = names(data.single)[i]) +
    # aes_string(x = reorder( names(data.single)[i], names(data.single)[i], function(x)-length(x)))  +
    xlab(colnames(data.single[i])) + 
    geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8")  +
    guides(fill=FALSE) + 
    facet_wrap(~ Location.Governorate , ncol=3) +
    xlab("") + 
    coord_flip() + 
   # coord_fixed() + ##used to maintain the adspect ratio of the plot when it needs to be saved
    ggtitle(title)+
    theme(plot.title=element_text(face="bold", size=9),
          plot.background = element_rect(fill = "transparent",colour = NA))
  #assign(paste("plot", variablename, sep=""), plot)
  ggsave(filename=paste("out/",i,"_",variablename,"_barplot_facet_gov.png",sep=""), plot=plot, width=8, height=6,units="in", dpi=300)
  
  ## Now generating the same series of graph with "facet" by Governorate 
  rm(plot)
  plot <- p + 
    aes_string(x = names(data.single)[i]) +
    # aes_string(x = reorder( names(data.single)[i], names(data.single)[i], function(x)-length(x)))  +
    xlab(colnames(data.single[i])) + 
    geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8")  +
    guides(fill=FALSE) + 
    facet_wrap(~ subgov , ncol=3) +
    xlab("") + 
    coord_flip() + 
    # coord_fixed() + ##used to maintain the adspect ratio of the plot when it needs to be saved
    ggtitle(title)+
    theme(plot.title=element_text(face="bold", size=9),
          plot.background = element_rect(fill = "transparent",colour = NA))
  #assign(paste("plot", variablename, sep=""), plot)
  ggsave(filename=paste("out/",i,"_",variablename,"_barplot_facet_subgov.png",sep=""), plot=plot, width=8, height=6,units="in", dpi=300)
  
}


### Now let's create propoortion graphs --
for (i in 6:133 ) {
  rm(variablename)
  variablename <- names(data.single)[i]
  title <- attributes(data.single)$variable.labels[i]
  rm(freq)
  ## Simple proportion
  #frequ <- as.data.frame(prop.table(table (data.single[ , i])))
  plot <- ggplot(data.single, aes(data.single[ , i])) +
    geom_bar(aes(y = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x]))),fill="#2a87c8",colour="#2a87c8") +
    facet_wrap(~subgov, ncol=4) +
   guides(fill=FALSE) + 
   ylab("Frequency") +
   xlab("") + 
   coord_flip() + 
   # coord_fixed() + ##used to maintain the adspect ratio of the plot when it needs to be saved
   ggtitle(title)+
   theme(plot.title=element_text(face="bold", size=9),
         plot.background = element_rect(fill = "transparent",colour = NA))
   ggsave(filename=paste("out/",i,"_",variablename,"_frequency.png",sep=""), plot=plot, width=8, height=10,units="in", dpi=300)
}  

