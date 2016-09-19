## Generation of a exploratory graphs

## Visual exploration of data will provide a quick overview 


##########################################################
## box plot to display interaction between variables

box1 <- ggplot(data, aes(x=Housing.Type_of_residence, y=Household_information.Family_Size, 
                         fill=Household_information.Marital_status) ) + 
  geom_boxplot() 
ggsave("out/box1.png",box1, width=8, height=6, units="in", dpi=300)

box2 <- ggplot(data, aes(x=Housing.Duration_of_stay_same_residenc, y=Household_information.Family_Size, 
                         fill=Housing.Type_of_residence) ) + 
  geom_boxplot() 
ggsave("out/box2.png",box2, width=8, height=6, units="in", dpi=300)

box3 <- ggplot(data, aes(x=Housing.Duration_of_stay_same_residenc, y=Household_information.Family_Size, 
                         fill=Housing.Assessor_How_do_you_evaluate_) ) + 
  geom_boxplot() 
ggsave("out/box3.png",box3, width=8, height=6, units="in", dpi=300)

box4 <- ggplot(data, aes(x=Documentation.members_lacking_documents, y=Household_information.Family_Size, 
                         fill=Housing.Assessor_How_do_you_evaluate_) ) + 
  geom_boxplot() 
ggsave("out/box4.png",box4, width=10, height=5, units="in", dpi=300)


##########################################################
##  Line chart to see evolution over time
lineplot <- ggplot(data, aes(x = start, y = Household_information.Family_Size)) + 
  geom_line() +
  scale_x_date(labels = date_format("%b %Y")) +
  labs(x = "Time (months)", y = "# if Ind") +
  ggtitle("# Individual monitored") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
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
  ggtitle("How Assessor evaluate Housing? # ind")
ggsave("out/barHousingevaluate.png", bar.Housing.Assessor_How_do_you_evaluate_, width=8, height=6,units="in", dpi=300)


#################################################
## We will use barplot for select_one variable

## extracting unique choice questions
#datalabel[datalabel$type=="select_one" ,7]
# datalabel[datalabel$type=="select_mutiple" ,7]

data.single <- data[ ,c("Household_information.Family_Size", "Location.Governorate", "subgov"," Household_information.Sex_of_PA",                               
                        "Household_information.Marital_status",  "Household_information.number_members_changed" ,                 
                        "Household_information.Specify_location_unknown", "Household_information.Has_any_family_member_left_Iraq" ,        
                        "Household_information.left_family_member_go_to", "Housing.Type_of_residence" ,                                    
                        "Housing.Camp_housing_type",  "Housing.sharing_the_residence" ,                           
                        "Housing.Duration_of_stay_same_residenc",  "Housing.If_the_stay_is_less_than_12_mo",                        
                        "Housing.received_assistance_last12mont", "Housing.Assessor_How_do_you_evaluate_",
                        "Livelihood.member_under_18_working", "Livelihood.Monthly_family_income"  ,                            
                        "Livelihood.If_Remittances_specify_source" , "Livelihood.Do_you_have_any_debts" ,                             
                        "Livelihood.received_cash_assist_in12month",  "Livelihood.Have_received_food_12month",                         
                        "Documentation.familiar_with_doc_procedures",  "Access_to_Health_Services.family_have_access_health_serv",      
                        "Access_to_Health_Services.type_family_health_access", "Access_to_Health_Services.Why_not_family_acess_health",         
                        "Access_to_Health_Services.measles_vac", "Access_to_Health_Services.polio_vac",                           
                        "Access_to_Health_Services.vaccination_card", "Specific_Needs.Reason_risk_of_deportation",                     
                        "Specific_Needs.Reason_for_member_detention", "Specific_Needs.Categorization_of_household_vu",                 
                        "Referral.Assessor_dose_the_case_require", "Communication.main_source_information",
                        "Communication.How_do_communicate_needs",
                        
                        ## and now breakdown for multiple choice questions
                        "newly_born" ,   "death_in_the_f","not_registered",  
                        "member_left",   "different_regi","separation_div",  
                        "location_unknown" , "floor_mattress","chairs_traditi",                        
                        "kitchen_set",   "computer",      "internet",     
                        "blankets",      "beds",          "stove",        
                        "washing_machin","cabinets",      "fridge",       
                        "television",    "water_heater",  "water_cooler", 
                        "air_cooler",    "heater",        "running_water",
                        "electricity",   "sanitation_fac", "seperate_cooki",                        
                        "windows",       "main_door",     "weather_sealed" ,                       
                        "proceeds_from_work__monthly", "remittances",   "finical_support_from_relative_",        
                        "no_birth_certi_1" , "residency_docu_1" ,  "marriage_certi_1",                      
                        "valid_unhcr_ce","valid_unhcr_lo","national_passp_1" ,                     
                        "civil_id",  "syrian_identif","syrian_family_" ,                       
                        "1__financial_constraints__tran",  "2__not_feel_safe", "3__not_accepted_at_the_school_",        
                        "4__not_having_the_documents_re" ,"5__disability_medical_conditio", "6__the_child_is_working",               
                        "7__lack_of_arabic_school",  "8__missed_the_registration_dea",
                        "serious_medical_condition", "mental_phy_dis",                        
                        "child_or_adolescent_at_risk" ,  "single_parent", "elderly_at_risk",                       
                        "woman_at_risk", "specific_legal_and_physical_pr" ,  "serious_medica" ,                       
                        "serious_med_ot","psychological_","high_risk_preg"  ,                      
                        "hearing_impair","speech_impairm","sight_impairme" ,                       
                        "mental_disabil","mental_disab_s","physical_dis_s" ,                       
                        "physical_dis_m","unaccompanied_","separated_chil" ,                       
                        "child_no_schoo","child_edu_spe", "child_as_fight",                        
                        "child_spouse",  "worst_form_lab","at_risk_of_dep",                        
                        "detention",     "is_the_family_","physical_prote" ,                       
                        "domestic_viole","torture",  "winterization_", "summarization_"  ,                      
                        "multi_purpose_","cri",           "registration" 
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

## Now we can start plotting in a loop
# the key is at the line here
# p + aes_string(x = names(mydata)[i])
# Use aes_string instead of aes, so that when you look at summary(ggplot_obj), 
# the mapping for x-values that are changing will be the actual string and not a variable i.

p <- ggplot(data.frame(data.single), aes(y=data.single$Household_information.Family_Size)) + ylab("# of Ind") + scale_y_continuous(labels=format_si())
for (i in 3:34 ) {
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
    ggtitle(title)+
    theme(plot.title=element_text(face="bold", size=9),
          plot.background = element_rect(fill = "transparent",colour = NA))
  #assign(paste("plot", variablename, sep=""), plot)
  ggsave(filename=paste("out/plot_",i,variablename,".png",sep=""), plot=plot, width=8, height=6,units="in", dpi=300)
}


## Now trying ot facet by gvt 

p <- ggplot(data.frame(data.single), aes(y=data.single$Household_information.Family_Size)) + 
    ylab("# of Ind") + 
    scale_y_continuous(labels=format_si())
for (i in 3:34 ) {
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
    facet_wrap(~ Location.Governorate , ncol=3) +
    # coord_flip()+
    xlab("") + 
    coord_flip() + 
    ggtitle(title)+
    theme(plot.title=element_text(face="bold", size=9),
          plot.background = element_rect(fill = "transparent",colour = NA))
  #assign(paste("plot", variablename, sep=""), plot)
  ggsave(filename=paste("out/plot_",i,variablename,"facet_gov.png",sep=""), plot=plot, width=8, height=6,units="in", dpi=300)
}


### Now let's create propoortion graphs --

