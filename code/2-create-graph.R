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
  ggsave(filename=paste("out/",variablename,"_barplot.png",sep=""), plot=plot, width=8, height=6,units="in", dpi=300)

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
  ggsave(filename=paste("out/",variablename,"_barplot_facet_gov.png",sep=""), plot=plot, width=8, height=6,units="in", dpi=300)
  
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
  ggsave(filename=paste("out/",variablename,"_barplot_facet_subgov.png",sep=""), plot=plot, width=8, height=6,units="in", dpi=300)
  
### Now let's create propoortion graphs --

  rm(freq)
  ## Simple proportion
  #frequ <- as.data.frame(prop.table(table (data.single[ , i])))
  plotfreq <- ggplot(data.single, aes(data.single[ , i])) +
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
   ggsave(filename=paste("out/",variablename,"_frequency.png",sep=""), plot=plotfreq, width=8, height=10,units="in", dpi=300)
}  

