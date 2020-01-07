##loading libraries
library(shiny)
library(readxl)
library(varhandle)
library(reshape2)
library(tidyverse)
library(factoextra)
library(FactoMineR)

##Preparing Data
#Chart 1

prevalence_data <- read.csv("Data/prevalence_table.csv")
colnames(prevalence_data) <- c("Prevalence", "Population", "Percentage_Within_Race", "Year", "Race")
prevalence_data[,1] <- unfactor(prevalence_data[,1])
prevalence_data[,1] <- as.numeric(gsub(",","",prevalence_data[,1]))
prevalence_data[,2] <- unfactor(prevalence_data[,2])
prevalence_data[,2] <- as.numeric(gsub(",","",prevalence_data[,2]))




#Chart 2
dwt<- read_xls("Data/esrd1.xls", sheet= 5)
dwt1<- dwt[-(1:3),]
dwt2<- t(dwt1)
dfdwt2<- as.data.frame(dwt2)
dfdwt2<- dfdwt2[-1,]
white1<- as.numeric(paste(dfdwt2$V32))
black1<- as.numeric(paste(dfdwt2$V33))
time<- as.numeric(paste(dfdwt2$V1))
df<- as.data.frame(cbind(white1, black1, time))
df1<- melt(df, id.vars = "time")

#Chart 3
esrd<- read.csv("Data/esrd5.csv")
esrd<-as.data.frame(esrd)
esrdnongender<- esrd[, -c(2,3)]
map1<- esrdnongender%>%
  filter(nephrology.care== "0 or <6")


#Regression for median age
median <- read.csv("Data/median.csv")
median <- as.data.frame(median)
t_median <- t(median)  
colnames(t_median) <- rownames(median)  
rownames(t_median) <- colnames(median)
race_subset <- as.data.frame(t_median[,31:44]) 
race_subset[[1]] <-as.numeric(race_subset[[1]])
race_subset[[2]] <-as.numeric(race_subset[[2]])
race_subset[[3]] <-as.numeric(race_subset[[3]])
race_subset[[4]] <-as.numeric(race_subset[[4]])
race_subset[[5]] <-as.numeric(race_subset[[5]])
race_subset[[6]] <-as.numeric(race_subset[[6]])
race_subset[[7]] <-as.numeric(race_subset[[7]])
race_subset[[8]] <-as.numeric(race_subset[[8]])
race_subset[[9]] <-as.numeric(race_subset[[9]])
race_subset[[10]] <-as.numeric(race_subset[[10]])
race_subset[[11]] <-as.numeric(race_subset[[11]])
race_subset[[12]] <-as.numeric(race_subset[[12]])
race_subset[[13]] <-as.numeric(race_subset[[13]])
race_subset[[14]] <-as.numeric(race_subset[[14]])
colnames(race_subset) <- (c('White', 'Black', 'AI/AN', 'Asian', 'NH/PI', 'Other/Multi', 'Unknown',
                            'None_given', 'Hispanic', 'Non-Hispanic', 'Unknown', 'None_given2 ', 
                            'Non-Hispanic White', 'Non-Hispanic Black'))
race_subset <- race_subset[2:22,c(1:6, 9:10, 13:14)]
race_subset_2 <- race_subset
race_subset_2 <- race_subset_2 %>%
  mutate(id = c(1996:2016)) %>%
  gather(key, value, -id) 
lm(race_subset_2$value ~ factor(race_subset_2$key))
#summaryreg<- summary(reg1)

# Preparing USmap and other plots
West_States<- c("AZ", "AK", "CA", "CO", "HI", "ID", "MT", "NV", "NM", "OR", "UT",
                "WA", "WY")
NorthEast_States<- c("CT", "DE", "ME", "MA", "NJ", "NH", "NY", "PA", "RI", "VT")
MidWest_States<- c("IL", "IA", "KS", "IN", "KY", "MI", "MO", "MN", "NE", 
                   "ND", "OH", "SD", "WI")
South_States<- c("AR", "AL", "FL", "DC", "GA", "LA", "MD","MS", 
                 "NC", "OK", "TN", "SC", "TX", "VA", "WV")

#c("GA", "FL", "AL", "LA", "MS", "SC", "TN", "AR", "NC")

library (usmap)
DFC_FAC2018<- read.csv("
                       Data/DFC_SOCRATA_FAC_DATA.csv")
star_a<- subset(DFC_FAC2018, select = DFC_FAC_FeatureSelection) %>%
  drop_na(Five.Star)%>%   
  mutate(HighStar= if_else(Five.Star<=3, 0, 1))
zxcv<- star_a[, c(1,3,7)] %>% mutate(GeoRegion= case_when(
  NETWORK<5 ~ "NorthEast",
  (NETWORK <13 & NETWORK>8) ~ "MidWest",
  NETWORK>14 ~ "West",
  TRUE ~ "South"
))
zxcv$GeoRegion<-as.factor(zxcv$GeoRegion)

zxcv_agg <- aggregate(zxcv[, 2],
                      by = list(zxcv$State),
                      FUN = mean)
zxcv_bystate <- aggregate(zxcv[, -c(1,3,4)],
                          by = list(zxcv$State),
                          FUN = sum)
colnames(zxcv_agg)<- c("state", "AvgFiveStar")
# END Preparing USmap

# Start Preparing kmeans
DFC_FAC_FeatureSelection<- c(2,3,5,7:12,14:21,50:51,53:57,61,63,65,68:75,85:91,93:99,101)
profit_fac<- subset(DFC_FAC2018, Profit.or.Non.Profit == "Profit",
                    select = DFC_FAC_FeatureSelection)
nonprofit_fac<- subset(DFC_FAC2018, Profit.or.Non.Profit == "Non-Profit", 
                       select = DFC_FAC_FeatureSelection)
a<- rbind(profit_fac, nonprofit_fac) %>% drop_na(Standard.Infection.Ratio)
FAC_ForCluster<- a[c(12,7,1,3,10, 14,15,30,33,37,40,45,48,49,50, 2,4 )] %>%
  drop_na(c(1:17))
FAC_ChainCluster<- FAC_ForCluster[, -c(2:5, 16, 17)]
kmeans.cluster<- FAC_ChainCluster[, -1] %>%kmeans(centers= 3)
# END Preparing kmeans

### NEW STUFF ###
shinyApp(
  shinyUI(
    navbarPage("My Application",
               tabPanel("Racial Disparity Analysis", uiOutput('page1')),
               tabPanel("Avg Care Quality", uiOutput('page2')),
               tabPanel("Cluster Viz", 
                  mainPanel(plotOutput('page3'))
              )
        )
  ),
  shinyServer(function(input, output, session) {
    output$page1 <- renderUI({
        HTML(
          paste(
            h3("Prevalence of ESRD Within Racial Groups"),'<br/>',
            plotOutput("prevalance"),
            h3("Percent of dialysis patients waitlisted for kidney transplants"),'<br/>',
            plotOutput("coolplot"),
            h4("Total Pre-Dialysis Nephrology Care of Less Than 6 Months"),'<br/>',
            plotOutput("ABCXYZ"),
            h4("Regression Results for Race as a factor 
               for median age of patients diagnosed with ESRD"),'<br/>',
            plotOutput("Results")
          )
        )
    })
    
    output$page2<- renderUI({
      mainPanel(
        tabsetPanel(
          tabPanel("U.S.",
            mainPanel(plotOutput("usavgstar")) 
          ),
          tabPanel("Western Region",
            mainPanel(plotOutput("weststar"))
          ),
          tabPanel("Southern Region",
            mainPanel(plotOutput("southstar")) 
          ),
          tabPanel("MidWest",
            mainPanel(plotOutput("midweststar")) 
          ),
          tabPanel("NorthEast",
            mainPanel(plotOutput("northeaststar"))
          )
        )
      )
    })
    
    output$page3<-renderPlot({
        fviz_cluster(kmeans.cluster, data= FAC_ChainCluster[, -1], 
                     geom= c("point", "text")
        )
    })
    
    output$usavgstar<- renderPlot({
      plot_usmap(data = zxcv_agg, values = "AvgFiveStar", lines = "red") + 
        scale_fill_continuous(low = "white", high = "red", 
        name = "Dialysis Facilitiy Five.Star Score" ) + 
        theme(legend.position = "right")
    })
    
    output$southstar<-renderPlot({
      plot_usmap(data= zxcv_agg, values= "AvgFiveStar", include = South_States) +
        labs(title = "Selected Southern States")+
        scale_fill_continuous(low = "white", high = "red", name = "DFC Avg Star" )+
        theme(legend.position= "right")
    })
    
    output$midweststar<-renderPlot({
      plot_usmap(data= zxcv_agg, values= "AvgFiveStar", include = MidWest_States) +
        labs(title = "Selected Southern States")+
        scale_fill_continuous(low = "white", high = "red", name = "DFC Avg Star" )+
        theme(legend.position= "right")
    })
    
    output$northeaststar<-renderPlot({
      plot_usmap(data= zxcv_agg, values= "AvgFiveStar", include = NorthEast_States) +
        labs(title = "Selected Southern States")+
        scale_fill_continuous(low = "white", high = "red", name = "DFC Avg Star" )+
        theme(legend.position= "right")
    })
    
    output$weststar<-renderPlot({
      plot_usmap(data= zxcv_agg, values= "AvgFiveStar", include = West_States) +
        labs(title = "Selected Southern States")+
        scale_fill_continuous(low = "white", high = "red", name = "DFC Avg Star" )+
        theme(legend.position= "right")
    })
    
    output$prevalance<- renderPlot({
      ggplot(prevalence_data, (aes(x= Year, y=Percentage_Within_Race , col= Race)))+
        geom_point(alpha= 0.3)+
        geom_smooth(method= "auto")+
        labs(title = "Percentage of Racial Populations With ESRD", y = "Percentage of Racial Population With Diagnosis", x   = "Years: 1996 - 2016")
         })
    
    output$coolplot<- renderPlot({
      ggplot(df1, aes(x= time,value, col=variable )) + 
        geom_point() + 
        geom_smooth()+
        ggtitle("Percentage of Patients Waitlisted for kidney transplants")
    }) 
    
    output$ABCXYZ<- renderPlot({
      ggplot(data= map1, aes(x= Year, y = value)) + 
        geom_line(aes(y = White, colour= "White")) + 
        geom_line(aes(y = Black.African.American, colour= "Black.African.American"))+
        ylab("0 or <6 months")+
        ggtitle("Total Pre-Dialysis Nephrology Care of Less Than 6 Months")
    })
    
    output$Results<- renderImage({
      filename <- normalizePath(file.path('./images',
                                          paste('image', input$n, '.png', sep='')))
      list(src= filename)
                  }, deleteFile = FALSE)
    
  })
  
 )

## Prevalance
## Median Age data table, regression results
## Pre- Nephrology care/ Kidney waitlisting
## Stephen's plot- t-test (Code chunk 18/19- pick 2/3 t-tests)
##code 13- read the tables. 
## final regression results for median income and stations
## US Maps 