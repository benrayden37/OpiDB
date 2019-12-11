library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)

#VDH DATA
#All Opioids
Year <- c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017) #data from VDH_Opi_All.xlsx
All_Opioids <- c(516,538,530,498,601,572,684,775,812,1138,1229) 
VDH_Opi_Rate <- c(6.7,6.9,6.7,6.2,7.4,7,8.3,9.3,9.7,13.6,14.5)
VDH_Opi_TotalN <- 7893
VDH_Opi_TotalRate <- 8.8
db <- data.frame(Year,All_Opioids,VDH_Opi_Rate)
modelfit <- lm(All_Opioids ~ Year)
#516+538+530+498+601+572+684+775+812+1138+1229 / 7 #= 7893/11 = 717.545455

#Heroin
#data from VDH_Hero_All.xlsx
Heroin <- c(100,89,107,48,101,135,213,241,342,448,558) 
VDH_Hero_Rate <- c(1.3,1.1,1.4,0.6,1.2,1.6,2.6,2.9,4.1,5.3,6.6)
VDH_Hero_TotalN <- 2382
VDH_Hero_TotalRate <- 2.7
dbh <- data.frame(Year,Heroin,VDH_Hero_Rate)
modelfit1 <- lm(Heroin ~ Year)
#216.545455

#Fetanyl
#data from VDH_Fent_All.xlsx
Fentanyl <- c(48,68,43,64,54,50,102,134,225,624,770) 
VDH_Fent_Rate <- c(0.6,0.9,0.5,0.8,0.7,0.6,1.2,1.6,2.7,7.4,9.1)
VDH_Fent_TotalN <- 2182
VDH_Fent_TotalRate <- 2.4
dbf <- data.frame(Year,Fetanyl,VDH_Fent_Rate)
modelfit2 <- lm(Fetanyl ~ Year) # -119936.33  || 59.71  
#198.545455

#Perscription Opioids
#data from VDH_Pers_All.xlsx
Perscription_Opioids <- c(401,422,417,426,496,435,460,499,398,472,507) 
VDH_Pers_Rate <- c(5.2,5.4,5.3,5.3,6.1,5.3,5.6,6,4.7,5.6,6)
VDH_Pers_TotalN <- 4933
VDH_Pers_TotalRate <- 5.5
dbp <- data.frame(Year,Perscription_Opioids,VDH_Pers_Rate)
modelfit3 <- lm(Perscription_Opioids ~ Year) # -13873.327  || 7.118  
#448.454545

#All Drugs
#data from VDH_All_All.xlsx
All_Drugs <- c(721,735,713,690,819,799,914,994,1028,1428,1534) 
VDH_All_Rate <- c(9.3,9.5,9,8.6,10.1,9.8,11.1,11.9,12.3,17,18.1)
VDH_All_TotalN <- 10375
VDH_All_TotalRate <- 11.6
dba <- data.frame(Year,All_Drugs,VDH_All_Rate)
modelfit4 <- lm(All_Drugs ~ Year) # -154255.18  || 77.14
#943.181818

#Benzos
#data from VDH_Benz_All.xlsx
Benzos <- c(142,154,161,183,217,172,238,236,180,215,200) 
VDH_Benz_Rate <- c(1.8,2,2,2.3,2.7,2.1,2.9,2.8,2.1,2.6,2.4)
VDH_Benz_TotalN <- 2098
VDH_Benz_TotalRate <- 2.3
dbb <- data.frame(Year,Benzos,VDH_Benz_Rate)
modelfit5 <- lm(Benzos ~ Year) # -12942.145 || 6.527  
#190.727273

#Cocaine
#data from VDH_Coca_All.xlsx
Cocaine <- c(155,104,80,93,120,84,137,145,174,292,398) 
VDH_Coca_Rate <- c(2,1.3,1,1.2,1.5,1,1.7,1.7,2.1,3.5,4.7)
VDH_Coca_TotalN <- 1782
VDH_Coca_TotalRate <- 2
dbc <- data.frame(Year,Cocaine,VDH_Coca_Rate)
modelfit6 <- lm(Cocaine ~ Year) # -43187.45 || 21.55 
#162

#DBALL
dbAll <- data.frame(All_Opioids,Heroin,Fetanyl,Perscription_Opioids,Benzos,Cocaine,All_Drugs)
dbYear <- data.frame(Year)
dbAll2 <- data.frame(Year,All_Opioids,Heroin,Fentanyl,Perscription_Opioids,Benzos,Cocaine,All_Drugs)
modfit <- lm(dbAll)

#SAMHSA Data
samOG <- read.csv(file = "/Users/benrayden/Desktop/fun code/Shiny/SAMHSA_ALL_OPI.csv", header=T)

dfsam <- data.frame(Metric = c("12+", "12-17", "18+", "18-25", "26+", "Males", "Females", "Large-Metro", "Small-Metro", "Non-Metro"), 
                    Variable = c(rep("Virginia",10), rep("US_AVG", 10)),
                    Deaths = c(279,23,256,63,192,155,124,201,48,30, 245.16,37.44,226.46,55.46,171,135.08,110.08,139.8,73.12,32.24))

ggplot(data = dfsam, aes(x = Metric, y = Value, fill = Variable)) +
  geom_bar(stat="identity", position=position_dodge())

#VSP ARREST DATA
vspOG <- read.csv(file = "/Users/benrayden/Desktop/fun code/Shiny/VSP_Heroin_Arrests_99_17_Up.csv", header=T)
vsp <- read.csv(file = "/Users/benrayden/Desktop/fun code/Shiny/VSP_stacked_bar_data.csv", header=T)
vsp1 <- read.csv(file = "/Users/benrayden/Desktop/fun code/Shiny/VSP_stacked_notot.csv", header=T)


#SAMHDA N-MHSS Data
hmhssOG <- read.csv(file = "/Users/benrayden/Desktop/fun code/Shiny/SAMHDAFACILITY.csv", header=T)

#dfhmhss <- data.frame(Year = c("2010", "2014", "2017"), 
                      #Offered = c(rep("Yes",13), rep("No",13), rep("Missing",13)),
                      #Amount = c())


#UI
ui <- shinyUI(
  dashboardPage(
    dashboardHeader(title = "OpiDB", titleWidth = 230),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem(text = "Home", tabName = "home", icon=icon("home")),
        menuItem("VDH Data", tabName = "vdhdata", icon=icon("database")),
        menuItem("VDH Plot", tabName = "vdhplot", icon=icon("line-chart")),
        menuItem("SAMHSA Data", tabName = "samhsadata", icon=icon("database")),
        menuItem("SAMHSA Plot", tabName = "samhsaplot", icon=icon("line-chart")),
        menuItem("VSP Data", tabName = "vspdata", icon=icon("database")),
        menuItem("VSP Plot", tabName = "vspplot", icon=icon("line-chart")),
        menuItem("OpiDB Github",  href = "https://github.com/benrayden37/OpiDB", icon=icon("code"))
        # https://fontawesome.com/icons?d=gallery
      )
    ),
    
    
    dashboardBody(
      # within tabitems(), define the pages for sidebar menu items
      tabItems(
        tabItem(tabName = "home",
                h3("The OpiDB is an educational resource which serves to provides its users with reliable data and interactive visualizations of the trends in the Opioid Epidemic."),
                br(),
                h4("All of the cleaned data used to create the visualizations can be viewed via the side bar tabs labeled '_____ Data'."),
                br(),
                h4("All of the cleaned and uncleaned data can be accessed and downloaded by clicking the 'OpiDB Github' tab, and is available in .csv and .xlsx formats."),
                br(),
                fluidRow(box(title = "All Reported Virginia Drug Deaths", plotlyOutput("stackedL", height = 450), width = 8))
        ),
        tabItem(tabName = "vdhdata", dataTableOutput("mydatatable")),
        tabItem(tabName = "vdhplot",
                sidebarPanel(width = 2,
                             selectInput('var', 'Drug Type Selector', choices = c("All Opioids"=1, "Heroin"=2, "Fetanyl"=3, "Perscription Opioids"=4, "Benzos"=5, "Cocaine"=6, "All Drugs"=7, width = 2))
                ),
                mainPanel(
                  fluidRow(box(title = "Deaths in Virginia From Selected Substance", plotlyOutput("myhist", height = 400), width = 11)),
                  box(title = "Summary Statistics", tableOutput("summary"), width = 4)
                )
        ),
        tabItem(tabName = "samhsadata", dataTableOutput("mydatatable1")),
        tabItem(tabName = "samhsaplot",
                fluidRow(box(title = "US National Average vs. Virginia State Average", plotlyOutput("SAMHSAp1", height = 700), width = 10))
        ),
        tabItem(tabName = "vspdata", dataTableOutput("mydatatable2")),
        tabItem(tabName = "vspplot",
                fluidRow(
                  tabBox(width = 10, height = 600,
                    tabPanel(title = "Virginia State Police Arrest Data", plotlyOutput("stackedVSP", height = 600)),
                    tabPanel(title = "Virginia State Police Arrest Data", plotlyOutput("boxVSP", height = 600)))
                )
        )
      )
    )
  )
    
)


server <- shinyServer(function(input, output){
  
  ## for display of mtcars dataset
  output$mydatatable <- renderDataTable({
    dbAll2
  })
  
  ##Reactive VDH Plot
  y <- reactive({
    dbAll[,as.numeric(input$var)]
  })
  
  output$myhist <- renderPlotly({
    colm = as.numeric(input$var)
    Deaths <- dbAll[,colm]
    gg <- ggplot(dbAll, aes(x=Year, y = y())) + geom_point(alpha = 0.8, size=1.5, col="blue") + geom_smooth(method="lm") + 
      scale_x_continuous(breaks = seq(2007, 2017, 1)) + 
      labs(size= "Average Rate of Deaths/Total Population", x="Year", y="Overdose Deaths", 
           subtitle = "2007-2017", caption = "Source: Virginia Deparment of Health") + 
      theme(
        plot.subtitle = element_text(color = "red"),
        plot.caption = element_text(color = "red", face = "italic"),
        axis.title.x = element_text(size=10,face="bold"),
        axis.title.y = element_text(size=10,face="bold")
      )
    
    ggplotly(gg)
    
    
  })
  
  #Reactive VDH stats
  sliderValues <- reactive({
    colm = as.numeric(input$var)
    Deaths <- dbAll[,colm]
    sumry <- summary(Deaths)
    data.frame(Name = c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max."),
               Value = as.character(sumry),
               stringsAsFactors=FALSE)
  })
  
  output$summary <- renderTable({
    sliderValues()
  })
  
  # VDH stacked line plot
  output$stackedL <- renderPlotly({
    
    df <- dbAll2
    # plot
    gg2 <- ggplot(df, aes(x=Year)) + 
      geom_line(aes(y=All_Opioids, col="All_Opioids")) + 
      geom_line(aes(y=Heroin, col="Heroin")) + 
      geom_line(aes(y=Fentanyl, col="Fentanyl")) +
      geom_line(aes(y=Perscription_Opioids, col="Perscription_Opioids")) +
      geom_line(aes(y=All_Drugs, col="All_Drugs")) +
      geom_line(aes(y=Benzos, col="Benzos")) +
      geom_line(aes(y=Cocaine, col="Cocaine")) +
      labs(title="Drug Related Deaths Over Time in Virginia", 
           subtitle="Drawn From Wide Data format", 
           caption="Source: Virginia Department of Health", y="Deaths") +  # title and caption
      scale_x_continuous(breaks = seq(2007, 2017, 1)) +  # change to monthly ticks and labels
      scale_color_manual(name="", 
                         values = c("All_Opioids"="#00ba38", "Heroin"="#f8766d", "Fentanyl"="blue", "Perscription_Opioids"="purple",
                                    "All_Drugs"="cyan", "Benzos"="orange", "Cocaine"="maroon")) +  # line color
      theme(panel.grid.minor = element_blank())
    
    ggplotly(gg2)
  })
  
  ## Dataset for SAMHSA
  output$mydatatable1 <- renderDataTable({
    samOG
  })
  
  # SAMHSA Plots
  output$SAMHSAp1 <- renderPlotly({
    
    gg3 <- ggplot(data = dfsam, aes(x = Metric, y = Deaths, fill = Variable)) +
      geom_bar(stat="identity", position=position_dodge()) + 
      labs(title = "Opioid Overdose Rates in Virginia when Compared to the US Average") +
      coord_flip()
    
    ggplotly(gg3)
  })
  
  
  ## Dataset for VSP
  output$mydatatable2 <- renderDataTable({
    vspOG
  })
  
  
  ##VSP Plot
  output$stackedVSP <- renderPlotly({
    
    # plot
    gg4 <- ggplot(vspOG, aes(x=Year)) + 
      geom_line(aes(y=X15.19, col="15-19")) + 
      geom_line(aes(y=X20.24, col="20-24")) + 
      geom_line(aes(y=X25.29, col="25-29")) +
      geom_line(aes(y=X30.34, col="30-34")) +
      geom_line(aes(y=X35.39, col="35-39")) +
      geom_line(aes(y=X40.44, col="40-44")) +
      geom_line(aes(y=X45.49, col="45-49")) +
      geom_line(aes(y=X50.54, col="50-54")) +
      geom_line(aes(y=X55.59, col="55-59")) +
      geom_line(aes(y=X60.64, col="60-64")) +
      geom_line(aes(y=X65., col="65+")) +
      labs(title="Heroin Arrests in Virginia 1999-2017", 
           subtitle="Drawn From Wide Data format", 
           caption="Source: Economics", y="Arrests") +  # title and caption
      scale_x_continuous(breaks = seq(1999, 2017, 1)) +  # change to monthly ticks and labels
      scale_color_manual(name="", 
                         values = c("15-19"="grey", "20-24"="#f8766d", "25-29"="blue", "30-34"="purple",
                                    "35-39"="cyan", "40-44"="grey", "45-49"="grey", "50-54"="grey", 
                                    "55-59"="grey", "60-64"="grey", "65+"="grey")) +  # line color
      theme(panel.grid.minor = element_blank())
    
    ggplotly(gg4)
  })
  
  ##VSP Box Plot
  output$boxVSP <- renderPlotly({
    
    gg5 <- ggplot(vsp1) + geom_boxplot(aes(x=AgeGroup, y = Overdoses, fill=AgeGroup), alpha=0.4) +
      geom_point(aes(x = AgeGroup, y = Overdoses, color=AgeGroup), position="jitter", size=2) + theme_bw() + 
      labs(title="Heroin Arrests in Virginia by Age Group", y = "Arrests")
    ggplotly(gg5)
  })
  
  vspOG
  
  #UNADDED NEW PLOTS
  
  #stacked bar (Doesnt work right)
  ggplot(vsp1) + geom_bar(aes(x = Year, fill = AgeGroup))
  
  #box plot (very cool)
  gg5 <- ggplot(vsp1) + geom_boxplot(aes(x=AgeGroup, y = Overdoses, fill=AgeGroup), alpha=0.4) +
    geom_point(aes(x = AgeGroup, y = Overdoses, color=AgeGroup), position="jitter", size=2) + theme_bw() + 
    labs(title="Heroin Arrests in Virginia by Age Group", y = "Arrests")
  ggplotly(gg5)
  
  #scater
  ggplot(data = vsp1, aes(x = Year, y = Overdoses)) + geom_point(aes(color=as.factor(AgeGroup)), size = 2.5) +
    labs(title = "Heroin Arrests in Virginia 1999-2017", y = "Arrests") + geom_smooth(method = "lm")
  
  #scater w lines (sucks)
  ggplot(data = vsp1, aes(x = Year, y = Overdoses, color = as.factor(AgeGroup))) +
    geom_point() + # Add a layer for the scatterplot
    geom_smooth(method = "lm")
  
  output$stackedL <- renderPlotly({
    
  #VDH ALL DRUGS GRAYSCALE FOR REPORT FA19
    df <- dbAll2
    # plot
    g2 <- ggplot(df, aes(x=Year)) + 
      geom_line(aes(y=All_Opioids, col="All_Opioids")) + 
      geom_line(aes(y=Heroin, col="Heroin")) + 
      geom_line(aes(y=Fentanyl, col="Fentanyl")) +
      geom_line(aes(y=Perscription_Opioids, col="Perscription_Opioids")) +
      geom_line(aes(y=All_Drugs, col="All_Drugs")) +
      geom_line(aes(y=Benzos, col="Benzos")) +
      geom_line(aes(y=Cocaine, col="Cocaine")) +
      labs(title="Drug Related Deaths Over Time in Virginia", 
           subtitle="Drawn From Wide Data format", 
           caption="Source: Virginia Department of Health", y="Deaths") +  # title and caption
      scale_x_continuous(breaks = seq(2007, 2017, 1)) +  # change to monthly ticks and labels
      scale_color_manual(name="", 
                         values = c("All_Opioids"="#00ba38", "Heroin"="#f8766d", "Fentanyl"="blue", "Perscription_Opioids"="purple",
                                    "All_Drugs"="cyan", "Benzos"="grey", "Cocaine"="grey")) +  # line color
      theme(panel.grid.minor = element_blank())
    
    ggplotly(g2)
  })
  
}
)

shinyApp(server = server, ui = ui)

