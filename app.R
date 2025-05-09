#load libraries
library(shiny)
library(ggplot2)
library(scales)
library(rgdal)
library(raster)
library(shinythemes)
library(tableHTML)
library(ggpubr)
library(shinyBS)
library(ggspatial)
library(wesanderson)
library(cowplot)
library(egg)

#if (interactive()) {

#dataset
githubURL<-('https://raw.githubusercontent.com/danielvilasgonzalez/redtideapp/main/runs_processed.rds')
download.file(githubURL,"runs_processed.rds", method="curl")
load("runs_processed.rds")
githubURL<-('https://raw.githubusercontent.com/danielvilasgonzalez/redtideapp/main/fxn1all.rds')
download.file(githubURL,"fxn1all.rds", method="curl")
load("fxn1all.rds")
githubURL<-('https://raw.githubusercontent.com/danielvilasgonzalez/redtideapp/main/red_tide_maps.rds')
download.file(githubURL,"red_tide_maps.rds", method="curl")
redtidemaps <- readRDS("red_tide_maps.rds")
githubURL<-('https://raw.githubusercontent.com/danielvilasgonzalez/redtideapp/main/red_tide_maps_png_DV.rds')
download.file(githubURL,"red_tide_maps_png_DV.rds", method="curl")
redtidemapsDV <- readRDS("red_tide_maps_png_DV.rds")
#githubURL<-('https://raw.githubusercontent.com/danielvilasgonzalez/redtideapp/main/cb_2018_us_nation_5m.shp')
#download.file(githubURL,"cb_2018_us_nation_5m.shp", method="curl")
#githubURL<-('https://raw.githubusercontent.com/danielvilasgonzalez/redtideapp/main/cb_2018_us_nation_5m.shx')
#download.file(githubURL,"cb_2018_us_nation_5m.shx", method="curl")
#githubURL<-('https://raw.githubusercontent.com/danielvilasgonzalez/redtideapp/main/cb_2018_us_nation_5m.dbf')
#download.file(githubURL,"cb_2018_us_nation_5m.dbf", method="curl")
#githubURL<-('https://raw.githubusercontent.com/danielvilasgonzalez/redtideapp/main/cb_2018_us_nation_5m.prj')
#download.file(githubURL,"cb_2018_us_nation_5m.prj", method="curl")
#us<- raster::shapefile('cb_2018_us_nation_5m.shp')
#FL <- raster::extent(-87.5,-81,25,30.5)
#fl <- raster::crop(us, FL)

#########################    
# Define UI for the app
#########################
ui <- fluidPage(tags$head(
  tags$style(make_css(list('.shiny-output-error-validation', 
                           c('font-size', 'font-family', 'color'), 
                           c('24px', 'arial', 'red'))))),
  
  #theme = shinytheme("sandstone"),
  
  # App title ----
  titlePanel("RedTideVIS"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
        #Loading message
    sidebarPanel(tags$head(tags$style(type='text/css','
               #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 10px 0px 10px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 200%;
               color: #000000;
               background-color: #BA2720;
               z-index: 105;
             }
             .tooltip .tooltiptext {
                          width: 120px;
  top: 100%;
  left: 50%;
  margin-left: -60px;
                        }')),   conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                  tags$div("Loading...",id="loadmessage")),
     #Text
    #helpText('Create biomass trends and losses and mortality rate trends due to red tide 
     #                     in the West Florida Shelf ecosystem (1985-2020).',align='left'),
    #Map
    #img(src="https://raw.githubusercontent.com/danielvilasgonzalez/redtideapp/main/map2.png", align = "left", height = '300px', width = '300px'),

    #Select FG      
    conditionalPanel(
      condition = "input.tabs != 'maps'" ,
      selectInput(inputId = "FG",
                             label = "Choose a functional group:",
                      choices = colnames(read.csv('https://raw.githubusercontent.com/danielvilasgonzalez/redtideapp/main/biomass/run1.csv',check.names=FALSE))[3:83],
                      selected = 'gag 0')),
    
    bsTooltip("FG", "What functional group would like to visualize results from?",
              "right", trigger = "hover", options = list(container = "body")),
    
    #select comparison variable
    conditionalPanel(
      condition = "input.tabs != 'maps'" ,
      selectInput(inputId = "comparison",
                  label = "Comparison by:",
                  choices = c("Groups that red tide effects are applied to"='FG',
                              'Red tide effects considered'='fxn',
                              'Red tide effects threshold'='m_sens',
                              'Mortality rate of change (slope)'='m_slope',
                              'Foraging threshold'='f_sens'))),
    
    bsTooltip("comparison", "What parameter configuration would you like to compare?",
              "right", trigger = "hover", options = list(container = "body")),
    
    #select which groups red tides effects are applied to  
    conditionalPanel(
      condition = "input.tabs != 'maps'" ,
      checkboxGroupInput( inputId="redtideFG", label="Groups that red tide effects are applied to:",
                 choices=c('All consumer groups','Only gag groups'),
                 selected = c('All consumer groups','Only gag groups'))),
    
    bsTooltip("redtideFG", "Check the options below to select runs in which red tide effects were applied to only gag groups and/or all consumer group",
              "right", trigger = "hover", options = list(container = "body")),

    #What red tide effects are considered
    conditionalPanel(
      condition = "input.tabs != 'maps'" ,
      checkboxGroupInput(inputId = "fxn",
                 label = "Red tide effects considered:",
                  choices = c('Mortality','Mortality and foraging'),
                 selected = c('Mortality','Mortality and foraging'))),
    
    bsTooltip("fxn", "Check the options below to select runs in which red tide effects were applied to only gag groups and/or all consumer group",
              "right", trigger = "hover", options = list(container = "body")),

    #Mortality sensitivity
    conditionalPanel(
      condition = "input.tabs != 'maps'" ,
      checkboxGroupInput(inputId = "m_sens",            
                  label = "Red tide effects threshold:",
                  choices =  c('Low','Medium low','Medium','Medium high','High'),
                  selected =  c('Medium low','Medium','Medium high'))),
    
    bsTooltip("m_sens", "Check the options below to select runs depending on the sensitivity of marine organisms to die due to red tides",
              "right", trigger = "hover", options = list(container = "body")),
    
    #Mortality slope
    conditionalPanel(
      condition = "input.tabs != 'maps'" ,
      checkboxGroupInput(inputId = "m_slope", 
                 label = "Mortality rate of change (slope):",
                  choices = c('Flat','Medium flat','Medium steep','Steep'),
                  selected = c('Medium flat','Medium steep'))),
    
    bsTooltip("m_slope", "Check the options below to select runs depending on the mortality rate of change caused by red tide mortality to marine organisms",
              "right", trigger = "hover", options = list(container = "body")),
    
    #year range
    conditionalPanel(
      condition = "input.tabs != 'maps'" ,
      sliderInput(inputId = "rang", 
                         label = "Year range to plot:",
                         min=2002,max=2020,sep = "",
                         value=c(2002,2020))),
    
    bsTooltip("m_slope", "Check the options below to select runs depending on the mortality rate of change caused by red tide mortality to marine organisms",
              "right", trigger = "hover", options = list(container = "body")),
    
    #If foraging included, select foraging sensitivity  
    conditionalPanel(
        condition = "input.fxn.includes('Mortality and foraging') && input.tabs != 'maps'" ,
        # Input: Selector for choosing mortality slope 
        #radioButtons(inputId = "f_sens",
        checkboxGroupInput(inputId = "f_sens", 
                   label = "Foraging threshold:",
                    choices = c('Low','Medium','High'),
                    selected =  c('Medium'))),
    
    bsTooltip("f_sens", "Check the options below to select runs depending on the sensitivity of marine oraganisms forage capacity to red tides",
              placement = "right", trigger = "hover", options = list(container = "body")),
    
    #years for the red tide maps
    conditionalPanel(
      condition = "input.tabs == 'maps'" ,
      selectInput(inputId = "year",
                  label = "Choose a year for red tide maps:",
                  choices = as.character(c(2002:2020)))),
    
    bsTooltip("year", "What year would you like to visualize red tide severity maps?",
              placement = "right", trigger = "hover", options = list(container = "body")),
  
    #br(),
    #Action button
    #submitButton("Update"),
    #br(),
    #helpText('Download mortality rate trend with the specified options',align='left'),
    #conditionalPanel(
    #  condition = "input.tabs != 'maps'" ,
    #  downloadButton("download", "Download red tide mortality report", 
    #               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
    #br(),
    #logo NOAA and NCBS_UF
    img(src="https://raw.githubusercontent.com/danielvilasgonzalez/redtideapp/main/NOAArestore.png", align = "left", height="65%", width="65%"),br(),br(),br(),br(),br(),
    img(src="https://raw.githubusercontent.com/danielvilasgonzalez/redtideapp/main/UF_IFAS.jpg", align = "left", height="45%", width="45%"),
    br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),width=3),#,width=3
    
    
    #Tab panels outputs
      mainPanel(
        tabsetPanel( id = "tabs",
          tabPanel("Read me", 
                   h2("Overview"),
                   p("This tool represents predicted biomass, biomass loss due to red tide, and annual red tide mortality rate from the year 2002 to 2020 from the West Florida Shelf (WFS) spatiotemporal ecosystem model. It allows the exploration of predictions for multiple groups/species and multiple model configurations."),
                   h2("App layout"),
                   p("This tab provides a walk-through. The sidebar to the left provides options for the various configurations of the red tide effects and the displayed group or species. The 'Red tide effects plots' tab shows multiple plots of predicted biomass, biomass loss due to red tide, and annual red tide mortality rate from the year 2002 to 2020, and multiple plots of the selected foraging and response functions. The 'Annual Red Tide Mortality Index Summary' tab shows a dataset with the predicted annual red tide mortality rate (mean and standard deviation) from the year 2002 to 2020. Besides visualizing the dataset, the user can also copy or download the annual red tide mortality rate information in several formats (csv, excel and pdf) by clicking in the specific button. The 'Red tide maps' tab shows the red tide severity maps included in the ecosystem model and it allows to spatially locate the most severe red tide effects."),
                   h2("Modeled ecosystem"),
                   p("The WFS ecosystem was developed with Ecospace that is the spatial component of the ecosystem modeling software Ecopath with Ecosim (EwE). It was simulated over a map with a spatial resolution of 10 minutes, from 1985 to 2020 at monthly steps. The WFS Ecospace model includes 17 fishing fleets and 83 functional groups which represent individual species, life stages, or groups of functionally similar species. Depth, rugosity, sea surface temperature, sea bottom temperature, sea surface salinity, and red tide were incorporated as monthly raster data."),      
                   h2("Ecosystem modeling procedure"),
                   p("The 'Annual Red Tide Mortality Index Summary' tab shows predictions of red tide mortality rate and 'Red tide effects plots' tabs show predictions of biomass, biomass loss due to red tide, and annual red tide mortality rate. These predictions will be updated whenever the user (you) chooses different options. These options represent a total of 160 different configurations on the ecosystem modeling approach because of the lack of empirical information on the red tide effects on species."),
                   p(tags$ul(
                     tags$li("The user may select a single functional group to display predictions (single option)."), 
                     tags$li("The user can select how predictions are aggregated and compared. This is only useful when the selected parameter has multiple options selected (single option)."), 
                     tags$li("The user can specify whether red tide effects are applied to all consumer groups or only gag grouper groups. This option was included because this analysis was focused on the gag grouper population (multiple options)."),
                     tags$li("The user can determine whether red tide effects on foraging capacity are considered (multiple options)."), 
                     tags$li("The user can define the red tide effects threshold (multiple options)."), 
                     tags$li("The user may define the slope (rate of change) on the mortality response function (multiple options)."), 
                     tags$li("The user may set the year range for the biomass, biomass loss and red tide mortality plots"), 
                     tags$li("The user can define the threshold of the foraging response function due to red tide effects. This is only applicable if the foraging response function is applied (multiple options)."), 
                     tags$li("If the 'Red tide maps' tab is selected, the user can specify the year for the visualization of the red tide severity maps (single option).") 
                     #tags$li("The user can download a csv file of the predicted dataset that is shown in the 'Summary' tab by clicking the 'Download' button."),                      
                     )),
                   br(),
                   br(),
                   h4("This tool is intended to explore red tide effects on the WFS ecosystem with special emphasis on gag grouper, so we should be cautious about the interpretation of some species outputs, especially biomass.", style = "color:blue")),
                   tabPanel("Red Tide Effects Plots", value ='bio', plotOutput(outputId = "bio")),
          tabPanel("Annual Red Tide Mortality Index Summary",value='summary', DT::dataTableOutput('summary')),
          tabPanel("Red Tide Maps", 
                   br(),
                   p("Monthly West Florida Shelf maps. Color palette represents fluorescent line height derived from MODIS-Aqua satellite (harmful and non-harmful blooms intensity). White asterisks identify sample locations with less than 1,000 cell/L (non red tide effects). Red asterisks identify sample locations with more than 1,000 cell/L (red tide effects)"),
                   
                   value='maps', plotOutput(outputId = "maps"))))))
  

#server function builds a list-like object named output that contains all of the code needed to update the R objects in your app
server <- function(input, output) {
  
  output$bio <-  renderPlot({
    
    #example
    #input<-list()
    #input$redtideFG<-c('All consumer groups','Only gag groups')
    #input$m_sens<- c(unique(as.vector(data_runs$m_sens)))
    #input$m_slope <- c(unique(as.vector(data_runs$m_slope)))
    #input$fxn <- c(unique(as.vector(data_runs$fxn)))[2]
    #input$FG<-'gag 0'
    #input$f_sens<-c(unique(as.vector(data_runs$f_sens)))[-1]
    #input$comparison<-"FG"
    #input$year<-as.character(c(2002:2020))[6]
    #input$rang<-c(2002,2020)
    
    #require these inputs
    req(input$FG)

    #show error if none option is selected in each variable
    validate(
      need(input$redtideFG, "Please you must select an option for 'Red tides applied to' variable"),
      need(input$m_slope, "Please you must select an option for 'Mortality slope' variable"),
      need(input$m_sens, "Please you must select an option for 'Red tide effects threshold' variable"),
      need(input$fxn, "Please you must select an option for 'Red tides effects considered' variable")
    )
    
    if ('Mortality and foraging' %in%  input$fxn ) {
      validate(
        need(input$f_sens, "Please you must select an option for 'Foraging threshold' variable"))
    }
    
    #get information on data runs
    data_runs<-read.csv('https://raw.githubusercontent.com/danielvilasgonzalez/redtideapp/main/data_runs.csv')
    
    #filter by using red tide on gag or all consumer groups
    filtered_redtideFG <-subset(data_runs,FG %in% input$redtideFG)
    filtered_msens <- subset(filtered_redtideFG,m_sens %in% input$m_sens)
    filtered_mslope <- subset(filtered_msens,m_slope %in% input$m_slope)
    filtered_fxn <- subset(filtered_mslope,fxn %in% input$fxn)
    
    #filter by foraging sensitivity
    fully_filtered  <- 
      if('Mortality and foraging' %in% input$fxn){
        rbind(subset(filtered_fxn,f_sens %in% c(input$f_sens)),subset(filtered_fxn,is.na(f_sens)))
      } else {
        filtered_fxn
      } 
    
    #get runs
    runs<-fully_filtered$runs
    runs1<-paste0('run',formatC(fully_filtered$runs, width = 5, format = "d", flag = "0"))
    
    #get classification
    clas<-unique(fully_filtered[, input$comparison])
    #get columns from classification
    cols<-fully_filtered[,input$comparison]
    
    #######################
    # MORTALITY FXNs
    #######################
    
    #mortality fxn data
    fxn1m<-subset(fxn1all,fxn=='mortality')
    
    #unique slope and sensitivity label
    m_fxn<-unique(fully_filtered[,c('m_slope','m_sens')])
    m_fxn$variable<-paste(m_fxn$m_sens,m_fxn$m_slope,sep = '_')
    v<-unique(m_fxn$variable)
    
    #subset from the mortality data fxn
    fxn1m<-subset(fxn1m,variable %in% v)

    #palette
    palette1<-c('Flat'="#1ba3c6",'Medium flat'="#33a65c",'Medium steep'="#d5bb21",'Steep'="#f06719")
    palette1<-palette1[input$m_slope]
    
    #plot mortality
    p1m<-ggplot(fxn1m,aes(x=cells,y=value,color=slope,group=slope))+
      geom_line(size=1)+
      scale_color_manual(values = palette1)+
      theme_bw()+
      theme(panel.grid.minor =  element_blank(),axis.text.x = element_text(size=12,angle=45,vjust = 1,hjust = 1),axis.text.y = element_text(size=12))+
      theme(panel.grid.minor = element_blank(),strip.text.x = element_text(size = 12,color='black'), 
            strip.background = element_rect(color="black", fill="white", size=1.2, linetype="solid"))+
      theme(aspect.ratio = 1,text = element_text(size=12),
            strip.text.x =  element_text(size=12,family = 'sans'),legend.text = element_text(size=12))+
      labs(color='mortality \nslope', title="Red tide mortality response curves")+
      ylab('Proportion killed')+
      xlab('K. brevis cells/L')+
      scale_x_continuous(limits = c(0,1000000),labels = comma)+
      facet_wrap(~sens,nrow = 1)
    
   
    #######################
    # FORAGING FXNs
    #######################
    
    #foraging fxn data
    fxn1f<-subset(fxn1all,fxn=='foraging')
    
    #unique slope and sensitivity label
    f_fxn<-unique(fully_filtered[,c('f_sens','m_sens')])
    f_fxn$variable<-paste(f_fxn$m_sens,f_fxn$f_sens,sep = '_')
    v<-unique(f_fxn$variable)
    
    #subset from the mortality data fxn
    fxn1f<-subset(fxn1f,variable %in% v)
    
    #palette
    palette2<-c('Low'="#e03426",'Medium'="#eb73b3",'High'="#a26dc2")
    palette2<-palette2[input$f_sens]
    
    p1f<-ggplot(fxn1f,aes(x=cells,y=value,color=slope,group=slope))+
      geom_line(size=0.8)+
      scale_color_manual(values = palette2)+
      theme_bw()+
      theme(panel.grid.minor =  element_blank(),axis.text.x = element_text(size=12,angle=45,vjust = 1,hjust = 1),axis.text.y = element_text(size=12))+
      theme(panel.grid.minor = element_blank(),strip.text.x = element_text(size = 12,color='black'), 
            strip.background = element_rect(color="black", fill="white", size=1.2, linetype="solid"))+
      theme(aspect.ratio = 1,text = element_text(size=12),
            strip.text.x =  element_text(size=12,family = 'sans'),legend.text = element_text(size=12))+
      labs(color='foraging \nthreshold',title = "Foraging response curves")+
      ylab('Cells foraging capacity')+
      xlab('K. brevis cells/L')+
      scale_x_continuous(limits = c(0,500000),labels = comma)+
      facet_wrap(~sens,nrow = 1)
    
    #filter by range of years
    data<-data[c(as.character(input$rang[1]:input$rang[2])),,,]
    
    #biomass
    b<-data.frame(data[,input$FG,runs1,'bio'])
    colnames(b)<-cols
    #loss
    l<-data.frame(data[,input$FG,runs1,'los'])
    colnames(l)<-cols
    #mortality
    m<-data.frame(data[,input$FG,runs1,'mor'])
    colnames(m)<-cols

    title<-
      if (input$comparison=='m_sens') {
        'Red tide effects threshold'
      } else  if (input$comparison=='m_slope') {
        "Mortality slope"
      } else  if (input$comparison=='fxn') {
        "Red tide effects applied"
      } else  if (input$comparison=='f_sens') {
        "Foraging threshold"
      } else  if (input$comparison=='FG') {
        "Groups applied red tide effects"}
    
    palette<-
      if (input$comparison=='m_sens') {
        c('Low'="#FF0000",'Medium low'="#00A08A",'Medium'="#F2AD00",'Medium high'="#5BBCD6",'High'="#F98400")
      } else  if (input$comparison=='m_slope') {
        c('Flat'="#FF0000",'Medium flat'="#00A08A",'Medium steep'="#F2AD00",'Steep'="#5BBCD6")
      } else  if (input$comparison=='fxn') {
        c('Mortality'="#FF0000",'Mortality and foraging'="#00A08A")
      } else  if (input$comparison=='f_sens') {
        c('Low'="#FF0000",'Medium'="#00A08A",'High'="#5BBCD6")
      } else  if (input$comparison=='FG') {
        c('All consumer groups'="#FF0000",'Only gag groups'="#00A08A")}
    
    palette<-
      if (input$comparison=='m_sens') {
        palette[input$m_sens]
      } else  if (input$comparison=='m_slope') {
        palette[input$m_slope]
      } else  if (input$comparison=='fxn') {
        palette[input$fxn]
      } else  if (input$comparison=='f_sens') {
        palette[input$f_sens]
      } else  if (input$comparison=='FG') {
        palette[input$redtideFG]}
    
    
    if (length(clas)>=2) {
      
      bdata1<-data.frame(matrix(NA,nrow = 1,ncol = 6))
      colnames(bdata1)<-c('year','mean','q5','q95','sd','group')
      ldata1<-bdata1
      mdata1<-bdata1

      for (c in clas) {

      #c<-clas[2]
        
      index<-which(cols %in% c)
      
      bcsv2<-b[,index]
      lcsv2<-l[,index]
      mcsv2<-m[,index]
      
    
      if (is.numeric(bcsv2)) {
        
        bdata<-data.frame(year=c(input$rang[1]:input$rang[2]),mean=bcsv2,q5=rep(NA,times=length(input$rang[1]:input$rang[2])),q95=rep(NA,times=length(input$rang[1]:input$rang[2])),group=rep(c,times=length(input$rang[1]:input$rang[2])))
        ldata<-data.frame(year=c(input$rang[1]:input$rang[2]),mean=lcsv2,q5=rep(NA,times=length(input$rang[1]:input$rang[2])),q95=rep(NA,times=length(input$rang[1]:input$rang[2])),group=rep(c,times=length(input$rang[1]:input$rang[2])))
        mdata<-data.frame(year=c(input$rang[1]:input$rang[2]),mean=mcsv2,q5=rep(NA,times=length(input$rang[1]:input$rang[2])),q95=rep(NA,times=length(input$rang[1]:input$rang[2])),group=rep(c,times=length(input$rang[1]:input$rang[2])))
        
        } else {
          
               #biomass calc
          bdata<-data.frame(year=c(input$rang[1]:input$rang[2]),
                            mean=round(rowMeans(bcsv2),digits = 6),
                            q5=round(apply(bcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[1,],digits = 6),
                            q95=round(apply(bcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[2,],digits = 6),
                            sd=round(apply(bcsv2, 1, sd,  na.rm = TRUE),digits = 6),
                            group=rep(c,times=length(input$rang[1]:input$rang[2])))
          
          #loss calc
          ldata<-data.frame(year=c(input$rang[1]:input$rang[2]),
                            mean=round(rowMeans(lcsv2),digits = 6),
                            q5=round(apply(lcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[1,],digits = 6),
                            q95=round(apply(lcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[2,],digits = 6),
                            sd=round(apply(lcsv2, 1, sd,  na.rm = TRUE),digits = 6),
                            group=rep(c,times=length(input$rang[1]:input$rang[2])))
          
          #red tide mortality
          mdata<-data.frame(year=c(input$rang[1]:input$rang[2]),
                            mean=round(rowMeans(mcsv2),digits = 6),
                            q5=round(apply(mcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[1,],digits = 6),
                            q95=round(apply(mcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[2,],digits = 6),
                            sd=round(apply(mcsv2, 1, sd,  na.rm = TRUE),digits = 6),
                            group=rep(c,times=length(input$rang[1]:input$rang[2])))
          
     }
        
      
      bdata1<-rbind(bdata1,bdata)
      ldata1<-rbind(ldata1,ldata)
      mdata1<-rbind(mdata1,mdata)
      
      }
      
        bdata2<-bdata1[2:nrow(bdata1),]
        ldata2<-ldata1[2:nrow(ldata1),]
        mdata2<-mdata1[2:nrow(mdata1),]

        bdata2 <- bdata2[,colSums(is.na(bdata2))<nrow(bdata2)]
        ldata2 <- ldata2[,colSums(is.na(ldata2))<nrow(ldata2)]
        mdata2 <- mdata2[,colSums(is.na(mdata2))<nrow(mdata2)]
    

        pal<-c("#FF0000", "#00A08A", "#F2AD00",  "#5BBCD6","#F98400")
        nb.cols<-length(unique(clas))
        palette<-pal[1:nb.cols]
        
        
        #"#FF0000", "#00A08A", "#F2AD00",  "#5BBCD6","#F98400"
        
        if (input$comparison== 'm_sens') {
          bdata2$group <- factor(bdata2$group, levels=c('Low','Medium low','Medium','Medium high','High'))
          ldata2$group <- factor(ldata2$group, levels=c('Low','Medium low','Medium','Medium high','High'))
          mdata2$group <- factor(mdata2$group, levels=c('Low','Medium low','Medium','Medium high','High'))
          
        } else if (input$comparison== 'f_sens') {
          bdata2$group <- factor(bdata2$group, levels=c('Low','Medium','High'))
          ldata2$group <- factor(ldata2$group, levels=c('Low','Medium','High'))
          mdata2$group <- factor(mdata2$group, levels=c('Low','Medium','High'))
          
        }else if (input$comparison =='m_slope') {
          bdata2$group <- factor(bdata2$group, levels=c('Flat','Medium flat','Medium steep','Steep'))
          ldata2$group <- factor(ldata2$group, levels=c('Flat','Medium flat','Medium steep','Steep'))
          mdata2$group <- factor(mdata2$group, levels=c('Flat','Medium flat','Medium steep','Steep'))
        }
        
        bdata2<-bdata2[which(bdata2$year>=2002),]
        ldata2<-ldata2[which(ldata2$year>=2002),]
        mdata2<-mdata2[which(mdata2$year>=2002),] 
            
        #convert from t to millions of pounds
        bdata2[,c('mean','q5','q95','sd')]<-bdata2[,c('mean','q5','q95','sd')]*2000*170000/1000000
        ldata2[,c('mean','q5','q95','sd')]<-ldata2[,c('mean','q5','q95','sd')]*2000*170000/1000000
        
        #color transparent grid
        col_grid <- rgb(235, 235, 235, 100, maxColorValue = 500)
        
      if (ncol(bdata2) >= 4) {
        
    
        p1<-ggplot()+ #p1<-
          geom_line(data=bdata2,aes(x=year,y=mean,color=group),size=1)+
          geom_ribbon(data=bdata2,aes(x=year,ymin=ifelse(mean-sd<=0,0,mean-sd),ymax=mean+sd,fill=group),alpha=0.2)+
          theme_bw()+
          theme(panel.grid.minor = element_line(color = col_grid, size = 0.3, linetype = 'dashed'),
                panel.grid.major = element_line(color = col_grid, size = 0.5),
                strip.background =element_rect(fill="grey92"),
                text = element_text(size=14),
                legend.position = 'none',
                axis.text.x = element_text(size=12))+
          #scale_x_continuous(breaks = c(2002,2005,2010,2015,2020),labels = c(2002,2005,2010,2015,2020),
           #                  minor_breaks = c(2002:2004,2006:2009,2011:2014,2015:2019))+
          scale_fill_manual(values = palette  ,na.translate=FALSE, name=title)+
          scale_color_manual(values =palette  ,na.translate=FALSE, name=title)+
          ylab("Biomass (million pounds)")+ 
          expand_limits(y = 0)
        
        p2<-ggplot()+
          geom_line(data=ldata2,aes(x=year,y=mean,color=group),size=1)+
          geom_ribbon(data=ldata2,aes(x=year,ymin=ifelse(mean-sd<=0,0,mean-sd),ymax=mean+sd,fill=group),alpha=0.2)+
          theme_bw()+
          theme(panel.grid.minor = element_line(color = col_grid, size = 0.3, linetype = 'dashed'),
                panel.grid.major = element_line(color = col_grid, size = 0.5),
                strip.background =element_rect(fill="grey92"),
                text = element_text(size=14),
                legend.position = 'none',
                axis.text.x = element_text(size=12))+
          #scale_x_continuous(breaks = c(2002,2005,2010,2015,2020),labels = c(2002,2005,2010,2015,2020),
          #                   minor_breaks = c(2002:2004,2006:2009,2011:2014,2015:2019))+
          scale_fill_manual(values = palette  ,na.translate=FALSE, name=title)+
          scale_color_manual(values =palette  ,na.translate=FALSE, name=title)+
          ylab("Biomass loss (million pounds)")+ 
          expand_limits(y = 0)
        
        p3<-ggplot()+
          geom_line(data=mdata2,aes(x=year,y=mean,color=group),size=1)+
          geom_ribbon(data=mdata2,aes(x=year,ymin=ifelse(mean-sd<=0,0,mean-sd),ymax=mean+sd,fill=group),alpha=0.2)+
          theme_bw()+
          theme(panel.grid.minor = element_line(color = col_grid, size = 0.3, linetype = 'dashed'),
                panel.grid.major = element_line(color = col_grid, size = 0.5),
                strip.background =element_rect(fill="grey92"),
                text = element_text(size=14),
                legend.position = 'none',
                axis.text.x = element_text(size=12))+
          #scale_x_continuous(breaks = c(2002,2005,2010,2015,2020),labels = c(2002,2005,2010,2015,2020),
          #                   minor_breaks = c(2002:2004,2006:2009,2011:2014,2015:2019))+
          scale_fill_manual(values = palette  ,na.translate=FALSE, name=title)+
          scale_color_manual(values =palette  ,na.translate=FALSE, name=title)+
          ylab("Annual red tide mortality rate")+ 
          expand_limits(y = 0)
        
        #get legend
        legend <- get_legend(
          p3 +  theme(legend.position = "right"))
        
        # arrange the three plots in a single row
        prow <- plot_grid(
          p1 + theme(legend.position="none"),
          p2 + theme(legend.position="none"),
          p3 + theme(legend.position="none"),
          legend,
          hjust = -1,
          vjust = 1.1,
          nrow = 1,
          rel_widths = c(3,3,3,2))    
        
        
        pp<-
          if (!('Mortality and foraging' %in%  input$fxn )) {
            plot_grid(prow,p1m,nrow = 2,rel_heights = c(6,4))
          } else if (length(unique(fxn1m$sens))==1 & length(unique(fxn1f$sens))==1) {
            u<-plot_grid(p1m,p1f,nrow = 1)      
            plot_grid(prow,u,nrow = 2,rel_heights = c(6,4))
          } else{
            plot_grid(prow,p1m,p1f,nrow = 3,rel_heights = c(6,4,4))
          }
        
        #add title selected FG
        annotate_figure(pp, top = text_grob(input$FG,color = "black", face = "bold", size = 20))
        
      } else {
      
        p1<- ggplot()+
          geom_line(data=bdata2,aes(x=year,y=mean,color=group),size=1)+
          theme_bw()+
          theme(panel.grid.minor = element_line(color = col_grid, size = 0.3, linetype = 'dashed'),
                panel.grid.major = element_line(color = col_grid, size = 0.5),
                strip.background =element_rect(fill="grey92"),
                text = element_text(size=14),
                legend.position = 'none',
                axis.text.x = element_text(size=12))+
          #scale_x_continuous(breaks = c(2002,2005,2010,2015,2020),labels = c(2002,2005,2010,2015,2020),
          #                   minor_breaks = c(2002:2004,2006:2009,2011:2014,2015:2019))+
          scale_fill_manual(values = palette  ,na.translate=FALSE, name=title)+
          scale_color_manual(values =palette  ,na.translate=FALSE, name=title)+
          ylab("Biomass (million pounds)")+ 
          expand_limits(y = 0)
        
        p2<- ggplot()+
          geom_line(data=ldata2,aes(x=year,y=mean,color=group),size=1)+
          theme_bw()+
          theme(panel.grid.minor = element_line(color = col_grid, size = 0.3, linetype = 'dashed'),
                panel.grid.major = element_line(color = col_grid, size = 0.5),
                strip.background =element_rect(fill="grey92"),
                text = element_text(size=14),
                legend.position = 'none',
                axis.text.x = element_text(size=12))+
          #scale_x_continuous(breaks = c(2002,2005,2010,2015,2020),labels = c(2002,2005,2010,2015,2020),
          #                   minor_breaks = c(2002:2004,2006:2009,2011:2014,2015:2019))+
          scale_fill_manual(values = palette  ,na.translate=FALSE, name=title)+
          scale_color_manual(values =palette  ,na.translate=FALSE, name=title)+
          ylab("Biomass loss (million pounds)")+ 
          expand_limits(y = 0)
        
        
        p3<-ggplot()+
          geom_line(data=mdata2,aes(x=year,y=mean,color=group),size=1)+
          theme_bw()+
          theme(panel.grid.minor = element_line(color = col_grid, size = 0.3, linetype = 'dashed'),
                panel.grid.major = element_line(color = col_grid, size = 0.5),
                strip.background =element_rect(fill="grey92"),
                text = element_text(size=14),
                legend.position = 'none',
                axis.text.x = element_text(size=12))+
          #scale_x_continuous(breaks = c(2002,2005,2010,2015,2020),labels = c(2002,2005,2010,2015,2020),
          #                   minor_breaks = c(2002:2004,2006:2009,2011:2014,2015:2019))+
          scale_fill_manual(values = palette  ,na.translate=FALSE, name=title)+
          scale_color_manual(values =palette  ,na.translate=FALSE, name=title)+
          ylab("Annual red tide mortality rate")+ 
          expand_limits(y = 0)
        
        
      }
        
        #get legend
        legend <- get_legend(
          p3 +  theme(legend.position = "right"))
        
        # arrange the three plots in a single row
        prow <- plot_grid(
          p1 + theme(legend.position="none"),
          p2 + theme(legend.position="none"),
          p3 + theme(legend.position="none"),
          legend,
          hjust = -1,
          vjust = 1.1,
          nrow = 1,
          rel_widths = c(3,3,3,2))    
        
        pp<-
          if (!('Mortality and foraging' %in%  input$fxn )) {
            plot_grid(prow,p1m,nrow = 2,rel_heights = c(6,4))
          } else if (length(unique(fxn1m$sens))==1 & length(unique(fxn1f$sens))==1) {
            u<-plot_grid(p1m,p1f,nrow = 1)      
            plot_grid(prow,u,nrow = 2,rel_heights = c(6,4))
          } else{
            plot_grid(prow,p1m,p1f,nrow = 3,rel_heights = c(6,4,4))
          }
        
        #add title selected FG
        annotate_figure(pp, top = text_grob(input$FG,color = "black", face = "bold", size = 20))
        
        
      
     
    } else {
      
      bcsv2<-b
      lcsv2<-l
      mcsv2<-m
      
    
      if (is.numeric(b)) {
        
        bdata<-data.frame(year=c(input$rang[1]:input$rang[2]),mean=bcsv2,q5=rep(NA,times=length(input$rang[1]:input$rang[2])),q95=rep(NA,times=length(input$rang[1]:input$rang[2])),group=rep(clas,times=length(input$rang[1]:input$rang[2])))
        ldata<-data.frame(year=c(input$rang[1]:input$rang[2]),mean=lcsv2,q5=rep(NA,times=length(input$rang[1]:input$rang[2])),q95=rep(NA,times=length(input$rang[1]:input$rang[2])),group=rep(clas,times=length(input$rang[1]:input$rang[2])))
        mdata<-data.frame(year=c(input$rang[1]:input$rang[2]),mean=mcsv2,q5=rep(NA,times=length(input$rang[1]:input$rang[2])),q95=rep(NA,times=length(input$rang[1]:input$rang[2])),group=rep(clas,times=length(input$rang[1]:input$rang[2])))
        
      } else {
        
        #biomass calc
        bdata<-data.frame(year=c(input$rang[1]:input$rang[2]),
                          mean=round(rowMeans(bcsv2),digits = 6),
                          q5=round(apply(bcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[1,],digits = 6),
                          q95=round(apply(bcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[2,],digits = 6),
                          sd=round(apply(bcsv2, 1, sd,  na.rm = TRUE),digits = 6),
                          group=rep(clas,times=length(input$rang[1]:input$rang[2])))
 
        #loss calc
        ldata<-data.frame(year=c(input$rang[1]:input$rang[2]),
                          mean=round(rowMeans(lcsv2),digits = 6),
                          q5=round(apply(lcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[1,],digits = 6),
                          q95=round(apply(lcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[2,],digits = 6),
                          sd=round(apply(lcsv2, 1, sd,  na.rm = TRUE),digits = 6),
                          group=rep(clas,times=length(input$rang[1]:input$rang[2])))
        
        #red tide mortality
        mdata<-data.frame(year=c(input$rang[1]:input$rang[2]),
                          mean=round(rowMeans(mcsv2),digits = 6),
                          q5=round(apply(mcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[1,],digits = 6),
                          q95=round(apply(mcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[2,],digits = 6),
                          sd=round(apply(mcsv2, 1, sd,  na.rm = TRUE),digits = 6),
                          group=rep(clas,times=length(input$rang[1]:input$rang[2])))
        }
      
      bdata2 <- bdata[,colSums(is.na(bdata))<nrow(bdata)]
      ldata2 <- ldata[,colSums(is.na(ldata))<nrow(ldata)]
      mdata2 <- mdata[,colSums(is.na(mdata))<nrow(mdata)]
      
      
      bdata2<-bdata2[which(bdata2$year>=2002),]
      ldata2<-ldata2[which(ldata2$year>=2002),]
      mdata2<-mdata2[which(mdata2$year>=2002),]    
      
      #convert from t to millions of pounds
      bdata2[,c('mean','q5','q95','sd')]<-bdata2[,c('mean','q5','q95','sd')]*2000*170000/1000000
      ldata2[,c('mean','q5','q95','sd')]<-ldata2[,c('mean','q5','q95','sd')]*2000*170000/1000000
      
      #color transparent grid
      col_grid <- rgb(235, 235, 235, 100, maxColorValue = 500)
      
      if (ncol(bdata2) >= 4) {
        
        
        
       p1<-ggplot()+ #p1<-
          geom_line(data=bdata2,aes(x=year,y=mean,color=group),size=1)+
          geom_ribbon(data=bdata2,aes(x=year,ymin=ifelse(mean-sd<=0,0,mean-sd),ymax=mean+sd,fill=group),alpha=0.2)+
          theme_bw()+
          theme(panel.grid.minor = element_line(color = col_grid, size = 0.3, linetype = 'dashed'),
                panel.grid.major = element_line(color = col_grid, size = 0.5),
                strip.background =element_rect(fill="grey92"),
                text = element_text(size=14),
                legend.position = 'none',
                axis.text.x = element_text(size=12))+
          #scale_x_continuous(breaks = c(2002,2005,2010,2015,input$rang[2]),labels = c(2002,2005,2010,2015,2020),
          #                   minor_breaks = c(2002:2004,2006:2009,2011:2014,2015:2019))+
          scale_fill_manual(values = palette  ,na.translate=FALSE, name=title)+
          scale_color_manual(values =palette  ,na.translate=FALSE, name=title)+
          ylab("Biomass (million pounds)")+ 
          expand_limits(y = 0)
        
        p2<-ggplot()+
          geom_line(data=ldata2,aes(x=year,y=mean,color=group),size=1)+
         geom_ribbon(data=ldata2,aes(x=year,ymin=ifelse(mean-sd<=0,0,mean-sd),ymax=mean+sd,fill=group),alpha=0.2)+
          theme_bw()+
          theme(panel.grid.minor = element_line(color = col_grid, size = 0.3, linetype = 'dashed'),
                panel.grid.major = element_line(color = col_grid, size = 0.5),
                strip.background =element_rect(fill="grey92"),
                text = element_text(size=14),
                legend.position = 'none',
                axis.text.x = element_text(size=12))+
          #scale_x_continuous(breaks = c(2002,2005,2010,2015,2020),labels = c(2002,2005,2010,2015,2020),
          #                   minor_breaks = c(2002:2004,2006:2009,2011:2014,2015:2019))+
          scale_fill_manual(values = palette  ,na.translate=FALSE, name=title)+
          scale_color_manual(values =palette  ,na.translate=FALSE, name=title)+
          ylab("Biomass loss (million pounds)")+ 
          expand_limits(y = 0)
   
        p3<-ggplot()+
          geom_line(data=mdata2,aes(x=year,y=mean,color=group),size=1)+
          geom_ribbon(data=mdata2,aes(x=year,ymin=ifelse(mean-sd<=0,0,mean-sd),ymax=mean+sd,fill=group),alpha=0.2)+
          theme_bw()+
          theme(panel.grid.minor = element_line(color = col_grid, size = 0.3, linetype = 'dashed'),
                panel.grid.major = element_line(color = col_grid, size = 0.5),
                strip.background =element_rect(fill="grey92"),
                text = element_text(size=14),
                legend.position = 'none',
                axis.text.x = element_text(size=12))+
          #scale_x_continuous(breaks = c(2002,2005,2010,2015,2020),labels = c(2002,2005,2010,2015,2020),
          #                   minor_breaks = c(2002:2004,2006:2009,2011:2014,2015:2019))+
          scale_fill_manual(values = palette  ,na.translate=FALSE, name=title)+
          scale_color_manual(values =palette  ,na.translate=FALSE, name=title)+
          ylab("Annual red tide mortality rate")+ 
          expand_limits(y = 0)
  
        #get legend
        legend <- get_legend(
          p3 +  theme(legend.position = "right"))
        
        # arrange the three plots in a single row
        prow <- plot_grid(
        p1 + theme(legend.position="none"),
        p2 + theme(legend.position="none"),
        p3 + theme(legend.position="none"),
        legend,
        hjust = -1,
        vjust = 1.1,
        nrow = 1,
        rel_widths = c(3,3,3,2))    
       
    pp<-
       if (!('Mortality and foraging' %in%  input$fxn )) {
          plot_grid(prow,p1m,nrow = 2,rel_heights = c(6,4))
       } else if (length(unique(fxn1m$sens))==1 & length(unique(fxn1f$sens))==1) {
          u<-plot_grid(p1m,p1f,nrow = 1)      
          plot_grid(prow,u,nrow = 2,rel_heights = c(6,4))
       } else{
          plot_grid(prow,p1m,p1f,nrow = 3,rel_heights = c(6,4,4))
       }
    
     #add title selected FG
     annotate_figure(pp, top = text_grob(input$FG,color = "black", face = "bold", size = 20))
        
       } else {
        
        p1<- ggplot()+
          geom_line(data=bdata2,aes(x=year,y=mean,color=group),size=1)+
         theme_bw()+
          theme(panel.grid.minor = element_line(color = col_grid, size = 0.3, linetype = 'dashed'),
                panel.grid.major = element_line(color = col_grid, size = 0.5),
                strip.background =element_rect(fill="grey92"),
                text = element_text(size=14),
                legend.position = 'none',
                axis.text.x = element_text(size=12))+
          #scale_x_continuous(breaks = c(2002,2005,2010,2015,2020),labels = c(2002,2005,2010,2015,2020),
          #                   minor_breaks = c(2002:2004,2006:2009,2011:2014,2015:2019))+
          scale_fill_manual(values = palette  ,na.translate=FALSE, name=title)+
          scale_color_manual(values =palette  ,na.translate=FALSE, name=title)+
          ylab("Biomass (million pounds)")+ 
          expand_limits(y = 0)
        
        p2<- ggplot()+
          geom_line(data=ldata2,aes(x=year,y=mean,color=group),size=1)+
          theme_bw()+
          theme(panel.grid.minor = element_line(color = col_grid, size = 0.3, linetype = 'dashed'),
                panel.grid.major = element_line(color = col_grid, size = 0.5),
                strip.background =element_rect(fill="grey92"),
                text = element_text(size=14),
                legend.position = 'none',
                axis.text.x = element_text(size=12))+
          #scale_x_continuous(breaks = c(2002,2005,2010,2015,2020),labels = c(2002,2005,2010,2015,2020),
          #                   minor_breaks = c(2002:2004,2006:2009,2011:2014,2015:2019))+
          scale_fill_manual(values = palette  ,na.translate=FALSE, name=title)+
          scale_color_manual(values =palette  ,na.translate=FALSE, name=title)+
          ylab("Biomass loss (million pounds)")+ 
          expand_limits(y = 0)
        
      
        p3<-ggplot()+
          geom_line(data=mdata2,aes(x=year,y=mean,color=group),size=1)+
          theme_bw()+
          theme(panel.grid.minor = element_line(color = col_grid, size = 0.3, linetype = 'dashed'),
                panel.grid.major = element_line(color = col_grid, size = 0.5),
                strip.background =element_rect(fill="grey92"),
                text = element_text(size=14),
                legend.position = 'none',
                axis.text.x = element_text(size=12))+
          #scale_x_continuous(breaks = c(2002,2005,2010,2015,2020),labels = c(2002,2005,2010,2015,2020),
          #                   minor_breaks = c(2002:2004,2006:2009,2011:2014,2015:2019))+
          scale_fill_manual(values = palette  ,na.translate=FALSE, name=title)+
          scale_color_manual(values =palette  ,na.translate=FALSE, name=title)+
          ylab("Annual red tide mortality rate")+ 
          expand_limits(y = 0)
        
      
       }
      
      #get legend
      legend <- get_legend(
        p3 +  theme(legend.position = "right"))
      
      # arrange the three plots in a single row
      prow <- plot_grid(
        p1 + theme(legend.position="none"),
        p2 + theme(legend.position="none"),
        p3 + theme(legend.position="none"),
        legend,
        hjust = -1,
        vjust = 1.1,
        nrow = 1,
        rel_widths = c(3,3,3,2))    
      
      pp<-
        if (!('Mortality and foraging' %in%  input$fxn )) {
          plot_grid(prow,p1m,nrow = 2,rel_heights = c(6,4))
        } else if (length(unique(fxn1m$sens))==1 & length(unique(fxn1f$sens))==1) {
          u<-plot_grid(p1m,p1f,nrow = 1)      
          plot_grid(prow,u,nrow = 2,rel_heights = c(6,4))
        } else{
          plot_grid(prow,p1m,p1f,nrow = 3,rel_heights = c(6,4,4))
        }
      
      #add title selected FG
      annotate_figure(pp, top = text_grob(input$FG,color = "black", face = "bold", size = 20))

    }
  },width = 1200,height = 900)
  
 
  #create red tide severity maps
   output$maps <-  renderPlot({
  
    req(input$year)
    redtideyear<-redtidemaps[[as.character(input$year)]]
    redtideyearDV<-redtidemapsDV[[as.character(input$year)]]
    
    par(mar = c(0, 0, 0, 0))
    plot(NA, xlim = c(0, 1400), ylim = c(0, 1000), type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "",bty="n")
    rasterImage(redtideyear, 0, 0, 700, 1000)
    rasterImage(redtideyearDV, 700, 0, 1400, 1000)
   
  },width = 1400,height = 700,res=50)
  
#create dataframe of red tide mortality rate
    mort<-reactive({
    #require these inp uts
    req(input$FG)
    req(input$comparison)
    
    #example
    #input<-list()
    #input$redtideFG<-'All consumer groups'
    #input$m_sens<- c(unique(as.vector(data_runs$m_sens)))[-1]
    #input$m_slope <- c(unique(as.vector(data_runs$m_slope)))
    #input$fxn <- c(unique(as.vector(data_runs$fxn)))[2]
    #input$FG<-'gag 0'
    #input$f_sens<-c(unique(as.vector(data_runs$f_sens)))[-1][1]
    #input$comparison<-"m_sens"
    
    #show error if none option is selected in each variable
    validate(
      need(input$redtideFG, "Please you must select an option for 'Red tides applied to' variable"),
      need(input$m_slope, "Please you must select an option for 'Mortality slope' variable"),
      need(input$m_sens, "Please you must select an option for 'Red tide effects threshold' variable"),
      need(input$fxn, "Please you must select an option for 'Red tides effects considered' variable")
    )
    
    #get information on data runs
    data_runs<-read.csv('https://raw.githubusercontent.com/danielvilasgonzalez/redtideapp/main/data_runs.csv')
    
    #filter by using red tide on gag or all consumer groups
    filtered_redtideFG <-subset(data_runs,FG %in% input$redtideFG)
    filtered_msens <- subset(filtered_redtideFG,m_sens %in% input$m_sens)
    filtered_mslope <- subset(filtered_msens,m_slope %in% input$m_slope)
    filtered_fxn <- subset(filtered_mslope,fxn %in% input$fxn)
    
    #filter by foraging sensitivity
    fully_filtered  <- 
      if('Mortality and foraging' %in% input$fxn){
        rbind(subset(filtered_fxn,f_sens %in% c(input$f_sens)),subset(filtered_fxn,is.na(f_sens)))
      } else {
        filtered_fxn
      } 
    
    #get runs
    runs<-fully_filtered$runs
    runs1<-paste0('run',formatC(fully_filtered$runs, width = 5, format = "d", flag = "0"))
    
    #get classification
    clas<-unique(fully_filtered[, input$comparison])
    #get columns from classification
    cols<-fully_filtered[,input$comparison]
    
    #filter by range of years
    data<-data[c(as.character(input$rang[1]:input$rang[2])),,,]
    
    #biomass
    b<-data.frame(data[,input$FG,runs1,'bio'])
    colnames(b)<-cols
    #loss
    l<-data.frame(data[,input$FG,runs1,'los'])
    colnames(l)<-cols
    #mortality
    m<-data.frame(data[,input$FG,runs1,'mor'])
    colnames(m)<-cols
    
    
    if (length(clas)>=2) {
      
      bdata1<-data.frame(matrix(NA,nrow = 1,ncol = 6))
      colnames(bdata1)<-c('year','mean','q5','q95','sd','group')
      ldata1<-bdata1
      mdata1<-bdata1
     
      for (c in clas) {
        
        #c<-clas[1]
        
        index<-which(cols %in% c)
        
        bcsv2<-b[,index]
        lcsv2<-l[,index]
        mcsv2<-m[,index]
        
        
        if (is.numeric(bcsv2)) {
          
          bdata<-data.frame(year=c(input$rang[1]:input$rang[2]),mean=bcsv2,q5=rep(NA,times=length(input$rang[1]:input$rang[2])),q95=rep(NA,times=length(input$rang[1]:input$rang[2])),sd=rep(NA,times=length(input$rang[1]:input$rang[2])),group=rep(c,times=length(input$rang[1]:input$rang[2])))
          ldata<-data.frame(year=c(input$rang[1]:input$rang[2]),mean=lcsv2,q5=rep(NA,times=length(input$rang[1]:input$rang[2])),q95=rep(NA,times=length(input$rang[1]:input$rang[2])),sd=rep(NA,times=length(input$rang[1]:input$rang[2])),group=rep(c,times=length(input$rang[1]:input$rang[2])))
          mdata<-data.frame(year=c(input$rang[1]:input$rang[2]),mean=mcsv2,q5=rep(NA,times=length(input$rang[1]:input$rang[2])),q95=rep(NA,times=length(input$rang[1]:input$rang[2])),sd=rep(NA,times=length(input$rang[1]:input$rang[2])),group=rep(c,times=length(input$rang[1]:input$rang[2])))
          
        } else {
          
          #biomass calc
          bdata<-data.frame(year=c(input$rang[1]:input$rang[2]),
                            mean=round(rowMeans(bcsv2),digits = 6),
                            q5=round(apply(bcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[1,],digits = 6),
                            q95=round(apply(bcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[2,],digits = 6),
                            sd=round(apply(bcsv2, 1, sd,  na.rm = TRUE),digits = 6),
                            group=rep(c,times=length(input$rang[1]:input$rang[2])))
          
          #loss calc
          ldata<-data.frame(year=c(input$rang[1]:input$rang[2]),
                            mean=round(rowMeans(lcsv2),digits = 6),
                            q5=round(apply(lcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[1,],digits = 6),
                            q95=round(apply(lcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[2,],digits = 6),
                            sd=round(apply(lcsv2, 1, sd,  na.rm = TRUE),digits = 6),
                            group=rep(c,times=length(input$rang[1]:input$rang[2])))

          #red tide mortality rate
          mdata<-data.frame(year=c(input$rang[1]:input$rang[2]),
                            mean=round(rowMeans(mcsv2),digits = 6),
                            q5=round(apply(mcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[1,],digits = 6),
                            q95=round(apply(mcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[2,],digits = 6),
                            sd=round(apply(mcsv2, 1, sd,  na.rm = TRUE),digits = 6),
                            group=rep(c,times=length(input$rang[1]:input$rang[2])))
        }
        
        
        bdata1<-rbind(bdata1,bdata)
        ldata1<-rbind(ldata1,ldata)
        mdata1<-rbind(mdata1,mdata)
        
      }
      
      bdata2<-bdata1[2:nrow(bdata1),]
      ldata2<-ldata1[2:nrow(ldata1),]
      mdata2<-mdata1[2:nrow(mdata1),]
      
      bdata2 <- bdata2[,colSums(is.na(bdata2))<nrow(bdata2)]
      ldata2 <- ldata2[,colSums(is.na(ldata2))<nrow(ldata2)]
      mdata2 <- mdata2[,colSums(is.na(mdata2))<nrow(mdata2)]
      
      bdata2<-bdata2[which(bdata2$year>=2002),]
      ldata2<-ldata2[which(ldata2$year>=2002),]
      mdata2<-mdata2[which(mdata2$year>=2002),]    
      
      bdata2$mean<-bdata2$mean*2000*170000/1000000
      bdata2$q5<-bdata2$q5*2000*170000/1000000
      bdata2$q95<-bdata2$q95*2000*170000/1000000
      ldata2$mean<-ldata2$mean*2000*170000/1000000
      ldata2$q5<-ldata2$q5*2000*170000/1000000
      ldata2$q95<-ldata2$q95*2000*170000/1000000
      
      mdata2$mean<-round(mdata2$mean,digits = 6)
      mdata2$q5<-round(mdata2$q5,digits = 6)
      mdata2$q95<-round(mdata2$q95,digits = 6)
      
      colnames(mdata2)<-c('year','mean','5% CI','95% CI','SD','group')
      mdata2$FG<-input$FG
      
      mdata2<-mdata2[c('year','mean','SD','group','FG')]
      
      mdata2
      
     } else {
      
       bcsv2<-b
       lcsv2<-l
       mcsv2<-m
       
       if (is.numeric(b)) {
         
         bdata<-data.frame(year=c(input$rang[1]:input$rang[2]),mean=bcsv2,q5=rep(NA,times=length(input$rang[1]:input$rang[2])),q95=rep(NA,times=length(input$rang[1]:input$rang[2])),group=rep(clas,times=length(input$rang[1]:input$rang[2])))
         ldata<-data.frame(year=c(input$rang[1]:input$rang[2]),mean=lcsv2,q5=rep(NA,times=length(input$rang[1]:input$rang[2])),q95=rep(NA,times=length(input$rang[1]:input$rang[2])),group=rep(clas,times=length(input$rang[1]:input$rang[2])))
         mdata<-data.frame(year=c(input$rang[1]:input$rang[2]),mean=mcsv2,q5=rep(NA,times=length(input$rang[1]:input$rang[2])),q95=rep(NA,times=length(input$rang[1]:input$rang[2])),group=rep(clas,times=length(input$rang[1]:input$rang[2])))
         
       } else {
         
         #biomass calc
         bdata<-data.frame(year=c(input$rang[1]:input$rang[2]),
                           mean=round(rowMeans(bcsv2),digits = 6),
                           q5=round(apply(bcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[1,],digits = 6),
                           q95=round(apply(bcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[2,],digits = 6),
                           sd=round(apply(bcsv2, 1, sd,  na.rm = TRUE),digits = 6),
                           group=rep(c,times=length(input$rang[1]:input$rang[2])))
         
         #loss calc
         ldata<-data.frame(year=c(input$rang[1]:input$rang[2]),
                           mean=round(rowMeans(lcsv2),digits = 6),
                           q5=round(apply(lcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[1,],digits = 6),
                           q95=round(apply(lcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[2,],digits = 6),
                           sd=round(apply(lcsv2, 1, sd,  na.rm = TRUE),digits = 6),
                           group=rep(c,times=length(input$rang[1]:input$rang[2])))
         
         #red tide mortality rate
         mdata<-data.frame(year=c(input$rang[1]:input$rang[2]),
                           mean=round(rowMeans(mcsv2),digits = 6),
                           q5=round(apply(mcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[1,],digits = 6),
                           q95=round(apply(mcsv2, 1, quantile, probs = c(0.05, 0.95),  na.rm = TRUE)[2,],digits = 6),
                           sd=round(apply(mcsv2, 1, sd,  na.rm = TRUE),digits = 6),
                           group=rep(c,times=length(input$rang[1]:input$rang[2])))
       }
       
       bdata2 <- bdata[,colSums(is.na(bdata))<nrow(bdata)]
       ldata2 <- ldata[,colSums(is.na(ldata))<nrow(ldata)]
       mdata2 <- mdata[,colSums(is.na(mdata))<nrow(mdata)]
       
       bdata2<-bdata2[which(bdata2$year>=2002),]
       ldata2<-ldata2[which(ldata2$year>=2002),]
       mdata2<-mdata2[which(mdata2$year>=2002),]    
       
       bdata2$mean<-bdata2$mean*2000*170000/1000000
       bdata2$q5<-bdata2$q5*2000*170000/1000000
       bdata2$q95<-bdata2$q95*2000*170000/1000000
       ldata2$mean<-ldata2$mean*2000*170000/1000000
       ldata2$q5<-ldata2$q5*2000*170000/1000000
       ldata2$q95<-ldata2$q95*2000*170000/1000000
       
       mdata2$mean<-round(mdata2$mean,digits = 6)
       mdata2$q5<-round(mdata2$q5,digits = 6)
       mdata2$q95<-round(mdata2$q95,digits = 6)
       
       colnames(mdata2)<-c('year','mean','5% CI','95% CI','SD','group')
       mdata2$FG<-input$FG
       
       mdata2<-mdata2[c('year','mean','SD','group','FG')]
       
       mdata2
     }  })
    
    output$summary <- DT::renderDataTable({   
    
      mort()
    #data.table::data.table(mort())

      },rownames= FALSE, extensions = 'Buttons', options = list(pageLength = 100,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf')))

}  
shinyApp(ui = ui, server = server)
#
