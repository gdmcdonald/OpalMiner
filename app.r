library(shinydashboard)
library(DT)
library(rvest)
library(data.table)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)

# define the scraping function
scrapeOpalData<-function(username,password){
  
  login_url <- "https://www.opal.com.au/login/index"
  session <- html_session(login_url)
  
  form <- html_form(read_html(login_url))[[2]]
  
  filled_form <- set_values(form,
                            h_username = username,
                            h_password = password)
  
  submit_form(session, filled_form)
  
  # Initialize an empty list
  transaction_data_frames_list = list()
  
  # Loop through each page starting with page 1 (limit = page 100) and scrape opal data.
  for (pageIndex in 1:100) {
    
    url <- jump_to(session, paste0("https://www.opal.com.au/registered/opal-card-transactions/opal-card-activities-list?pageIndex=",pageIndex))
    
    web_table<-url%>%
      read_html()%>%
      html_nodes(xpath = '//*[@id="transaction-data"]')
    
    transaction_data<-web_table%>%
      html_table(trim = F)
    
    # Check that we are logged in and recieving data
    if (length(transaction_data)==0) {
      print("incorrect login info")
      break}
    
    this_transaction_data_frame <- transaction_data[[1]]
    # How many elements = rows*columns in the data frame? 
    # 1x1 "NA" = no data.
    num_el = nrow(this_transaction_data_frame)*ncol(this_transaction_data_frame)
    
    # Test for having gone beyond the last page
    # Written this way to catch the empty set as well as the 1x1 "NA" dataframe I expect it to return.
    if (!isTRUE(num_el>1)) break
    
    # Extract the mode of transport from the alt text of the little "T", "F" or "B" images (including NA for not a trip)
    scope<-web_table%>%html_children()
    mode_of_transport <- scope[3] %>%
      html_nodes(xpath = '//tr/td[3]') %>%
      html_node("img") %>% 
      html_attr(name = "alt")
    
    this_transaction_data_frame$Mode <- factor(mode_of_transport,
                                               levels = c("train","bus","lightrail","ferry"), 
                                               exclude = NULL)
    
    transaction_data_frames_list[[pageIndex]]<-this_transaction_data_frame #put all pages' transactions together
  }
  
  # Combine the list into one big dataframe
  transactions_df<-rbindlist(transaction_data_frames_list)
  
  #Reformat the date/time, reformat the money to numeric
  transactions_df<-transactions_df%>%
    mutate(DateTime = paste0(substring(`Date/time`,4,13),
                             " ",
                             substring(`Date/time`,14,18))%>%
             dmy_hm( tz = "UTC"),
           
           Fare_n = Fare%>%
             substring(2)%>%
             as.numeric(),
           Discount_n = Discount%>%
             substring(2)%>%
             as.numeric(),
           Amount_n = if_else(is.na(Mode),
                              Amount%>%
                                substring(2)%>%
                                as.numeric(),
                              Discount_n -Fare_n),
           TimeOfDay = as.POSIXct(strftime(DateTime, 
                                           format="%H:%M:%S",
                                           tz = "GMT"), 
                                  format="%H:%M:%S", tz = "GMT"),
           Day = as.Date(DateTime),
           DayOfWeek = weekdays(Day),
           Weekday = ifelse(DayOfWeek %in% c("Saturday", "Sunday"),"Weekend","Weekday"),
           DayOfWeek = factor(DayOfWeek,levels = c( "Monday",  
                                                    "Tuesday",
                                                    "Wednesday", 
                                                    "Thursday", 
                                                    "Friday",
                                                    "Saturday", 
                                                    "Sunday") ))
  
  return(transactions_df)
  
}

#example usage
#transactions_df<-scrapeOpalData(username = "blah@gmail.com",
#                                password = "notrealpassword")

plot_day<-function(transactions_df,weekday_i_want){
  
  dff<-transactions_df%>%
    filter(!is.na(Mode),DayOfWeek == weekday_i_want)
  
  if(nrow(dff)<1){return(plotly_empty())}
  
  is.weekday<-(dff$Weekday[1]=="Weekday")
  
  wmi2<-dff%>%
    mutate(timeHr = strftime(TimeOfDay,format="%H",tz = "GMT")%>%as.numeric())%>%
    group_by(Mode, timeHr)%>%
    summarise(my_count = n())
  
  wmi3<-wmi2%>%
    group_by(timeHr)%>%
    summarise(total_count = sum(my_count))
  
  max_count<-max(wmi3$total_count)
  
  pl1<-plot_ly(data = wmi2,
               x = ~timeHr, 
               y = ~my_count,
               color = ~Mode, 
               type = "bar",
               xaxis  = paste0("x",weekday_i_want))
  if(is.weekday){
    pl2<-pl1%>%
      layout(barmode = "stack",
             yaxis = list(title = weekday_i_want),
             xaxis = list(range = c(0, 24),
                          title = "Time of Day (24Hr)",
                          autotick = FALSE,
                          ticks = "outside",
                          tick0 = 0,
                          dtick = 3),
             shapes = list(
               list(type = "rect",
                    fillcolor = "red", line = list(color = "red"), opacity = .2,
                    x0 = 7, x1 = 9, xref = paste0("x",weekday_i_want),
                    y0 = 0, y1 = max_count, yref = paste0("y",weekday_i_want)),
               list(type = "rect",
                    fillcolor = "red", line = list(color = "red"), opacity = .2,
                    x0 = 16, x1 = 18.5, xref = paste0("x",weekday_i_want),
                    y0 = 0, y1 = max_count, yref = paste0("y",weekday_i_want))
             ))
  } else {
    pl2<-pl1%>%
      layout(barmode = "stack",
             yaxis = list(title = weekday_i_want),
             xaxis = list(range = c(0, 24),
                          title = "Time of Day (24Hr)",
                          autotick = FALSE,
                          ticks = "outside",
                          tick0 = 0,
                          dtick = 3)
      )
  }
  return(pl2)
}

plot_day_other<-function(transactions_df,weekday_i_want){
  
  dff<-transactions_df%>%
    filter(!is.na(Mode),DayOfWeek == weekday_i_want)
  
  if(nrow(dff)<1){return(plotly_empty())}
  
  is.weekday<-(dff$Weekday[1]=="Weekday")
  
  wmi2<-dff%>%group_by(Mode, timeHr)%>%
    summarise(my_count = sum(count))
  
  wmi3<-wmi2%>%
    group_by(timeHr)%>%
    summarise(total_count = sum(my_count))
  
  max_count<-max(wmi3$total_count)
  
  pl1<-plot_ly(data = wmi2,
               x = ~timeHr, 
               y = ~my_count,
               color = ~Mode, 
               type = "bar",
               xaxis  = paste0("x",weekday_i_want))
  if(is.weekday){
    pl2<-pl1%>%
      layout(barmode = "stack",
             yaxis = list(title = weekday_i_want),
             xaxis = list(range = c(0, 24),
                          title = "Time of Day (24Hr)",
                          autotick = FALSE,
                          ticks = "outside",
                          tick0 = 0,
                          dtick = 3),
             shapes = list(
               list(type = "rect",
                    fillcolor = "red", line = list(color = "red"), opacity = .2,
                    x0 = 7, x1 = 9, xref = paste0("x",weekday_i_want),
                    y0 = 0, y1 = max_count, yref = paste0("y",weekday_i_want)),
               list(type = "rect",
                    fillcolor = "red", line = list(color = "red"), opacity = .2,
                    x0 = 16, x1 = 18.5, xref = paste0("x",weekday_i_want),
                    y0 = 0, y1 = max_count, yref = paste0("y",weekday_i_want))
             ))
  } else {
    pl2<-pl1%>%
      layout(barmode = "stack",
             yaxis = list(title = weekday_i_want),
             xaxis = list(range = c(0, 24),
                          title = "Time of Day (24Hr)",
                          autotick = FALSE,
                          ticks = "outside",
                          tick0 = 0,
                          dtick = 3)
      )
  }
  return(pl2)
}

loc_by_timeHr<-function(all_joined_patronage,this_lat,this_lon,square_box,tap_type){
  
  all_joined_patronage%>%
    filter(tap == tap_type,
           lat>this_lat-square_box,
           lat<this_lat+square_box,
           lon>this_lon-square_box,
           lon>this_lon+square_box)#%>%
  #group_by(Mode,DateTime,timeHr,TimeOfDay,Day,DayOfWeek,Weekday)%>%
  #summarise(total_count = sum(count))%>%
  #ungroup()
  
}

make_others_df<-function(transactions_df,all_joined_patronage){
  
}

#load all_joined_patronage dataframe
load("JoinedPatronage.RData")
all_joined_patronage%>%
  mutate(Mode = factor(Mode,levels = c("train","bus","lightrail","ferry")))


dbHeader <- dashboardHeader(title = "OpalMiner")
dbHeader$children[[2]]$children <-  tags$a(href='https://www.opal.com.au/',
                                           tags$img(src='https://d1ic4altzx8ueg.cloudfront.net/finder-au/wp-uploads/2016/01/opal-250x250.jpg',height='40',width='40'), "OpalMiner" )

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Log in",        tabName = "login",    icon = icon("unlock")),
    menuItem("Data",          tabName = "data",     icon = icon("file")),
    menuItem("Date and Time", tabName = "datetime", icon = icon("calendar")), 
    menuItem("Neighbours Date & Time ", tabName = "otherdatetime", icon = icon("users")), 
    menuItem("Locations",     tabName= "locations", icon = icon("map")),
    menuItem("Tutorial",      tabName= "tutorial",  icon = icon("question-circle"))
  ),
  collapsed = TRUE)

body <- dashboardBody(tabItems(
  
  ## login tab
  
  tabItem(tabName = "login",
          textInput("username", "Opal Username/email:"),
          passwordInput("password", "Password:"),
          actionButton("loginButton","Login", icon("unlock-alt")),
          verbatimTextOutput("value")),
  
  
  ## tabular data tab
  
  tabItem(tabName = "data",
          box(downloadButton('downloadData', 'Download Opal Data'),
              title = "Export your opal card data as a .csv file"
          ),
          DT::DTOutput("opalDataTable")),
  
  ## date/time tab
  
  tabItem(tabName = "datetime",
          #plotlyOutput("ggplotly_time_plot",height = "1000px")
          
          plotlyOutput("mondayPlot",height = "200px"),
          plotlyOutput("tuesdayPlot",height = "200px"),
          plotlyOutput("wednesdayPlot",height = "200px"),
          plotlyOutput("thursdayPlot",height = "200px"),
          plotlyOutput("fridayPlot",height = "200px"),
          plotlyOutput("saturdayPlot",height = "200px"),
          plotlyOutput("sundayPlot",height = "200px")
  ),
  tabItem(tabName = "otherdatetime",
          #plotlyOutput("ggplotly_time_plot",height = "1000px")
          
          plotlyOutput("mondayPlotO",height = "200px"),
          plotlyOutput("tuesdayPlotO",height = "200px"),
          plotlyOutput("wednesdayPlotO",height = "200px"),
          plotlyOutput("thursdayPlotO",height = "200px"),
          plotlyOutput("fridayPlotO",height = "200px"),
          plotlyOutput("saturdayPlotO",height = "200px"),
          plotlyOutput("sundayPlotO",height = "200px")
  )
  
  
))






ui <- dashboardPage(
  dbHeader,
  sidebar,
  body,
  skin = "black"
)

server <- function(input, output, session) {
  output$value <- renderText({
    req(input$loginButton)
    withProgress({
      transactions_df<<-scrapeOpalData(username = input$username,
                                       password = input$password)
      DT::datatable(transactions_df[,c("DayOfWeek","DateTime","Details","Amount")], 
                    options = list(pageLength = 13))
      
    },message = "Loading Opal Data")
    
    updateTabItems(session, "tabs", selected = "data")
    
    isolate("Logged In")
    
  })
  
  ## DT library to renderDataTable 
  output$opalDataTable <- renderDT(
    {
      DT::datatable(transactions_df[,c("DayOfWeek","DateTime","Details","Amount")], 
                    options = list(pageLength = 13))
    }
  )
  
  output$downloadData <- downloadHandler(
    filename <- function(){
      paste("OpalCardData.csv")
    },
    
    content = function(file) {
      write.csv(transactions_df[,c("DayOfWeek",
                                   "DateTime",
                                   "Details",
                                   "Mode",
                                   "Fare_n",
                                   "Discount_n",
                                   "Amount_n",
                                   "Transactionnumber")],
                file = file)
    },
    
    contentType = "text/csv"
  )
  
  ## plot ggplotly time plot
  output$ggplotly_time_plot <- renderPlotly({
    req(transactions_df)
    
    # demoDay<-strftime(transactions_df$TimeOfDay[1], 
    #                   format="%d/%m/%Y",
    #                   tz = "GMT")
    
    convert2time<-function(my_time){
      as.POSIXct(my_time, 
                 format="%H:%M:%S", 
                 tz = "GMT")
    }
    
    
    #make the plot with ggplot
    time_ggplot<-ggplot(transactions_df%>%
                          filter(!is.na(Mode)),
                        aes(x = TimeOfDay,
                            fill = Mode,
                            text = strftime(TimeOfDay, 
                                            format="%H:%M:%S",
                                            tz = "GMT"),
                            more_text = Details))+
      geom_histogram(bins = 24)  +
      scale_x_datetime(date_labels = "%H:%M",
                       date_minor_breaks = "1 hour")+
      theme(axis.text.x = element_text(angle = 90, 
                                       hjust = 1,
                                       vjust = .5) )+
      labs(y="Frequency",x="Time of day (24hr)", fill="Trip")+
      # annotate("rect", 
      #          xmin=convert2time("07:00:00"), 
      #          xmax=convert2time("09:00:00"), 
      #          ymin=0, 
      #          ymax=Inf, 
      #          alpha=0.2, 
      #          fill="red")+
      # annotate("rect", 
      #          xmin=convert2time("16:00:00"), 
      #          xmax=convert2time("18:30:00"), 
      #          ymin=0, 
    #          ymax=Inf, 
    #          alpha=0.2, 
    #          fill="red")+
    facet_grid(DayOfWeek~.)+
      ggtitle("Trips vs. Time Of Day")
    
    
    #convert to plotly, label so you can listen for clicks...
    ggplotly(time_ggplot, 
             source = "TimePlot",
             tooltip = c("fill","text","more_text"))%>%
      layout(#title = 'Highlighting with Rectangles',
        shapes = list(
          list(type = "rect",
               fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
               x0 = convert2time("07:00:00"), x1 = convert2time("09:00:00"), xref = "x",
               y0 = 4, y1 = 12.5, yref = "y"),
          list(type = "rect",
               fillcolor = "blue", line = list(color = "blue"), opacity = 0.2,
               x0 = convert2time("16:00:00"), x1 = convert2time("18:30:00"), xref = "x",
               y0 = 4, y1 = 12.5, yref = "y")))
    
  })
  
  output$mondayPlot<-renderPlotly(plot_day(transactions_df, "Monday"))
  output$tuesdayPlot<-renderPlotly(plot_day(transactions_df, "Tuesday"))
  output$wednesdayPlot<-renderPlotly(plot_day(transactions_df, "Wednesday"))
  output$thursdayPlot<-renderPlotly(plot_day(transactions_df, "Thursday"))
  output$fridayPlot<-renderPlotly(plot_day(transactions_df, "Friday"))
  output$saturdayPlot<-renderPlotly(plot_day(transactions_df, "Saturday"))
  output$sundayPlot<-renderPlotly(plot_day(transactions_df, "Sunday"))
  
  #bit of a cheat to make "others" stuck in one location
  others_df<-rbind(loc_by_timeHr(all_joined_patronage,-33.86658, 151.2070, 0.01, "on"),
                   loc_by_timeHr(all_joined_patronage,-33.86658, 151.2070, 0.01, "off"))
  output$mondayPlotO<-renderPlotly(plot_day_other(others_df, "Monday"))
  output$tuesdayPlotO<-renderPlotly(plot_day_other(others_df, "Tuesday"))
  output$wednesdayPlotO<-renderPlotly(plot_day_other(others_df, "Wednesday"))
  output$thursdayPlotO<-renderPlotly(plot_day_other(others_df, "Thursday"))
  output$fridayPlotO<-renderPlotly(plot_day_other(others_df, "Friday"))
  output$saturdayPlotO<-renderPlotly(plot_day_other(others_df, "Saturday"))
  output$sundayPlotO<-renderPlotly(plot_day_other(others_df, "Sunday"))
  
  
}

shinyApp(ui, server)

