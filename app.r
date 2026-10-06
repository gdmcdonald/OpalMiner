library(shiny)
library(shinydashboard)
library(DT)
library(rvest)
library(data.table)
library(tidyverse)
library(lubridate)
library(plotly)

# ---- constants -------------------------------------------------------------

WEEKDAY_NAMES <- c("Monday", "Tuesday", "Wednesday", "Thursday",
                   "Friday", "Saturday", "Sunday")

# Opal off-peak discount applied to train fares (check against current fares)
OFFPEAK_DISCOUNT <- 0.3

# "Neighbours" are currently taps within NEIGHBOUR_BOX degrees of this point
# (central Sydney). ~0.01 degrees is roughly 1 km.
NEIGHBOUR_LAT <- -33.86658
NEIGHBOUR_LON <- 151.2070
NEIGHBOUR_BOX <- 0.01

# ---- scraping --------------------------------------------------------------

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
  slot = 0
  # Opal seems to serve a card's data again for card indexes that don't exist,
  # so remember each card's first page and stop when one repeats.
  seen_first_pages <- character(0)
  # Loop through each card, then each page of that card's transactions.
  for (cardIndex in 1:9) {
    pages_with_data <- 0

    for (pageIndex in 1:100) {

      url <- jump_to(session, paste0("https://www.opal.com.au/registered/opal-card-transactions/opal-card-activities-list?cardIndex=",cardIndex,"&pageIndex=",pageIndex))

      web_table<-url%>%
        read_html()%>%
        html_nodes(xpath = '//*[@id="transaction-data"]')

      transaction_data<-web_table%>%
        html_table(trim = F)

      # No transaction table at all.
      if (length(transaction_data)==0) {
        # On the very first page of the first card this means we are not
        # logged in. Anywhere else it just means there is no such card/page.
        if (cardIndex == 1 && pageIndex == 1) {
          message("incorrect login info")
          showNotification("Incorrect login information. You may need to register your opal card on www.opal.com.au")
          return(NULL)
        }
        break
      }

      this_transaction_data_frame <- transaction_data[[1]]
      # How many elements = rows*columns in the data frame?
      # 1x1 "NA" = no data.
      num_el = nrow(this_transaction_data_frame)*ncol(this_transaction_data_frame)

      # Test for having gone beyond the last page
      # Written this way to catch the empty set as well as the 1x1 "NA" dataframe I expect it to return.
      if (!isTRUE(num_el>1)) break

      if (pageIndex == 1) {
        page_key <- paste(unlist(this_transaction_data_frame), collapse = "|")
        if (page_key %in% seen_first_pages) break # same data as an earlier card
        seen_first_pages <- c(seen_first_pages, page_key)
      }

      slot <- slot+1 #update loop counter if there's data
      pages_with_data <- pages_with_data + 1
      # Extract the mode of transport from the alt text of the little "T", "F" or "B" images (including NA for not a trip)
      scope<-web_table%>%html_children()
      mode_of_transport <- scope[3] %>%
        html_nodes(xpath = '//tr/td[3]') %>%
        html_node("img") %>%
        html_attr(name = "alt")

      this_transaction_data_frame$Mode <- factor(mode_of_transport,
                                                 levels = c("train","bus","lightrail","ferry"),
                                                 exclude = NULL)

      transaction_data_frames_list[[slot]]<-this_transaction_data_frame #put all pages' transactions together
    }

    # A card with no transactions means we have run out of cards.
    if (pages_with_data == 0) break
  }

  if (slot == 0) {
    showNotification("No Opal transactions were found for this login.")
    return(NULL)
  }

  # Combine the list into one big dataframe
  # (distinct() is a safety net against any repeated rows across cards/pages)
  transactions_df<-rbindlist(transaction_data_frames_list)%>%
    distinct()

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
           DayOfWeek = factor(DayOfWeek,levels = WEEKDAY_NAMES) )

  return(as_tibble(transactions_df))

}

#example usage
#transactions_df<-scrapeOpalData(username = "blah@gmail.com",
#                                password = "notrealpassword")

# ---- summary maths ---------------------------------------------------------

# Summarise the last `num_weeks` weeks of a transactions data frame.
#
# Averages are totals over the window divided by the number of weeks in the
# window (so weeks with no trips count as zero). If the data covers fewer than
# `num_weeks` weeks, the data's own span is used instead.
#
# Peak = weekday, within 60 min of 08:00 or 75 min of 17:15
# (i.e. 07:00-09:00 and 16:00-18:30).
#
# Returns NULL if there are no trips, otherwise a list.
summarise_opal <- function(transactions_df, num_weeks){

  if (is.null(transactions_df) || nrow(transactions_df) == 0) return(NULL)

  max_date <- max(transactions_df$DateTime, na.rm = TRUE)
  min_date <- min(transactions_df$DateTime, na.rm = TRUE)

  span_weeks <- max(1, as.numeric(difftime(max_date, min_date, units = "days")) / 7)
  n_weeks    <- min(num_weeks, span_weeks)
  window_start <- max_date - lubridate::days(7 * num_weeks)

  trips <- transactions_df %>%
    filter(!is.na(Mode), DateTime > window_start) %>%
    mutate(minutes_since_midnight = lubridate::hour(DateTime) * 60 +
                                    lubridate::minute(DateTime),
           Peak_time = Weekday == "Weekday" &
                       (abs(minutes_since_midnight - 8*60)      < 60 |
                        abs(minutes_since_midnight - (17*60+15)) < 75))

  if (nrow(trips) == 0) return(NULL)

  by_mode <- trips %>%
    group_by(Mode) %>%
    summarise(av_spent_per_week = -sum(Amount_n,   na.rm = TRUE) / n_weeks,
              av_trips_per_week = n()                            / n_weeks,
              av_disc_per_week  =  sum(Discount_n, na.rm = TRUE) / n_weeks,
              .groups = "drop")

  reward_trips <- if ("Fare Applied" %in% names(trips)) {
    sum(trips$`Fare Applied` %in% "Travel Reward")
  } else 0

  peak_train <- trips %>% filter(Mode == "train", Peak_time)

  av_num_trips <- sum(by_mode$av_trips_per_week)

  list(
    table                 = by_mode %>%
      mutate(av_spent_per_week = round(av_spent_per_week, 2),
             av_trips_per_week = round(av_trips_per_week, 1),
             av_disc_per_week  = round(av_disc_per_week, 1)),
    n_weeks               = n_weeks,
    dollars_spent         = sum(by_mode$av_spent_per_week),
    av_num_trips          = av_num_trips,
    dollars_saveable      = max(0, -sum(peak_train$Amount_n, na.rm = TRUE) / n_weeks) *
                              OFFPEAK_DISCOUNT,
    peak_trip_number      = nrow(peak_train) / n_weeks,
    av_reward_trips       = reward_trips / n_weeks,
    pct_half_price        = if (av_num_trips > 0) reward_trips / n_weeks / av_num_trips * 100 else 0,
    dollars_already_saved = sum(by_mode$av_disc_per_week)
  )
}

# ---- plotting --------------------------------------------------------------

# Shaded rectangle (in x-axis hours, full plot height) for peak periods
peak_rect <- function(x0, x1){
  list(type = "rect",
       fillcolor = "red", line = list(color = "red"), opacity = .2,
       x0 = x0, x1 = x1, xref = "x",
       y0 = 0,  y1 = 1,  yref = "paper")
}

# Stacked bar of counts per hour, coloured by Mode.
# `counts` needs columns Mode, timeHr, my_count.
plot_day_bars <- function(counts, weekday_i_want, is.weekday){

  pl <- plot_ly(data = counts,
                x = ~timeHr,
                y = ~my_count,
                color = ~Mode,
                type = "bar") %>%
    layout(barmode = "stack",
           yaxis = list(title = weekday_i_want),
           xaxis = list(range = c(0, 24),
                        title = "Time of Day (24Hr)",
                        autotick = FALSE,
                        ticks = "outside",
                        tick0 = 0,
                        dtick = 3))

  if (is.weekday) {
    pl <- pl %>% layout(shapes = list(peak_rect(7, 9), peak_rect(16, 18.5)))
  }
  pl
}

plot_day<-function(transactions_df,weekday_i_want){

  dff<-transactions_df%>%
    filter(!is.na(Mode),DayOfWeek == weekday_i_want)

  if(nrow(dff)<1){return(plotly_empty())}

  counts <- dff%>%
    mutate(timeHr = lubridate::hour(DateTime))%>%
    group_by(Mode, timeHr)%>%
    summarise(my_count = n(), .groups = "drop")

  plot_day_bars(counts, weekday_i_want, dff$Weekday[1]=="Weekday")
}

plot_day_other<-function(transactions_df,weekday_i_want){

  dff<-transactions_df%>%
    filter(!is.na(Mode),DayOfWeek == weekday_i_want)

  if(nrow(dff)<1){return(plotly_empty())}

  counts <- dff%>%
    group_by(Mode, timeHr)%>%
    summarise(my_count = sum(count), .groups = "drop")

  plot_day_bars(counts, weekday_i_want, dff$Weekday[1]=="Weekday")
}

# Taps of one type ("on"/"off") inside a square box around a point
loc_by_timeHr<-function(all_joined_patronage,this_lat,this_lon,square_box,tap_type){

  all_joined_patronage%>%
    filter(tap == tap_type,
           lat>this_lat-square_box,
           lat<this_lat+square_box,
           lon>this_lon-square_box,
           lon<this_lon+square_box)

}

#load all_joined_patronage dataframe
load("JoinedPatronage.RData")
all_joined_patronage <- all_joined_patronage%>%
  mutate(Mode = factor(Mode,levels = c("train","bus","lightrail","ferry")))

# "Others" are stuck in one location (see NEIGHBOUR_* above); this never
# changes, so build it once rather than once per session.
others_df <- rbind(loc_by_timeHr(all_joined_patronage,NEIGHBOUR_LAT, NEIGHBOUR_LON, NEIGHBOUR_BOX, "on"),
                   loc_by_timeHr(all_joined_patronage,NEIGHBOUR_LAT, NEIGHBOUR_LON, NEIGHBOUR_BOX, "off"))


dbHeader <- dashboardHeader(title = "OpalMiner")
dbHeader$children[[2]]$children <-  tags$a(href='https://www.opal.com.au/',
                                           tags$img(src='opal.jpg',height='40',width='40'), "OpalMiner" )

sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
    menuItem("Log in",        tabName = "login",    icon = icon("unlock")),
    menuItem("Data",          tabName = "data",     icon = icon("file")),
    menuItem("Date and Time", tabName = "datetime", icon = icon("calendar")),
    menuItem("Neighbours Date & Time", tabName = "otherdatetime", icon = icon("users")),
    menuItem("Trip  and $ Summary",       tabName = "money",    icon = icon("flag")),
    menuItem("Locations",     tabName= "locations", icon = icon("map")),
    menuItem("About",      tabName= "about",  icon = icon("question-circle"))
  ),
  collapsed = TRUE)

body <- dashboardBody(tabItems(

  ## login tab

  tabItem(tabName = "login",
          textInput("username", "Opal Username/email:"),
          passwordInput("password", "Password:"),
          actionButton("loginButton","Login", icon("unlock-alt")),
          verbatimTextOutput("value"),
          br(),br(),
          box(p(strong("OpalMiner")," shows you the weekly patterns in your public transport usage."),
              br(),
              p("It allows you to compare your usage to that of a comprable cohort of people tapping on and off at locations near where you do."),
              br(),
              p("It also shows you how much money you could save by shifting the times at which you ride the train."),
              br(),
              p("We do not save any of your data or information, anywhere."),
              br(),
              tags$img(src='miner.jpg',width='40%',align = "middle"),width = 12
          )),


  ## tabular data tab

  tabItem(tabName = "data",
          box(downloadButton('downloadData', 'Download Opal Data'),
              title = "Export your opal card data as a .csv file"
          ),
          DT::DTOutput("opalDataTable")),


  ## date/time tab

  tabItem(tabName = "datetime",
          box(
            plotlyOutput("mondayPlot",height = "200px"),
            plotlyOutput("tuesdayPlot",height = "200px"),
            plotlyOutput("wednesdayPlot",height = "200px"),
            plotlyOutput("thursdayPlot",height = "200px"),
            plotlyOutput("fridayPlot",height = "200px"),
            plotlyOutput("saturdayPlot",height = "200px"),
            plotlyOutput("sundayPlot",height = "200px"),
            title = "Your Weekly Opal Usage",
            width = 12)
  ),
  tabItem(tabName = "otherdatetime",
          box(
            plotlyOutput("mondayPlotO",height = "200px"),
            plotlyOutput("tuesdayPlotO",height = "200px"),
            plotlyOutput("wednesdayPlotO",height = "200px"),
            plotlyOutput("thursdayPlotO",height = "200px"),
            plotlyOutput("fridayPlotO",height = "200px"),
            plotlyOutput("saturdayPlotO",height = "200px"),
            plotlyOutput("sundayPlotO",height = "200px"),
            title = "Your Neighbours' Weekly Opal Usage",
            width = 12)
  ),
  tabItem(tabName = "money",
          box(textOutput("text_summary"),
              title = "Summary"),
          DT::dataTableOutput("summary_table"),
          sliderInput("num_weeks","Last n weeks", min = 1, max = 52, value = 12, step = 1)
  ),
  tabItem(tabName = "locations",
          box(tags$img(src='map.gif',width='80%'),
              title = "Map",
              width = 12)),

  tabItem(tabName = "about",
          box(p(strong("OpalMiner")," shows you the weekly patterns in your public transport usage."),
              br(),
              p("It allows you to compare your usage to that of a comprable cohort of people tapping on and off at locations near where you do."),
              br(),
              p("It also shows you how much money you could save by shifting the times at which you ride the train."),
              br(),
              p("We do not save any of your data or information, anywhere."),
              br(),
              tags$img(src='miner.jpg',width='70%'),
              title = "About")
  )

))


ui <- dashboardPage(title = "OpalMiner",
                    dbHeader,
                    sidebar,
                    body,
                    skin = "black"
)

server <- function(input, output, session) {

  # Everything user-specific lives in these session-scoped reactive values,
  # never in global variables, so users cannot see each other's data.
  transactions  <- reactiveVal(NULL)
  login_status  <- reactiveVal("")

  observeEvent(input$loginButton, {

    df <- tryCatch(
      withProgress(
        scrapeOpalData(username = input$username,
                       password = input$password),
        message = "Loading Opal Data",
        detail = "This may take a minute or two"),
      error = function(e){
        showNotification(paste("Could not retrieve Opal data:", conditionMessage(e)),
                         type = "error")
        NULL
      })

    # Don't leave the password sitting in the page once we've used it.
    updateTextInput(session, "password", value = "")

    if (is.null(df)) {
      transactions(NULL)
      login_status("Not Logged In, perhaps your password is incorrect")
      return()
    }

    # Tap-on reversals are not real trips
    df$Mode[grepl("^Tap on reversal", df$Details)] <- NA

    transactions(df)
    login_status("Logged In, click the hamburger to see the menu above.")
    updateTabItems(session, "tabs", selected = "data")
  })

  output$value <- renderText(login_status())

  ## DT library to renderDataTable
  output$opalDataTable <- renderDT({
    req(transactions())
    DT::datatable(transactions()[,c("DayOfWeek","DateTime","Details","Amount")],
                  options = list(pageLength = 13))
  })

  output$downloadData <- downloadHandler(
    filename = function(){
      "OpalCardData.csv"
    },

    content = function(file) {
      req(transactions())
      write.csv(transactions()[,c("DayOfWeek",
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

  # One plot per weekday, for you (e.g. mondayPlot) and your neighbours (mondayPlotO)
  for (day in WEEKDAY_NAMES) {
    local({
      this_day <- day
      id <- paste0(tolower(this_day), "Plot")

      output[[id]] <- renderPlotly({
        req(transactions())
        plot_day(transactions(), this_day)
      })

      output[[paste0(id, "O")]] <- renderPlotly(plot_day_other(others_df, this_day))
    })
  }

  #summary info, shared by the table and the text
  summary_data <- reactive({
    req(transactions())
    summarise_opal(transactions(), input$num_weeks)
  })

  output$summary_table <- renderDT({
    s <- summary_data()
    req(s)
    DT::datatable(s$table,
                  colnames = c("Mode of Transport", "Amount Spent in a week ($, av.)", "Number of trips per week (av.)","Av. discounts per week ($)"))
  })

  output$text_summary <- renderText({
    s <- summary_data()
    validate(need(s, "No trips found in the selected period."))

    period <- if (s$n_weeks < input$num_weeks) {
      paste0("In the ", round(s$n_weeks, 1), " weeks of data available")
    } else {
      paste0("In the last ", input$num_weeks, " weeks")
    }

    paste0(period,
           " you are spending $",
           round(s$dollars_spent,2),
           " per week on average for ",
           round(s$av_num_trips,1),
           " trips per week. You could save $",
           round(s$dollars_saveable,2),
           " by moving the remaining ",
           round(s$peak_trip_number,1),
           " of your peak-time trips to off-peak times. ",
           round(s$pct_half_price,0),
           "% of your trips (",
           round(s$av_reward_trips,1),
           " trips) are half-price after the 8-trip Weekly Travel Reward. In total, you are already saving $",
           round(s$dollars_already_saved,2),
           " per week by travelling at off-peak times and taking advantage of discounts offered by Opal.")
  })
}

shinyApp(ui, server)
