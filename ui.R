library(shiny)

l = list.files(path  = "log")
t = gsub("RDS.oldcsvs.", "", l)
p = strptime(t, format = "%Y-%m-%d.%H:%M:%S")


shinyUI(fluidPage(
  titlePanel("Obtaining Representative Samples with Respondent Driven Sampling"),
  sidebarLayout(
    sidebarPanel(
     
      sliderInput("day", label = "date",  min = min(p), max  = max(p),value = min(p)),
      checkboxInput( "removeInvalids", label = "Remove Invalid Nodes", value = TRUE),
      checkboxInput("removeInvalidRefs", label = "Remove Invalid Referrals", value = TRUE),
      checkboxInput("asTree", label = "Display Data as Tree", value = TRUE)
      
      
      
    ),
    mainPanel(
      textOutput("text"),
      plotOutput("plot"),
      helpText(h4("Overview"), 
               span("This app displays (altered for anonymity) data from a", 
                   a("Respondent-driven", 
                     href="https://en.wikipedia.org/wiki/Snowball_sampling#Respondent-driven_sampling"), 
                   span("survey conducted at UW-Madison in Spring 2016."),
               h4("How was the sampling conducted?"),
               div("The primary researcher (\"Survey Originator\") sent a",
               a("Qualtrics", 
                 href="https://www.qualtrics.com/"),
               span("survey to four (later expanded to six) students as starter participants, these initial participants completed the survey and referred up to five additional people, some of whom were selected to also receive the survey. Surveys were queued for one day to allow time for the participants to notify each other of the study. If a participant did not finish the survey within three days, they become ineligible, and a new survey is queued for a previously unselected participant. To learn more about this process you can see the protocol implementation",
               a("here,", href="https://github.com/AlanJSayler/RDS")))
               
    )
    )
  ))
))
