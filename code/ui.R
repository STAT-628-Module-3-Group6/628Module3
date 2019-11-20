library(shiny)
library(ggvis)
# Define UI for application that calculator body fat
ui <- fluidPage(
  titlePanel("Chinese Restaurant Shiny App"),
  sidebarPanel(
    textInput("name", "Restaurant Name:"),
    selectInput("ambience", "Ambience:",
                c(Casual = "casual", Trendy = "trendy", Intimate = "intimate",  "No Idea" = "noidea", 
                  "None of these above" = "none"), selected = "noidea", multiple = T),
    selectInput("mealtype", "Meal Type:",
                c(Breakfast = "breakfast", Brunch = "brunch", Lunch = "lunch", Dinner = "dinner", 
                  Latenight = "latenight", "None of these above" = "none", Idea = "noidea"), selected = "noidea", multiple = T),
    selectInput("noiselevel", "Noise Level:",
                c(Quiet = "quiet", Average = "average", Loud = "loud", "Very Loud" = "noisy", 
                  "No Idea" = "noidea")),
    selectInput("goodforgroups", "Good For Groups:",
                c(True = "true", False = "false", "No Idea" = "noidea")),
    submitButton("Do")
  ),
  
  mainPanel(tabsetPanel(
    tabPanel("Which is your restaurant",
             h4(textOutput("name")),
             uiOutput("ui"),
             submitButton("Confirm")),
    tabPanel("Description Words",
             tableOutput("table")),
    tabPanel("Advice(Category)",
             h4(textOutput("result")),
             h4(textOutput("category")),
             h4(textOutput("result1")),
             h4(textOutput("result2")),
             h4(textOutput("result3")),
             h4(textOutput("result4"))),
    tabPanel("Advice(Reviews)",
             h4(textOutput("review")),
             h4(textOutput("result5")),
             h4(textOutput("result6")),
             h4(textOutput("result7"))),
    tabPanel("Rating Distribution",
             ggvisOutput("plot"),
             h4(textOutput("star"))),
    tabPanel("Contact us",
             h4("Contact us if there is a problem!"),
             h4("Yun Mo: ymo22@wisc.edu"),
             h4("He Wang: hwang789@wisc.edu"),
             h4("Zifeng Wang: zwang2395@wisc.edu"),
             h4("Qintao Ying: qying5@wisc.edu"))
  ))
)
