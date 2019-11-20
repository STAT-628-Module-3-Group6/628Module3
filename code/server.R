library(shiny)
library(ggvis)
library(dplyr)

server <- function(input, output){
  
  data <- read.csv("/Users/qintaoying/Desktop/STAT 628/module_3/Rshiny/attributes.csv")

  findName <- function(x){
    isTrue = FALSE
    for (i in data$name) {
      if(tolower(input$name) == tolower(i)){
        isTrue = TRUE
        break
      }
    }
    return(isTrue)
  }
  
  output$name <- renderText({
    if(findName() == FALSE){
      paste("Sorry, we can't find your restaurant in our database.")
    }
    else{
      paste()
    }
  })
  
  button <- reactive({
    if(findName() == F) {
      return()
    }
    rest_name <- data[tolower(data$name) == tolower(input$name),]
    
    all <- c()
    for(i in 1:nrow(rest_name)){
      all <- c(all,paste(rest_name[i,]$name, ", ",rest_name[i,]$state, ", ", rest_name[i,]$business_id))
    }
    radioButtons("dynamic", "Which below is your restaurant:", choiceNames = all, choiceValues = all, selected = all[1])
  })
  
  output$ui <- renderUI({
    button()
  })
  
  description <- read.csv("/Users/qintaoying/Desktop/STAT 628/module_3/Rshiny/DescriptWords.csv")
  
  repf <- function(x){
    x <- gsub("[","",x,fixed = T)
    x <- gsub("]","",x,fixed = T)
    x <- gsub("(","",x,fixed = T)
    x <- gsub(")","",x,fixed = T)
    x <- gsub("',",":",x,fixed = T)
    x <- gsub("'","",x,fixed = T)
    x
  }
  
  words <- reactive({
    id = strsplit(input$dynamic[1], " , ")[[1]][3]
    id = gsub(" ","",id)
    this = description[description$business_id == id, ]
    service = as.character(this$service)
    service = repf(service)
    wait = as.character(this$wait)
    wait = repf(wait)
    price = as.character(this$wait)
    price = repf(price)
    data.frame(
      Aspect = c("Service", "Waiting Time","Price"),
      words = c(service, wait, price)
    )
  })
  
  output$table <- renderTable({
    words()
  })
  
  output$result <- renderText({
    if(findName() == FALSE){
      paste("Sorry, we can't find your restaurant in our database.")
    }
    else{
      paste("Here are some advice for your restaurant:")
    }
  })
  
  output$category <- renderText({
    if(findName() == FALSE){
      paste()
    }
    else{
      paste("Advice based on your restaurant category:")
    }
  })
  # Advice 1
  advice1 <- reactive({
    if(findName() == F){
      paste()
    }
    else if(is.null(input$ambience)|is.na(input$ambience)|input$ambience=="noidea"){
      paste("1.Please declare specifically what type of your restaurant ambience is and try to make it casual.")
    }
    else if(input$ambience=="none"){
      paste("1.It would be better if your restaurant is full of laughters, it helps creating an intimate automosphere.")
    }
    else{
      paste("1.For the ambience, You are doing pretty well! Maintain it!")
    }
  })

  output$result1 <- renderText({
    advice1()
  })
  # Advice 2
  advice2 <- reactive({
    if(findName() == F){
      paste()
    }
    else if(is.null(input$mealtype)|is.na(input$mealtype)|input$mealtype=="noidea"){
      paste("2.How could you have no idea about what type of meal you are serving? Be specific or it makes you very unprofessional.")
    }
    else if(!("dinner" %in% input$mealtype)){
      paste("2.People prefer dinner than the others. Maybe it could be better for you to provide dinner.")
    }
    else if("breakfast" %in% input$mealtype |"brunch" %in% input$mealtype|"latenight" %in% input$mealtype){
      paste("2.Sometimes doing more has nothing to do with doing well. You can try to focus mostly on lunch and dinner.")
    }
    else{
      paste("2.You do have a very popular meal type. Good luck!")
    }
  })

  output$result2 <- renderText({
    advice2()
  })
  #Advice 3
  advice3 <- reactive({
    if(findName() == F){
      paste()
    }
    else if(input$noiselevel=="noidea"){
      paste("3.Please be specific about your restaurant noise level. It's important to Yelpers at least.")
    }
    else if(input$noiselevel=="loud"){
      paste("3.Please make it a little bit quiet!")
    }
    else if(input$noiselevel=="noisy"){
      paste("3.Oh, God! Don't be too loud! People sitting next each other almost can't hear each other!")
    }
    else{
      paste("3.You do have a quiet decent environment! That's good!")
    }
  })

  output$result3 <- renderText({
    advice3()
  })
  #Advice 4
  advice4 <- reactive({
    if(findName() == F){
      paste()
    }
    else if(input$goodforgroups=="noidea"){
      paste("4.Please tell yelpers that you have big tables for people in group.")
    }
    else if(input$goodforgroups=="false"){
      paste("4.It's a shame that a chinese restaurant is not good for people in group! Add a few really big tables if you can.")
    }
    else{
      paste("4.Your restaurant is good for people in groups. Good job!")
    }
  })

  output$result4 <- renderText({
    advice4()
  })
  
  output$review <- renderText({
    if(findName() == FALSE){
      paste()
    }
    else{
      paste("Advice based on your restaurant reviews:")
    }
  })
  
  review <- read.csv("/Users/qintaoying/Desktop/STAT 628/module_3/Rshiny/Output.csv")
  # Advice 5
  advice5 <- reactive({
    if(findName() == F){
      paste()
    }
    else{
      id = strsplit(input$dynamic[1], " , ")[[1]][3]
      id = gsub(" ","",id)
      this = review[review$business_id == id, ]
      food = this$high_food
      if(is.null(food) | is.na(food)){
        paste("1.It seems that your food is not very extraordinary.")
      }
      else{
        food = as.character(food)
        food = gsub("'", "", food)
        food = gsub(" ", "", food)
        food = gsub("(", "", food, fixed = T)
        food = gsub(")", "", food, fixed = T)
        food = gsub(",", " ", food)
        paste("1.Your", food, "is fond of customers, so you should keep the flavor.", sep = " ")
      }
    }
  })
  
  output$result5 <- renderText({
    advice5()
  })
  
  # Advice 6
  advice6 <- reactive({
    if(findName() == F){
      paste()
    }
    else{
      id = strsplit(input$dynamic[1], " , ")[[1]][3]
      id = gsub(" ","",id)
      this = review[review$business_id == id, ]
      food = this$low_food
      if(is.null(food) | is.na(food)){
        paste("2.No unwelcomed food, most of them can be accepted.")
      }
      else{
        food = as.character(food)
        food = gsub("'", "", food)
        food = gsub(" ", "", food)
        food = gsub("(", "", food, fixed = T)
        food = gsub(")", "", food, fixed = T)
        food = gsub(",", " ", food)
        paste("2.Your", food, "more or less has a bad effect on your Yelp rating. Maybe you can improve its recipe or discard it.", sep = " ")
      }
    }
  })
  
  output$result6 <- renderText({
    advice6()
  })
  
  # Advice 7
  advice7 <- reactive({
    if(findName() == F){
      paste()
    }
    else{
      id = strsplit(input$dynamic[1], " , ")[[1]][3]
      id = gsub(" ","",id)
      this = review[review$business_id == id, ]
      a <- paste("")
      if(this$wait == 1){
        a <- paste(a, "3.You should speed up, don't keep customer waiting too long. ")
      }
      else{
        a <- paste(a, "3.You have a short waiting time which is good. ")
      }
      if(this$service == 1){
        a = paste(a, "And You need to improve your service! Customer is God! ")
      }
      else{
        a = paste(a, "Good Service. ")
      }
      if(this$price == 1){
        a = paste(a, "Maybe it's time to compare your price with your opponents.")
      }
      else{
        a = paste(a, "Good price!")
      }
      paste(a)
    }
  })
  
  output$result7 <- renderText({
    advice7()
  })
  # Create an interactive plot

  restaurant <- reactive({
    data
  })
  
  # Function for generating tooltip text
  rest_tooltip <- function(x){
    if (is.null(x)) return(NULL)
    if (is.null(x$business_id)) return(NULL)
    
    all_rest <- isolate(restaurant())
    rest <- all_rest[all_rest$business_id == x$business_id, ]
    
    paste0("<b>", rest$name, "</b><br>",
           "Noise Level: ", rest$NoiseLevel,"<br>",
           "Ambience: ", rest$RestaurantsGoodForGroups,"<br>",
           "Price Range: ", rest$RestaurantsPriceRange2)
  }
  
  
  
  vis <- reactive({
    
    xvar <- prop("x", as.symbol("stars"))
    yvar <- prop("y", as.symbol("ReviewRating"))
    
    restaurant %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_points(size := 50, size.hover := 200,
                   fillOpacity := 0.2, fillOpacity.hover :=  0.5, key := ~business_id) %>%
      add_tooltip(rest_tooltip, "hover") %>%
      add_axis("x", title = "Stars") %>%
      add_axis("y", title = "Average Review Rating") %>%
      set_options(width = 500, height = 500)
  })
  
  vis %>% bind_shiny("plot")
  
  final <- reactive({
    id = strsplit(input$dynamic[1], " , ")[[1]][3]
    id = gsub(" ","",id)
    this = data[data$business_id == id, ]
    paste("Your restaurant star rating is ", this$stars, ". And your average review rating is ", this$ReviewRating)
  })
  
  output$star <- renderText({
    final()
  })
}