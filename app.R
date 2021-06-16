library(shiny)


##User Interface
ui <-navbarPage("Black-grass model",
                
                ##Tabbed pages
                ##Introduction
                tabPanel("Introduction",
                         fluidPage(
                             fluidRow(
                                 p("Farmers around the world, face many challenges. We often hear about problems of insect pests or deadly diseases destroying crops worldwide. But in fact, one of the biggest problems faced by farmers is weed control."),
                                 img(src = 'bg.png', align="centre", width="500"),
                                 p("This plant is called black-grass and it is one that is extremely well-known to farmers across north-west Europe. Black-grass is an especially problematic grass weed found in cereal crops, like wheat. To help farmers plan how to manage their fields better we need to be able to predict how many blackgrass plants will grow each year. To do this we can build a", strong("model.")),
                                 p("A model is simply a mathematical description of something that we can observe in the real-world. Here we are going to describe how the number of black-grass plants in year 2 is related to the number of black-grass plants in year 1."),
                                 p("We can build 3 different models, each with a little bit more complexity than the first."),
                                 
                                 #3 columns with a short description of each model
                                 column(3,style = "background-color:#EEEEEE;", h2("Simple Model"), 
                                        p("We can go to lots of fields in 2 consecutive years and count the number of plants. We can then look at these data to see if there is a connection between the number of plants counted in year 1 and the number of plants counted in year 2."),
                                        p("Click on the", strong("Simple Model"), "tab at the top of the page to explore these data and build a simple model. When you have finished come back here by clicking on the", strong("Introduction"), "tab.")),
                                 column(3, offset = 1, style = "background-color:#EEEEEE;", h2("Life Cycle Model"),
                                 p("A simple model works well if we need an answer quickly and we have some data to help us make the prediction. However, sometimes we want to predict things that will happen in the future or in different circumstances. "),
                                 p("If this is the case we must dig deeper into the biology of the plant. If we can understand the processes that lead to an increase in the number of plants from one year to the next we can describe these processes mathematically giving us more control over our model."),
                                 p("Click on the", strong("Life-cycle Model"), "tab at the top of the page to explore this type of model further. When you have finished come back here by clicking on the", strong("Introduction"), "tab.")),
                                 column(3, offset = 1, style = "background-color:#EEEEEE;", h2("Scenario Model"),
                                        p("A scenario model is basically the same as a life-cycle model but rather than use generic data for each step in the life-cycle we can use different numbers according to a particular scenario.For example - what if we want to use our model to predict what will happen somewhere else where the weather is very different to the fields we went to? In this case we may have done some experiments to see what happens to each process within the life-cycle if there is more rain. "),
                                        p("Click on the", strong("Scenario Model"), "tab at the top of the page to explore this type of model further. When you have finished come back here by clicking on the", strong("Introduction"), "tab."))
                             )
                             
                         )),
                
                ##Simple Model
                tabPanel("Simple Model",
                         fluidPage(
                             fluidRow(
                                 h1("Fitting a simple Model"),
                                      p("Here we are going to predict how many black-grass plants will grow in year 2, based on how many grow in year 1. We can fit a linear model to these data. Use the sliders on the right to find the line of best fit.")),
                             #scatterplot and slider inputs
                             column(8, plotOutput(outputId="abscatter")),
                             column(3, offset=1, sliderInput(inputId="mparam",
                                                             label="Choose a value for the 'm' parameter", 
                                                             value=5, min=1, max=10),
                                    sliderInput(inputId="cparam",
                                                label="Choose a value for the 'c' parameter",
                                                value=35, min=10, max=60,step=5))),
                             fluidRow(#predict y2 based on y1
                             h1("Using the model to make predictions"),
                             p("We can use this model to predict how many black-grass plants will grow in year 2 for any given number of blackgrass plants in year 1"),
                             numericInput(inputId="y1plants",label="Number of plants in year 1",value="20"),
                             textOutput("y2plants"),
                             textOutput("simplemodwarning"))),
                
                ##Life-cycle Model
                tabPanel("Life-cycle Model",
                         fluidPage(
                             fluidRow(p("If we want to be able to adapt our predictions to different circumstances we need to have a slightly more detailed model. One common solution is to describe the life-cycle of the plant mathematically." ),
                             img(src = 'lifcyclepic.png', align="centre", width="500"),
                             p("Put simply, plants produce seeds which can then grow into more plants and the process starts again. We can describe each of these stages mathematically and so this model is very similar to the previous simple model, only there are more steps along the way."),
                             p("Let's start with 100 plants and predict how many plants there will be after one year. You need to look at the data for each stage of the life cycle using the tabs below and complete the exercises to describe the process mathematically. The numbers in the panel on the left will update automatically as you do so you can begin to predict how many adult plants will grow."),
                             
                             sidebarLayout(
                                 ## Side panel - The life cycle will go in here and auto update
                                 sidebarPanel( h3("Plants in year 1"),
                                               textOutput("lcstartplants"),
                                               h3("Seeds produced"),
                                               textOutput("lcseedprod"),
                                               h3("Plants in year 2"),
                                               textOutput("lcendplants")),
                                 
                                 
                                 
                                 ##Main panel -  will have tabs for each stage of the life cycle
                                 mainPanel(
                                     tabsetPanel(type="tabs",
                                                 tabPanel("Plants to seed", 
                                                          p("The number of seeds each plant produces depends on how many plants are nearby. If it is very crowded there is not enough space for the plants to grow big and so they produce fewer seeds. We did an experiment where we grew different numbers of plants together in one pot and counted the number of seeds they produced. You can see these data summarised in the graph below. Do you recognise the shape of this curve? There are lots of different equations we can use to fit curves to our data.", strong("Select the best option from the three given."), "We can then use this equation to predict the number of seeds produced for any number of plants."),
                                                          plotOutput(outputId="seedprodscatter"),
                                                          radioButtons("lcseedprodtype", "Choose the equation type that best fits the data",
                                                                       c("Linear" = "lin",
                                                                         "Quadratic" = "quad",
                                                                         "Reciprocal" = "rec"))
                                                          ),
                                                tabPanel("Seed to plants",
                                                         p("Not all seeds will start growing. To find out what percentage of seeds will grow we did an experiment. We counted out 100 seeds and gave them enough water and light to start growing. After 14 days we counted how many shoots we could see. We repeated this 10 times. Here you can see the data we collected, and below it are some summary values. How could you use this data to decide what percentage of seeds typically grow into seedlings?", strong("Calculate the percentage of seeds which grow"), "and put your answer in the box below. If you are stuck try looking at some of the summary statistics by clicking the button."),
                                                         tableOutput('seedgerm'),
                                                         actionButton("showsum", "Show summary statistics"),
                                                         verbatimTextOutput("summaryseedgerm"),
                                                         h2("Decide which value to use in our model"),
                                                         numericInput(inputId="germpercent",label="What percentage of seeds start to grow?",value="50",min = 1, max = 100),
                                                         p("Think about what effect this might have on our life-cycle model and then take a look on the left hand side of the page and see if you were right")
                                               
                             ))))))),
                
                
                
                
                ##Scenario Model
                tabPanel("Scenario Model",
                         fluidPage(
                             fluidRow(p("We can use the life-cycle model we built to predict what will happen to the number of black-grass plants for many years into the future. This allows us to investigate how we might be able to manage this weed without having to do lots and lots of experiments year after year."),
                                      p("Here is how our life cycle model looks when we loop through it multiple times - in other words we put the number of plants we get out at the end back into the beginning."),
                                      plotOutput(outputId="loopmod2"),
                                      p("We can also explore other scenarios. Select the scenario you would like to explore from the options below. "),
                                      radioButtons("scenariochoice", "Which scenario would you like to explore?",
                                                   c("Try to guess what might happen to the model before you look at the output."="none",
                                                     "You apply a herbicide which kills 70% of plants before they produce seed each year."="herb",
                                                     "Wet weather in the Autumn allows 10% more seeds to germinate."="wet"))
                             
                                      
                                      )))
)

##Behind the scenes
server <- function(input,output){
    
    ##Calculations for simple model
    #Generate the data
    adata <- rnorm(50,mean=25,sd=5)
    bdata <- adata*7+22+(rnorm(50, mean=0, sd=10))
    #create the scatter plot
    output$abscatter <- renderPlot(#code to get saved to the hist output
        {   #the {} creates a code block - can be multiple lines
            plot(adata,bdata,xlim=c(0,40),ylim=c(0,275),xlab="Number of plants in year 1",ylab="Number of plants in year 2")
            abline(a=input$cparam,b=input$mparam)
            text(x=35,y=25,"y=mx+c", col="blue")
        })
    output$y2plants <- renderText({ 
        y2plants <- input$mparam*input$y1plants+input$cparam
        paste("The predicted number of plants in year 2 is", y2plants, "when there are", input$y1plants, "plants in year 1")
        })
    output$simplemodwarning <- renderText({
        if(input$y1plants<min(adata)){
            paste("Warning: Be cautious of extrapolating this model outside of the range of the original data.")
        }
        else if(input$y1plants>max(adata)){
            paste("Warning: Be cautious of extrapolating this model outside of the range of the original data.")
        }
        else{
          paste("")  
        }
        })
    
    
    ##lifecycle model
    #plants to seed
    plantsdata <- rnorm(100,mean=200,sd=100)
    seeddata <- (((8*plantsdata)/(1+(0.006*plantsdata)))+(rnorm(100, mean=0, sd=50)))*7
    #create the scatter plot
    
    linfit <- lm(seeddata ~ plantsdata)
    quadfit <- lm(seeddata ~ plantsdata+ I(plantsdata^2))
    output$seedprodscatter <- renderPlot(#code to get saved to the hist output
        {   #the {} creates a code block - can be multiple lines
            plot(plantsdata,seeddata,xlim=c(0,450),ylim=c(0,10000),xlab="Number of plants in year 1",ylab="Number of seeds produced")
            if (input$lcseedprodtype == "lin"){
                abline(linfit)
            }
            else if (input$lcseedprodtype == "quad"){
                lines(sort(plantsdata), fitted(quadfit)[order(plantsdata)], type='l') 
            }
            else if (input$lcseedprodtype == "rec"){
                curve((((8*x)/(1+(0.006*x)))*7), add = TRUE)
            }
                   
        })
    
    ##Germination
    growingseeds <- as.data.frame(100*0.3+rnorm(10,mean=0,sd=2))
    names(growingseeds) <- c("Number of seeds that grow")
    growingseeds <- round(growingseeds)
    output$seedgerm <- renderTable(growingseeds)
    observeEvent(input$showsum, {
        output$summaryseedgerm <- renderPrint({summary(growingseeds)})
    })
    
    
    
    ##Life-cycle model
    mystartplants <- 100
    output$lcstartplants <- renderText(paste(mystartplants))
    
    #seedprod is a function of startplants
    myseedprod <- reactive({
        if (input$lcseedprodtype == "lin"){
            predict(linfit, data.frame(plantsdata = c(mystartplants)))
        }
        else if (input$lcseedprodtype == "quad"){
            predict(quadfit, data.frame(plantsdata = c(mystartplants)))
        }
        else if (input$lcseedprodtype == "rec"){
            ((8*mystartplants)/(1+(0.006*mystartplants)))*7
        }
    })
    
    output$lcseedprod <- renderText({myseedprod()})
    
    myendplants <- reactive({myseedprod()*input$germpercent/100}) #endplants is a function of seed prod
    output$lcendplants <- renderText({myendplants()})
    
    #Loop model,
    year <- seq(1,10)
    plants <- year
    plants[1] <- mystartplants
    hplants <- year
    hplants[1] <- mystartplants
    wplants <- year
    wplants[1] <- mystartplants
    for (i in 1 : 9){
        seed <- ((8*plants[i])/(1+(0.006*plants[i])))*7
        hseed<- (((8*plants[i])/(1+(0.006*plants[i])))*7)*0.3
        
        plants[i+1] <- seed*30/100
        hplants[i+1] <- hseed*30/100
        wplants[i+1] <- seed*33/100
        }
    

    output$loopmod <- renderPlot(#code to get saved to the hist output
        {   #the {} creates a code block - can be multiple lines
            plot(year,plants,xlab="Year",ylab="Number of plants", type="b", ylim=c(0,3000))
        })
    
    
    output$loopmod2 <- renderPlot(#code to get saved to the hist output
        {   #the {} creates a code block - can be multiple lines
         
             if (input$scenariochoice == "none"){
                plot(year,plants,xlab="Year",ylab="Number of plants", type="b", ylim=c(0,3000))
            }
            if (input$scenariochoice == "herb"){
                plot(year,plants,xlab="Year",ylab="Number of plants", type="b", ylim=c(0,3000))
                lines(year,hplants,xlab="Year",ylab="Number of plants", col="red", type="b")
                text(x=8,y=1000,"Herbicide", col="red")
            }
            else if (input$scenariochoice == "wet"){
                plot(year,plants,xlab="Year",ylab="Number of plants", type="b", ylim=c(0,3000))
                lines(year,wplants,xlab="Year",ylab="Number of plants", col="blue", type="b")
                text(x=2,y=2500,"Wet", col="blue")
            }
            
        })
}

shinyApp(server=server,ui=ui)