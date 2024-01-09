#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# change initial text
# background color instead of letter color

library(shiny)
# library(shinyWidgets)


source("wordle_functions.R")

# fluidPage(
#     verticalLayout(
#         titlePanel("Vertical layout example"),
#         plotOutput("plot1"),
#         wellPanel(
#             sliderInput("n", "Number of points", 10, 200,
#                         value = 50, step = 10)
#         )
#     )
# )

# Define UI for application that draws a histogram
ui <- fluidPage(verticalLayout(
    titlePanel("the wordlenatoR"),
    htmlOutput("Intro"),
    br(),
    p("Enter your guesses and feedback received from the wordle app here."),
    textInput("guess1", "Guess 1:", "xxxxx"),
    "Feedback for 1st guess:",
    fluidRow(
        splitLayout(
            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
            cellWidths = c("0%","18%", "18%", "18%", "18%", "18%"),
            selectInput("f11", "1st letter:",
                        choices = c("grey", "yellow", "green")),
            selectInput("f12", "2nd letter:",
                        choices = c("grey", "yellow", "green")),
            selectInput("f13", "3rd letter:",
                        choices = c("grey", "yellow", "green")),
            selectInput("f14", "4th letter:",
                        choices = c("grey", "yellow", "green")),
            selectInput("f15", "5th letter:",
                        choices = c("grey", "yellow", "green"))
        )
    ),
    actionButton("goButton", "Enter!"),
    p("Click the button to compute eliminate words from the target word 
          dictionary and get suggestions for next guesses."),
    htmlOutput("guess1"),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div("Loading ... this may take up to 5 minutes if you are unlucky...",
                              htmlOutput("wait"),
                              id="loadmessage")),
    br(),
    # ###################################################################### 
    # # 2nd guess
    # ######################################################################
    textInput("guess2", "Guess 2:", "xxxxx"),
    "Feedback for 2nd guess:",
    fluidRow(
        splitLayout(
            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
            cellWidths = c("0%","18%", "18%", "18%", "18%", "18%"),
            selectInput("f21", "1st letter:",
                        choices = c("grey", "yellow", "green")),
            selectInput("f22", "2nd letter:",
                        choices = c("grey", "yellow", "green")),
            selectInput("f23", "3rd letter:",
                        choices = c("grey", "yellow", "green")),
            selectInput("f24", "4th letter:",
                        choices = c("grey", "yellow", "green")),
            selectInput("f25", "5th letter:",
                        choices = c("grey", "yellow", "green"))
        )
    ),
    actionButton("goButton2", "Enter!"),
    p("Click the button to compute eliminate words from the target word 
          dictionary and get suggestions for next guesses."),
    htmlOutput("guess2"),
    br(),
    # ###################################################################### 
    # # 3rd guess
    # ######################################################################
    textInput("guess3", "Guess 3:", "xxxxx"),
    "Feedback for 3rd guess:",
    fluidRow(
        splitLayout(
            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
            cellWidths = c("0%","18%", "18%", "18%", "18%", "18%"),
            selectInput("f31", "1st letter:",
                        choices = c("grey", "yellow", "green")),
            selectInput("f32", "2nd letter:",
                        choices = c("grey", "yellow", "green")),
            selectInput("f33", "3rd letter:",
                        choices = c("grey", "yellow", "green")),
            selectInput("f34", "4th letter:",
                        choices = c("grey", "yellow", "green")),
            selectInput("f35", "5th letter:",
                        choices = c("grey", "yellow", "green"))
        )
    ),
    actionButton("goButton3", "Enter!"),
    p("Click the button to compute eliminate words from the target word 
          dictionary and get suggestions for next guesses."),
    htmlOutput("guess3"),
    br(),
    # ###################################################################### 
    # # 4th guess
    # ######################################################################
    textInput("guess4", "Guess 4:", "xxxxx"),
    "Feedback for 4th guess:",
    fluidRow(
        splitLayout(
            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
            cellWidths = c("0%","18%", "18%", "18%", "18%", "18%"),
            selectInput("f41", "1st letter:",
                        choices = c("grey", "yellow", "green")),
            selectInput("f42", "2nd letter:",
                        choices = c("grey", "yellow", "green")),
            selectInput("f43", "3rd letter:",
                        choices = c("grey", "yellow", "green")),
            selectInput("f44", "4th letter:",
                        choices = c("grey", "yellow", "green")),
            selectInput("f45", "5th letter:",
                        choices = c("grey", "yellow", "green"))
        )
    ),
    actionButton("goButton4", "Enter!"),
    p("Click the button to compute eliminate words from the target word 
          dictionary and get suggestions for next guesses."),
    htmlOutput("guess4"),
    br(),
    # ###################################################################### 
    # # 5th guess
    # ######################################################################
    textInput("guess5", "Guess 5:", "xxxxx"),
    "Feedback for 5th guess:",
    fluidRow(
        splitLayout(
            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
            cellWidths = c("0%","18%", "18%", "18%", "18%", "18%"),
            selectInput("f51", "1st letter:",
                        choices = c("grey", "yellow", "green")),
            selectInput("f52", "2nd letter:",
                        choices = c("grey", "yellow", "green")),
            selectInput("f53", "3rd letter:",
                        choices = c("grey", "yellow", "green")),
            selectInput("f54", "4th letter:",
                        choices = c("grey", "yellow", "green")),
            selectInput("f55", "5th letter:",
                        choices = c("grey", "yellow", "green"))
        )
    ),
    actionButton("goButton5", "Enter!"),
    p("Click the button to compute eliminate words from the target word 
          dictionary and get suggestions for next guesses."),
    htmlOutput("guess5"),
    br()
)# ,
# mainPanel(
#     htmlOutput("Intro"),
#     htmlOutput("guess1"),
#     htmlOutput("guess2"),
#     htmlOutput("guess3"),
#     htmlOutput("guess4"),
#     htmlOutput("guess5"))
)

# Define server logic required 
server <- function(input, output) {
    
    # # builds a reactive expression that only invalidates 
    # # when the value of input$goButton becomes out of date 
    # # (i.e., when the button is pressed)
    # ntext <- eventReactive(input$goButton, {
    #     input$guess1
    # })
    # output$nText <- renderText({
    #     ntext()
    # })
    initial_guess = readRDS(file = "second_guess.RDS")
    initial_text = paste0(initial_guess[c(1, 3, 4, 7, 8),1], "(", 
                          round(initial_guess[c(1, 3, 4, 7, 8),2] * 100), "%)", 
                          collapse = ", ")
    output$Intro = renderText({
        paste0("Welcome to the wordlenatoR, the best wordle solver in existence.",
               " To see how the wordlenatoR works check out the theory behind it ",
               "<a href='https://github.com/maierhofert/wordlenatoR/blob/main/wordlenatoR.pdf'>here</a>",
               ". There are 2315 words in the wordle target word dictionary. 
        Here is a list of the best first guesses, with the average percentage of words they will
        eliminate from the target word dictionary: <br>",
               initial_text)
    })
    output$wait = renderText("<img src='mr-bean-waiting.gif' width='300px' height='auto'/>")
    
    guess1 <- eventReactive(input$goButton, {
        this.guess = unlist(strsplit(tolower(input$guess1), split = ""))
        est_red = est_reduction(this.guess, D_[sample(1:nrow(D_), 200),, drop = FALSE]) / 2
        Dprime_ = reduce_dictionary(guess_ = this.guess, 
                                    this.feedback = c(input$f11, input$f12, 
                                                      input$f13, input$f14, 
                                                      input$f15),
                                    D_)
        saveRDS(Dprime_, "Dprime_.RDS")
        
        paste0("<b>",
               "<span style=\"background-color:", input$f11, "; color:lightgrey\">", toupper(this.guess[1]), "</span>",
               "<span style=\"background-color:", input$f12, "; color:lightgrey\">", toupper(this.guess[2]), "</span>",
               "<span style=\"background-color:", input$f13, "; color:lightgrey\">", toupper(this.guess[3]), "</span>",
               "<span style=\"background-color:", input$f14, "; color:lightgrey\">", toupper(this.guess[4]), "</span>",
               "<span style=\"background-color:", input$f15, "; color:lightgrey\">", toupper(this.guess[5]), "</span>",
               "</b>", 
               "<br/>",
               "On average, this guess eliminates ", floor(est_red), 
               "% of words from the target word dictionary. In this case, it eliminated ",
               floor((1 - nrow(Dprime_) / nrow(D_)) * 100), "%.")
    })
    dict1 <- eventReactive(input$goButton, {
        this.guess = unlist(strsplit(tolower(input$guess1), split = ""))
        Dprime_ = readRDS("Dprime_.RDS")
        # get estimated reduction for all permissible words
        # note that this is not optimal as some words that are not possible target words
        # could lead to a greater expected reduction in possible target words
        res_data = data.frame(guess = apply(Dprime_, 1, paste0, collapse = ""),
                              est_reduction = NA)
        samp = sample(1:nrow(Dprime_), min(50, nrow(Dprime_)))
        samp = sort(samp)
        for (i in samp) {
            res_data$est_reduction[i] = est_reduction(guess_ = Dprime_[i,], Dprime_ = Dprime_) / nrow(Dprime_) * 100
        }
        # there are many good options
        next_guess = res_data[order(res_data$est_reduction, decreasing = TRUE),]
        this_text = paste0(next_guess[1:3,1], "(", round(next_guess[1:3,2]), "%)", 
                           collapse = ", ")
        win = nrow(res_data) == 1
        if (win) {
            paste0("The last word in the target dictionary is ",
                   next_guess[1,1],
                   ". Congratulations, you won! <br>",
                   "<img src='leonardo-dicaprio-cheers.gif' />"
            )
        } else {
            paste0("There are ", nrow(Dprime_), " words in the target word dictionary.
               Good next guesses are (with expected % elimination): ",
                   this_text)
        }
        
    })
    output$guess1 <- renderText({
        paste0(guess1(), "<br/>", dict1())
    })
    # ###################################################################### 
    # # 2nd guess
    # ######################################################################
    guess2 <- eventReactive(input$goButton2, {
        this.guess = unlist(strsplit(tolower(input$guess2), split = ""))
        Dprime_ = readRDS("Dprime_.RDS")
        est_red = est_reduction(this.guess, 
                                Dprime_[sample(1:nrow(Dprime_), 
                                               min(200, nrow(Dprime_))),, drop = FALSE]) / 
            min(2, nrow(Dprime_) / 100)
        D2prime_ = reduce_dictionary(guess_ = this.guess, 
                                    this.feedback = c(input$f21, input$f22, 
                                                      input$f23, input$f24, 
                                                      input$f25),
                                    Dprime_)
        saveRDS(D2prime_, "D2prime_.RDS")
        
        paste0("<b>",
               "<span style=\"background-color:", input$f21, "; color:lightgrey\">", toupper(this.guess[1]), "</span>",
               "<span style=\"background-color:", input$f22, "; color:lightgrey\">", toupper(this.guess[2]), "</span>",
               "<span style=\"background-color:", input$f23, "; color:lightgrey\">", toupper(this.guess[3]), "</span>",
               "<span style=\"background-color:", input$f24, "; color:lightgrey\">", toupper(this.guess[4]), "</span>",
               "<span style=\"background-color:", input$f25, "; color:lightgrey\">", toupper(this.guess[5]), "</span>",
               "</b>", 
               "<br/>",
               "On average, this guess eliminates ", round(est_red), 
               "% of words from the target word dictionary. In this case, it eliminated ",
               floor((1 - nrow(D2prime_) / nrow(Dprime_)) * 100), "%.")
    })
    dict2 <- eventReactive(input$goButton2, {
        this.guess = unlist(strsplit(tolower(input$guess2), split = ""))
        Dprime_ = readRDS("Dprime_.RDS")
        D2prime_ = readRDS("D2prime_.RDS")
        # get estimated reduction for all permissible words
        # note that this is not optimal as some words that are not possible target words
        # could lead to a greater expected reduction in possible target words
        res_data = data.frame(guess = apply(D2prime_, 1, paste0, collapse = ""),
                              est_reduction = NA)
        samp = sample(1:nrow(D2prime_), min(50, nrow(D2prime_)))
        samp = sort(samp)
        for (i in samp) {
            res_data$est_reduction[i] = est_reduction(guess_ = D2prime_[i,], 
                                                      Dprime_ = D2prime_) / nrow(D2prime_) * 100
        }
        # there are many good options
        next_guess = res_data[order(res_data$est_reduction, decreasing = TRUE),]
        this_text = paste0(next_guess[1:3,1], "(", round(next_guess[1:3,2]), "%)", 
                           collapse = ", ")
        
        win = nrow(res_data) == 1
        if (win) {
            paste0("The last word in the target dictionary is ",
                   next_guess[1,1],
                   ". Congratulations, you won! <br>",
                   "<img src='leonardo-dicaprio-cheers.gif' />"
            )
        } else {
            paste0("There are ", nrow(D2prime_), " words in the target word dictionary.
               Good next guesses are (with expected % elimination): ",
                   this_text)
        }
    })
    output$guess2 <- renderText({
        paste0("<br/>", guess2(), "<br/>", dict2())
    })
    # ###################################################################### 
    # # 3rd guess
    # ######################################################################
    guess3 <- eventReactive(input$goButton3, {
        this.guess = unlist(strsplit(tolower(input$guess3), split = ""))
        D2prime_ = readRDS("D2prime_.RDS")
        est_red = est_reduction(this.guess, 
                                D2prime_[sample(1:nrow(D2prime_), 
                                               min(200, nrow(D2prime_))),, drop = FALSE]) / 
            min(2, nrow(D2prime_) / 100)
        D3prime_ = reduce_dictionary(guess_ = this.guess, 
                                    this.feedback = c(input$f31, input$f32, 
                                                      input$f33, input$f34, 
                                                      input$f35),
                                    D2prime_)
        saveRDS(D3prime_, "D3prime_.RDS")
        
        paste0("<b>",
               "<span style=\"background-color:", input$f31, "; color:lightgrey\">", toupper(this.guess[1]), "</span>",
               "<span style=\"background-color:", input$f32, "; color:lightgrey\">", toupper(this.guess[2]), "</span>",
               "<span style=\"background-color:", input$f33, "; color:lightgrey\">", toupper(this.guess[3]), "</span>",
               "<span style=\"background-color:", input$f34, "; color:lightgrey\">", toupper(this.guess[4]), "</span>",
               "<span style=\"background-color:", input$f35, "; color:lightgrey\">", toupper(this.guess[5]), "</span>",
               "</b>", 
               "<br/>",
               "On average, this guess eliminates ", round(est_red), 
               "% of words from the target word dictionary. In this case, it eliminated ",
               floor((1 - nrow(D3prime_) / nrow(D2prime_)) * 100), "%.")
    })
    dict3 <- eventReactive(input$goButton3, {
        this.guess = unlist(strsplit(tolower(input$guess3), split = ""))
        D2prime_ = readRDS("D2prime_.RDS")
        D3prime_ = readRDS("D3prime_.RDS")
        # get estimated reduction for all permissible words
        # note that this is not optimal as some words that are not possible target words
        # could lead to a greater expected reduction in possible target words
        res_data = data.frame(guess = apply(D3prime_, 1, paste0, collapse = ""),
                              est_reduction = NA)
        samp = sample(1:nrow(D3prime_), min(50, nrow(D3prime_)))
        samp = sort(samp)
        for (i in samp) {
            res_data$est_reduction[i] = est_reduction(guess_ = D3prime_[i,], 
                                                      Dprime_ = D3prime_) / nrow(D3prime_) * 100
        }
        # there are many good options
        next_guess = res_data[order(res_data$est_reduction, decreasing = TRUE),]
        this_text = paste0(next_guess[1:3,1], "(", round(next_guess[1:3,2]), "%)", 
                           collapse = ", ")
        
        win = nrow(res_data) == 1
        if (win) {
            paste0("The last word in the target dictionary is ",
                   next_guess[1,1],
                   ". Congratulations, you won! <br>",
                   "<img src='leonardo-dicaprio-cheers.gif' />"
            )
        } else {
            paste0("There are ", nrow(D3prime_), " words in the target word dictionary.
               Good next guesses are (with expected % elimination): ",
                   this_text)
        }
    })
    output$guess3 <- renderText({
        paste0("<br/>", guess3(), "<br/>", dict3())
    })
    # ###################################################################### 
    # # 4th guess
    # ######################################################################
    guess4 <- eventReactive(input$goButton4, {
        this.guess = unlist(strsplit(tolower(input$guess4), split = ""))
        D3prime_ = readRDS("D3prime_.RDS")
        est_red = est_reduction(this.guess, 
                                D3prime_[sample(1:nrow(D3prime_), 
                                                min(200, nrow(D3prime_))),, drop = FALSE]) / 
            min(2, nrow(D3prime_) / 100)
        D4prime_ = reduce_dictionary(guess_ = this.guess, 
                                     this.feedback = c(input$f41, input$f42, 
                                                       input$f43, input$f44, 
                                                       input$f45),
                                     D3prime_)
        saveRDS(D4prime_, "D4prime_.RDS")
        
        paste0("<b>",
               "<span style=\"background-color:", input$f41, "; color:lightgrey\">", toupper(this.guess[1]), "</span>",
               "<span style=\"background-color:", input$f42, "; color:lightgrey\">", toupper(this.guess[2]), "</span>",
               "<span style=\"background-color:", input$f43, "; color:lightgrey\">", toupper(this.guess[3]), "</span>",
               "<span style=\"background-color:", input$f44, "; color:lightgrey\">", toupper(this.guess[4]), "</span>",
               "<span style=\"background-color:", input$f45, "; color:lightgrey\">", toupper(this.guess[5]), "</span>",
               "</b>", 
               "<br/>",
               "On average, this guess eliminates ", round(est_red), 
               "% of words from the target word dictionary. In this case, it eliminated ",
               floor((1 - nrow(D4prime_) / nrow(D3prime_)) * 100), "%.")
    })
    dict4 <- eventReactive(input$goButton4, {
        this.guess = unlist(strsplit(tolower(input$guess4), split = ""))
        D3prime_ = readRDS("D3prime_.RDS")
        D4prime_ = readRDS("D4prime_.RDS")
        # get estimated reduction for all permissible words
        # note that this is not optimal as some words that are not possible target words
        # could lead to a greater expected reduction in possible target words
        res_data = data.frame(guess = apply(D4prime_, 1, paste0, collapse = ""),
                              est_reduction = NA)
        samp = sample(1:nrow(D4prime_), min(50, nrow(D4prime_)))
        samp = sort(samp)
        for (i in samp) {
            res_data$est_reduction[i] = est_reduction(guess_ = D4prime_[i,], 
                                                      Dprime_ = D4prime_) / nrow(D4prime_) * 100
        }
        # there are many good options
        next_guess = res_data[order(res_data$est_reduction, decreasing = TRUE),]
        this_text = paste0(next_guess[1:3,1], "(", round(next_guess[1:3,2]), "%)", 
                           collapse = ", ")
        
        win = nrow(res_data) == 1
        if (win) {
            paste0("The last word in the target dictionary is ",
                   next_guess[1,1],
                   ". Congratulations, you won! <br>",
                   "<img src='leonardo-dicaprio-cheers.gif' />"
            )
        } else {
            paste0("There are ", nrow(D4prime_), " words in the target word dictionary.
               Good next guesses are (with expected % elimination): ",
                   this_text)
        }
    })
    output$guess4 <- renderText({
        paste0("<br/>", guess4(), "<br/>", dict4())
    })
    # ###################################################################### 
    # # 5th guess
    # ######################################################################
    guess5 <- eventReactive(input$goButton5, {
        this.guess = unlist(strsplit(tolower(input$guess5), split = ""))
        D4prime_ = readRDS("D4prime_.RDS")
        est_red = est_reduction(this.guess, 
                                D4prime_[sample(1:nrow(D4prime_), 
                                                min(200, nrow(D4prime_))),, drop = FALSE]) / 
            min(2, nrow(D4prime_) / 100)
        D5prime_ = reduce_dictionary(guess_ = this.guess, 
                                     this.feedback = c(input$f51, input$f52, 
                                                       input$f53, input$f54, 
                                                       input$f55),
                                     D4prime_)
        saveRDS(D5prime_, "D5prime_.RDS")
        
        paste0("<b>",
               "<span style=\"background-color:", input$f51, "; color:lightgrey\">", toupper(this.guess[1]), "</span>",
               "<span style=\"background-color:", input$f52, "; color:lightgrey\">", toupper(this.guess[2]), "</span>",
               "<span style=\"background-color:", input$f53, "; color:lightgrey\">", toupper(this.guess[3]), "</span>",
               "<span style=\"background-color:", input$f54, "; color:lightgrey\">", toupper(this.guess[4]), "</span>",
               "<span style=\"background-color:", input$f55, "; color:lightgrey\">", toupper(this.guess[5]), "</span>",
               "</b>", 
               "<br/>",
               "On average, this guess eliminates ", round(est_red), 
               "% of words from the target word dictionary. In this case, it eliminated ",
               floor((1 - nrow(D5prime_) / nrow(D4prime_)) * 100), "%.")
    })
    dict5 <- eventReactive(input$goButton5, {
        this.guess = unlist(strsplit(tolower(input$guess5), split = ""))
        D4prime_ = readRDS("D4prime_.RDS")
        D5prime_ = readRDS("D5prime_.RDS")
        # get estimated reduction for all permissible words
        # note that this is not optimal as some words that are not possible target words
        # could lead to a greater expected reduction in possible target words
        res_data = data.frame(guess = apply(D5prime_, 1, paste0, collapse = ""),
                              est_reduction = NA)
        samp = sample(1:nrow(D5prime_), min(50, nrow(D5prime_)))
        samp = sort(samp)
        for (i in samp) {
            res_data$est_reduction[i] = est_reduction(guess_ = D5prime_[i,], 
                                                      Dprime_ = D5prime_) / nrow(D5prime_) * 100
        }
        # there are many good options
        next_guess = res_data[order(res_data$est_reduction, decreasing = TRUE),]
        this_text = paste0(next_guess[,1])
        
        win = nrow(res_data) == 1
        if (win) {
            paste0("The last word in the target dictionary is ",
                   next_guess[1,1],
                   ". Congratulations, you won! <br>",
                   "<img src='leonardo-dicaprio-cheers.gif' />"
            )
        } else {
            paste0("There are ", nrow(D5prime_), " words in the target word dictionary.
               Good next guesses are (with expected % elimination): ",
                   this_text)
        }
    })
    output$guess5 <- renderText({
        paste0("<br/>", guess5(), "<br/>", dict5())
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
