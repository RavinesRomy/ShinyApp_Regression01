library(shiny)
library(ggplot2)
library(plotly)
library(rmarkdown)
library(knitr)
library(pander)
library(data.table)
library(sjPlot)
library(htmlTable)
library(DT)

options(shiny.maxRequestSize = 100*1024^2)

ui <- fluidPage(
    # Add CSS to the header with tags

    tags$head(
        tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Encode+Sans+Semi+Condensed:wght@400&display=swap');
      @import url('https://fonts.googleapis.com/css2?family=Oswald&display=swap');
      h1 {
        font-family: 'Oswald', sans-serif;
        font-size: 50;
        font-weight: 600;
        line-height: 1.1;
        color: #41541e;
      }

      h3 {
        font-family: 'Oswald', sans-serif;
        font-size: 24;
        font-weight: 100;
        line-height: 1.1;
        color: #41541e;
      }

      h4 {
        font-family: 'Encode Sans Semi Condensed', sans-serif;
        font-size: 20;
        font-weight: 600;
        line-height: 1.1;
        color: #41541e;
      }

      h5 {
        font-family: 'Encode Sans Semi Condensed', sans-serif;
        font-size: 16;
        font-weight: 400;
        line-height: 1.1;
        color: #41541e;
      }
      hr {border-top: 1px solid #41541e;}

      body {
        background-color: #E9E7DA;
        font-family: 'Encode Sans Semi Condensed', sans-serif;
      }

    "))
    ),

    # Application title
    headerPanel("Simple Linear Regression"),
    h3("   Econometrics 01 - rrodriguezra@nebrija.es"),
    h5(tags$a(href = "https://www.antoinesoetewey.com/", "   Adapted from Stats and R by A. Soetewey")),
    withMathJax(),
    sidebarLayout(
                sidebarPanel(
#value = 'inputData',
#title = 'Data Import',
                  h4("Enter data"),
                  #tags$b("Data:"),
                  textInput("x", "x", value = "80, 120, 90, 80, 87, 75, 34", placeholder = "Enter values separated by a comma with decimals as points, e.g. 1.2, 4.4, 5, 5.03, etc."),
                  textInput("y", "y", value = "850, 1200, 850, 750, 950, 775, 500", placeholder = "Enter values separated by a comma with decimals as points, e.g. 1.2, 4.4, 5, 5.03, etc."),
                  hr(),
                  checkboxInput("manual", "Using a file", FALSE),
                  h4("Import data"),
                    fileInput(inputId = "inFile", "Choose a CSV File",
                              accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv"
                              )
                    ),
                    checkboxInput("header", "File with Header", TRUE),
                    verbatimTextOutput("txt"),
                hr(),
                tags$b("Plot:"),
                checkboxInput("se", "Add confidence interval around the regression line", FALSE),
                textInput("xlab", label = "Axis labels:", value = "x", placeholder = "x label"),
                textInput("ylab", label = NULL, value = "y", placeholder = "y label"),
                hr(),
                radioButtons("format", "Download report:", c("HTML"), inline = TRUE ),
                checkboxInput("echo", "Show code in report?", FALSE),
                downloadButton("downloadReport"),
                hr(),
                HTML('<a rel="license" href="http://creativecommons.org/licenses/by/2.0/be/" target="_blank"><img alt="Licence Creative Commons" style="border-width:0"
        src="http://i.creativecommons.org/l/by/2.0/be/80x15.png"/></a>
        Used in accordance with <a href="http://sites.uclouvain.be/RShiny"
        target="_blank"><font face="Courier">RShiny@UCLouvain</font></a>. Original Code at <a href="https://github.com/AntoineSoetewey/statistics-202/blob/master/app.R" target="_blank">GitHub</a>.'),
                hr()
                ),
                mainPanel(
                 tabsetPanel(
                             tabPanel("Analysis",
                                      br(),
                                      tags$h4("Regression plot:"),
                                      plotlyOutput("plot"),
                                      br(),
                                      uiOutput("results"),
                                      br(),
                                      tags$h4("Interpretation:"),
                                      uiOutput("interpretation"),
                                      br(),
                                      tags$h4("Your data:"),
                                      DT::dataTableOutput("tbl"),
                                      br(),
                                      uiOutput("data"),
                                      br(),
                                      tags$h4("Compute parameters by hand:"),
                                      uiOutput("by_hand"),
                                      br(),
                                      tags$h4("Compute parameters in R:"),
                                      htmlOutput("tabmodel"),
                                      br(),
                                      verbatimTextOutput("summary"),

                             ),
                             tabPanel("Instructions",
                                      br(),
                                      tags$li("Select how to inform your data: Enter manually or Upload a CSV file."),
                                      br(),
                                      tags$li("Check - Using a file - if you intend to analyze the data in the CSV file."),
                                      br()
                             )
                    )
                )
)
)

server <- function(input, output, session) {

  #### Read CSV

    output$txt <- renderText({
        req(input$inFile)
        message("Reading data...")
        rdata <- fread(input$inFile$datapath, header = input$header,
                       sep = ",", data.table = F, verbose = F)

        message("Done")

        paste(" Number of rows: ", nrow(rdata), "\n", "Variables: ", colnames(rdata)[1],colnames(rdata)[2])
    })

    mydata <- reactive({
        read.csv2(input$inFile$datapath,header = input$header,sep = ",")
    })

    #### Data as numeric

    extract <- function(text) {
        text <- gsub(" ", "", text)
        split <- strsplit(text, ",", fixed = FALSE)[[1]]
        as.numeric(split)
    }

    # Data output
    output$tbl <- DT::renderDataTable({

        ifelse(input$manual,
               {
                 mydt=as.data.frame(mydata())
                 y = as.numeric(mydt[,1])
                 x = as.numeric(mydt[,2])},
              {
               y <- extract(input$y)
               x <- extract(input$x)}
        )

        mydf <- data.frame(x, y)
        DT::datatable(mydf,
                      extensions = "Buttons",
                      options = list(
                          lengthChange = FALSE,
                          dom = "Blfrtip",
                          buttons = c("copy", "csv", "excel") #, "pdf", "print")
                      )
        )
    })

    #### Check data

    output$data <- renderUI({
        ifelse(input$manual,
               {
                   mydt=as.data.frame(mydata())
                   y = as.numeric(mydt[,1])
                   x = as.numeric(mydt[,2])},
               {
                 y <- extract(input$y)
                 x <- extract(input$x)}
        )
        if (anyNA(x) | length(x) < 2 | anyNA(y) | length(y) < 2) {
            "Invalid input or not enough observations"
        } else if (length(x) != length(y)) {
            "Number of observations must be equal for x and y"
        } else {
            withMathJax(
                paste0("\\(\\bar{x} =\\) ", round(mean(x), 3)),
                br(),
                paste0("\\(\\bar{y} =\\) ", round(mean(y), 3)),
                br(),
                paste0("\\(n =\\) ", length(x))
            )
        }
    })

    #### By Hand

    output$by_hand <- renderUI({
        ifelse(input$manual,
                {
                   mydt=as.data.frame(mydata())
                   y = as.numeric(mydt[,1])
                   x = as.numeric(mydt[,2])},
               {
                 y <- extract(input$y)
                 x <- extract(input$x)}
        )
        fit <- lm(y ~ x)
        withMathJax(
            paste0("\\(\\hat{\\beta}_1 = \\dfrac{\\big(\\sum^n_{i = 1} x_i y_i \\big) - n \\bar{x} \\bar{y}}{\\sum^n_{i = 1} (x_i - \\bar{x})^2} = \\) ", round(fit$coef[[2]], 3)),
            br(),
            paste0("\\(\\hat{\\beta}_0 = \\bar{y} - \\hat{\\beta}_1 \\bar{x} = \\) ", round(fit$coef[[1]], 3)),
            br(),
            br(),
            paste0("\\( \\Rightarrow y = \\hat{\\beta}_0 + \\hat{\\beta}_1 x = \\) ", round(fit$coef[[1]], 3), " + ", round(fit$coef[[2]], 3), "\\( x \\)")
        )
    })

    #### By R (Summary)

    output$summary <- renderPrint({
        ifelse(input$manual,
               {
                 mydt=as.data.frame(mydata())
                 y = as.numeric(mydt[,1])
                 x = as.numeric(mydt[,2])},
               {
                   y <- extract(input$y)
                   x <- extract(input$x)}
        )
        fit <- lm(y ~ x)
        summary(fit)
        #tab_model(fit)
    })

    #### Render tab_model

    output$tabmodel <- renderUI({
      ifelse(input$manual,
             {
               mydt=as.data.frame(mydata())
               y = as.numeric(mydt[,1])
               x = as.numeric(mydt[,2])},
             {
               y <- extract(input$y)
               x <- extract(input$x)}
      )
      fit <- lm(y ~ x)
      #summary(fit)
      custom_table <- tab_model(fit,show.se = TRUE, show.stat = TRUE, collapse.ci = TRUE)
                                #col.order = c("est", "se", "stat", "p"))
      HTML(custom_table$knitr)

    })



    #### Results

    output$results <- renderUI({
        ifelse(input$manual,
               {
                 mydt=as.data.frame(mydata())
                 y = as.numeric(mydt[,1])
                 x = as.numeric(mydt[,2])},
               {
                   y <- extract(input$y)
                   x <- extract(input$x)}
        )
        fit <- lm(y ~ x)
        withMathJax(
            paste0(
                "Adj. \\( R^2 = \\) ", round(summary(fit)$adj.r.squared, 3),
                ", \\( \\beta_0 = \\) ", round(fit$coef[[1]], 3),
                ", \\( \\beta_1 = \\) ", round(fit$coef[[2]], 3),
                ", P-value ", "\\( = \\) ", signif(summary(fit)$coef[2, 4], 3)
            )
        )
    })


    #### Interpretation

    output$interpretation <- renderUI({
        ifelse(input$manual,
               {
                 mydt=as.data.frame(mydata())
                 y = as.numeric(mydt[,1])
                 x = as.numeric(mydt[,2])},
                {
                   y <- extract(input$y)
                   x <- extract(input$x)}
        )
        fit <- lm(y ~ x)
        if (summary(fit)$coefficients[1, 4] < 0.05 & summary(fit)$coefficients[2, 4] < 0.05) {
            withMathJax(
                paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
                br(),
                br(),
                paste0("\\( \\beta_0 : \\) ","For a (hypothetical) value of ", input$xlab, " = 0, the mean of ", input$ylab, " = ", round(fit$coef[[1]], 3), "."),
                br(),
                br(),
                paste0("\\( \\beta_1 : \\) ","For an increase of one unit of ", input$xlab, ", ", input$ylab, ifelse(round(fit$coef[[2]], 3) >= 0, " increases (in mean) by ", " decreases (on average) by "), abs(round(fit$coef[[2]], 3)), ifelse(abs(round(fit$coef[[2]], 3)) >= 2, " units", " unit"), ".")
            )
        } else if (summary(fit)$coefficients[1, 4] < 0.05 & summary(fit)$coefficients[2, 4] >= 0.05) {
            withMathJax(
                paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
                br(),
                paste0("\\( \\beta_0 : \\) ","For a (hypothetical) value of ", input$xlab, " = 0, the mean of ", input$ylab, " = ", round(fit$coef[[1]], 3), "."),
                br(),
                paste0("\\( \\beta_1 : \\)", " is not significantly different from 0 (p-value = ", round(summary(fit)$coefficients[2, 4], 3), ") so there is no significant relationship between ", input$xlab, " and ", input$ylab, ".")
            )
        } else if (summary(fit)$coefficients[1, 4] >= 0.05 & summary(fit)$coefficients[2, 4] < 0.05) {
            withMathJax(
                paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
                br(),
                paste0("\\( \\beta_0 : \\)", " is not significantly different from 0 (p-value = ", round(summary(fit)$coefficients[1, 4], 3), ") so when ", input$xlab, " = 0, the mean of ", input$ylab, " is not significantly different from 0."),
                br(),
                paste0("\\( \\beta_1 : \\) ","For an increase of one unit of ", input$xlab, ", ", input$ylab, ifelse(round(fit$coef[[2]], 3) >= 0, " increases (on average) by ", " decreases (on average) by "), abs(round(fit$coef[[2]], 3)), ifelse(abs(round(fit$coef[[2]], 3)) >= 2, " units", " unit"), ".")
            )
        } else {
            withMathJax(
                paste0("(Make sure the assumptions for linear regression (independance, linearity, normality and homoscedasticity) are met before interpreting the coefficients.)"),
                br(),
                paste0("\\( \\beta_0 \\)", " and ", "\\( \\beta_1 \\)", " are not significantly different from 0 (p-values = ", round(summary(fit)$coefficients[1, 4], 3), " and ", round(summary(fit)$coefficients[2, 4], 3), ", respectively) so the mean of ", input$ylab, " is not significantly different from 0.")
            )
        }
    })


    #### Regression Plot

    output$plot <- renderPlotly({
        ifelse(input$manual,
               {
                 mydt=as.data.frame(mydata())
                 y = as.numeric(mydt[,1])
                 x = as.numeric(mydt[,2])},
               {
                   y <- extract(input$y)
                   x <- extract(input$x)}
        )
        fit <- lm(y ~ x)
        dat <- data.frame(x, y)
        p <- ggplot(dat, aes(x = x, y = y)) +
            geom_point() +
            stat_smooth(method = "lm", se = input$se) +
            ylab(input$ylab) +
            xlab(input$xlab) +
            theme_minimal()
        ggplotly(p)
    })

    #### Download Report

    output$downloadReport <- downloadHandler(
      filename = function() {
              paste("report", sep = ".",
              switch(input$format, HTML = "html")
        )
      },

      content = function(file) {
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        rmarkdown::render(input="report.Rmd", #tempReport,
                          output_format = switch(input$format, PDF = pdf_document(), HTML = html_document()),
                          output_file = file
        )
      }
    )


#Fin de server
}




shinyApp(ui, server)
