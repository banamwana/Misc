library(shiny)
library(shinydashboard)
library(ggpubr)
library(rhandsontable)
library(episensr)
library(ggplot2)

### Got to build up from basics to find out why not working
#browseURL("https://shiny.rstudio.com/articles/debugging.html")


ui <- dashboardPage(skin = "yellow",
                    
  dashboardHeader(title = "Probabilistic Bias Analysis - Information Bias",
                  titleWidth = 12),
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(skin = "blue",
    
    # Explanation
    box(width = 12,
        h2("Probabilistic Bias Analysis - Information Bias"),
        htmlOutput("prob_intro"),
        br(),
        h4("Social Support and Therapy Completion - Background"),
        htmlOutput("intro"),
        br(),
        htmlOutput("inst"),
        br(),
        htmlOutput("inst2"),
        br(),
        htmlOutput("intro2"),
        br(),
        helpText(a("Link - episensr: Basic Sensitivity Analysis of Epidemiological Results",
                   href="https://CRAN.R-project.org/package=episensr")
                    )
        ),
    
    # Crude Data and Simulation Panel
    box(width = 12, title = NULL,
        
        #Crude Data
        column(width = 7,
               h4("Unadjusted Two-by-Two Table"),
               br(),
               rHandsontableOutput("tt"),
               br(),
               selectInput(inputId = "type",
                             label = "Misclassification Type (Outcome or Exposure)",
                             choices = c("outcome", "exposure"),
                             selected = "outcome")
               ),
        
        #Summary or Record
        column(width = 5,
               wellPanel(
                sliderInput(inputId = "sims",
                            label = "Number Simulations",
                            min = 10000, max = 1000000,
                            value = 10000, step = 10000),
                sliderInput(inputId = "bins",
                            label = "Number Histogram Bins",
                            min = 10, max = 200,
                            value = 50, step = 5)
                 
                 
                 )
               )
        

        ),
    
    box(width = 12, title = "Trapezoidal Distribution of Bias Parameters",
        htmlOutput("classification"),
        br(),
        withMathJax(
          helpText("$$Min \\le Lower~Mode \\le Upper~Mode \\le Max$$")
                    ),
        
        # Sensitivity Table
        column(width = 6,
                 wellPanel(
                   fluidRow(
                     column(width = 5,
                            h4("Sensitivity")
                            ),
                     column(width = 4,
                            numericInput(inputId = "corr.se",
                                         label = "Correlation",
                                         min = 0.01, max = 1, value = 0.8,
                                         step = 0.01)
                            )
                     ),
                   rHandsontableOutput("se")
                 )
               ),
        
        #Specificity table
        column(width = 6,
              wellPanel(
                fluidRow(
                  column(width = 5,
                         h4("Specificity")
                         ),
                  column(width = 4,
                         numericInput(inputId = "corr.sp",
                                      label = "Correlation",
                                      min = 0.01, max = 1, value = 0.8,
                                      step = 0.01)
                        )
                ),
                rHandsontableOutput("sp")
              )
        )
    ),
    
    box(width = 5,
        
        actionButton(inputId = "sim", label = "Simulate"),

        span(textOutput("error"), style="color:red"),
        span(textOutput("rerun"), style="color:red")


        ),
    
    #Results
    box(width = 12, title = "Simulation Results",
        tableOutput("obs"),
        tableOutput("tab"),
        plotOutput("hist1"),
        br(),
        plotOutput("hist2"),
        br(),
        plotOutput("biasparams"),
        br()
        )
    
    )
  
)


server <-  function(input, output, session) {

  output$intro <- renderPrint(
    cat("Suppose researchers interested in the effect of social support on 
        adherence to therapy among breast cancer patients published the following study protocol: 
        \"Treatment completion was assessed at the end of follow-up as (yes/no) 
        based on study participant's self-report of whether or not they completed 
        the therapies prescribed by their oncologist.\"")

  
    )
  
  output$inst <- renderPrint(
    
    cat("Do you believe assessing the outcome in this manner may introduce bias 
        into the study? Using the features below, you may incorporate your uncertainty 
        in bias parameters and simulate bias-adjusted results. This Shiny app uses
        the <i>probsens</i> function within the <i>episensr</i> package in R to conduct a 
        probabilistic bias analysis using Monte Carlo Simulation. Indicate the number of bins 
        for histograms of the results and choose the bias parameters and their correlation. 
        You may run as many as 1 million simulations!")
  
  )
  
  output$inst2 <- renderPrint(
    
    cat("Alternately, you may perform your own analyses of either outcome or exposure 
        misclassification by inputting summary counts from your own data and selecting 
        the misclassification type.")
  )
  
  output$intro2 <- renderPrint(
    
    cat("<i>-Adapted from an exercise developed by Lindsay Collin for the course 
        \"Applying Quantitative Bias Analysis to Epidemiologic Data,\" taught by 
        Dr. Timothy Lash at Emory Rollins School of Public Health.</i>")
  
  )
  

  
  output$prob_intro <- renderPrint(
    
    cat("<i>Probabilistic Bias Analysis is a form of Quantitative Bias Analysis 
        that allows adjustment of effect estimates using bias parameters drawn 
        from a probability distribution and concise presentation of results. 
        In the case of Information Bias/Misclassification, this approach can
        account for the uncertainty of diagnostic Sensitivity and Specificity.</i>")
  
  )
  
  output$classification <- renderPrint(
    
    cat("Classification of <i>Disease among Exposure Groups</i> or 
        <i>Exposure among Disease Groups</i>")
    
  )
  
  tt <- data.frame(
    col1 = as.integer(c(1770, 1243)),
    col2 = as.integer(c(2218, 2761))
  )

  
  se <- data.frame(
      Min.Se = c(0.72,0.72),
      Lower.Mode.Se = c(0.76,0.76),
      Upper.Mode.Se = c(0.78, 0.78),
      Max.Se = c(0.8,0.8)
      )
  
  sp <- data.frame(
      Min.Sp = c(0.8,0.8),
      Lower.Mode.Sp = c(0.84,0.84),
      Upper.Mode.Sp = c(0.87, 0.87),
      Max.Sp = c(0.9,0.9)
      )
  
  #output$tt <- renderTable(tt, rownames = TRUE, digits = 0)
  
  output$tt <- renderRHandsontable(rhandsontable(tt, readOnly = FALSE,
                                   rowHeaders = c("D+", "D-"),
                                   colHeaders = c("E+", "E-")
                                   ))
        # hot_validate_numeric(cols = c(1,2)))
  
  output$se <- renderRHandsontable(rhandsontable(se, readOnly = FALSE,
                                   rowHeaders = c("E/D+", "E/D-"),
                                   colHeaders = c("Min", "Lower Mode",
                                                  "Upper Mode", "Max")
                                   ))
        # hot_validate_numeric(cols = c(1,2,3,4), min = 0.6))
      
  output$sp <- renderRHandsontable(rhandsontable(sp, readOnly = FALSE,
                                   rowHeaders = c("E/D+", "E/D-"),
                                   colHeaders = c("Min", "Lower Mode",
                                                  "Upper Mode", "Max")
                                   ))
      # hot_validate_numeric(cols = c(1,2,3,4), min = 0.7))
  
  # cell counts
  observeEvent( input$sim, {
    
    se <- hot_to_r(input$se)
    
    sp <- hot_to_r(input$sp)
    
    tt_input <- as.matrix(hot_to_r(input$tt))
    
    seca <- as.numeric(se[2, ])
    spca <- as.numeric(sp[2, ])
    seexp <- as.numeric(se[1, ])
    spexp <- as.numeric(sp[1, ])
    
    seca.txt <- paste(seca, collapse = ",")
    spca.txt <- paste(spca, collapse = ",")
    seexp.txt <- paste(seexp, collapse = ",")
    spexp.txt <- paste(spexp, collapse = ",")
    
              
    prob_bias <- tryCatch(
       {
         probsens(tt_input,
              type = input$type,
              reps = input$sims,
              seca.parms = list(dist = "trapezoidal", parms = seca),
              spca.parms = list(dist = "trapezoidal", parms = spca),
              seexp.parms = list(dist = "trapezoidal", parms = seexp),
              spexp.parms = list(dist = "trapezoidal", parms = spexp),
              corr.se = input$corr.se,
              corr.sp = input$corr.sp)
         
       }, warning = function(w){
           return(w$message)
       }, error = function(e){
           return(e$message)
         }
       )
        
     if(is(prob_bias, "list")){
       
      obsvd <- matrix(prob_bias$obs.measures[2, ],
                      nrow = 1, byrow = TRUE)
      rownames(obsvd) <- "Observed OR"
      colnames(obsvd) <- c("Estimate", "LCL", "UCL")
      result <- prob_bias$adj.measures[c(2,4), ]
      row.names(result) <- c("Sys. Error OR", "Sys. + Rand. Error OR")
      plot.data <- prob_bias$sim.df
    
      output$obs <- renderTable(obsvd, rownames = TRUE)
      output$tab <- renderTable(result, rownames = TRUE)
      
    
      #plot functions
      or_plot <- function(var, title, fill){
        ggplot(plot.data, aes(x = get(var))) +
          geom_histogram(bins = input$bins, fill = fill, col = "black") +
          xlab(title) + ylab("Count") + theme_bw() +
          theme(
            axis.title.x.bottom = element_text(family = "",
                                      face = "bold.italic",
                                      colour = 'black', size = 14),
            axis.title.y.left = element_text(family = "",
                                      face = "bold",
                                      colour = 'black', size = 14)
          )
        }
      
      param_plot <- function(var, title, fill, xmin, xmax){
        ggplot(plot.data, aes(x = get(var))) +
          geom_histogram(bins = input$bins/2, fill = fill, col = "black") +
          xlab(title) + ylab("Count") + theme_bw() +
          theme(
            axis.title.x.bottom = element_text(family = "",
                                      face = "bold.italic",
                                      colour = 'black', size = 14),
            axis.title.y.left = element_text(family = "",
                                      face = "bold",
                                      colour = 'black', size = 14)
            ) +
          scale_x_continuous(limits = c(xmin, xmax))
      }
      
      #Systematic Error plot
      sysor_plot <- or_plot(var = "corr.OR", title = "Systematic Error Odds Ratios",
                fill = "#0033CC")
      
      output$hist1 <- renderPlot(sysor_plot)
      
      #Systematic + Rand Error plot
      sys_rand_plot <- or_plot(var = "tot.OR", title = "Systematic and Random Error Odds Ratios",
                fill = "#CC0000")
      
      output$hist2 <- renderPlot(sys_rand_plot)
      
      #Bias Param Plots
      seca.plot <- param_plot(var = "seca", title = "Sensitivity Unexposed",
                              fill = "#00CC66",
                              xmin = min(c(seca, seexp)),
                              xmax = max(c(seca, seexp)))
      spca.plot <- param_plot(var = "spca", title = "Specificity Unexposed",
                              fill = "#00CC66",
                              xmin = min(c(spca, spexp)),
                              xmax = max(c(spca, spexp)))
      seexp.plot <- param_plot(var = "seexp", title = "Sensitivity Exposed",
                               fill = "#66CCFF",
                               xmin = min(c(seca, seexp)),
                               xmax = max(c(seca, seexp)))
      spexp.plot <- param_plot(var = "spexp", title = "Specificity Exposed",
                               fill = "#66CCFF",
                               xmin = min(c(spca, spexp)),
                               xmax = max(c(spca, spexp)))
        
      output$biasparams <- renderPlot(
        ggarrange(seca.plot, spca.plot, seexp.plot, spexp.plot, ncol = 2, nrow = 2
        )
      )
      
      output$error <- renderText({
        ""
        })
      
      output$rerun <- renderText({
        ""
        })
       
     } else {
             output$error <- renderText({
               prob_bias
               })
             output$rerun <- renderText({
               "Please adjust inputs and rerun simulation."
               })
     }
    

    
  })

}

shinyApp(ui = ui, server = server)

