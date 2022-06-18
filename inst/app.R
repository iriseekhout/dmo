
library(shiny)
library(ggplot2)

theme_set(theme_light())

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Missing data Mechanisms"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId="alpha", label="percentage of missing data", min=0, max=100, value=30, step = 1),
      numericInput(inputId="n", label="sample size", value=100, min = 0 , step = 1),
      selectInput(inputId="g", label="missing data mechanism", choices=c("MCAR", "MAR", "MNAR"), selected = "MCAR", multiple = FALSE, selectize = TRUE)
    )
    ,
    
    mainPanel(
      fluidRow(
      plotOutput(outputId="plot1"),
      ),
      fluidRow(
        column(width = 4,
               p("Descriptives for Knee pain score"),
               tableOutput(outputId = "table1")),
      column(width = 8,
             p("Linear regression parameters for the relation between BMI and Knee pain."),
      tableOutput(outputId = "table2")),
      ),
      fluidRow(
      plotOutput(outputId="hist1")
      ),
      fluidRow(
      plotOutput(outputId="boxp1")
      )
    )
  )
)
)


library(MASS)
library(mice)
library(ggplot2)
library(dmo)

#colnames(dat) <- c("V1", "V2", "V3")
#dat <- data.frame(dat)

mn <- c(21,15,10)
var <- c((2.33*2.33),(3.26*3.26),(3.25*3.25))
cov <- matrix(c(3,3,3,3,2,2,3,3,2),3,3)
diag(cov) <- var


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  #correlation <- reactive({correlation <- input$cor1})
  dat1 <- reactive({
    set.seed(9817)
    dat1 <- mvrnorm(input$n, mn, cov)
    #dat1 <- mvrnorm(100, mn, cov)
    dat1 <- data.frame(dat1)
    colnames(dat1) <- c("X","Y", "V2")
    dat1
    })
  
  
  dat2 <- reactive({
    
    if(input$g=="MCAR"){
      set.seed(23183)
      misout <- MCAR(x=dat1(), alpha=(input$alpha/100), pattern=t(c(1,0,1)))
    } 
    if(input$g=="MAR"){
      set.seed(23183)
      misout <- dmo::MAR(x=dat1(), alpha=(input$alpha/100), pattern=t(c(1,0,1)))
    }
  if(input$g=="MNAR"){
    set.seed(23183)
    misout <- dmo::MNAR(x=dat1(), alpha=(input$alpha/100), pattern=t(c(1,0,1)))
  }
    
    
  dat2 <- data.frame(misout)
  colnames(dat2) <- c("X","Y", "V2")
  dat2$Missing <- factor(ifelse(!is.na(dat2$Y),"Observed","Missing"), levels=c("Observed", "Missing"))
  dat2
  })
  
  output$plot1 <- renderPlot({
    
    ggplot(dat1(), aes(X, Y, group=dat2()$Missing))+
      geom_point(aes(shape=dat2()$Missing),size=3)+
      scale_shape_manual(values=c(19,1))+
      labs(title="Observed versus missing data points",x="BMI", y="Knee pain score")+
      theme (text= element_text (size=16), legend.title = element_blank())
    
  })
  
  output$table1 <- renderTable({
    
    meany <-mean(dat2()$Y,na.rm=TRUE)
    sdy <-sd(dat2()$Y, na.rm=TRUE)
    table1 <-matrix(c(meany,sdy), nrow=1)
    colnames(table1) <- c("Mean", "Standard deviation")
    #rownames(table1) <- "Knee pain score"
    table1
  })
  
  output$table2 <- renderTable({
   
    coefs <- coefficients(summary(lm(Y ~ X, data=dat2())))
    coefx <- coefs[2,1]
    SEx <- coefs[2,2]
    pva <- round(coefs[2,4],3)
    table2 <-matrix(c(coefx, SEx, pva), nrow=1)
    colnames(table2) <- c("Coefficient", "Standard error", "P-value")
    #rownames(table1) <- "Knee pain score"
    table2
  })
  
  
  output$hist1 <- renderPlot({
   
    ymast <- data.frame(Missing=c(rep("complete", nrow(dat1())), rep("incomplete", nrow(dat2()))),  Y=c(dat1()$Y, dat2()$Y))
    if (input$alpha==0){ymast <- ymast[1:nrow(dat1()),]}
    ggplot(ymast, aes(Y, fill=Missing))+ geom_histogram(bins=20, alpha=.8,position="identity") +
      labs(title="Histogram of the complete versus incomplete data situation", x="Knee pain score")+
      theme (text= element_text (size=16))
    
    colors <- c("complete" = "blue", "incomplete" = "red")
    ggplot(ymast, aes(x = Y, fill = Missing)) +
      geom_histogram(bins = 20, alpha = 0.5, position = "identity")+ 
      labs(title="Histogram of the complete versus incomplete data situation", x="Knee pain score")+
      scale_fill_manual(values = colors)+
      facet_wrap(.~Missing)
    
  })
  
  output$boxp1 <- renderPlot({
    
    
    ggplot(data = dat2(), aes(x = Missing, y = X)) +
      geom_boxplot() +
      xlab("Missing data indicator (R1)") +
      ylab("BMI")
  })
  
})

# Run the application
shinyApp(ui = ui, server = server)






