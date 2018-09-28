

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Missing data Mechanisms"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        sliderInput(inputId="alpha", label="percentage of missing data", min=0, max=100, value=25, step = 1, round = FALSE, format = NULL, locale = NULL, ticks = TRUE, animate = FALSE, width = NULL, sep = ",", pre = NULL, post = NULL),
        numericInput(inputId="cor1", label="correlation between variables", value=0.5, min = 0, max = 1, step = 0.01),
        selectInput(inputId="g", label="missing data mechanism", choices=c("MCAR", "MAR", "MNAR"), selected = "MCAR", multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)
        )
         ,

mainPanel(
  plotOutput(outputId="plot1"),
  tableOutput(outputId = "table"),
  
  plotOutput(outputId="hist1")
      )
)
 )
)


library(MASS)
library(mice)
library(ggplot2)

means <- c(25,10,10)
var <- c((1.33*1.33),(3.26*3.26),(3.25*3.25))
cov <- matrix(c(3,3,3,3,2,2,3,3,2),3,3)
diag(cov) <- var
dat <- mvrnorm(100, means, cov)
colnames(dat) <- c("V1", "V2", "V3")
dat <- data.frame(dat)

#make missings
MAR <- function(x, alpha, pattern, f, a, quant,g)
{
  xobs <- testcand1 <- testpip <- tests <- testincompl <- testcand2 <- testresp <- fltest <- cltest<- bltest <- list()
  x <- data.matrix(x)
  n <- NROW(x)
  m <- NCOL(x)
  sf <- sum (f)
  f <- data.matrix (f/sf)
  u <- runif(n)
  res <- outer(u, cumsum(f), ">")   # vervanging voor loop
  cand <- rowSums(res) + 1          # vervanging voor loop
  testcand1 <- cbind(u, cand)
  ss <- x %*% t(a*pattern)
  sortss <- apply(ss, 2, function(z) sort(z) )
  p <- data.matrix (rep(0,n))
  for( i in 1:nrow(pattern) ){
    gi <- cbind(1, g[i])
    quanti <- cbind(cbind(0, quant[i,]), 1)
    sumx <- 0
    for (j in 1:NCOL(gi)){
      sumx <- sumx + (quanti[j+1] - quanti[j]) %*% gi[j]
    }
    si <- matrix(sortss[, i], n, 1)
    ci <- si[ceiling(NROW(si) %*% t(quant[i,]))]
    boolcan <- cand == i
    boolcan <-ifelse(boolcan == T, 1, 0)
    si <- data.matrix(ss[which(boolcan !=0), i])
    cl <- rep(1, NROW(si))
    for(j in 1:NROW(ci)){
      fl <- rep(ci[j], NROW(si))
      fltest [[i]] <- fl
      bl <- ifelse(si > fl, 1, 0)
      cl <- cl + bl
    }
    pip <- data.matrix(rep(0, NROW(si)))
    for(j in 1:(NROW(ci) + 1)){
      boolcl <- cl == j
      boolcl <- ifelse(boolcl == T, 1, 0)
      if ( any(boolcl == 1) ){
        pip[which(boolcl !=0)] <- rep( ((alpha * gi[j]) / sumx) , sum(boolcl))
        testpip [[i]] <- pip
        tests [[i]] <- si
        cltest [[i]] <- cl
        bltest [[i]] <- boolcl
      }
    }
    p[which(boolcan !=0)] <- pip
  }
  u <- data.matrix(runif(n))
  incompl <- ifelse(u<= p, 1, 0)
  testincompl <- cbind (u, p, incompl)
  cand <- cand * incompl
  testcand2 <- (cand)
  resp <- diag(0, n, m)
  bool <- cand == 0
  bool <- ifelse(bool == T, 1, 0)
  resp[which(bool !=0), c(1:m)] <- matrix(1, sum(bool), m)
  bool <- cand > 0                # vervanging voor loop
  bool <- ifelse(bool == T, 1, 0) # vervanging voor loop
  if ( any(bool == 1) )	{resp[which(bool !=0), c(1:m)] <- matrix((pattern[cand,]), byrow = T)}  # vervanging voor loop
  testresp <- (apply (resp, 1, prod))
  xobs <- ifelse(resp==1, x , NA)
  list (xobs=xobs, testcand1=testcand1, testincompl=testincompl, testcand2=testcand2, testresp=testresp, testpip=testpip, tests=tests, fltest=fltest, cltest=cltest, bltest=bltest)
}


MNAR <- function(x, alpha, pattern, f,a)
{
  #a=pattern
  quant=data.matrix(c(rep(0.5,nrow(pattern))))
  g=data.matrix(c(rep(4,nrow(pattern))))
  xobs <- testcand1 <- testpip <- tests <- testincompl <- testcand2 <- testresp <- fltest <- cltest<- bltest <- sscore <-  list()
  x <- data.matrix(x)
  n <- NROW(x)
  m <- NCOL(x)
  sf <- sum (f)
  f <- data.matrix (f/sf)
  u <- runif(n)
  res <- outer(u, cumsum(f), ">")   # vervanging voor loop
  cand <- rowSums(res) + 1          # vervanging voor loop
  testcand1 <- cbind(u, cand)
  ss <- x %*% t(a)
  sscore <- ss
  p <- data.matrix (rep(0,n))
  for( i in 1:nrow(pattern) ){
    gi <- cbind(1, g[i ])
    quanti <- cbind(cbind(0, quant[i, ]), 1)
    sumx <- 0
    for (j in 1:NCOL(gi)){
      sumx <- sumx + (quanti[j+1] - quanti[j]) %*% gi[j]
    }
    si <- ss[, i]
    si <- sort (si)
    ci <- si[ceiling(NROW(si) %*% t(quant[i,]))]
    boolcan <- cand == i
    boolcan <-ifelse(boolcan == T, 1, 0)
    si <- data.matrix(ss[which(boolcan !=0), i])
    cl <- rep(1, NROW(si))
    for(j in 1:NROW(ci)){
      fl <- rep(ci[j], NROW(si))
      fltest [[i]] <- fl
      bl <- ifelse(si > fl, 1, 0)
      cl <- cl + bl
    }
    pip <- data.matrix(rep(0, NROW(si)))
    for(j in 1:(NROW(ci) + 1)) {
      boolcl <- cl == j
      boolcl <- ifelse(boolcl == T, 1, 0)
      if ( any(boolcl == 1) ){
        pip[which(boolcl !=0)] <- rep( ((alpha * gi[j]) / sumx) , sum(boolcl))
        testpip [[i]] <- pip
        tests [[i]] <- si
        cltest [[i]] <- cl
        bltest [[i]] <- boolcl
      }
    }
    p[which(boolcan !=0)] <- pip
  }
  u <- data.matrix(runif(n))
  incompl <- ifelse(u<= p, 1, 0)
  testincompl <- cbind (u, p, incompl)
  cand <- cand * incompl
  testcand2 <- (cand)
  resp <- diag(0, n, m)
  bool <- cand == 0
  bool <- ifelse(bool == T, 1, 0)
  resp[which(bool !=0), c(1:m)] <- matrix(1, sum(bool), m)
  bool <- cand > 0                # vervanging voor loop
  bool <- ifelse(bool == T, 1, 0) # vervanging voor loop
  if ( any(bool == 1) )  {resp[which(bool !=0), c(1:m)] <- matrix((pattern[cand,]), byrow = T)}  # vervanging voor loop
  testresp <- (apply (resp, 1, prod))
  xobs <- ifelse(resp==1, x , NA)
  list (xobs=xobs, testcand1=testcand1, testincompl=testincompl, testcand2=testcand2, testresp=testresp, testpip=testpip, tests=tests, fltest=fltest, cltest=cltest, bltest=bltest, sscore=sscore)
}

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
 
   correlation <- reactive({correlation <- input$cor1})
 
  g <- reactive({
   if(input$g == "MCAR"){g<-1}
   if(input$g == "MAR"){g<-4}
    g
 })
    
 alpha <- reactive({alpha <- input$alpha})
 
  output$plot1 <- renderPlot({ 
   dat$Y <- dat$V1*correlation()+runif(nrow(dat))
   dat$Y <- (dat$Y - mean(dat$Y)) + 15
   dat1 <- data.frame(cbind(dat$V1, dat$Y, dat$V2))
   if(input$g!="MNAR"){
     set.seed(23183)
   misout <- MAR(x=dat1, alpha=(alpha()/100), pattern=t(c(1,0,1)), f=c(1), a=t(c(1,0,0)), quant=t(c(0.5)), g=c(g()))
   dat2 <- data.frame(misout$xobs)
   }
   if(input$g=="MNAR"){
     set.seed(61085)
   misout <- MNAR(x=dat1, alpha=(alpha()/100), pattern=t(c(1,0,1)),a=t(c(0,1,0)), f=c(1))
   #dat2 <- dat1
   #qq <- quantile(dat1$Y, probs=1-(alpha()/100))
   #dat2$Y[dat2$Y > qq] <- NA
   dat2 <- data.frame(misout$xobs)
   }
   
   dat1 <- data.frame(dat1)
   colnames(dat1) <- c("V1", "Y", "V3")
   colnames(dat2) <- c("V1", "Y", "V3")
   
  plotdata <- data.frame(X=dat1$V1, Y=dat1$Y, Missing=factor(ifelse(!is.na(dat2$Y),"Observed","Missing"), levels=c("Observed", "Missing")))
  ggplot(plotdata, aes(X, Y, group=Missing))+
    geom_point(aes(shape=Missing),size=3)+
    scale_shape_manual(values=c(19,1))+
    labs(title="Observed versus missing data points",x="BMI", y="Knee pain score")+
    theme (text= element_text (size=16))
  
})

 output$table <- renderTable({
   dat$Y <- dat$V1*correlation()+runif(nrow(dat))
   dat$Y <- (dat$Y - mean(dat$Y)) + 15
   dat1 <- data.frame(cbind(dat$V1, dat$Y, dat$V2))
   if(input$g!="MNAR"){
     set.seed(23183)
     data <-MAR(x=dat1, alpha=(alpha()/100), pattern=t(c(1,0,1)), f=c(1), a=t(c(1,0,0)),quant=t(c(0.5)), g=c(g()))
     dat2 <- data.frame(data$xobs)
     colnames(dat2) <- c("V1", "Y", "V3")
     
   }
   if(input$g=="MNAR"){
     set.seed(61085)
     data <- MNAR(x=dat1, alpha=(alpha()/100), pattern=t(c(1,0,1)),a=t(c(0,1,0)), f=c(1))
     #dat2 <- dat1
     #dat2$Y[dat1$Y > quantile(dat1$Y, probs=1-(alpha()/100))] <- NA
     #data <- data.frame(dat2)
     dat2 <- data.frame(data$xobs)
     colnames(dat2) <- c("V1", "Y", "V3")
     
   }
   meany <-mean(dat2$Y,na.rm=TRUE)
   sdy <-sd(dat2$Y, na.rm=TRUE)
   table1 <-matrix(c(meany,sdy), nrow=1)
   colnames(table1) <- c("mean", "standard deviation")
   rownames(table1) <- "Knee pain score"
   table1
 })
 
 
output$hist1 <- renderPlot({
  dat$Y <- dat$V1*correlation()+runif(nrow(dat))
  dat$Y <- (dat$Y - mean(dat$Y)) + 15
  dat1 <- data.frame(cbind(dat$V1, dat$Y, dat$V2))
  if(input$g!="MNAR"){
    set.seed(23183)
  misout <- MAR(x=dat1, alpha=(alpha()/100), pattern=t(c(1,0,1)), f=c(1), a=t(c(1,0,0)), quant=t(c(0.5)), g=c(g()))
  dat2 <- data.frame(misout$xobs)}
  if(input$g=="MNAR"){
    set.seed(61085)
    misout <- MNAR(x=dat1, alpha=(alpha()/100), pattern=t(c(1,0,1)),a=t(c(0,1,0)), f=c(1))
    #dat2 <- dat1
    #dat2$Y[dat1$Y > quantile(dat1$Y, probs=1-(alpha()/100))] <- NA
    dat2 <- data.frame(misout$xobs)}
  
  
  dat1 <- data.frame(dat1)
  colnames(dat1) <- c("V1", "Y", "V3")
  colnames(dat2) <- c("V1", "Y", "V3")
  
  ymast <- data.frame(Missing=c(rep("complete", nrow(dat1)), rep("incomplete", nrow(dat2))),  Y=c(dat1$Y, dat2$Y))
  if (input$alpha==0){ymast <- ymast[1:nrow(dat1),]}
  ggplot(ymast, aes(Y, fill=Missing))+ geom_histogram(bins=20, alpha=.8,position="identity") +
    labs(title="Histogram of the complete versus incomplete data situation", x="Knee pain score")+
    theme (text= element_text (size=16))  
  

})
})

# Run the application 
shinyApp(ui = ui, server = server)

