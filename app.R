# ---------------------------------
# Textbook collection evaluation 
# This is an app currently in development that is intended to visualize data relating to textbook collections
# Written by:
# Leanne Olson
# Metadata Management Librarian
# Western University, London, Ontario, Canada
# lolson3@uwo.ca
# Please feel free to contact me with questions or sugggestions

# ---------------------------------
#Installing the packages used by this app.  Uncomment these if you don't have them currently installed.
#install.packages("shiny")
#install.packages('DT')
#install.packages('shinythemes')
#install.packages('ggplot2')
#install.packages('RColorBrewer')

# ---------------------------------
#Loading the packages used in this app

library(shiny)
library(DT)
library(shinythemes)
library(ggplot2)
library(RColorBrewer)
# ---------------------------------

#This set of instructions loads the csv files containing textbook information

#Textbooks file with all information
textbooks<-read.csv("Textbooks.csv")

#Textbooks file broken down by discipline
textbooksbydis<-read.csv("Textbook_disciplines.csv")

#Textbooks file with only the information to be displayed in the Data Table
textbooksclean<-read.csv("Textbooks_clean.csv")

# ---------------------------------

#This set of instructions loads the selected csv file for a call number range into variable callrange
#callrange is used to generate the graph "Textbook circulation compared to nearby items"
#I am hoping in the future to enable switching call number ranges within the applet rather than requiring users to alter the code here

callrange<-read.csv("BF637.csv")
#callrange<-read.csv("BL722.csv")
#callrange<-read.csv("CB245.csv")
#callrange<-read.csv("CB430.csv")
#callrange<-read.csv("D157.csv")
#callrange<-read.csv("DD203.csv")
#callrange<-read.csv("E96.csv")
#callrange<-read.csv("F1033.csv")
# ---------------------------------

ui <- shinyUI(fluidPage(
  #theme=shinytheme("yeti"),
  titlePanel("2016 Full Term Textbook List"),
  navbarPage("Menu:",
    
    # Generates scatterplot tab to visualize different textbook variables;
    # gives an overall picture of textbooks across the library system
             
    tabPanel("Scatterplot",
             verticalLayout(
                 plotOutput("TextbookPlot",
                            click="plot_click"
                            ),
                 verbatimTextOutput("click_info"),
          wellPanel(
          checkboxInput("subjectcheckbox", 
                        label = "Break down by subject", value = FALSE),
          selectInput("xcol", "X Variable:", 
                  choices=colnames(textbooks[5:7]),
                  selected=colnames(textbooks)[[5]]
                  ),
          selectInput("ycol", "Y Variable:", 
                  choices=colnames(textbooks[5:7]),
                  selected=colnames(textbooks)[[7]]
                  ),
          sliderInput("pointsize", 
                  "Point size", min = 1, max = 8, value = 4)
      ),
        mainPanel(
          ))),
    
    # Generates bar graph tab to visualize different textbook variables, 
    # useful to show differences in textbooks by faculty or individual subject area         
    
  tabPanel("Bar Graphs",
    sidebarLayout(
      sidebarPanel(
                    selectInput("barycol", "Y Variable:", 
                        choices=colnames(textbooksbydis[5:13]),
                        selected=colnames(textbooksbydis)[[6]]
                    ),

        checkboxInput("subjectcheckboxbar", 
                   label = "Break down by subject", value = TRUE)


    ),
    mainPanel(plotOutput("TextbookPlotBar")
              )
      )),
 
  # Generates the circulation comparison tab, 
  # to visualize how a textbook in a call number range compares against similar subject area books
  
 tabPanel("Circulation Comparisons",
          verticalLayout(
            plotOutput("circPlot"),
            wellPanel(
              print("This graph displays total circulation for textbooks and 
                    other editions of the textbook, compared to non-textbooks in 
                    the same subject range. Archived and non-loanable materials have 
                    been filtered out.
                    ")
            ),
            mainPanel(
            )
          )
 ),
 
 # Generates a data table or spreadsheet of the data; not currently working
 
 tabPanel("Data Table",
   fluidRow(
     paste("The data table includes textbooks from the 2016 full term that are currently held by
Western Libraries or the Affiliated University College libraries"),
     hr(),
      DT::dataTableOutput("textbookstable"))
          ))
  ))
server <- shinyServer(function(input, output) {

  selectedscatterData<-reactive(
    {textbooks[ , c(input$xcol, input$ycol)]
      })
  selectedbarData<-reactive(
    {textbooksbydis[ , c(input$barycol)]
      })
  selectedCircVar<-reactive(
    {textbook1[ , c(input$ycirccol)]
    })
  
  output$TextbookPlot <- renderPlot({

    disciplinecolour<-factor(textbooks$Discipline)
    if(input$subjectcheckbox)
      disciplinecolour<-factor(textbooks$Subject)
    theme_set(theme_bw(base_size = 18))

    mycolors = c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 6), 
                 brewer.pal(name="Spectral", n = 8))
    mycolors2 = c(brewer.pal(name="Blues", n = 8), brewer.pal(name="Greens", n = 6), 
                 brewer.pal(name="Oranges", n = 8))
    p<-ggplot(selectedscatterData(), aes_string(x=input$xcol, 
                          y=input$ycol, fill=disciplinecolour)) + 
      geom_point(shape=21, size=input$pointsize) + scale_fill_manual(values = mycolors2) +
      labs(fill=" ") + xlab(input$xcol) + ylab(input$ycol) + 
      ggtitle("Textbooks for 2016 Full Term Currently Collected by Western & Affiliates")
    print(p)
    })
  
  output$circPlot <- renderPlot({

    palette(c("grey","orange","green"))
    cols<-factor(callrange$IsText)
        barplot(callrange$Circ,  xlab="Call number", ylab="Circulation", 
           names.arg=callrange$X99,  
          main = "Textbook circulation compared to nearby items", 
          col=cols 
                    )
        legend("topright", legend = c("Non-textbook", "Textbook", "Alternate edition"),
               fill=c("grey", "orange", "green"))
    
  })
  output$TextbookPlotBar <- renderPlot({
 
    col.rainbow<-rainbow(23)
    palette(col.rainbow)
    ifelse(input$subjectcheckboxbar==TRUE,  
           disciplinecolour<-as.factor(textbooksbydis$Subject),
           disciplinecolour<-as.factor(textbooksbydis$Discipline))
    ifelse(input$subjectcheckboxbar==TRUE,  
           disciplinenames<-as.factor(textbooksbydis$Subject),
           disciplinenames<-as.factor(textbooksbydis$Discipline))

   par(mar=c(10,4,2,2), xpd=TRUE)
   
   if(input$subjectcheckboxbar)
   {
   barplot(selectedbarData(), col=disciplinecolour, 
                     ylab=input$barycol,  las=2, names.arg = textbooksbydis$Subject)
   }else{

     barplot(selectedbarData(), col=disciplinecolour, 
          
              ylab=input$barycol, las=2, 
             names.arg = textbooksbydis$Discipline)
  }

   output$textbookstable<-DT::renderDataTable({DT::datatable(textbooksclean)})
   
})
})
# Run the application 
shinyApp(ui = ui, server = server)