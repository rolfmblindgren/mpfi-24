library(shiny)
library(shiny.semantic)
library(ggplot2)
Sys.setenv(PATH=paste0("/usr/local/texlive/2021/bin/x86_64-linux/:",Sys.getenv("PATH")))

grid_charts <- grid_template(
    default = list(areas = rbind(c("chart1")),
                   rows_height = c("100%"),
                   cols_width = c("100%"))
)

my_data <- read.csv("https://eu.jotform.com/csv/213463841840051")
my_data <- my_data[which(!is.na(my_data[["Klientens.id"]])),]

my_scales <- c(31:42)
my_scales_f <- c(31:36)
my_scales_r <- c(37:42)

ui <- semanticPage(
    title = "MPFI 24-kalkulator",
    h1("MPFI 24-kalkulator"),
    sidebar_layout(
        sidebar_panel(
            dropdown_input(
                "simple_dropdown",
                my_data["Klientens.id"],
                type = "search selection"),
            p("Selected client:"),
            textOutput("selected_client"), 
            downloadButton("report", "Generate report")), 
        main_panel(
            grid(grid_charts,
                 chart1 = plotOutput("plot1")#,chart2 = plotOutput("plot2")
                 )
        )
    )
)

server <- shinyServer(function(input, output, session) {

        output$report <- downloadHandler(
        ## For PDF output, change this to "report.pdf"
        filename <- function() {
            paste0(session$input$simple_dropdown,".pdf")
        },

        content <- function(file) {
            ## Copy the report file to a temporary directory before
            ## processing it, in case we don't have write permissions to the
            ## current working dir (which can happen when deployed).
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)

            
            ## Set up parameters to pass to Rmd document
            params <- list(
                n = list(
                    my_data[which(
                        my_data[,"Klientens.id"]==session$input$simple_dropdown),],
                    my_scales,my_scales_f,my_scales_r))
            
            ## Knit the document, passing in the `params` list, and eval it
            ## in a child of the global environment (this isolates the code
            ## in the document from the code in this app).
	message(Sys.getenv("PATH"))
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
                              )
        }


    )


    
    output$selected_client <-
        renderText({
        
            
            a <- input[["simple_dropdown"]]

            paste(a, collapse = ", ")

        })

    output$plot1 <- renderPlot({

        update_dropdown_input(session, "simple_dropdown")
            
        my_data <- my_data[which(my_data[,"Klientens.id"]==session$input$simple_dropdown),]
            
        df <- data.frame(
            Skala <- c(colnames(my_data[my_scales]),
                       c("","Fleksibilitet","Rigiditet")),
            Skår <- c(as.vector(unlist(my_data[my_scales])),NA,
                      c(mean(as.numeric(my_data[my_scales_f])),
                        mean(as.numeric(my_data[my_scales_r])))))
        
        colnames(df) <- c("Skala","Skår")
                                        #        df$Skala <- factor(df$Skala, levels = df$Skala[order(df$Skår)])
        df$Skala <- factor(df$Skala, levels = df$Skala)

       
        ggplot(df,aes(x=Skala,y=Skår,fill=Skår)) +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                  legend.position="none") +
            geom_bar(stat = "identity")
        
        
    })

})



shinyApp(ui = ui, server = server)
