library(shiny)
# Define UI
ui <- shinyUI(
     fluidPage(
     titlePanel("Have I Been Pwned Search"),
     sidebarLayout(
          sidebarPanel(
 #              tags$p("Link to Shiny App github repo: ", tags$a("https://github.com/bbrewington/have-we-been-pwned"))
               HTML("<div>Link to github repo:
                         <a href=https://github.com/bbrewington/have-we-been-pwned target=_blank style=display: inline>github.com/bbrewington/have-we-been-pwned</a></div>
                    <div>Data source: <a href=http://haveibeenpwned.com target=_blank style=display: inline>haveibeenpwned.com</a></div>")),
          mainPanel(
               verticalLayout(
                    wellPanel(
                         textInput(inputId = "accounts", "Accounts (comma separated list): "),
                         submitButton("Search Accounts against www.haveibeenpwned.com")),
                    tableOutput("table")
          ))
)))

# Define server logic
server <- shinyServer(function(input, output) {
     output$table <- renderTable({
     withProgress(message = "Searching Accounts", expr = {
     require(httr)
     require(dplyr)
     require(readr)
if(nchar(input$accounts) > 0){
     accounts <- strsplit(x = input$accounts, split = "(, )|,")  
     r <- vector(mode = "list", length = length(accounts))
     results <- vector(mode = "list", length = length(accounts))
     
     for(i in 1:length(accounts)){
          r[[i]]$raw <- GET(paste0("https://haveibeenpwned.com/api/v2/breachedaccount/",accounts[i]))
          r[[i]]$content <- content(r[[i]]$raw)
          
          if(length(r[[i]]$content) > 0){
               results[[i]]$breach =
                    lapply(r[[i]]$content, function(x){
                         data.frame(account = accounts[i], Title = x$Title, 
                                    BreachDate = x$BreachDate, AddedDate = x$AddedDate, 
                                    Description = x$Description,
                                    stringsAsFactors = FALSE)}
                    ) %>%
                    bind_rows()
          } else {
               results[[i]]$breach = data.frame(account = accounts[i], Title = NA, 
                                                BreachDate = NA, AddedDate = NA, 
                                                Description = NA,
                                                stringsAsFactors = FALSE)
          }
          
          Sys.sleep(1.51)
     }
     
     return(lapply(results, function(x) x[[1]]) %>% bind_rows())
} else {
     return(data.frame())
}

})
})
})

# Run the application 
shinyApp(ui = ui, server = server)
