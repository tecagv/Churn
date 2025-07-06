install.packages("h2o")
install.packages("shiny")
install.packages("shinybusy")
install.packages("ui")

library(shiny)
library(shinybusy)
library(h2o)
h2o.init()

#Interface da aplicação
ui <- fluidPage(
  titlePanel("Auto Machine Learing"),
  
  tabsetPanel(
    tabPanel("Criação do Modelo" ,
             fluidRow(
               column(4,fileInput("arquivo", "Selecione o arquivo:",multiple = FALSE,accept = c(".csv"))),
               column(4,numericInput("Tempo", "Tempo máximo (min.):",value = 1, min = 1)),
               column(4,actionButton("Processar","Processar"))
             ),
             fluidRow(
               column(1, tableOutput("Dados"))
             )
    ),
    tabPanel("Previsão",
             fluidRow(
               column(12,fileInput("arquivo2", "Selecione o arquivo:",multiple = FALSE,accept = c(".csv"))),
               column(6,actionButton("Processar2","Processar"))
             ),
             fluidRow(
               column(6, tableOutput("prev"))
             ) 
             
    )
  )
)

server <- function(input, output) {
  
  #evento de criação do modelo
  observeEvent(input$Processar, {
    
    file1 <- input$arquivo
    
    #Carrega dados
    imp = read.csv(file1$datapath, sep = ";")
    
    #Pega o nome da classe - ultima coluna
    y = colnames(imp[length(imp)])
    
    #Transforma para objeto do h20
    dados = as.h2o(imp)
    
    #Divide em treino e teste
    dados = h2o.splitFrame(data=dados, ratios=0.7)
    treino = dados[[1]]
    teste = dados[[2]]
    
    #Variavel de resposta categorica deve ser fator
    treino[,y] = as.factor(treino[,y])
    teste[,y] = as.factor(teste[,y])
    
    show_modal_spinner()
    #Cria modelo
    modelo <<- h2o.automl(x = colnames(treino[1:(length(imp)-1)]), y = y,training_frame = treino, max_runtime_secs = input$Tempo * 60 )
    remove_modal_spinner()
    
    #Melhores colocados
    lb =as.data.frame(modelo@leaderboard)
    output$Dados <- renderTable({lb})
  })
  #evento para a previsão 		
  observeEvent(input$Processar2, {
    file2 <- input$arquivo2
    
    #Carrega dados para previsão
    imp = read.csv(file2$datapath, sep = ";")
    imp = as.h2o(imp)
    
    #Faz a previsão e exibe o resultado
    previsao <- h2o.predict(modelo@leader, imp)
    previsao = as.data.frame(previsao)
    output$prev <- renderTable({previsao})
    
  })
  
}

shinyApp(ui = ui, server = server)