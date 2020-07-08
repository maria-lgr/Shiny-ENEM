library(shinythemes)
library(shiny)
library(shinydashboard)
library(shinyWidgets) ## RODAR help(shinyWidgets)
library(highcharter)
library(dplyr)
library(viridisLite)
library(forecast)
library(treemap)
library(tidyverse)
library(data.table)
library(ggplot2)
library(sf)
library(RColorBrewer)
library(magrittr)
library(gridExtra)


ui <- dashboardPage(
    skin = "black",
    
    dashboardHeader(
        
        title="Visualização sobre o ENEM",
        titleWidth = 280
        ),
    dashboardSidebar(
        tags$head(tags$style(HTML('
                                /* logo */
                                .skin-black .main-header .logo {
                                background-color: #E95420;
                                color: #FFFFFF;
                                border: #C34113;
                                }
                                /* logo when hovered */
                                .skin-black .main-header .logo:hover {
                                background-color: #E95420;
                                color: #FFFFFF;
                                }
                                /* navbar (rest of the header) */
                                .skin-black .main-header .navbar {
                                background-color: #E95420;
                                }
                                /* main sidebar */
                                .skin-black .main-sidebar {
                                background-color: #FFDEAC;
                                }
                                /* active selected tab in the sidebarmenu */
                                .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #FA702B;
                                color: #FFFFFF; 
                                }
                                /* other links in the sidebarmenu */
                                .skin-black .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #FFDEAC;
                                color: #C34113; 
                                }
                                /* other links in the sidebarmenu when hovered */
                                .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #E95420;
                                color: #FFFFFF
                                }
                                /* toggle button  */
                                .skin-black .main-header .navbar .sidebar-toggle{
                                background-color: #E95420;
                                color: #FFFFFF;
                                border: #C34113;
                                }
                                /* toggle button when hovered  */
                                .skin-black .main-header .navbar .sidebar-toggle:hover{
                                background-color: #C34113;
                                color: #FFFFFF;
                                border: #C34113;
                                }
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #FCFCFC;
                                }
                                ')
                             )
                  ),
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-bar")),
            menuItem("Dúvidas?",tabName="duvidas",icon=icon("question")),
            menuItem("Fontes",tabName="fontes", icon=icon("book"))
            )
        ),
    
    dashboardBody(
        
        tags$head(tags$link(rel = "stylesheet",
                            type = "text/css", href = "style.css")
                  ),
        tabItems(
            
            tabItem(tabName = "dashboard",
                    fluidPage(theme = shinytheme("united"),
                              
                              titlePanel(tags$strong("Dashboard",a(img(src='logoestatistica.png',align="right",width=240,height=43),
                                                                    href='http://www.est.ufmg.br/portal/'))),
                              
                              navbarPage("",
                                         
                                         tabPanel("Análise das notas",
                                                  tabsetPanel(type = "tabs",
                                                              
                                                              tabPanel("Mapa de calor",
                                                                       column(8,
                                                                              sidebarLayout(
                                                                                  sidebarPanel(title = "", width = 4,
                                                                                               selectInput('ano', "Ano", 
                                                                                                           choices = c(2012,2013,2014,2015,2016,2017,2018), 
                                                                                                           selected=2018),
                                                                                               selectInput('coisa', "Prova",
                                                                                                           choices = c("Ciências da natureza"="cn",
                                                                                                                       "Ciências humanas"="ch",
                                                                                                                       "Linguagens e códigos"="pt",
                                                                                                                       "Matemática"="mat",
                                                                                                                       "Redação"="red",
                                                                                                                       "Nota média"="media")),
                                                                                               selectInput('med', label='Medida resumo', 
                                                                                                           choices = c("média"="media",
                                                                                                                       "variância"="variancia",
                                                                                                                       "mediana"="mediana"))
                                                                                               ),
                                                                                  
                                                                                  mainPanel(title = "", width = 8, 
                                                                                            highchartOutput('mapa')
                                                                                            )
                                                                                  )
                                                                              ),
                                                                       column(3,
                                                                              infoBoxOutput("Box1", width = NULL),
                                                                              infoBoxOutput("Box2", width = NULL),
                                                                              infoBoxOutput("Box3", width = NULL)
                                                                              )
                                                                       ),
                                                              
                                                              tabPanel("Série histórica",
                                                                       fluidRow(  
                                                                           sidebarLayout(
                                                                               sidebarPanel(title = "",  width = 3, 
                                                                                            solidHeader = TRUE, status = "warning",
                                                                                            selectInput('medida', label='Medida resumo', 
                                                                                                        choices = c("média"="media",
                                                                                                                    "variância"="variancia",
                                                                                                                    "mediana"="mediana")),
                                                                                            selectInput('estado', label='Estado', 
                                                                                                        choices = c("Brasil"="BR","Acre"="AC",
                                                                                                                    "Alagoas"="AL","Amapá"="AP",
                                                                                                                    "Amazonas"="AM","Bahia"="BA",
                                                                                                                    "Ceará"="CE","Distrito Federal"="DF",
                                                                                                                    "Espírito Santo"="ES","Goiás"="GO",
                                                                                                                    "Maranhão"="MA","Mato Grosso"="MT",
                                                                                                                    "Mato Grosso do Sul"="MS","Minas Gerais"="MG",
                                                                                                                    "Pará"="PA","Paraíba"="PB",
                                                                                                                    "Paraná"="PR","Pernambuco"="PE",
                                                                                                                    "Piauí"="PI","Rio de Janeiro"="RJ",
                                                                                                                    "Rio Grande do Norte"="RN","Rio Grande do Sul"="RS",
                                                                                                                    "Rondônia"="RO","Roraima"="RR",
                                                                                                                    "Santa Catarina"="SC","São Paulo"="SP",
                                                                                                                    "Sergipe"="SE","Tocantins"="TO"
                                                                                                                    )
                                                                                                        )
                                                                                            ),
                                                                               mainPanel(title = "", width = 9, 
                                                                                         solidHeader = T, status = "warning",
                                                                                         highchartOutput('lines')
                                                                                         )
                                                                               )
                                                                           )
                                                                       ),
                                                              
                                                              tabPanel("Gráfico polar",
                                                                       fluidRow( 
                                                                           sidebarLayout(
                                                                               sidebarPanel(title = "", width = 3,
                                                                                            selectInput('anopolar', "Ano", 
                                                                                                        choices = c(2012,2013,2014,2015,2016,2017,2018), selected=2018),
                                                                               
                                                                                            selectInput('medpolar', label='Medida resumo', 
                                                                                                        choices = c("média"="media",
                                                                                                                    "variância"="variancia",
                                                                                                                    "mediana"="mediana")),
                                                                               
                                                                                            selectInput('estadopolar', label='Estado', selected="BR", multiple = T,
                                                                                                        c("Brasil"="BR","Acre"="AC",
                                                                                                          "Alagoas"="AL","Amapá"="AP",
                                                                                                          "Amazonas"="AM", "Bahia"="BA",
                                                                                                          "Ceará"="CE","Distrito Federal"="DF",            
                                                                                                          "Espírito Santo"="ES", "Goiás"="GO",
                                                                                                          "Maranhão"="MA","Mato Grosso"="MT",
                                                                                                          "Mato Grosso do Sul"="MS","Minas Gerais"="MG",
                                                                                                          "Pará"="PA","Paraíba"="PB",
                                                                                                          "Paraná"="PR", "Pernambuco"="PE",
                                                                                                          "Piauí"="PI","Rio de Janeiro"="RJ",
                                                                                                          "Rio Grande do Norte"="RN","Rio Grande do Sul"="RS",
                                                                                                          "Rondônia"="RO","Roraima"="RR",
                                                                                                          "Santa Catarina"="SC","São Paulo"="SP",
                                                                                                          "Sergipe"="SE","Tocantins"="TO")
                                                                                                        )
                                                                                            ),
                                                                               
                                                                               mainPanel(title = "", width = 9, 
                                                                                         highchartOutput('polar',height = '450px')
                                                                                         )
                                                                               )
                                                                           )
                                                                       )
                                                              )
                                                  ),
                                         
                                         tabPanel("Perfil dos candidatos",
                                                  fluidRow(
                                                      tabsetPanel(type = "tabs",
                                                                  
                                                                  tabPanel("Análise geral",
                                                                           fluidRow(
                                                                               column(2,
                                                                                      box(status="primary",width = NULL,
                                                                                          selectInput('anopf', "Ano", selected=2018,
                                                                                                      choices = c(2012,2013,2014,2015,2016,2017,2018)),
                                                                                          
                                                                                          selectInput('estadopf', label='Estado', 
                                                                                                      choices = c("Brasil"="BR","Acre"="AC",
                                                                                                                  "Alagoas"="AL","Amapá"="AP",
                                                                                                                  "Amazonas"="AM","Bahia"="BA",
                                                                                                                  "Ceará"="CE","Distrito Federal"="DF",
                                                                                                                  "Espírito Santo"="ES","Goiás"="GO",
                                                                                                                  "Maranhão"="MA","Mato Grosso"="MT",
                                                                                                                  "Mato Grosso do Sul"="MS","Minas Gerais"="MG",
                                                                                                                  "Pará"="PA","Paraíba"="PB",
                                                                                                                  "Paraná"="PR","Pernambuco"="PE",
                                                                                                                  "Piauí"="PI","Rio de Janeiro"="RJ",
                                                                                                                  "Rio Grande do Norte"="RN","Rio Grande do Sul"="RS",
                                                                                                                  "Rondônia"="RO","Roraima"="RR",
                                                                                                                  "Santa Catarina"="SC","São Paulo"="SP",
                                                                                                                  "Sergipe"="SE","Tocantins"="TO")
                                                                                                      )
                                                                                          )
                                                                                      ),
                                                                               column(10,
                                                                                      box(width = NULL,status="primary",
                                                                                          fluidRow(
                                                                                               column(6,align="left",
                                                                                                      highchartOutput('histidades',height="250px")),
                                                                                               column(6,align="left",
                                                                                                      highchartOutput('pizzaraca',height="250px"))
                                                                                               )
                                                                                          )
                                                                                      )
                                                                               ),
                                                                           fluidRow(
                                                                               column(2,
                                                                                      box(width = NULL,height = 2, 
                                                                                          selectInput('anopf2', "Ano", selected=2018,
                                                                                                      choices = c(2012,2013,2014,2015,2016,2017,2018)),
                                                                                          
                                                                                          selectInput('estadopf2', label='Estado', 
                                                                                                      choices = c("Brasil"="BR","Acre"="AC",
                                                                                                                  "Alagoas"="AL","Amapá"="AP",
                                                                                                                  "Amazonas"="AM","Bahia"="BA",
                                                                                                                  "Ceará"="CE","Distrito Federal"="DF",
                                                                                                                  "Espírito Santo"="ES","Goiás"="GO",
                                                                                                                  "Maranhão"="MA","Mato Grosso"="MT",
                                                                                                                  "Mato Grosso do Sul"="MS","Minas Gerais"="MG",
                                                                                                                  "Pará"="PA","Paraíba"="PB",
                                                                                                                  "Paraná"="PR","Pernambuco"="PE",
                                                                                                                  "Piauí"="PI","Rio de Janeiro"="RJ",
                                                                                                                  "Rio Grande do Norte"="RN","Rio Grande do Sul"="RS",
                                                                                                                  "Rondônia"="RO","Roraima"="RR",
                                                                                                                  "Santa Catarina"="SC","São Paulo"="SP",
                                                                                                                  "Sergipe"="SE","Tocantins"="TO")
                                                                                                      )
                                                                                          )
                                                                                      ),
                                                                               column(10,
                                                                                      box(width = NULL,height=2,
                                                                                          fluidRow(
                                                                                              column(6,align="left",
                                                                                                     highchartOutput('histidades2',height="250px")),
                                                                                              column(6,align="left",
                                                                                                     highchartOutput('pizzaraca2',height="250px"))
                                                                                              )
                                                                                          )
                                                                                      )
                                                                               )
                                                                           ),
                                                                  
                                                                  tabPanel("Boxplots",
                                                                           fluidRow(
                                                                               sidebarLayout(
                                                                                   sidebarPanel(title = "", width = 3,
                                                                                                selectInput('anobp', "Ano", selected=2018,
                                                                                                            choices = c(2012,2013,2014,2015,2016,2017,2018)),
                                                                                                
                                                                                                selectInput('estadobp', label='Estado', 
                                                                                                            choices = c("Brasil"="BR","Acre"="AC",
                                                                                                                        "Alagoas"="AL","Amapá"="AP",
                                                                                                                        "Amazonas"="AM","Bahia"="BA",
                                                                                                                        "Ceará"="CE","Distrito Federal"="DF",
                                                                                                                        "Espírito Santo"="ES","Goiás"="GO",
                                                                                                                        "Maranhão"="MA","Mato Grosso"="MT",
                                                                                                                        "Mato Grosso do Sul"="MS","Minas Gerais"="MG",
                                                                                                                        "Pará"="PA","Paraíba"="PB",
                                                                                                                        "Paraná"="PR","Pernambuco"="PE",
                                                                                                                        "Piauí"="PI","Rio de Janeiro"="RJ",
                                                                                                                        "Rio Grande do Norte"="RN","Rio Grande do Sul"="RS",
                                                                                                                        "Rondônia"="RO","Roraima"="RR",
                                                                                                                        "Santa Catarina"="SC","São Paulo"="SP",
                                                                                                                        "Sergipe"="SE","Tocantins"="TO")
                                                                                                            ),
                                                                                                
                                                                                                selectInput('varbp', "Variável", 
                                                                                                            choices = c("Sexo","Idade","Raça","Renda")
                                                                                                            )
                                                                                                ),
                                                                                   
                                                                                   mainPanel(htmlOutput("showfile")
                                                                                             )
                                                                                   
                                                                                   )
                                                                               ) #fluidrow de fora
                                                                           ),
                                                                  
                                                                  tabPanel("Inscritos", 
                                                                           fluidRow( 
                                                                               sidebarLayout(
                                                                                   sidebarPanel(
                                                                                       title = "",  width = 3, solidHeader = TRUE, 
                                                                                       selectInput('estadoae', label='Estado', 
                                                                                                   choices = c("Brasil"="BR","Acre"="AC",
                                                                                                               "Alagoas"="AL","Amapá"="AP",
                                                                                                               "Amazonas"="AM","Bahia"="BA",
                                                                                                               "Ceará"="CE","Distrito Federal"="DF",
                                                                                                               "Espírito Santo"="ES","Goiás"="GO",
                                                                                                               "Maranhão"="MA","Mato Grosso"="MT",
                                                                                                               "Mato Grosso do Sul"="MS","Minas Gerais"="MG",
                                                                                                               "Pará"="PA", "Paraíba"="PB",
                                                                                                               "Paraná"="PR","Pernambuco"="PE",
                                                                                                               "Piauí"="PI", "Rio de Janeiro"="RJ",
                                                                                                               "Rio Grande do Norte"="RN", "Rio Grande do Sul"="RS",
                                                                                                               "Rondônia"="RO","Roraima"="RR",
                                                                                                               "Santa Catarina"="SC","São Paulo"="SP",
                                                                                                               "Sergipe"="SE","Tocantins"="TO")
                                                                                                   )
                                                                                       ),
                                                                                   
                                                                                   mainPanel(title = "", width = 9, solidHeader = T, 
                                                                                             highchartOutput('inscritos')
                                                                                             )
                                                                                   )
                                                                               )
                                                                           )
                                                                  )
                                                      ),
                                                  ),
                                         
                                         tabPanel("Portadores de deficiência",
                                                  fluidRow(tabsetPanel(type = "tabs",
                                                                       
                                                                       tabPanel("Treemap"),
                                                                       
                                                                       tabPanel("Série histórica", "Conteúdo 2")
                                                                       )
                                                           ) 
                                                  )
                                         )#navbarpage
                              )#fluidPage
                    ),#tabItem dashboard
            
            tabItem(tabName = "duvidas",
                    fluidPage(theme = shinytheme("united"),
                              setBackgroundColor(color = "white"),
                              titlePanel(tags$strong("Dúvidas?")),
                              navbarPage("",
                                         
                                         tabPanel("Como interpretar os gráficos",
                                                  fluidRow(
                                                      tabsetPanel(type = "tabs",
                                                                  
                                                                  tabPanel("Boxplot",
                                                                           fluidPage(
                                                                               fluidRow(
                                                                                   column(7,
                                                                                          fluidRow(
                                                                                          p("O ",strong("Boxplot "),"apresenta a distribuição dos dados de uma amostra. Para 
                                                                                          construí-lo, os valores observados devem ser ordenados em ordem crescente, pois 
                                                                                          assim podemos saber quais valores estão em certas posições: a ",strong("Mediana"), 
                                                                                          "também chamada Q2 ou Segundo quartil, que é o valor na posição 50% (percentil 50%) 
                                                                                          desses valores ordenados, o ",strong("Q1"), "ou primeiro quartil (percentil 25%), o ",
                                                                                          strong("Q3"), "ou Terceiro quartil (percentil 75%), o ",strong("Mínimo"), "encontrado 
                                                                                          com a fórmula da equação 1 e o ",strong("Máximo"), "encontrado com a fórmula da equação 
                                                                                          2. Além disso, o boxplot nos mostra os ",strong("outliers"), "que são observações que 
                                                                                          foram superiores ao máximo ou inferiores ao mínimo calculados."),
                                                                                          withMathJax(),
                                                                                          helpText('eq. 1 $$Q1-1.5\\cdot\\left(Q3-Q1\\right)$$ e eq. 2 $$Q3+1.5\\cdot\\left(Q3-Q1\\right)$$ '),
                                                                                          br(),
                                                                                          
                                                                                          img(src="exbp.png", width=550),
                                                                                          br(),
                                                                                          
                                                                                          p("Na figura acima, podemos ver as posições dos quartis. No exemplo ao lado, podemos 
                                                                                          ver a distribuição das observações para as categorias A, B e C. No boxplot da categoria 
                                                                                          C, o Q1 é 526.3, então sabemos que 25% da amostra é inferior a este valor; o Q3 é 679.1, 
                                                                                          portanto 75% das observações da amostra são inferiores a esse valor. Note que a categoria 
                                                                                          C tem um outlier, pois esta observação foi 1007.2 e é superior ao Máximo de 773.3."),
                                                                                          style="text-align:justify;color:#404040;background-color:#FAFAD2;padding:20px;border-radius:15px")
                                                                                          ),
                                                                                   
                                                                                   column(5,
                                                                                          highchartOutput("exboxplot")
                                                                                          )
                                                                                   )
                                                                               )
                                                                           ),
                                                                  
                                                                  tabPanel("Gráfico Polar",
                                                                           fluidPage(
                                                                               fluidRow(
                                                                                   column(6,
                                                                                          br(),
                                                                                          p("O ",strong("Gráfico polar "), "é usado para comparar os valores que diferentes 
                                                                                          indivíduos ou categorias assumem, com relação a várias variáveis. Em outras palavras, 
                                                                                          ele apresenta de maneira mais visualmente agrável o que um gráfico de barras com 
                                                                                          duas ou mais barras (variáveis) para cada indivíduo representaria."),
                                                                                          br(), 
                                                                                          p("No gráfico ao lado, estamos comparando os valores correspondentes às categorias A, 
                                                                                          B, C, D e E nas variáveis var1 e var2."),
                                                                                          style="text-align:justify;color:#404040;background-color:#FAFAD2;padding:20px;border-radius:15px"),
                                                                                   
                                                                                   column(6,highchartOutput("expolar"))
                                                                                   )
                                                                               )
                                                                           ),
                                                                  
                                                                  tabPanel("Histograma",
                                                                           fluidPage(
                                                                               fluidRow(
                                                                                   column(6, 
                                                                                        
                                                                                               br(),
                                                                                               p("O ",strong("Histograma"), "apresenta a densidade dos dados, ou seja, ele 
                                                                                               nos mostra quantos indvíduos estão presentes em cada intervalo. O intervalo 
                                                                                               é indicado por (X, Y], em que X é exclusivo e Y inclusivo, ou seja, valores 
                                                                                               iguais a X estão no intervalo anterior, de forma que o intervalo (X, Y] 
                                                                                               inclui valores superiores a X e menores ou iguais a Y, pois Y é inclusivo."),
                                                                                               br(),
                                                                                               
                                                                                               p("Por exemplo, neste histograma, temos 4 indivíduos na faixa de (15, 16]; 
                                                                                               como o intervalo é fechado em 16,todos os 4 indivíduos são superiores a 15 
                                                                                               e menores ou iguais a 16, ou seja, todos os indvíduos dessa faixa tem valor 
                                                                                               16."),
                                                                                               style="text-align:justify;color:#404040;background-color:#FAFAD2;padding:20px;border-radius:15px"),
                                                                                        
                                                                                        column(6,highchartOutput("exhist"))
                                                                                        )
                                                                                    )
                                                                                ),
                                                                  
                                                                  tabPanel("Treemap",
                                                                           fluidPage(
                                                                               fluidRow(
                                                                                   column(6,
                                                                                          br(),
                                                                                          p("O ",strong("Treemap"),"é utilizado para visualizarmos os valores observados
                                                                                          em diferentes elementos que fazem parte de um todo maior. Os tamanhos dos 
                                                                                          quadriláteros são proporcionais aos valores que eles representam, o que torna 
                                                                                          este modelo de gráfico ideal para comparar as diferenças dos valores 
                                                                                          observados em diferentes grupos, que podem ser distinguidos entre si pelas 
                                                                                          cores que assumem."),
                                                                                          br(),
                                                                                          
                                                                                          p("No Treemap ao lado, vemos que os valores dos elementos presentes no grupo A
                                                                                          superam os dos grupos B e C. Também podemos ver claramente as diferenças dentro 
                                                                                          dos grupos, como no grupo B em o elemento 7 supera o 3."),
                                                                                          style="text-align:justify;color:#404040;background-color:#FAFAD2;padding:20px;border-radius:15px"),
                                                                                   
                                                                                   column(6,highchartOutput("extree"))
                                                                                   )
                                                                               )
                                                                           )
                                                                  )
                                                      )
                                                  )
                                         )
                              )#fluidpage duvidas
                    ),#tabitem duvidas
            
            tabItem(tabName = "fontes",
                    fluidPage(theme = shinytheme("united"),
                              setBackgroundColor(color = "white"),
                              titlePanel(tags$strong("Fontes"))
                              )
                    )#tabitem fontes
            ) #tabItems
        ) #dashboardbody
    ) #dashboardpage
            
            

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    provas <- c("cn"="prova de Ciências da natureza","ch"="prova de Ciências Humanas","pt"="prova de Linguagens e códigos","mat"="prova de Matemática","red"="Redação","media"="nota média (nota total)")
    provas2 <- c("cn"="Ciências da natureza","ch"="Ciências Humanas","pt"="Linguagens e códigos","mat"="Matemática","red"="Redação","media"="Nota média (nota total)")
    prepest <- c("BR"="no Brasil","AC"="no Acre","AL"="em Alagoas","AP"="no Amapá","AM"="no Amazonas","BA"="na Bahia","CE"="no Ceará","DF"="no Distrito Federal","ES"="no Espírito Santo","GO"="em Goiás","MA"="no Maranhão","MT"="em Mato Grosso","MS"="em Mato Grosso do Sul","MG"="em Minas Gerais","PA"="no Pará","PB"="na Paraíba","PR"="no Paraná","PE"="em Pernambuco","PI"="no Piauí","RJ"="no Rio de Janeiro","RN"="no Rio Grande do Norte","RS"="no Rio Grande do Sul","RO"="em Rondônia","RR"="em Roraima","SC"="em Santa Catarina","SP"="em São Paulo","SE"="em Sergipe","TO"="no Tocantins")
    est <- c("BR"="Brasil","AC"="Acre","AL"="Alagoas","AP"="Amapá","AM"="Amazonas","BA"="Bahia","CE"="Ceará","DF"="Distrito Federal","ES"="Espírito Santo","GO"="Goiás","MA"="Maranhão","MT"="Mato Grosso","MS"="Mato Grosso do Sul","MG"="Minas Gerais","PA"="Pará","PB"="Paraíba","PR"="Paraná","PE"="Pernambuco","PI"="Piauí","RJ"="Rio de Janeiro","RN"="Rio Grande do Norte","RS"="Rio Grande do Sul","RO"="Rondônia","RR"="Roraima","SC"="Santa Catarina","SP"="São Paulo","SE"="Sergipe","TO"="Tocantins")
    medidas <- c("media"="Média","variancia"="Variância","mediana"="Mediana")
    
    output$mapa <- renderHighchart({
        mre <- fread(file="mre2.csv")
        mre <- mre[,-1]
        filtrado <- filter(mre,ano==input$ano,prova==input$coisa)
        n <- 4
        stops <- data.frame(q = 0:n/n,
                            c = substring(viridis(n + 1), 0, 7),
                            stringsAsFactors = FALSE)
        stops <- list_parse2(stops)
        hcmap("countries/br/br-all", data = filtrado, value = input$med,
              joinBy = c("hc-a2", "SG_UF_RESIDENCIA"), name=paste(input$coisa,input$med),
              dataLabels = list(enabled = TRUE, format = '{point.code}'),
              tooltip = list(valueDecimals = 3)) %>%
            hc_chart(backgroundColor = "white") %>% 
            hc_title(text = paste(medidas[[input$med]],"das notas da",provas[[input$coisa]],"no Brasil em",input$ano)) %>%
            hc_colorAxis(stops = stops) %>%
            hc_legend(layout = "vertical", align = "right", valueDecimals = 2)%>%
            hc_mapNavigation(enabled = TRUE) %>%
            hc_tooltip(headerFormat="",
                      # pointFormat='<b>Estado: </b> {point.hc-a2} <br>
                       #             <b>Ano: </b> {point.ano}<br>
                        #            <b>Prova:</b>  {point.prova} <br>    
                         #           <b>Média: </b> {point.media:.3f} <br>
                          #          <b>Variância: </b> {point.variancia:.1f}<br>
                           #         <b>Mediana: </b> {point.mediana:.1f}'  # ,
                       formatter = JS("function(){
                                        switch(this.point.prova){
                                            case 'cn': y='Ciências da Natureza'
                                                break;
                                            case 'ch': y='Ciências Humanas' 
                                                break;
                                            case 'mat': y='Matemática' 
                                                break;
                                            case 'pt': y='Linguagens e códigos' 
                                                break;
                                            case 'red': y='Redação' 
                                                break;
                                            case 'media': y='Nota média'
                                                break;
                                            default: y=''}
                                        switch(this.point.SG_UF_RESIDENCIA){
                                            case 'AC': x='Acre'
                                                break;
                                            case 'AL': x='Alagoas' 
                                                break;
                                            case 'AP': x='Amapá' 
                                                break;
                                            case 'AM': x='Amazonas' 
                                                break;
                                            case 'BA': x='Bahia' 
                                                break;
                                            case 'CE': x='Ceará'
                                                break;
                                            case 'DF': x='Distrito Federal' 
                                                break;
                                            case 'ES': x='Espírito Santo' 
                                                break;
                                            case 'GO': x='Goiás' 
                                                break;
                                            case 'MA': x='Maranhão' 
                                                break;
                                            case 'MT': x='Mato Grosso'
                                                break;
                                            case 'MS': x='Mato Grosso do Sul' 
                                                break;
                                            case 'MG': x='Minas Gerais' 
                                                break;
                                            case 'PA': x='Pará' 
                                                break;
                                            case 'PB': x='Paraíba' 
                                                break;
                                            case 'PR': x='Paraná'
                                                break;
                                            case 'PE': x='Pernambuco' 
                                                break;
                                            case 'PI': x='Piauí' 
                                                break;
                                            case 'RJ': x='Rio de Janeiro' 
                                                break;
                                            case 'RN': x='Rio Grande do Norte' 
                                                break;
                                            case 'RS': x='Rio Grande do Sul'
                                                break;
                                            case 'RO': x='Rondônia' 
                                                break;
                                            case 'RR': x='Roraima' 
                                                break;
                                            case 'SC': x='Santa Catarina' 
                                                break;
                                            case 'SP': x='São Paulo' 
                                                break;
                                            case 'SE': x='Sergipe'
                                                break;
                                            case 'TO': x='Tocantins'
                                                break;
                                            default: x=''}
                                        return ('<b>' + x + '</b> ('+this.point.SG_UF_RESIDENCIA+')<br>Ano: <b>'+ this.point.ano + '<br>Prova: <b>' + y + '<br>Média: <b>' + this.point.media + '<br>Variância: <b>' + this.point.variancia + '<br>Mediana: <b>' + this.point.mediana)
                                        }")
                       )   
    })
    
    output$Box1 <- renderInfoBox({
        color <- 'blue'
        mre <- fread(file="med_res_estados.csv")
        mre <- mre[,-1]
        filtrado <- filter(mre,ano==input$ano,prova==input$coisa,medida=="media",SG_UF_RESIDENCIA=="BR")
        infoBox(value = round(filtrado$valor,3), title = HTML(paste('Média das notas da',br(),provas[[input$coisa]], br(),'no Brasil em',input$ano)), color = color)
    })
    
    output$Box2 <- renderValueBox({
        color <- 'red'
        mre <- fread(file="med_res_estados.csv")
        mre <- mre[,-1]
        filtrado <- filter(mre,ano==input$ano,prova==input$coisa,medida=="variancia",SG_UF_RESIDENCIA=="BR")
        infoBox(value = round(filtrado$valor,1), title = HTML(paste('Variância das notas da',br(),provas[[input$coisa]], br(),'no Brasil em',input$ano)), color = color)
    })
    
    output$Box3 <- renderValueBox({
        color <- 'green'
        mre <- fread(file="med_res_estados.csv")
        mre <- mre[,-1]
        filtrado <- filter(mre,ano==input$ano,prova==input$coisa,medida=="mediana",SG_UF_RESIDENCIA=="BR")
        infoBox(value = round(filtrado$valor,1), title = HTML(paste('Mediana das notas da',br(),provas[[input$coisa]], br(),'no Brasil em',input$ano)), color = color)
    })
        
        output$lines <- renderHighchart({
            mre <- fread(file="med_res_estados.csv")
            mre <- mre[,-1]
            filtr <- mre %>%
                filter(medida==input$medida,SG_UF_RESIDENCIA==input$estado) 
            filtr <- select(filtr,ano,prova,valor)
            filtr$valor <- round(filtr$valor,3)
            filtr <- arrange(filtr,ano)
            highchart() %>% 
                hc_chart(backgroundColor = "white") %>%
                hc_title(text=paste(medidas[[input$medida]],prepest[[input$estado]],"ao longo dos anos")) %>%
                hc_add_series(filtr, "line", ColorString="#ff0000",name=c("Ciências humanas","Ciências da natureza","Matemática","Nota média","Linguagens e códigos","Redação"),hcaes(x = ano, y = valor,group=prova))
        })
        
    output$polar <- renderHighchart({
        mre <- fread(file="med_res_estados.csv")
        mre <- mre[,-1]
        mre <- filter(mre,SG_UF_RESIDENCIA %in% c(input$estadopolar))
        filtrado <- filter(mre,ano==input$anopolar,medida==input$medpolar)
        filtrado$valor <- round(filtrado$valor,3)
        highchart() %>%
            hc_chart(backgroundColor = "white") %>% 
            hc_add_series(data = filtrado, 
                          mapping = hcaes(y = round(valor,3), 
                                          name = prova,
                                          group = SG_UF_RESIDENCIA),
                          marker = list(symbol = "circle"),
                          type = "line") %>%
            hc_chart(polar = T) %>% 
            hc_title(text=paste("Variação da",medidas[[input$medpolar]],"entre estados em",input$anopolar)) %>%
            hc_xAxis(categories = c("Ciências da natureza", "Ciências Humanas", 
                                    "Linguagem e códigos", "Matemática", 
                                    "Redação", "Média")) %>%
            hc_tooltip(backgroundColor = grey(0.95),
                       borderRadius = 0,
                       borderWidth = 2,
                       headerFormat="",
                       formatter = JS("function(){
                                        switch(this.point.prova){
                                            case 'cn': x='Ciências da Natureza'
                                                break;
                                            case 'ch': x='Ciências Humanas' 
                                                break;
                                            case 'mat': x='Matemática' 
                                                break;
                                            case 'pt': x='Linguagens e códigos' 
                                                break;
                                            case 'red': x='Redação' 
                                                break;
                                            case 'media': x='Nota média'}
                                        return ('Prova: <b>' + x + '<br>Valor: <b>' + this.point.valor)
                                        }"))
    })
    
    output$inscritos <- renderHighchart({
        ins <- fread(file="inscritos2.csv")
        ins <- ins[,-1]
        ins$contagem[ins$contagem==0] <- NA
        filtr <- ins %>%
            filter(uf==input$estadoae) 
        filtr <- select(filtr,ano, contagem,info)
        filtr <- arrange(filtr,ano)
        highchart() %>% 
            hc_chart(backgroundColor = "white") %>%
            hc_title(text=paste("Inscritos",prepest[[input$estadoae]])) %>%
            hc_add_series(filtr, "line", ColorString="#ff0000",name=c("Candidatos que solicitaram certificado de Ensino Médio e fizeram as provas","Candidatos que solicitaram certificado de Ensino Médio","Candidatos que se inscreveram","Candidatos que fizeram todas as provas","Candidatos treineiros que fizeram todas as provas","Candidatos treineiros"),hcaes(x = ano, y = contagem,group=info))
    })
    
    g <- eventReactive(input$anopf,{
        if (input$anopf == 2018) {
            dados <- fread(file="idaderaca2018.csv")
            ano=2018
        }
        if (input$anopf == 2017) {
            dados <- fread(file="idaderaca2017.csv")
            ano=2017
        }
        if (input$anopf == 2016) {
            dados <- fread(file="idaderaca2016.csv")
            ano=2016
        }
        if (input$anopf == 2015) {
            dados <- fread(file="idaderaca2015.csv")
            ano=2015
        }
        if (input$anopf == 2014) {
            dados <- fread(file="idaderaca2014.csv")
            ano=2014
        }
        if (input$anopf == 2013) {
            dados <- fread(file="idaderaca2013.csv")
            ano=2013
        }
        if (input$anopf == 2012) {
            dados <- fread(file="idaderaca2012.csv")
            ano=2012
        }
        lista <- list("dados"=dados,"ano"=ano)
        return(lista)
    })
    
    
    leg <- eventReactive(list(input$anopf,input$estadopf),{
        if (input$anopf==2018 || input$anopf == 2017 || input$anopf == 2014 ||input$anopf == 2013 || input$anopf == 2012) {
            legenda <- c("Não declarado","Branca","Preta","Parda","Amarela","Indígena")
        }
        if(input$anopf==2016){
            if(input$estadopf=='AC'||input$estadopf=='AP'||input$estadopf=='MA'||input$estadopf=='TO'){
                legenda <- c("Não declarado","Branca","Preta","Parda","Amarela","Indígena")
            } 
            else {
                legenda <- c("Não declarado","Branca","Preta","Parda","Amarela","Indígena","Não dispõe da informação") 
            }
        }
        if (input$anopf == 2015) {
            legenda <- c("Não declarado","Branca","Preta","Parda","Amarela","Indígena","Não dispõe da informação")
        }
        return(legenda)
    })
    
    leg2 <- eventReactive(list(input$anopf2,input$estadopf2),{
        if (input$anopf2==2018 || input$anopf2 == 2017 || input$anopf2 == 2014 ||input$anopf2 == 2013 || input$anopf2 == 2012) {
            legenda2 <- c("Não declarado","Branca","Preta","Parda","Amarela","Indígena")
        }
        if(input$anopf2==2016){
            if(input$estadopf2=='AC'||input$estadopf2=='AP'||input$estadopf2=='MA'||input$estadopf2=='TO'){
                legenda2 <- c("Não declarado","Branca","Preta","Parda","Amarela","Indígena")
            } 
            else {
                legenda2 <- c("Não declarado","Branca","Preta","Parda","Amarela","Indígena","Não dispõe da informação") 
            }
        }
        if (input$anopf2 == 2015) {
            legenda2 <- c("Não declarado","Branca","Preta","Parda","Amarela","Indígena","Não dispõe da informação")
        }
        return(legenda2)
    })
    
    output$histidades <- renderHighchart({
        lista <- g()
        dados <- lista$dados
        ano <- lista$ano
        if(input$estadopf!="BR"){
            dados <- filter(dados,SG_UF_RESIDENCIA==input$estadopf)
        }
        h2 <- hist(dados$NU_IDADE, plot = FALSE)
        hchart(h2) %>%
            hc_chart(backgroundColor = "white") %>%
            hc_title(text=paste("Frequência das idades em",input$anopf,prepest[[input$estadopf]])) %>%
            hc_colors("#66CDAA") %>%
            hc_legend(enabled=F) %>% 
            hc_xAxis(max = 90)
    })
    
    output$histidades2 <- renderHighchart({
        lista <- g()
        dados <- lista$dados
        ano <- lista$ano
        if(input$estadopf2!="BR"){
            dados <- filter(dados,SG_UF_RESIDENCIA==input$estadopf2)
        }
        h2 <- hist(dados$NU_IDADE, plot = FALSE)
        hchart(h2) %>%
            hc_chart(backgroundColor = "white") %>%
            hc_title(text=paste("Frequência das idades em",input$anopf2,prepest[[input$estadopf2]])) %>%
            hc_colors("#66CDAA") %>%
            hc_legend(enabled=F)
    })
    
    output$pizzaraca <- renderHighchart({
        lista <- g()
        legenda <- leg()
        dados <- fread(file="racas_todos_anos.csv")
        dados <- filter(dados,estado==input$estadopf,ano==input$anopf)
        dados$TP_COR_RACA <- as.factor(dados$TP_COR_RACA)
        levels(dados$TP_COR_RACA) <- plyr::mapvalues(levels(dados$TP_COR_RACA), from = levels(dados$TP_COR_RACA), to = legenda)
        paleta <- brewer.pal(7,"Set2")
        hchart(dados, "pie", hcaes(x = TP_COR_RACA,y=tot)) %>%
            hc_chart(backgroundColor = "white") %>%
            hc_title(text=paste("Frequência das raças em",input$anopf, prepest[[input$estadopf]]))%>%
            hc_plotOptions(series=list(showInLegend=T,dataLabels=T)) %>%
            hc_colors(paleta) %>%
            hc_legend(align = "right", verticalAlign = "top",
                      layout = "vertical", x = 0, y = 100) %>%
            hc_tooltip(formatter = JS("function(){
                                return '<b>'+this.point.TP_COR_RACA+ '<br>Frequência: <b>' + this.point.tot +'<br>Porcentagem: <b>'+Highcharts.numberFormat(this.point.por)+'%'
                                 }"),useHTML = FALSE)
        
    })
    
    output$pizzaraca2 <- renderHighchart({
        lista <- g()
        legenda2 <- leg2()
        dados <- fread(file="racas_todos_anos.csv")
        dados <- filter(dados,estado==input$estadopf2,ano==input$anopf2)
        dados$TP_COR_RACA <- as.factor(dados$TP_COR_RACA)
        levels(dados$TP_COR_RACA) <- plyr::mapvalues(levels(dados$TP_COR_RACA), from = levels(dados$TP_COR_RACA), to = legenda2)
        paleta <- brewer.pal(7,"Set2")
        hchart(dados, "pie", hcaes(x = TP_COR_RACA,y=tot)) %>%
            hc_chart(backgroundColor = "white") %>%
            hc_title(text=paste("Frequência das raças em",input$anopf2, prepest[[input$estadopf2]]))%>%
            hc_plotOptions(series=list(showInLegend=T,dataLabels=F)) %>%
            hc_colors(paleta) %>%
            hc_legend(align = "right", verticalAlign = "top",
                      layout = "vertical", x = 0, y = 100) %>%
            hc_tooltip(formatter = JS("function(){
                                return '<b>'+this.point.TP_COR_RACA+ '<br>Frequência: <b>' + this.point.tot +'<br>Porcentagem: <b>'+Highcharts.numberFormat(this.point.por)+'%'
                                 }"),useHTML = FALSE)
        
    })
    
    output$exboxplot <- renderHighchart({
        set.seed(182)
        valor <- round(rnorm(20,550,200),1)
        categoria <- c("C","C","A","B","B","A","B","B","C","C","C","B","C","B","C","C","C","A","A","A")
        df <- data.frame(categoria,valor)
        paleta <- brewer.pal(3,"Set2")
        hcboxplot(x = valor, var=categoria,fillColor = "#AEC4FF", color="#4B7CFF") %>%
            hc_chart(backgroundColor = "white") %>%
            hc_title(text="Boxplot") %>%
            hc_tooltip(headerFormat="",
                       pointFormat='Máximo: <b>{point.high}<br>
                                    Q3: <b>{point.q3}<br>
                                    Mediana: <b>{point.median}<br>
                                    Q1: <b>{point.q1}<br>
                                    Mínimo: <b>{point.low}<br>',
                       valueDecimals=3)
    })
    
    
    
    output$exhist <- renderHighchart({
        set.seed(20)
        dados <- rbinom(100,25,0.8)
        h2 <- hist(dados)
        hchart(h2) %>%
            hc_chart(backgroundColor = "white") %>%
            hc_title(text="Histograma") %>%
            hc_colors("#66CDAA")
    })
    
    output$expolar <- renderHighchart({
        set.seed(27)
        valor <- round(rnorm(10,550,100),1)
        categoria <- c("A","A","B","B","C","C","D","D","E","E")
        grupo <- c("var1","var2","var1","var2","var1","var2","var2","var1","var1","var2")
        df <- data.frame(categoria,grupo,valor)
        highchart() %>%
            hc_chart(backgroundColor = "white") %>% 
            hc_add_series(data = df, mapping = hcaes(y = valor,name = categoria,group = grupo),
                          marker = list(symbol = "circle"),type = "line") %>%
            hc_chart(polar = T) %>% 
            hc_xAxis(categories = c("A", "B", "C", "D", "E")) %>%
            hc_tooltip(backgroundColor = grey(0.95),
                       borderRadius = 0,
                       borderWidth = 2)
    })
    
    output$extree <- renderHighchart({
        valor <- c(5, 8, 2, 14, 5, 13, 11)
        categoria <- c("A","A","B","A","C","C","B")
        sub <- c('1','2','3','4','5','6','7')
        df <- data.frame(sub,categoria,valor)
        paleta <- brewer.pal(7,"Set2")
        tm <- treemap(df, index = c("categoria", "sub"),
                      vSize = "valor", vColor = "categoria", palette = paleta)
        hctreemap(tm)
    })
    
    getPage<-function() {
        return(HTML(readLines("teste2.html")))
    }
    output$teste<-renderUI({getPage()})
    
    server <- function(input, output) {
        output$showfile <- renderUI(tags$a("single_link", href="teste2.html", target="_blank"))
    }
}

# Run the application 
shinyApp(ui = ui, server = server)