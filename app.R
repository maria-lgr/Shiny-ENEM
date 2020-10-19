 library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets) 
library(highcharter)
library(ggplot2)
library(ggthemes)
library(plotly)
library(treemap)
library(RColorBrewer)
library(viridisLite)
library(forecast)
library(tidyverse)
library(dplyr)
library(data.table)
library(gridExtra)
library(shinycssloaders)

# função para indicar que os gráficos estão carregando
loader <- function(x) {
    withSpinner(x,type = getOption("spinner.type", default = 6), color = "#FF7C4B")
    
}


ui <- dashboardPage(
    skin = "black",
    
    dashboardHeader(
        
        title="Visualização sobre o ENEM",
        titleWidth = 280
    ),
    dashboardSidebar(title="",
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
                                background-color: #F0F0F0;
                                }
                                /* active selected tab in the sidebarmenu */
                                .skin-black .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #D7D7D7;
                                color: #C34113; 
                                }
                                /* other links in the sidebarmenu */
                                .skin-black .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #F0F0F0;
                                color: #838383; 
                                }
                                /* other links in the sidebarmenu when hovered */
                                .skin-black .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #E4E4E4;
                                color: #C34113
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
                         menuItem("Interpretação dos gráficos",tabName="duvidas",icon=icon("info-circle")),
                         menuItem("Sobre o aplicativo",tabName="sobre_app", icon=icon("book-open")),
                         menuItem("Sobre os dados",tabName="sobre_dados", icon=icon("database")),
                         menuItem("Código-fonte", icon = icon("file-code-o"), href = "https://github.com/maria-lgr/Shiny-ENEM/blob/master/app.R")
                     )
    ),
    
    dashboardBody(title=" ",
                  
                  tags$head(tags$link(rel = "stylesheet",
                                      type = "text/css", href = "style.css")
                  ),
                  tabItems(
                       
                      # DASHBOARD ====
                      tabItem(tabName = "dashboard",
                              fluidPage(theme = shinytheme("united"),
                                        
                                        titlePanel(tags$strong("Dashboard",a(img(src='logo_estatistica.png',align="right",width=240,height=43),
                                                                             href='http://www.est.ufmg.br/portal/'))),
                                        
                                        navbarPage("",
                                                   
                                                   # Análise das notas ####
                                                   tabPanel("Análise das notas",
                                                            tabsetPanel(type = "tabs",
                                                                        
                                                                        tabPanel("Distribuição espacial",
                                                                                 column(8,
                                                                                        sidebarLayout(
                                                                                            sidebarPanel(title = "", width = 4,
                                                                                                         
                                                                                                         # input mapa notas ====
                                                                                                         selectInput('ano_mapa', "Ano", 
                                                                                                                     choices = c(2012,2013,2014,2015,2016,2017,2018), 
                                                                                                                     selected=2018),
                                                                                                         selectInput('prova_mapa', "Prova",
                                                                                                                     choices = c("Ciências da natureza"="cn",
                                                                                                                                 "Ciências humanas"="ch",
                                                                                                                                 "Linguagens e códigos"="pt",
                                                                                                                                 "Matemática"="mat",
                                                                                                                                 "Redação"="red",
                                                                                                                                 "Nota média"="media")),
                                                                                                         selectInput('medida_mapa', label='Medida resumo', 
                                                                                                                     choices = c("média"="media",
                                                                                                                                 "variância"="variancia",
                                                                                                                                 "mediana"="mediana"))
                                                                                            ),
                                                                                            
                                                                                            mainPanel(title = "", width = 8, 
                                                                                                      loader(highchartOutput('mapa'))
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
                                                                                                      
                                                                                                      # input série histórica notas ####
                                                                                                      selectInput('medida_serie', label='Medida resumo', 
                                                                                                                  choices = c("média"="media",
                                                                                                                              "variância"="variancia",
                                                                                                                              "mediana"="mediana")),
                                                                                                      selectInput('estado_serie', label='Estado', 
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
                                                                                                   loader(highchartOutput('lines'))
                                                                                         )
                                                                                     )
                                                                                 )
                                                                        ),
                                                                        
                                                                        tabPanel("Comparação entre estados",
                                                                                 fluidRow( 
                                                                                     sidebarLayout(
                                                                                         sidebarPanel(title = "", width = 3,
                                                                                                      
                                                                                                      # input gráfico polar ####
                                                                                                      selectInput('ano_polar', "Ano", 
                                                                                                                  choices = c(2012,2013,2014,2015,2016,2017,2018), selected=2018),
                                                                                                      
                                                                                                      selectInput('medida_polar', label='Medida resumo', 
                                                                                                                  choices = c("média"="media",
                                                                                                                              "variância"="variancia",
                                                                                                                              "mediana"="mediana")),
                                                                                                      
                                                                                                      selectInput('estado_polar', label='Estado', selected="BR", multiple = T,
                                                                                                                  c("Brasil"="BR","Acre"="AC",
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
                                                                                         ),
                                                                                         
                                                                                         mainPanel(title = "", width = 9, 
                                                                                                   loader(highchartOutput('polar',height = '450px'))
                                                                                         )
                                                                                     )
                                                                                 )
                                                                        ),
                                                                        # input número de acertos nas provas ####
                                                                        tabPanel("Número de acertos nas provas",
                                                                                 fluidRow(
                                                                                     sidebarLayout(
                                                                                         sidebarPanel(width=3,
                                                                                                      selectInput("ano_tri",'Ano',selected=2018,
                                                                                                                  choices = c(2012,2013,2014,2015,2016,2017,2018)),
                                                                                                      
                                                                                                      selectInput("medida_tri",'Medida',selected="media",
                                                                                                                  choices = c("Média"="media",
                                                                                                                              "Mediana"="mediana",
                                                                                                                              "Máxima"="max")),
                                                                                                      
                                                                                                      selectInput('estado_tri', label='Estado (gráfico de barra)', selected="BR",
                                                                                                                  c("Brasil"="BR","Acre"="AC",
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
                                                                                                                    "Sergipe"="SE","Tocantins"="TO")),
                                                                                                      
                                                                                                      
                                                                                                      selectInput('prova_tri', "Prova (mapa)",
                                                                                                                  choices = c("Ciências da natureza"="cn",
                                                                                                                              "Ciências humanas"="ch",
                                                                                                                              "Linguagens e códigos"="lc",
                                                                                                                              "Matemática"="mt"))
                                                                                         ),
                                                                                         mainPanel(title = "", width = 9, 
                                                                                                   fluidRow(
                                                                                                       column(6,
                                                                                                              loader(highchartOutput('tri',height = '400px',width='415px'))
                                                                                                       ),
                                                                                                       column(6,
                                                                                                              loader(highchartOutput('mapa_tri',height = '400px'))
                                                                                                       )
                                                                                                   )
                                                                                         )
                                                                                     )
                                                                                 )
                                                                        )
                                                            )
                                                   ),
                                                   
                                                   # Perfil dos candidatos ####
                                                   tabPanel("Perfil dos candidatos",
                                                            fluidRow(
                                                                tabsetPanel(type = "tabs",
                                                                            
                                                                            tabPanel("Idade e raça",
                                                                                     fluidRow(
                                                                                         fluidPage(
                                                                                             tabBox(width=12,
                                                                                                    
                                                                                                    tabPanel("Idades",
                                                                                                             fluidRow(
                                                                                                                 column(6,
                                                                                                                        box(width=12,
                                                                                                                            fluidRow(
                                                                                                                                
                                                                                                                                # input histograma 1 ####
                                                                                                                                column(6,
                                                                                                                                       selectInput('ano_analise_geral', "Ano", 
                                                                                                                                                   selected=2018,
                                                                                                                                                   choices = c(2012,2013,2014,2015,2016,2017,2018))
                                                                                                                                ),
                                                                                                                                column(6,
                                                                                                                                       selectInput('estado_analise_geral', label='Estado',
                                                                                                                                                   choices = c("Acre"="AC",
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
                                                                                                                                ),
                                                                                                                                column(12,
                                                                                                                                       loader(plotlyOutput('histidades'))
                                                                                                                                )
                                                                                                                                
                                                                                                                            )
                                                                                                                        )
                                                                                                                 ),
                                                                                                                 column(6,
                                                                                                                        box(width=12,
                                                                                                                            fluidRow(
                                                                                                                                # input histograma 2 ####
                                                                                                                                column(6,
                                                                                                                                       selectInput('ano_analise_geral_2', "Ano", 
                                                                                                                                                   selected=2018,
                                                                                                                                                   choices = c(2012,2013,2014,2015,2016,2017,2018))
                                                                                                                                ),
                                                                                                                                column(6,
                                                                                                                                       selectInput('estado_analise_geral_2', label='Estado',
                                                                                                                                                   choices = c("Acre"="AC",
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
                                                                                                                                ),
                                                                                                                                column(12,
                                                                                                                                       loader(plotlyOutput('histidades2'))
                                                                                                                                )
                                                                                                                                
                                                                                                                            )
                                                                                                                        )
                                                                                                                 )
                                                                                                             )
                                                                                                    ),
                                                                                                    
                                                                                                    tabPanel("Raças",
                                                                                                             fluidRow(
                                                                                                                 column(6,
                                                                                                                        box(width=12,
                                                                                                                            fluidRow(
                                                                                                                                # input gráfico de setor 1 ####
                                                                                                                                column(6,
                                                                                                                                       selectInput('ano_analise_geral_raca', "Ano", 
                                                                                                                                                   selected=2018,
                                                                                                                                                   choices = c(2012,2013,2014,2015,2016,2017,2018))
                                                                                                                                ),
                                                                                                                                column(6,
                                                                                                                                       selectInput('estado_analise_geral_raca', label='Estado',
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
                                                                                                                                ),
                                                                                                                                column(12,
                                                                                                                                       loader(highchartOutput('pizzaraca', height="300px"))
                                                                                                                                )
                                                                                                                                
                                                                                                                            )
                                                                                                                        )
                                                                                                                 ),
                                                                                                                 column(6,
                                                                                                                        box(width=12,
                                                                                                                            fluidRow(
                                                                                                                                # input gráfico de setor 2 ####
                                                                                                                                column(6,
                                                                                                                                       selectInput('ano_analise_geral_raca_2', "Ano", 
                                                                                                                                                   selected=2018,
                                                                                                                                                   choices = c(2012,2013,2014,2015,2016,2017,2018))
                                                                                                                                ),
                                                                                                                                column(6,
                                                                                                                                       selectInput('estado_analise_geral_raca_2', label='Estado',
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
                                                                                                                                ),
                                                                                                                                column(12,
                                                                                                                                       loader(highchartOutput('pizzaraca2', height="300px"))
                                                                                                                                )
                                                                                                                                
                                                                                                                            )
                                                                                                                        )
                                                                                                                 )
                                                                                                             )
                                                                                                    ),
                                                                                                    tabPanel("Idades e raças",
                                                                                                             fluidRow(
                                                                                                                 column(6,
                                                                                                                        box(width = NULL,
                                                                                                                            
                                                                                                                            # input gráfico de barras empilhado 1 ####
                                                                                                                            column(6,
                                                                                                                                   selectInput('ano_empilhado', "Ano", selected=2018,
                                                                                                                                               choices = c(2012,2013,2014,2015,2016,2017,2018))
                                                                                                                            ),
                                                                                                                            
                                                                                                                            column(6,
                                                                                                                                   selectInput('estado_empilhado', label='Estado',
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
                                                                                                                            ),
                                                                                                                            
                                                                                                                            column(12,
                                                                                                                                   loader(highchartOutput('barra_porcento')))
                                                                                                                            
                                                                                                                        )
                                                                                                                 ),
                                                                                                                 
                                                                                                                 column(6,
                                                                                                                        box(width = NULL,
                                                                                                                            
                                                                                                                            # input gráfico de barras empilhado 2 ####
                                                                                                                            column(6,
                                                                                                                                   selectInput('ano_empilhado_2', "Ano", selected=2018,
                                                                                                                                               choices = c(2012,2013,2014,2015,2016,2017,2018))
                                                                                                                            ),
                                                                                                                            
                                                                                                                            column(6,
                                                                                                                                   selectInput('estado_empilhado_2', label='Estado',
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
                                                                                                                            ),
                                                                                                                            
                                                                                                                            column(12,
                                                                                                                                   loader(highchartOutput('barra_porcento_2')))
                                                                                                                            
                                                                                                                        )
                                                                                                                 )
                                                                                                                 
                                                                                                             )
                                                                                                    )
                                                                                             )
                                                                                         )
                                                                                     )
                                                                                     
                                                                            ),
                                                                            
                                                                            tabPanel("Variáveis socioeconômicas contra notas",
                                                                                     fluidRow(height=NULL,
                                                                                              sidebarLayout(
                                                                                                  sidebarPanel(title = "", width = 3,
                                                                                                               
                                                                                                               # input boxplot ####
                                                                                                               selectInput('ano_boxplot', "Ano", selected=2018,
                                                                                                                           choices = c(2012,2013,2014,2015,2016,2017,2018)),
                                                                                                               
                                                                                                               
                                                                                                               selectInput('variavel_boxplot', "Variável", 
                                                                                                                           choices = c("Sexo"="TP_SEXO","Raça"="TP_COR_RACA",
                                                                                                                                       "Renda"="renda",
                                                                                                                                       "Escolaridade da mãe ou mulher responsável"="ensino_mae",
                                                                                                                                       "Escolaridade do pai ou homem responsável"="ensino_pai",
                                                                                                                                       "Acesso à internet"="internet", "Ensino Médio"="ensino")
                                                                                                               )
                                                                                                  ),
                                                                                                  
                                                                                                  mainPanel(fluidRow(imageOutput("boxplot_img",height = "550px"))
                                                                                                            )
                                                                                                  
                                                                                              )
                                                                                     )
                                                                            ),
                                                                            
                                                                            tabPanel("Número de inscritos", 
                                                                                     fluidPage(
                                                                                         
                                                                                         tabBox(width = 12,
                                                                                                
                                                                                                tabPanel("Série histórica",
                                                                                                         sidebarLayout(
                                                                                                             sidebarPanel(
                                                                                                                 title = "",  width = 3, solidHeader = TRUE, 
                                                                                                                 
                                                                                                                 # input série histórica inscritos ####
                                                                                                                 selectInput('estado_inscritos', label='Estado', 
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
                                                                                                             ),
                                                                                                             
                                                                                                             mainPanel(title = "", width = 9, solidHeader = T, 
                                                                                                                       loader(highchartOutput('inscritos'))
                                                                                                             )
                                                                                                         )
                                                                                                ),
                                                                                                
                                                                                                # input mapa inscritos #### 
                                                                                                tabPanel("Distribuição espacial",
                                                                                                         sidebarLayout(
                                                                                                             sidebarPanel(
                                                                                                                 title="", width=3, solidHeader=TRUE,
                                                                                                                 
                                                                                                                 selectInput('ano_mapa_inscritos',label="Ano", selected=2018,
                                                                                                                             choices=c(2012,2013,2014,2015,2016,2017,2018)),
                                                                                                                 
                                                                                                                 selectInput('inscritos_mapa',label="Especificação",
                                                                                                                             choices=c("Ausentes (todos os anos)"="ausentes",
                                                                                                                                       "Certificados de Ensino Médio \n(2012 a 2016)"="certificado",
                                                                                                                                       "Treineiros (2014 a 2018)"="treineiro"))#,
                                                                                                                 
                                                                                                             ),
                                                                                                             mainPanel(loader(highchartOutput('mapa_inscritos')))
                                                                                                         )
                                                                                                )
                                                                                         )
                                                                                     )
                                                                            ),
                                                                            
                                                                            tabPanel("Portadores de deficiências e necessidades especiais",
                                                                                     
                                                                                     fluidPage( 
                                                                                         tabBox(width = 12,
                                                                                                tabPanel("Treemap",
                                                                                                         sidebarLayout(
                                                                                                             sidebarPanel(
                                                                                                                 title = "",  width = 3, solidHeader = TRUE,
                                                                                                                 
                                                                                                                 # input treemap ####
                                                                                                                 selectInput('ano_treemap', label="Ano", selected=2018,
                                                                                                                             choices=c(2012,2013,2014,2015,2016,2017,2018)),
                                                                                                                 
                                                                                                                 selectInput('estado_treemap', label='Estado',
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
                                                                                                             ),
                                                                                                             
                                                                                                             mainPanel(title = "", width = 9, solidHeader = T,
                                                                                                                       loader(highchartOutput('treemap'))
                                                                                                             )
                                                                                                         )
                                                                                                ),
                                                                                                tabPanel("Série Histórica",
                                                                                                         sidebarLayout(
                                                                                                             sidebarPanel(
                                                                                                                 title = "",  width = 3, solidHeader = TRUE, 
                                                                                                                 
                                                                                                                 # input série temporal deficiências ####
                                                                                                                 selectInput('estado_deficiencias', label='Estado',
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
                                                                                                             ),
                                                                                                             
                                                                                                             mainPanel(title = "", width = 9, solidHeader = T, 
                                                                                                                       loader(highchartOutput('serie_deficiencias'))
                                                                                                             )
                                                                                                         )
                                                                                                )
                                                                                         )
                                                                                     )
                                                                            )
                                                                )
                                                            )
                                                   )
                                        )#navbarpage
                              )#fluidPage
                      ),#tabItem dashboard
                      
                      # INTERPRETAÇÃO DOS GRÁFICOS ####
                      tabItem(tabName = "duvidas",
                              fluidPage(theme = shinytheme("united"),
                                        setBackgroundColor(color = "white"),
                                        titlePanel(tags$strong("Interpretação dos gráficos",a(img(src='logo_estatistica.png',align="right",width=240,height=43),
                                                                                              href='http://www.est.ufmg.br/portal/'))),
                                        navbarPage("",
                                                   
                                                   tabPanel("Gráficos presentes no dashboard",
                                                            fluidRow(
                                                                tabsetPanel(type = "tabs",
                                                                            
                                                                            # exemplo boxplot ====
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
                                                                                                          strong("Q3"), "ou Terceiro quartil (percentil 75%), o ",strong("Limite inferior"), 
                                                                                                          "e o ",strong("Limite superior"), "encontrado com as fórmulas apresentadas abaixo.
                                                                                          Além disso, o boxplot nos mostra os ",strong("outliers"), "que são observações superiores 
                                                                                          ao limite superior ou inferiores ao limite inferior calculados."),
                                                                                                        withMathJax(),
                                                                                                        helpText('$$ \\text{Limite inferior} = Q1-1.5\\cdot\\left(Q3-Q1\\right) $$ $$\\text{Limite superior} = Q3+1.5\\cdot\\left(Q3-Q1\\right)$$ '),
                                                                                                        br(),
                                                                                                        
                                                                                                        div(img(src="exbp.png", width=550),style="text-align: center;"),
                                                                                                        br(),
                                                                                                        
                                                                                                        p("Na figura acima, podemos ver as posições dos quartis. No exemplo ao lado, podemos 
                                                                                          ver a distribuição das observações para as categorias A, B e C. No boxplot da categoria 
                                                                                          C, o Q1 é 526.3, então sabemos que 25% da amostra é inferior a este valor; o Q3 é 679.1, 
                                                                                          portanto 75% das observações da amostra são inferiores a esse valor. Note que a categoria 
                                                                                          C tem um outlier, pois esta observação foi 1007.2 e é superior ao limite superior de 773.3."),
                                                                                                        style="text-align:justify;color:#404040;background-color:#FAFAD2;padding:20px;border-radius:15px")
                                                                                             ),
                                                                                             
                                                                                             column(5,
                                                                                                    loader(highchartOutput("exboxplot"))
                                                                                             )
                                                                                         )
                                                                                     )
                                                                            ),
                                                                            
                                                                            # exemplo gráfico de barra empilhado ====
                                                                            tabPanel("Gráfico de barra empilhado",
                                                                                     fluidPage(
                                                                                         fluidRow(
                                                                                             column(6,
                                                                                                    p("No ",strong("Gráfico de barras empilhado em 100%")," temos uma junção dos gráficos de setor e histograma, já
                                                                                          que podemos ver as como dois ou mais grupos se comportam dentro de diferentes categorias. As colunas totalizam 100% e podemos observar 
                                                                                            melhor as frequências relativas dos grupos nas diferentes categorias"),
                                                                                                    br(),
                                                                                                    
                                                                                                    p("Ao lado temos um exemplo do gráfico 
                                                                                            de barras empilhado em 100%, em que podemos ver que apesar da categoria 2 totalizar
                                                                                            quantidades menores, as frequências relativas (porcentagens) de seus elementos dos grupos A e B são maiores que para a categoria 1."),
                                                                                                    style="text-align:justify;color:#404040;background-color:#FAFAD2;padding:20px;border-radius:15px"),
                                                                                             
                                                                                             column(6,
                                                                                                    #column(width=6,highchartOutput("ex_barra")),
                                                                                                    loader(highchartOutput("ex_porcento")))
                                                                                         )
                                                                                     )),
                                                                            
                                                                            # exemplo gráfico de barra com dois eixos ====
                                                                            tabPanel("Gráfico de barra com dois eixos",
                                                                                     fluidPage(
                                                                                         fluidRow(
                                                                                             column(6,
                                                                                                    p("No ",strong("Gráfico de barras com dois eixos")," podemos ver o contraste de duas medidas
                                                                                            observadas para uma mesma categoria; na barra é representada a contagem de uma medida e na linha
                                                                                            é apresentada a contagem de outra medida para uma mesma categoria."),
                                                                                                    br(),
                                                                                                    
                                                                                                    p("No exemplo ao lado, ambas medidas estão na mesma escala (0% a 100%), facilitando a comparação
                                                                                            entre as medidas. Vemos que para as quatro categorias, os valores da medida 2 (linha) foram inferiores aos
                                                                                            da medida 1 (barra). Com a razão entre as duas medidas, podemos ver o quanto uma é maior ou menor do que
                                                                                            a outra: quando a razão é maior que 1, a medida no numerador é maior que a no denominador, e quanto
                                                                                            maior a razão, mais discrepante é a diferença entre as medidas. Já quando a razão é menor que 1 
                                                                                            (entre 0 e 1) temos que a medida no numerador é menor que a no denominador, e quanto mais próxima de 0 
                                                                                            é a razão, mais discrepante é a diferença entre as medidas."),
                                                                                                    style="text-align:justify;color:#404040;background-color:#FAFAD2;padding:20px;border-radius:15px"),
                                                                                             
                                                                                             column(6,
                                                                                                    loader(highchartOutput('ex_barra_linha')))
                                                                                         )
                                                                                     )),
                                                                            
                                                                            # exemplo gráfico de linha ====
                                                                            tabPanel("Gráfico de linha",
                                                                                     fluidPage(
                                                                                         fluidRow(
                                                                                             column(6,
                                                                                                    p("O ",strong("Gráfico de linha")," é muito utilizado para a representação de ",strong("séries históricas"),
                                                                                                      ", uma vez que com ele podemos observar os valores assumidos por diferentes 
                                                                                            variáveis ou grupos de interesse ao longo do tempo, que é indicado no eixo x do gráfico."),
                                                                                                    br(),
                                                                                                    
                                                                                                    p("Podemos ver no exemplo que os valores assumidos pelo Grupo 2 foram aumentando com
                                                                                            o passar dos anos e que os valores assumidos pelos 3 grupos foram bem próximos no ano de 2017."),
                                                                                                    style="text-align:justify;color:#404040;background-color:#FAFAD2;padding:20px;border-radius:15px"),
                                                                                             
                                                                                             column(6,loader(highchartOutput("ex_linha")))
                                                                                         )
                                                                                     )),
                                                                            
                                                                            # exemplo gráfico polar ====
                                                                            tabPanel("Gráfico Polar",
                                                                                     fluidPage(
                                                                                         fluidRow(
                                                                                             column(6,
                                                                                                    p("O ",strong("Gráfico polar "), "é usado para comparar os valores que diferentes 
                                                                                          indivíduos ou categorias assumem, com relação a várias variáveis. Em outras palavras, 
                                                                                          ele apresenta de maneira mais visualmente agradável o que um gráfico de barras com 
                                                                                          duas ou mais barras (variáveis) para cada indivíduo representaria."),
                                                                                                    br(), 
                                                                                                    p("No gráfico ao lado, estamos comparando os valores correspondentes à categorias A, 
                                                                                          B, C, D e E nas variáveis var1 e var2."),
                                                                                                    style="text-align:justify;color:#404040;background-color:#FAFAD2;padding:20px;border-radius:15px"),
                                                                                             
                                                                                             column(6,loader(highchartOutput("expolar")))
                                                                                         )
                                                                                     )
                                                                            ),
                                                                            
                                                                            # exemplo gráfico de setor ====
                                                                            tabPanel("Gráfico de setor",
                                                                                     fluidPage(
                                                                                         fluidRow(
                                                                                             column(6,
                                                                                                    p("O ",strong("Gráfico de setor"),"ou ",strong("Gráfico de pizza"),", nos mostra a frequência 
                                                                                     de determinadas categorias ou grupos com relação ao todo, ou seja, nos mostra as proporções da amostra ou população
                                                                                     referentes a cada categoria."),
                                                                                                    br(),
                                                                                                    
                                                                                                    p("No exemplo, os elementos do grupo A, de frequência absoluta 15, representam 50% do total dos elementos."),
                                                                                                    style="text-align:justify;color:#404040;background-color:#FAFAD2;padding:20px;border-radius:15px"),
                                                                                             
                                                                                             column(6,loader(highchartOutput("ex_setor")))
                                                                                         )
                                                                                     )
                                                                            ),
                                                                            
                                                                            # exemplo histograma ====
                                                                            tabPanel("Histograma",
                                                                                     fluidPage(
                                                                                         fluidRow(
                                                                                             column(6, 
                                                                                                    p("O ",strong("Histograma"), "apresenta a densidade dos dados, ou seja, ele 
                                                                                               nos mostra quantos indvíduos estão presentes em cada intervalo. Os intervalos 
                                                                                               são definidos como (X, Y], em que X é exclusivo e Y inclusivo, ou seja, valores 
                                                                                               iguais a X estão no intervalo anterior, de forma que o intervalo (X, Y] 
                                                                                               inclui valores superiores a X e menores ou iguais a Y, pois Y é inclusivo."),
                                                                                                    br(),
                                                                                                    
                                                                                                    p("Por exemplo, neste histograma, temos 5 indivíduos na faixa de 15 a 20 anos; 
                                                                                               como o intervalo é fechado em 15,todos os 5 indivíduos têm idades superiores a 15 
                                                                                               e menores ou iguais a 20."),
                                                                                                    style="text-align:justify;color:#404040;background-color:#FAFAD2;padding:20px;border-radius:15px"),
                                                                                             
                                                                                             column(6,loader(plotlyOutput("exhist")))
                                                                                         )
                                                                                     )
                                                                            ),
                                                                            
                                                                            # exemplo treemap ====
                                                                            tabPanel("Treemap",
                                                                                     fluidPage(
                                                                                         fluidRow(
                                                                                             column(6,
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
                                                                                             
                                                                                             column(6,loader(highchartOutput("extree")))
                                                                                         )
                                                                                     )
                                                                            )
                                                                            
                                                                            
                                                                )
                                                            )
                                                   )
                                        )
                              )
                      ),
                      
                      # SOBRE O APLICATIVO ####
                      tabItem(tabName = "sobre_app",
                              fluidPage(theme = shinytheme("united"),
                                        setBackgroundColor(color = "white"),
                                        titlePanel(tags$strong("Sobre o aplicativo",a(img(src='logo_estatistica.png',align="right",width=240,height=43),
                                                                                      href='http://www.est.ufmg.br/portal/'))),
                                        
                                        fluidRow(
                                            column(9,
                                                   fluidRow(
                                                       p(" "),
                                                       
                                                       br(),
                                                       
                                                       p("Este aplicativo Shiny foi desenvolvido a partir do projeto de Iniciação Científica 
                                                   ",tags$b("“Análise de dados do Enem”"),", cujo principal objetivo era gerar visualizações 
                                                   interessantes sobre os microdados disponibilizados pelo Inep 
                                                   e então disponibilizá-las na web."),
                                                       
                                                       br(),
                                                       
                                                       p("Para o tratamento dos dados e criação do aplicativo, foi utilizado exclusivamente o software 
                                                 estatístico ",tags$b("RStudio"),". Dentre os pacotes utilizados, destacamos o ",tags$code("shiny"),
                                                         " e o ",tags$code("shinydashboard"),", usados para a criação do aplicativo em si, e os pacotes ",
                                                         tags$code("highcharter"), "e ",tags$code("ggplot2")," para geração dos gráficos; essas escolhas 
                                                 foram feitas para criar uma visualização 
                                                 mais fluida e interativa e disponibilizar ao usuário uma gama maior de informações a serem 
                                                 mostradas. Para saber mais sobre quais pacotes foram utilizados no tratamento dos dados e 
                                                 também onde esses dados foram obtidos, dê uma olhada na aba ",tags$em("Sobre os dados"),"."),
                                                       
                                                       br(),
                                                       
                                                       p("Além de disponibilizar apenas as visualizações neste aplicativo, tivemos a iniciativa 
                                                   de incluir uma aba com explicações sobre como interpretar cada gráfico apresentado, 
                                                   visto que alguns formatos de visualização podem não ser familiares para todos."),
                                                       
                                                       br(),
                                                       
                                                       tags$h3("Sobre os autores"),
                                                       
                                                       tags$li("Maria Luisa Gomes dos Reis - Integrante. 
                                                         Estudante do 5º período em Estatística pela Universidade Federal de Minas Gerais. "), 
                                                       tags$li("Cristiano de Carvalho Santos - Orientador. 
                                                         Professor Adjunto do Departamento de Estatística da Universidade Federal de Minas Gerais."), 
                                                       
                                                       style="text-align:justify;color:#404040;font-size:15px")
                                            ),
                                            
                                            column(3,
                                                   div(
                                                       fluidRow(
                                                           
                                                           br(),
                                                           
                                                           br(),
                                                           
                                                           tags$a(img(src='rstudio.png',height=60),
                                                                  href='https://rstudio.com/')),
                                                       fluidRow(
                                                           
                                                           br(),
                                                           
                                                           br(),
                                                           
                                                           tags$a(img(src='shiny.png',align="center",height=130),
                                                                  href='https://shiny.rstudio.com/')),
                                                       fluidRow(
                                                           
                                                           br(),
                                                           
                                                           br(),
                                                           
                                                           tags$a(img(src='highcharts.png',align="center",height=55),
                                                                  href='https://www.highcharts.com/')),
                                                       fluidRow(
                                                           
                                                           br(),
                                                           
                                                           br(),
                                                           
                                                           tags$a(img(src='ggplot2.png',align="centre",height=130),
                                                                  href='https://ggplot2.tidyverse.org/'))
                                                       ,style="text-align: center;")
                                            )
                                        )
                                        
                              )
                      ),
                      
                      # SOBRE OS DADOS ####
                      tabItem(tabName = "sobre_dados",
                              fluidPage(theme = shinytheme("united"),
                                        setBackgroundColor(color = "white"),
                                        titlePanel(tags$strong("Sobre os dados",a(img(src='logo_estatistica.png',align="right",width=240,height=43),
                                                                                  href='http://www.est.ufmg.br/portal/'))),
                                        fluidRow(
                                            column(9,
                                                   fluidRow(
                                                       p(" "),
                                                       
                                                       br(),
                                                       
                                                       p("Os microdados sobre os inscritos no",tags$b("Enem (Exame Nacional de Ensino Médio)"),
                                                         " podem ser obtidos no ",
                                                         tags$a(href="http://inep.gov.br/web/guest/microdados", "site do Inep"),". 
                                               No aplicativo, originalmente iríamos trabalhar com os bancos de dados de 2011 a 2018, 
                                               mas como os dados de 2011 estavam armazenados em um formato particularmente complicado 
                                               de trabalhar, decidimos usar apenas os de 2012 a 2018, que estavam armazenados no formato 
                                               CSV (comma separated values) e apresentavam uma estrutura similar entre si."),
                                                       
                                                       br(),
                                                       
                                                       tags$h3("Tratamento dos dados"),
                                                       
                                                       p("Primeiramente foram pré-selecionadas as variáveis pertinentes à análise para deixar os 
                                             bancos mais leves, uma vez que eles continham de 5 a 8 milhões de observações cada. Ainda assim, 
                                             a maioria dos gráficos só pôde ser gerada com a criação de bancos de dados menores que continham 
                                             apenas as medidas resumo das características de interesse para aquele gráfico. No caso dos boxplots, 
                                             foi preciso trabalhar com os bancos com todas as suas observações para a construção do gráfico; 
                                             como os bancos eram muito pesados, e demoravam a ser carregados, a solução encontrada foi plotar 
                                             esses gráficos fora de aplicativo e salvá-los em formato PNG, para então serem abertos no aplicativo. 
"),
                                                       
                                                       br(),
                                                       
                                                       p("Para montar esses bancos de dados menores acima mencionados, foram usadas funções do pacote do 
                                               R", tags$code("tidyverse")," e, mais especificamente, do pacote ",tags$code("dplyr"),". Além disso,
                                               os bancos foram salvos no formato RDS ao invés de CSV, já que esse aumenta a velocidade de 
                                               leitura dos dados."),
                                                       
                                                       br(),
                                                       
                                                       tags$h3("Observações"),
                                                       
                                                       tags$li("Nos gráficos em que especificamos o estado analisado, consideramos a ",tags$b("Unidade Federativa de residência"), 
                                                               "do candidato. Os bancos apresentavam, além desta informação, as Unidades Federativas
                                                     de nascimento e do local da prova do candidato;"),
                                                       
                                                       br(),
                                                       
                                                       tags$li("A ",tags$b("contabilização de acertos")," dos candidatos nas provas de Ciências da Natureza, 
                                                     Ciências Humanas, Linguagens e Códigos e Matemática se deu pela comparação entre o 
                                                     vetor de respostas e o vetor gabarito de cada uma dessas provas, para cada candidato. 
                                                     No entanto, para o ano de ",tags$b("2012"),", para a prova de Linguagens e Códigos",tags$b("421"),
                                                               " observações apresentaram vetor gabarito 
                                                     vazio, e portanto não foi possível computar o número de acertos para estas observações; estas observações foram, 
                                                     portanto, removidas da análise;"), 
                                                       
                                                       br(),
                                                       
                                                       tags$li("Para a construção do treemap e da série histórica sobre os", tags$b("portadores de deficiências e 
                                                     necessidades especiais"), "distinguimos as características entre deficiência e necessidade 
                                                     especial com base nos conceitos descritos em",
                                                               tags$a(href="http://www.mpgo.mp.br/portalweb/hp/41/docs/diferentes_deficiencias_e_seus_conceitos.pdf ", "aqui"),
                                                               ". Os microdados do Enem em si não apresentavam tal distinção;"),
                                                       
                                                       br(),
                                                       
                                                       tags$li("A variável", tags$i("renda per capta"), " presente nos boxplots não era uma variável original dos bancos: 
                                                     ela foi calculada como sendo o ponto médio das faixas de renda familiar dividido pelo número 
                                                     de pessoas que viviam com o respectivo candidato;"),
                                                       
                                                       br(),
                                                       
                                                       tags$li("As notas denominadas", tags$b(tags$i("Nota média")), "são a média aritmética simples das notas das provas 
                                                     de Ciências da Natureza, Ciências Humanas, Linguagens e Códigos e Matemática e da Redação; 
                                                     vale notar que muitas universidades adotam a média ponderada para calcular a nota final do 
                                                     candidato, mas aqui utilizamos a média simples a título de criar um índice geral para o 
                                                     desempenho do candidato;"),
                                                       
                                                       br(),
                                                       
                                                       tags$li("Em todas as análises consideramos os dados apenas dos",tags$b("candidatos presentes nas quatro provas"),
                                                               "com exceção da série histórica e a distribuição geográfica do número de inscritos, uma vez que foi necessário
                                                     o número de ausentes."),
                                                       
                                                       style="text-align:justify;color:#404040;font-size:15px")
                                            ),
                                            
                                            column(3,
                                                   div(
                                                       fluidRow(
                                                           
                                                           br(),
                                                           
                                                           br(),
                                                           
                                                           tags$a(img(src='Rlogo.png',height=100),
                                                                  href='https://www.r-project.org/')),
                                                       fluidRow(
                                                           
                                                           br(),
                                                           
                                                           br(),
                                                           
                                                           tags$a(img(src='tidyverse.png',height=130),
                                                                  href='https://www.tidyverse.org/')),
                                                       fluidRow(
                                                           
                                                           br(),
                                                           
                                                           br(),
                                                           
                                                           tags$a(img(src='dplyr.png',height=150),
                                                                  href='https://dplyr.tidyverse.org/'))
                                                       ,style="text-align: center;")
                                            )
                                        )
                              )
                      )
                  )
    ) 
)

            


server <- function(input, output) {
    
    # dicionários ####
    dic_provas_titulo <- c("cn"="prova de Ciências da natureza","ch"="prova de Ciências Humanas","pt"="prova de Linguagens e códigos","mat"="prova de Matemática","red"="Redação","media"="nota média (nota total)")
    preposicao_estado <- c("BR"="no Brasil","AC"="no Acre","AL"="em Alagoas","AP"="no Amapá","AM"="no Amazonas","BA"="na Bahia","CE"="no Ceará","DF"="no Distrito Federal","ES"="no Espírito Santo","GO"="em Goiás","MA"="no Maranhão","MT"="em Mato Grosso","MS"="em Mato Grosso do Sul","MG"="em Minas Gerais","PA"="no Pará","PB"="na Paraíba","PR"="no Paraná","PE"="em Pernambuco","PI"="no Piauí","RJ"="no Rio de Janeiro","RN"="no Rio Grande do Norte","RS"="no Rio Grande do Sul","RO"="em Rondônia","RR"="em Roraima","SC"="em Santa Catarina","SP"="em São Paulo","SE"="em Sergipe","TO"="no Tocantins")
    dic_estados <- c("BR"="Brasil","AC"="Acre","AL"="Alagoas","AP"="Amapá","AM"="Amazonas","BA"="Bahia","CE"="Ceará","DF"="Distrito Federal","ES"="Espírito Santo","GO"="Goiás","MA"="Maranhão","MT"="Mato Grosso","MS"="Mato Grosso do Sul","MG"="Minas Gerais","PA"="Pará","PB"="Paraíba","PR"="Paraná","PE"="Pernambuco","PI"="Piauí","RJ"="Rio de Janeiro","RN"="Rio Grande do Norte","RS"="Rio Grande do Sul","RO"="Rondônia","RR"="Roraima","SC"="Santa Catarina","SP"="São Paulo","SE"="Sergipe","TO"="Tocantins")
    dic_medidas <- c("media"="Média","variancia"="Variância","mediana"="Mediana")
    dic_medidas_tri <- c("media"="médias","mediana"="medianas","max"="máximas")
    

    # output mapa notas ####
    output$mapa <- renderHighchart({
        
        dados <- fread(file="mre2.csv")
        dados <- dados[,-1]
        dados_filtrado <- filter(dados,ano==input$ano_mapa,prova==input$prova_mapa)
        
        n <- 4
        stops <- data.frame(q = 0:n/n,
                            c = substring(viridis(n + 1), 0, 7),
                            stringsAsFactors = FALSE)
        stops <- list_parse2(stops)
        
        hcmap("countries/br/br-all", data = dados_filtrado, value = input$medida_mapa,
              joinBy = c("hc-a2", "SG_UF_RESIDENCIA"), name=paste(input$prova_mapa,input$medida_mapa),
              dataLabels = list(enabled = TRUE, format = '{point.code}'),
              tooltip = list(valueDecimals = 3)) %>%
            hc_chart(backgroundColor = "white") %>% 
            hc_title(text = paste(dic_medidas[[input$medida_mapa]],"das notas da",dic_provas_titulo[[input$prova_mapa]],"no Brasil em",input$ano_mapa)) %>%
            hc_colorAxis(stops = stops) %>%
            hc_legend(layout = "vertical", align = "right", valueDecimals = 2)%>%
            hc_mapNavigation(enabled = TRUE) %>%
            hc_tooltip(headerFormat="",
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
    
    # output infoboxes ####
    output$Box1 <- renderInfoBox({
        color <- 'blue'
        dados <- fread(file="med_res_estados.csv")
        dados <- dados[,-1]
        dados_filtrado <- filter(dados,ano==input$ano_mapa,prova==input$prova_mapa,medida=="media",SG_UF_RESIDENCIA=="BR")
        infoBox(value = round(dados_filtrado$valor,3), title = HTML(paste('Média das notas da',br(),dic_provas_titulo[[input$prova_mapa]], br(),'no Brasil em',input$ano_mapa)), color = color)
    })
    
    output$Box2 <- renderValueBox({
        color <- 'red'
        dados <- fread(file="med_res_estados.csv")
        dados <- dados[,-1]
        dados_filtrado <- filter(dados,ano==input$ano_mapa,prova==input$prova_mapa,medida=="variancia",SG_UF_RESIDENCIA=="BR")
        infoBox(value = round(dados_filtrado$valor,1), title = HTML(paste('Variância das notas da',br(),dic_provas_titulo[[input$prova_mapa]], br(),'no Brasil em',input$ano_mapa)), color = color)
    })
    
    output$Box3 <- renderValueBox({
        color <- 'green'
        dados <- fread(file="med_res_estados.csv")
        dados <- dados[,-1]
        dados_filtrado <- filter(dados,ano==input$ano_mapa,prova==input$prova_mapa,medida=="mediana",SG_UF_RESIDENCIA=="BR")
        infoBox(value = round(dados_filtrado$valor,1), title = HTML(paste('Mediana das notas da',br(),dic_provas_titulo[[input$prova_mapa]], br(),'no Brasil em',input$ano_mapa)), color = color)
    })
        
    # output série histórica notas ####
    output$lines <- renderHighchart({
        dados <- fread(file="med_res_estados.csv")
        dados <- dados[,-1]
        dados_filtrado <- dados %>%
            filter(medida==input$medida_serie,SG_UF_RESIDENCIA==input$estado_serie) 
        dados_filtrado <- select(dados_filtrado,ano,prova,valor)
        dados_filtrado$valor <- round(dados_filtrado$valor,3)
        dados_filtrado <- arrange(dados_filtrado,ano)
        highchart() %>% 
            hc_chart(backgroundColor = "white") %>%
            hc_title(text=paste(dic_medidas[[input$medida_serie]],"das notas",preposicao_estado[[input$estado_serie]],"ao longo dos anos")) %>%
            hc_add_series(dados_filtrado, "line", ColorString="#ff0000",name=c("Ciências humanas","Ciências da natureza",
                                                                               "Matemática","Nota média","Linguagens e códigos","Redação"),
                          hcaes(x = ano, y = valor,group=prova))
    })
        
    # output gráfico polar ####
    output$polar <- renderHighchart({
        dados <- fread(file="med_res_estados.csv")
        dados <- dados[,-1]
        dados <- filter(dados,SG_UF_RESIDENCIA %in% c(input$estado_polar))
        dados_filtrado <- filter(dados,ano==input$ano_polar,medida==input$medida_polar)
        dados_filtrado$valor <- round(dados_filtrado$valor,3)
        highchart() %>%
            hc_chart(backgroundColor = "white") %>% 
            hc_add_series(data = dados_filtrado, 
                          mapping = hcaes(y = round(valor,3), 
                                          name = prova,
                                          group = SG_UF_RESIDENCIA),
                          marker = list(symbol = "circle"),
                          type = "line") %>%
            hc_chart(polar = T) %>% 
            hc_title(text=paste("Variação da",dic_medidas[[input$medida_polar]],"entre estados em",input$ano_polar)) %>%
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
    
    
    # output gráfico de barras com dois eixos (número de acertos) ####
    output$tri <- renderHighchart({
        
        dados <- read_rds("dados_tri.rds")
        dados <- dados %>% mutate(prova=as.factor(prova))
        levels(dados$prova) <- plyr::mapvalues(levels(dados$prova), from = levels(dados$prova), to = c("Ciâncias da Natureza",
                                                                                                       "Ciências Humanas",
                                                                                                       "Linguagens e Códigos",
                                                                                                       "Matemática"))
        dados_nota <- dados %>% filter(ano==input$ano_tri,SG_UF_RESIDENCIA==input$estado_tri,tipo=="nota",medida==input$medida_tri) %>% 
            mutate(prop=round(valor/1000*100,1),valor=round(valor,3))
        dados_soma <- dados %>% filter(ano==input$ano_tri,SG_UF_RESIDENCIA==input$estado_tri,tipo=="soma",medida==input$medida_tri) %>% 
            mutate(prop=round(valor/45*100,1),valor=round(valor,1))
        dados_nota$razao <- round(dados_nota$prop/dados_soma$prop,2)
        dados_soma$razao <- round(dados_soma$prop/dados_nota$prop,2)
        
        hc <- highchart() %>% 
            hc_chart(backgroundColor = "white") %>%
            hc_legend(enabled=F) %>% 
            hc_yAxis(min = 0, title = list(text = "Percentual")) %>%
            hc_xAxis(categories = c("Ciências da natureza", "Ciências Humanas", 
                                    "Linguagem e códigos", "Matemática")) %>%
            hc_title(text=paste("Diferença entre as",dic_medidas_tri[[input$medida_tri]]," dos números de acertos e das notas ",preposicao_estado[[input$estado_tri]],"em ",input$ano_tri)) %>%
            hc_add_series(data = dados_nota,mapping = hcaes(y=prop,color=prova),
                          type="column",
                          tooltip = list(headerFormat="",
                                         pointFormat='<b>{point.prova}<br>
                                         Nota: <b>{point.valor}<br>
                                         Percentual: <b>{point.prop}%<br>
                                         Razão nota/acertos: <b>{point.razao}')) %>% 
            hc_add_series(data = dados_soma,mapping = hcaes(y=prop), 
                          type ="line",
                          tooltip = list(headerFormat="",
                                         pointFormat='<b>{point.prova}<br>
                          Número de acertos: <b>{point.valor}<br>
                          Percentual: <b>{point.prop}%<br>
                          Razão acertos/nota: <b>{point.razao}'))
        hc
    })
    
    
    # output mapa número de acertos ####
    output$mapa_tri <- renderHighchart({
        
        dados <- read_rds("dados_tri.rds")
        dados <- dados %>% filter(SG_UF_RESIDENCIA!="BR")
        dados_nota <- dados %>% filter(ano==input$ano_tri,tipo=="nota",medida==input$medida_tri,prova==input$prova_tri) %>% 
            transmute(SG_UF_RESIDENCIA,prova,ano,prop_nota=round(valor/1000*100,1),nota=round(valor,3))
        dados_soma <- dados %>% filter(ano==input$ano_tri,tipo=="soma",medida==input$medida_tri,prova==input$prova_tri) %>% 
            transmute(SG_UF_RESIDENCIA,prova,ano,prop_soma=round(valor/45*100,1),soma=round(valor,1))
        dados_filtrado <- left_join(dados_nota,dados_soma) %>% 
            mutate(razao_nota=round(prop_nota/prop_soma,2),razao_soma=round(prop_soma/prop_nota,2))
        dic_mapa_tri <- c("cn"="Ciências da Natureza","ch"="Ciências Humanas","lc"="Linguagens e Códigos",
                          "mt"="Matemática","media"="Média","mediana"="Mediana","max"="Máximo")
        
        n <- 4
        stops <- data.frame(q = 0:n/n,
                            c = substring(viridis(n + 1), 0, 7),
                            stringsAsFactors = FALSE)
        stops <- list_parse2(stops)
        
        hcmap("countries/br/br-all", data = dados_filtrado, value = "soma",
              joinBy = c("hc-a2", "SG_UF_RESIDENCIA"), 
              dataLabels = list(enabled = TRUE, format = '{point.code}')) %>%
            hc_chart(backgroundColor = "white") %>% hc_colorAxis(stops = stops) %>%
            hc_legend(layout = "vertical", align = "right", valueDecimals = 2)%>%
            hc_title(text = paste(dic_mapa_tri[[input$medida_tri]],"do número de acertos da prova de ",dic_mapa_tri[[input$prova_tri]],"no Brasil em ",input$ano_tri)) %>%
            hc_mapNavigation(enabled = TRUE) %>%
            hc_tooltip(headerFormat="",
                       formatter = JS("function(){
                                        switch(this.point.prova){
                                            case 'cn': y='Ciências da Natureza'
                                                break;
                                            case 'ch': y='Ciências Humanas' 
                                                break;
                                            case 'mt': y='Matemática' 
                                                break;
                                            case 'lc': y='Linguagens e códigos' 
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
                                        return ('<b>' + x + '</b> ('+this.point.SG_UF_RESIDENCIA+')<br>Ano: <b>'+ this.point.ano + '<br>Prova: <b>' + y + '<br>Número de acertos: <b>' +this.point.soma + '<br>Percentual de acertos: <b>'+Highcharts.numberFormat(this.point.prop_soma)+'%'+'<br>Nota: <b>' + this.point.nota)
                                        }")
            )   
    })
    
    
    # output série histórica inscritos ####
    output$inscritos <- renderHighchart({
        
        dados <- read_rds("inscritos.rds")
        dados$contagem[dados$contagem==0] <- NA
        dados_filtrado <- dados %>%
            filter(uf==input$estado_inscritos) 
        dados_filtrado <- select(dados_filtrado,ano, contagem,info)
        dados_filtrado <- arrange(dados_filtrado,ano)
        
        highchart() %>% 
            hc_chart(backgroundColor = "white") %>%
            hc_title(text=paste("Inscritos",preposicao_estado[[input$estado_inscritos]])) %>%
            hc_add_series(dados_filtrado, "line", ColorString="#ff0000",name=c("Candidatos que solicitaram certificado  <br> de Ensino Médio e fizeram as provas",
                                                                               "Candidatos que solicitaram certificado<br> de Ensino Médio",
                                                                               "Candidatos que se inscreveram",
                                                                               "Candidatos que fizeram todas as provas",
                                                                               "Candidatos treineiros que fizeram <br>todas as provas",
                                                                               "Candidatos treineiros"),
                          hcaes(x = ano, y = contagem,group=info))
    })
    
    # output mapa inscritos ####
    output$mapa_inscritos <- renderHighchart({
        
        dados <- read_rds("mapa_inscritos.rds")
        dados_filtrado <- dados %>% filter(ano==input$ano_mapa_inscritos,info==input$inscritos_mapa) %>%
            mutate(porcentagem=round(porcentagem*100,2))
        dic_info <- c("ausentes"="Candidatos ausentes","treineiro"="Candidatos treineiros","certificado"="Solicitações de certificado de ensino médio")
        
        n <- 4
        stops <- data.frame(q = 0:n/n,
                            c = substring(viridis(n + 1), 0, 7),
                            stringsAsFactors = FALSE)
        stops <- list_parse2(stops)
        
        hcmap("countries/br/br-all", data = dados_filtrado, value = "porcentagem",
              joinBy = c("hc-a2", "uf"), 
              dataLabels = list(enabled = TRUE, format = '{point.code}')) %>%
            hc_chart(backgroundColor = "white") %>% 
            hc_title(text = paste(dic_info[[input$inscritos_mapa]],"no Brasil em",input$ano_mapa_inscritos)) %>%
            hc_colorAxis(stops = stops) %>%
            hc_legend(layout = "vertical", align = "right", valueDecimals = 2)%>%
            hc_mapNavigation(enabled = TRUE) %>%
            hc_tooltip(headerFormat="",
                       formatter = JS("function(){
                                        switch(this.point.info){
                                            case 'treineiro': y='Treineiros'
                                                break;
                                            case 'ausentes': y='Ausentes' 
                                                break;
                                            case 'certificado': y='Certificado de ensino médio' 
                                                break;
                                            default: y=''}
                                        switch(this.point.uf){
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
                                        return ('<b>' + x + '</b> ('+this.point.uf+')<br>Ano: <b>'+ this.point.ano + '<br>' + y + ': <b>' + this.point.contagem + '<br>Porcentagem: <b>' +Highcharts.numberFormat(this.point.porcentagem)+'%' + '<br>Total de inscritos: <b>' + this.point.total)
                                        }")
            )   
    })
    
    # leitura dos bancos do histograma 1 ####
    g <- eventReactive(input$ano_analise_geral,{
        
        if (input$ano_analise_geral == 2018) {
            dados <- read_rds("idade2018.rds")
            ano=2018
        }
        if (input$ano_analise_geral == 2017) {
            dados <- read_rds("idade2017.rds")
            ano=2017
        }
        if (input$ano_analise_geral == 2016) {
            dados <- read_rds("idade2016.rds")
            ano=2016
        }
        if (input$ano_analise_geral == 2015) {
            dados <- read_rds("idade2015.rds")
            ano=2015
        }
        if (input$ano_analise_geral == 2014) {
            dados <- read_rds("idade2014.rds")
            ano=2014
        }
        if (input$ano_analise_geral == 2013) {
            dados <- read_rds("idade2013.rds")
            ano=2013
        }
        if (input$ano_analise_geral == 2012) {
            dados <- read_rds("idade2012.rds")
            ano=2012
        }
        lista <- list("dados"=dados,"ano"=ano)
        return(lista)
    })
    
    # leitura dos bancos do histograma 2 ####
    g2 <- eventReactive(input$ano_analise_geral_2,{
        
        if (input$ano_analise_geral_2 == 2018) {
            dados <- read_rds("idade2018.rds")
            ano=2018
        }
        if (input$ano_analise_geral_2 == 2017) {
            dados <- read_rds("idade2017.rds")
            ano=2017
        }
        if (input$ano_analise_geral_2 == 2016) {
            dados <- read_rds("idade2016.rds")
            ano=2016
        }
        if (input$ano_analise_geral_2 == 2015) {
            dados <- read_rds("idade2015.rds")
            ano=2015
        }
        if (input$ano_analise_geral_2 == 2014) {
            dados <- read_rds("idade2014.rds")
            ano=2014
        }
        if (input$ano_analise_geral_2 == 2013) {
            dados <- read_rds("idade2013.rds")
            ano=2013
        }
        if (input$ano_analise_geral_2 == 2012) {
            dados <- read_rds("idade2012.rds")
            ano=2012
        }
        lista <- list("dados"=dados,"ano"=ano)
        return(lista)
    })
    
    
    # legendas do histograma 1 ####
    leg <- eventReactive(list(input$ano_analise_geral,input$estado_analise_geral),{
        
        if (input$ano_analise_geral==2018 || input$ano_analise_geral == 2017 || input$ano_analise_geral == 2014 ||input$ano_analise_geral == 2013 || input$ano_analise_geral == 2012) {
            legenda <- c("Não declarado","Branca","Preta","Parda","Amarela","Indígena")
        }
        if(input$ano_analise_geral==2016){
            if(input$estado_analise_geral=='AC'||input$estado_analise_geral=='AP'||input$estado_analise_geral=='MA'||input$estado_analise_geral=='TO'){
                legenda <- c("Não declarado","Branca","Preta","Parda","Amarela","Indígena")
            } 
            else {
                legenda <- c("Não declarado","Branca","Preta","Parda","Amarela","Indígena","Não dispõe da informação") 
            }
        }
        if (input$ano_analise_geral == 2015) {
            legenda <- c("Não declarado","Branca","Preta","Parda","Amarela","Indígena","Não dispõe da informação")
        }
        return(legenda)
    })
    
    # legendas do histograma 2 ####
    leg2 <- eventReactive(list(input$ano_analise_geral_2,input$estado_analise_geral_2),{
        
        if (input$ano_analise_geral_2==2018 || input$ano_analise_geral_2 == 2017 || input$ano_analise_geral_2 == 2014 ||input$ano_analise_geral_2 == 2013 || input$ano_analise_geral_2 == 2012) {
            legenda2 <- c("Não declarado","Branca","Preta","Parda","Amarela","Indígena")
        }
        if(input$ano_analise_geral_2==2016){
            if(input$estado_analise_geral_2=='AC'||input$estado_analise_geral_2=='AP'||input$estado_analise_geral_2=='MA'||input$estado_analise_geral_2=='TO'){
                legenda2 <- c("Não declarado","Branca","Preta","Parda","Amarela","Indígena")
            } 
            else {
                legenda2 <- c("Não declarado","Branca","Preta","Parda","Amarela","Indígena","Não dispõe da informação") 
            }
        }
        if (input$ano_analise_geral_2 == 2015) {
            legenda2 <- c("Não declarado","Branca","Preta","Parda","Amarela","Indígena","Não dispõe da informação")
        }
        return(legenda2)
    })
    
    # função tema ggplot imitando highcharter ====
    theme_hc <- function(){
        theme(
            text                = element_text(size = 13,colour = "#333333"),
            title               = element_text(hjust=0.5), 
            axis.title.x        = element_text(hjust=.5),
            axis.title.y        = element_text(hjust=.5),
            panel.grid.major.y  = element_line(color='gray', size = .3),
            panel.grid.minor.y  = element_blank(),
            panel.grid.major.x  = element_blank(),
            panel.grid.minor.x  = element_blank(),
            panel.border        = element_blank(),
            panel.background    = element_blank(),
            legend.position     = "bottom",
            legend.title        = element_blank()
        )
    }
    
    # output histograma 1 ####
    output$histidades <- renderPlotly({
        
        lista <- g()
        dados <- lista$dados
        ano <- lista$ano
        ano <- 2018
        dados <- filter(dados,SG_UF_RESIDENCIA==input$estado_analise_geral)
        dados_maximo <- dados %>% filter(NU_IDADE>15 & NU_IDADE<20)
        lim_y <- length(dados_maximo$NU_IDADE)
        label_style <- list(bordercolor = "#3DB88F",font = list(size = 15,color = "#262626"),bgcolor="#EBEBEB")
        dados2 <- dados %>% mutate(faixa=findInterval(NU_IDADE,c(10,16,20,25,35,55)))
        
        p <- dados2 %>% ggplot(aes(NU_IDADE, text=paste("\nContagem: <B>",..count..,
                                                        "\n",round(xmin, 1), " a ", round(xmax,1),"anos"))) +
            geom_histogram(breaks=c(10,16,20,25,35,55,100),
                           col="#3DB88F", #ou #93DCC4
                           fill="#66CDAA") +
            labs(title=paste("Frequência das idades \n em",input$ano_analise_geral,preposicao_estado[[input$estado_analise_geral]]),
                 x="", y="") +
            theme_hc()

        p2 <- p %>% ggplotly(tooltip = "text") %>%
            style(hoverlabel=label_style) %>%
            layout(legend = list(x = 1, y=0.5, orientation = "v"))
    })
    
    # output histograma 2 ====
    output$histidades2 <- renderPlotly({
        lista <- g2()
        dados <- lista$dados
        ano <- lista$ano
        dados <- filter(dados,SG_UF_RESIDENCIA==input$estado_analise_geral_2)
        dados_maximo <- dados %>% filter(NU_IDADE>15 & NU_IDADE<20)
        lim_y <- length(dados_maximo$NU_IDADE)
        label_style <- list(bordercolor = "#3DB88F",font = list(size = 15,color = "#262626"),bgcolor="#EBEBEB")

        p <- dados %>% ggplot(aes(NU_IDADE, text=paste("\nContagem: <B>",..count..,
                                                       "\n",round(xmin, 1), " a ", round(xmax,1),"anos"))) +
            geom_histogram(breaks=c(10,16,20,25,35,55,100),
                           col="#3DB88F", #ou #93DCC4
                           fill="#66CDAA") +
            labs(x="", y="") + 
            ggtitle(paste("Frequência das idades \n em",input$ano_analise_geral_2,
                          preposicao_estado[[input$estado_analise_geral_2]]),) +
            theme_hc()

        p %>% ggplotly(tooltip = "text") %>%
            style(hoverlabel=label_style) %>%
            layout(legend = list(x = 1, y=0.5, orientation = "v"))
    })
    
    # output gráfico de setor 1 ####
    output$pizzaraca <- renderHighchart({
        
        legenda <- leg()
        dados <- fread(file="racas_todos_anos.csv")
        dados <- filter(dados,estado==input$estado_analise_geral_raca,ano==input$ano_analise_geral_raca)
        dados$TP_COR_RACA <- as.factor(dados$TP_COR_RACA)
        levels(dados$TP_COR_RACA) <- plyr::mapvalues(levels(dados$TP_COR_RACA), from = levels(dados$TP_COR_RACA), to = legenda)
        
        paleta <- brewer.pal(7,"Set2")
        
        hchart(dados, "pie", hcaes(x = TP_COR_RACA,y=tot)) %>%
            hc_chart(backgroundColor = "white") %>%
            hc_title(text=paste("Frequência das raças em",input$ano_analise_geral_raca, preposicao_estado[[input$estado_analise_geral_raca]]))%>%
            hc_plotOptions(series=list(showInLegend=T,dataLabels=T)) %>%
            hc_colors(paleta) %>%
            hc_legend(align = "right", verticalAlign = "top",
                      layout = "vertical", x = 0, y = 100) %>%
            hc_tooltip(formatter = JS("function(){
                                return '<b>'+this.point.TP_COR_RACA+ '<br>Frequência: <b>' + this.point.tot +'<br>Porcentagem: <b>'+Highcharts.numberFormat(this.point.por)+'%'
                                 }"),useHTML = FALSE)
        
    })
    
    # output gráfico de setor 2 ====
    output$pizzaraca2 <- renderHighchart({
        
        legenda2 <- leg2()
        dados <- fread(file="racas_todos_anos.csv")
        dados <- filter(dados,estado==input$estado_analise_geral_raca_2,ano==input$ano_analise_geral_raca_2)
        dados$TP_COR_RACA <- as.factor(dados$TP_COR_RACA)
        levels(dados$TP_COR_RACA) <- plyr::mapvalues(levels(dados$TP_COR_RACA), from = levels(dados$TP_COR_RACA), to = legenda2)
        
        paleta <- brewer.pal(7,"Set2")
        
        hchart(dados, "pie", hcaes(x = TP_COR_RACA,y=tot)) %>%
            hc_chart(backgroundColor = "white") %>%
            hc_title(text=paste("Frequência das raças em",input$ano_analise_geral_raca_2, preposicao_estado[[input$estado_analise_geral_raca_2]]))%>%
            hc_plotOptions(series=list(showInLegend=T,dataLabels=F)) %>%
            hc_colors(paleta) %>%
            hc_legend(align = "right", verticalAlign = "top",
                      layout = "vertical", x = 0, y = 100) %>%
            hc_tooltip(formatter = JS("function(){
                                return '<b>'+this.point.TP_COR_RACA+ '<br>Frequência: <b>' + this.point.tot +'<br>Porcentagem: <b>'+Highcharts.numberFormat(this.point.por)+'%'
                                 }"),useHTML = FALSE)
        
    })

    # labels gráficos de barras empilhado
    labels_faixas <- c("5 a 9 anos",
                       "10 a 14 anos","15 a 19 anos","20 a 24 anos","25 a 29 anos",
                       "30 a 34 anos","35 a 39 anos","40 a 44 anos","45 a 49 anos",
                       "50 a 54 anos","55 a 59 anos","60 a 64 anos","65 a 69 anos",
                       "70 a 74 anos","75 a 79 anos","80 a 84 anos","85 a 89 anos",
                       "90 a 94 anos","95 a 100 anos")
    labels_racas <- c("Não declarado","Branca","Preta","Parda",
                      "Amarela","Indígena","Não dispõe da informação")
    
    # output gráfico de barras empilhado 1 ====
    output$barra_porcento <- renderHighchart({
        
        dados <- fread("dados_barra.csv")
        dados$raca <- as.factor(dados$raca)
        dados$faixas <- as.factor(dados$faixas)
        levels(dados$raca) <- plyr::mapvalues(levels(dados$raca), from = levels(dados$raca), 
                                              to = labels_racas)
        levels(dados$faixas) <- plyr::mapvalues(levels(dados$faixas), from = levels(dados$faixas), 
                                                to = labels_faixas)
        dados_filtrado <- dados %>% 
            filter(estado==input$estado_empilhado,ano==input$ano_empilhado) %>%
            group_by(faixas) %>%
            mutate(porcentagem = freq/sum(freq))
        
        paleta <- brewer.pal(7,"Set2")
        
        hc <- dados_filtrado %>% 
            hchart('column', hcaes(x = 'faixas', y = 'freq', group = 'raca'),
                   stacking = "percent") %>% 
            hc_yAxis(title = "") %>% hc_xAxis(labels=list(enabled=F)) %>%
            hc_chart(backgroundColor = "white") %>%
            hc_title(text=paste("Frequência das raças por faixa etária em",input$ano_empilhado, preposicao_estado[[input$estado_empilhado]]))%>%
            hc_plotOptions(series=list(groupPadding=0,pointPadding=0)) %>%
            hc_colors(paleta) %>%
            hc_tooltip(formatter = JS("function(){
                                return '<b>'+this.point.faixas + '<br><b>' + this.point.raca +'<br>Frequência: <b>'+this.point.freq + '<br>Porcentagem: <b>'+Highcharts.numberFormat(this.point.porcentagem)+'%'
                                 }"),useHTML = FALSE)
    })
    
    # output gráfico de barras empilhado 2 ==== 
    output$barra_porcento_2 <- renderHighchart({
        
        dados <- fread("dados_barra.csv")
        dados$raca <- as.factor(dados$raca)
        dados$faixas <- as.factor(dados$faixas)
        levels(dados$raca) <- plyr::mapvalues(levels(dados$raca), from = levels(dados$raca), 
                                              to = labels_racas)
        levels(dados$faixas) <- plyr::mapvalues(levels(dados$faixas), from = levels(dados$faixas), 
                                                to = labels_faixas)
        dados_filtrado <- dados %>% 
            filter(estado==input$estado_empilhado_2,ano==input$ano_empilhado_2) %>%
            group_by(faixas) %>%
            mutate(porcentagem = freq/sum(freq))
        
        paleta <- brewer.pal(7,"Set2")
        
        hc <- dados_filtrado %>% 
            hchart('column', hcaes(x = 'faixas', y = 'freq', group = 'raca'),
                   stacking = "percent") %>% 
            hc_yAxis(title = "") %>% hc_xAxis(labels=list(enabled=F)) %>%
            hc_chart(backgroundColor = "white") %>%
            hc_title(text=paste("Frequência das raças por faixa etária em",input$ano_empilhado_2, preposicao_estado[[input$estado_empilhado_2]]))%>%
            hc_plotOptions(series=list(groupPadding=0,pointPadding=0)) %>%
            hc_colors(paleta) %>%
            hc_tooltip(formatter = JS("function(){
                                return '<b>'+this.point.faixas + '<br><b>' + this.point.raca +'<br>Frequência: <b>'+this.point.freq + '<br>Porcentagem: <b>'+Highcharts.numberFormat(this.point.porcentagem)+'%'
                                 }"),useHTML = FALSE)
    })
    
    # output boxplot (imagens) ====
    output$boxplot_img <- renderImage({
        
        imagem <- paste(paste(input$variavel_boxplot,"BR",input$ano_boxplot,sep="_"),"jpg",sep=".")
        img_dir <- paste("www/",imagem,sep="")
        
        return(list(src = img_dir,filetype = "image/jpg",alt=" "))
        
    }, deleteFile = FALSE)
    
    # leitura do banco para o treemap ====
    dados_treemap <- reactive({
        
        dados <- fread("deficiencias.csv")
        dados <- dados[,-1]
        dados <- filter(dados,SG_UF_RESIDENCIA==input$estado_treemap,ano==input$ano_treemap) %>%
            arrange(deficiencia)
    })
    
    # output treemap ####
    output$treemap <- renderHighchart({
        
        dados <- dados_treemap()
        if(input$ano_treemap==2012 || input$ano_treemap==2013 || input$ano_treemap==2014){
            labels_treemap <- c("Autismo","Baixa visão","Cegueira",
                                "Deficiência auditiva","Deficiência física",
                                "Deficiência mental","Déficit de atenção",
                                "Dislexia","Surdez","Surdez e cegueira")
        } else {
            labels_treemap <- c("Autismo","Baixa visão","Cegueira",
                                "Deficiência auditiva","Deficiência física",
                                "Deficiência mental","Déficit de atenção", "Discalculia",
                                "Dislexia","Surdez","Surdez e cegueira","Visão monocular")
        }
        dados$deficiencia <- as.factor(dados$deficiencia)
        levels(dados$deficiencia) <- plyr::mapvalues(levels(dados$deficiencia), 
                                                     from = levels(dados$deficiencia), 
                                                     to = labels_treemap)
        
        tm <- treemap(dados, index = c("tipo", "deficiencia"),
                      vSize = "contagem")
        
        hctreemap(tm) %>% 
            hc_title(text = paste("Portadores de deficências e necessidades especiais",preposicao_estado[[input$estado_treemap]],"em ",input$ano_treemap)) %>% 
            hc_chart(backgroundColor = "white") %>%
            hc_tooltip(headerFormat="",
                       formatter = JS("function(){
                                        switch(this.point.name){
                                            case 'Surdez': x='Deficiência'
                                                break;
                                            case 'Cegueira': x='Deficiência'
                                                break;
                                            case 'Surdez e cegueira': x='Deficiência'
                                                break;
                                            case 'Déficit de atenção': x='Necessidade especial'
                                                break;
                                            case 'Discalculia': x='Necessidade especial'
                                                break;
                                            case 'Dislexia': x='Necessidade especial'
                                                break;
                                            case 'Baixa visão': x='Deficiência'
                                                break;
                                            case 'Autismo': x='Deficiência'
                                                break;
                                            case 'Visão monocular': x='Deficiência'
                                                break;
                                            case 'Deficiência auditiva': x='Deficiência'
                                                break;
                                            case 'Deficiência física': x='Deficiência'
                                                break;
                                            case 'Deficiência mental': x='Deficiência'
                                                break;
                                            default: y=''}
                                        return ('<b>' + x + '</b> <br>'+ this.point.name + ': <b>' + this.point.value)
                                        }")
            )
    })
    
    # output série histórica deficiências ####
    output$serie_deficiencias <- renderHighchart({
        
        dados <- fread(file="deficiencias.csv")
        dados <- dados[,-1]
        dados$contagem[dados$contagem==0] <- NA
        dados_filtrado <- dados %>%
            filter(SG_UF_RESIDENCIA==input$estado_deficiencias) 
        dados_filtrado <- select(dados_filtrado,ano,contagem,deficiencia)
        
        highchart() %>% 
            hc_chart(backgroundColor = "white") %>%
            hc_title(text=paste("Portadores de deficiência e necessidades especiais",preposicao_estado[[input$estado_deficiencias]])) %>%
            hc_add_series(dados_filtrado, "line", ColorString="#ff0000", name=c("Autismo","Baixa visão","Cegueira",
                                                                                "Deficiência auditiva","Deficiência física",
                                                                                "Deficiência mental","Déficit de atenção","Discalculia",
                                                                                "Dislexia","Surdez","Surdez e cegueira","Visão monocular"),
                          hcaes(x = ano, y = contagem,group=deficiencia))
    })
    
    
    # output exemplo boxplot ####
    output$exboxplot <- renderHighchart({
        
        set.seed(182)
        valor_exemplo <- round(rnorm(20,550,200),1)
        categoria_exemplo <- c("C","C","A","B","B","A","B","B","C","C","C","B","C","B","C","C","C","A","A","A")
        df_exemplo <- data.frame(categoria_exemplo,valor_exemplo)
        
        paleta <- brewer.pal(3,"Set2")
        
        hcboxplot(x = valor_exemplo, var=categoria_exemplo,fillColor = "#AEC4FF", color="#4B7CFF") %>%
            hc_chart(backgroundColor = "white") %>%
            hc_title(text="Exemplo de boxplot") %>%
            hc_tooltip(headerFormat="",
                       pointFormat='Limite superior: <b>{point.high}<br>
                                    Q3: <b>{point.q3}<br>
                                    Mediana: <b>{point.median}<br>
                                    Q1: <b>{point.q1}<br>
                                    Limite inferior: <b>{point.low}<br>',
                       valueDecimals=3)
    })
    
    # output exemplo gráfico de linha ####
    output$ex_linha <- renderHighchart({
        
        dados <- fread(file="med_res_estados.csv")
        dados <- dados[,-1]
        dados_filtrado <- dados %>%
            filter(medida=="media",SG_UF_RESIDENCIA=="BR",ano %in% c(2015:2018),prova %in% c("mat","ch","red")) %>%
            arrange(ano)
        dados_filtrado$valor <- round(dados_filtrado$valor,3)
        
        highchart() %>% 
            hc_chart(backgroundColor = "white") %>%
            hc_title(text="Exemplo de gráfico de linhas") %>%
            hc_add_series(dados_filtrado, "line", ColorString="#ff0000",name=c("Grupo 1","Grupo 2","Grupo 3"),
                          hcaes(x = ano, y = valor,group=prova))
        
    })
    
    # output exemplo gráfico de setor ####
    output$ex_setor <- renderHighchart({
        
        dados <- data.frame(categoria=as.factor(c("A","B","C")),freq_abs=c(15,5,10),freq_re=c(50,17,33))
        paleta <- brewer.pal(7,"Set2")
        levels(dados$categoria) <- plyr::mapvalues(levels(dados$categoria), 
                                                   from = levels(dados$categoria), to = c("Grupo A","Grupo B","Grupo C"))
        hchart(dados, "pie", hcaes(x = categoria,y=freq_abs)) %>%
            hc_chart(backgroundColor = "white") %>%
            hc_title(text="Exemplo de gráfico de setor")%>%
            hc_plotOptions(series=list(showInLegend=T,dataLabels=T)) %>%
            hc_colors(paleta) %>%
            hc_legend(align = "right", verticalAlign = "top",
                      layout = "vertical", x = 0, y = 100) %>%
            hc_tooltip(formatter = JS("function(){
                                return '<b>'+this.point.categoria+ '<br>Frequência: <b>' + this.point.freq_abs +'<br>Porcentagem: <b>'+Highcharts.numberFormat(this.point.freq_re)+'%'
                                 }"),useHTML = FALSE)
    })
    
    
    # output exemplo gráfico de barra com dois eixos ####
    output$ex_barra_linha <- renderHighchart({
        
        dados <- read_rds("dados_tri.rds")
        dados <- dados %>% mutate(prova=as.factor(prova)) 
        levels(dados$prova) <- plyr::mapvalues(levels(dados$prova), from = levels(dados$prova), to = c("A","B","C","D"))
        dados_nota <- dados %>% filter(ano==input$ano_tri,SG_UF_RESIDENCIA==input$estado_tri,tipo=="nota",medida==input$medida_tri) %>% 
            mutate(prop=round(valor/1000*100,1),valor=round(valor,3))
        dados_soma <- dados %>% filter(ano==input$ano_tri,SG_UF_RESIDENCIA==input$estado_tri,tipo=="soma",medida==input$medida_tri) %>% 
            mutate(prop=round(valor/45*100,1),valor=round(valor,1))
        dados_nota$razao <- round(dados_nota$prop/dados_soma$prop,2)
        dados_soma$razao <- round(dados_soma$prop/dados_nota$prop,2)
        
        hc <- highchart() %>% 
            hc_chart(backgroundColor = "white") %>%
            hc_legend(enabled=F) %>% 
            hc_yAxis(min = 0, title = list(text = "Percentual")) %>%
            hc_xAxis(categories = c("A", "B","c","D")) %>%
            hc_title(text="Exemplo de gráfico de barra com dois eixos") %>%
            hc_add_series(data = dados_nota,mapping = hcaes(y=prop,color=prova),
                          type="column",
                          tooltip = list(headerFormat="",
                                         pointFormat='<b>Categoria {point.prova}<br>
                                         Valor da medida 1: <b>{point.prop}%<br>
                                         Razão medida 1/medida 2: <b>{point.razao}')) %>% 
            hc_add_series(data = dados_soma,mapping = hcaes(y=prop), 
                          type ="line",
                          tooltip = list(headerFormat="",
                                         pointFormat='<b>Categoria {point.prova}<br>
                                         Valor da medida 2: <b>{point.prop}%<br>
                                         Razão medida 2/medida 1: <b>{point.razao}'))
        hc
    })
    
    # output exemplo gráfico de barra empilhado 100% ====
    output$ex_porcento <- renderHighchart({
        
        dados <- data.frame(categoria=as.factor(c("A","B","C","A","B","C")),grupo=as.factor(c(1,1,1,2,2,2)),
                            freq_abs=c(15,8,11,12,7,4),porcentagem=c(0.44,0.24,0.32,0.52,0.30,0.17))
        
        hc <- dados %>% 
            hchart('column', hcaes(x = 'grupo', y = 'freq_abs', group = 'categoria'),
                   stacking = "percent") %>% #ou percent
            hc_yAxis(title = "") %>% hc_xAxis(title = "") %>%
            hc_chart(backgroundColor = "white") %>%
            hc_title(text="Exemplo de gráfico de barras empilhado em 100%") %>%
            hc_colors(brewer.pal(3,"Set2")) %>%
            hc_tooltip(formatter = JS("function(){
                                return '<b>Categoria '+this.point.grupo+ '<br>Grupo ' + this.point.categoria +': <b>'+this.point.freq_abs+'<br> Porcentagem: <b>'+Highcharts.numberFormat(this.point.porcentagem)+'%'
                                 }"),useHTML = FALSE)
    })
    
    # output exemplo histograma ####
    output$exhist <- renderPlotly({
        
        dados <- data.frame(x=c(9,10,11,16,17,17,18,19,21,22,25,26))
        label_style <- list(bordercolor = "#3DB88F",font = list(size = 15,color = "#262626"),bgcolor="#EBEBEB")
        
        p <- dados %>% ggplot(aes(x, text=paste("\nContagem: <B>",..count..,
                                                "\n",round(xmin, 1), " a ", round(xmax,1),"anos"))) + 
            geom_histogram(breaks=c(10,15,20,25,35),col="#3DB88F",fill="#66CDAA") +
            labs(title="Exemplo de histograma",x="", y="") + 
            theme_hc()
        
        p2 <- p %>% ggplotly(tooltip = "text") %>%
            style(hoverlabel=label_style) %>%
            layout(legend = list(x = 1, y=0.5, orientation = "v"))
        
    })
    
    # output exemplo gráfico polar
    output$expolar <- renderHighchart({
        
        set.seed(27)
        valor_exemplo <- round(rnorm(10,550,100),1)
        categoria_exemplo <- c("A","A","B","B","C","C","D","D","E","E")
        grupo <- c("var1","var2","var1","var2","var1","var2","var2","var1","var1","var2")
        df <- data.frame(categoria_exemplo,grupo,valor_exemplo)
        
        highchart() %>%
            hc_chart(backgroundColor = "white") %>% 
            hc_add_series(data = df, mapping = hcaes(y = valor_exemplo,name = categoria_exemplo,group = grupo),
                          marker = list(symbol = "circle"),type = "line") %>%
            hc_chart(polar = T) %>% 
            hc_title(text="Exemplo de gráfico polar") %>%
            hc_xAxis(categories = c("A", "B", "C", "D", "E")) %>%
            hc_tooltip(backgroundColor = grey(0.95),
                       borderRadius = 0,
                       borderWidth = 2)
    })
    
    # output exemplo treemap ====
    output$extree <- renderHighchart({
        
        valor_exemplo <- c(5, 8, 2, 14, 5, 13, 11)
        categoria_exemplo <- c("A","A","B","A","C","C","B")
        subcategoria_exemplo <- c('1','2','3','4','5','6','7')
        df <- data.frame(subcategoria_exemplo,categoria_exemplo,valor_exemplo)
        
        paleta <- brewer.pal(7,"Set2")
        
        tm <- treemap(df, index = c("categoria_exemplo", "subcategoria_exemplo"),
                      vSize = "valor_exemplo", vColor = "categoria_exemplo", palette = paleta)
        
        hctreemap(tm) %>% 
            hc_title(text="Exemplo de treemap")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
