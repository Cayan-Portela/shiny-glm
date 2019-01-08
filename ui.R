dashboardPage(skin = ("blue"),
              
              dashboardHeader(title = "ESALQ \\ USP",
                              
                              tags$li(class = "dropdown",
                                      tags$a(href = "mailto:cayanportela@hotmail.com ",
                                             tags$img(height = "18px", 
                                                      src = "email.png")
                                      )
                              )),
              
              dashboardSidebar(
                sidebarMenu(
                  menuItem( "Introdu\u00E7\u00E3o" , tabName = "intro"         , icon = icon("home")),
                  menuItem( "Leitura de Dados"      , tabName = "dados"         , icon = icon("table")),
                  menuItem( "An\u00E1lise Descritiva" , tabName = "aba_descritiva", icon = icon("area-chart")),
                  menuItem( "Ajuste de Modelos"    , tabName = "analise"       , icon = icon("binoculars")),
                  menuItem( "An\u00E1lise de Diagnósticos"   , tabName = "residuos"      , icon = icon('recycle')),
                  menuItem( "Livro"      , tabName = "livro"         , icon = icon('file-pdf-o')),
                  hr(),
                  
                  sidebarUserPanel(name = a("Cayan Atreio Portela", target = "_blank_",
                                            href = "http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4301098J2"),
                                   subtitle = "Estatístico",
                                   image = "teste2.jpg"),
                  sidebarUserPanel(name = a("Cristian M. V. Lobos", target = "_blank_",
                                            href = "http://buscatextual.cnpq.br/buscatextual/visualizacv.do?id=K4245185U2"),
                                   subtitle = "Professor Dr.",
                                   image = "cristian.jpg")
                )
              ),
              dashboardBody(
                includeCSS("www/custom.css"),
                tags$head(tags$style(HTML('.box{-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;}'))),
                tabItems(
                  
                  tabItem(tabName = "intro",includeHTML("intro.html")),
                  tabItem(tabName = "dados", 
                          fluidRow(
                            box(width = 6, solidHeader = TRUE,
                                fluidRow(
                                  column(8,
                                selectInput('dados_pre' , label = "Dados pre disponiveis" ,
                                            choices = c(Salário = 'salario2',
                                                        Rotenone = 'rotenone',
                                                        Store = 'store') ,
                                            width = '50%')),
                                column(4,
                                       br(), 
                                actionButton(inputId = 'carregar_dados_pre',
                                             label = 'Carregar'))),
                                br(),
                                fileInput(inputId = "file" , label = "Entre com seus próprios dados"),
                                fluidRow(
                                  column(4,
                                         checkboxInput(inputId = 'header', label = 'Cabe\u00E7alho', value = FALSE),
                                         checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE)),
                                  column(4 ,
                                         selectInput(inputId = 'dec' , label = "Decimal" , choices = c("Ponto" = "." ,
                                                                                                       "Vírgula"   =  ","))       )
                                ),
                                selectInput(inputId = 'sep', label = 'Separador', 
                                            choices = c("Automatico" = "auto" , Virgula = ',', 
                                                        "Ponto e Virgula" = ';',
                                                        Tab = '\t', Espaco= " ")),
                                textInput(inputId = 'nova_var' , label = 'Criar nova variavel' , value = "")
                            ),
                            box(width = 6, solidHeader = TRUE,
                                verbatimTextOutput("strR") %>% withSpinner() )
                          ),
                          fluidRow(
                            box(width = 12, solidHeader = TRUE,
                                DT::dataTableOutput("table") %>% withSpinner() )
                          )
                  ),
                  tabItem(tabName = "aba_descritiva",
                          fluidRow(
                            box(solidHeader = TRUE,
                                fluidRow(
                                  column(width = 3 , uiOutput("descritiva_eixox1")),
                                  column(width = 3 , uiOutput("descritiva_eixoy1")),
                                  column(width = 3 , uiOutput("descritiva_grupo1")) 
                                ),
                                br(),
                                plotlyOutput("grafico_dispersao") %>% withSpinner()),
                            box(solidHeader = TRUE,
                                fluidRow(
                                  column(width = 6 , uiOutput("var_summary"))
                                  #column(width = 3 , uiOutput("descritiva_eixox2")),
                                  #column(width = 3 , uiOutput("descritiva_eixoy2")),
                                  #column(width = 3 , uiOutput("descritiva_grupo2"))
                                ),
                                br(),
                                tableOutput("summary_"))
                          ),
                          fluidRow(
                            box(solidHeader = TRUE,
                                fluidRow(
                                  column(width = 3 , uiOutput("descritiva_eixox3")),
                                  column(width = 3 , uiOutput("descritiva_eixoy3")),
                                  column(width = 3 , uiOutput("descritiva_grupo3")), 
                                  column(width = 3 , selectInput("tipo_de_grafico" , "Tipo Gr\u00E1fico",
                                                                 choices = c("Dispersão" = "scatter",
                                                                              "Barras" =   "bar",
                                                                              "Boxplot" =  "box")) )
                                ),
                                br(),
                                plotlyOutput("grafico_plotly")
                            ),
                            box(solidHeader = TRUE,
                                fluidRow(
                                  column(width = 4 , uiOutput("descritiva_histo")),
                                  column(width = 5 , sliderInput("numero_bins", label = "N\u00FAmero de intervalos",
                                                                 min = 0 , max = 20 , value = 10))),
                                br(),
                                plotlyOutput("grafico_histograma")
                            )
                          )),
                  tabItem(tabName = "analise",
                          fluidRow(
                            box(width = 12 , solidHeader = TRUE,
                                column(4,
                                       uiOutput("dependente"),
                                       uiOutput("independentes")),
                                #actionButton("add", "Add UI")),
                                column(4,
                                       selectInput("familia","Distribui\u00E7\u00E3o",
                                                   choices = c("gaussian","binomial","Gamma",
                                                               "inverse.gaussian","poisson",
                                                               "quasi","quasibinomial","quasipoisson")),
                                       #uiOutput("myConditionalPanel")),
                                       selectInput("ligacao","Função de Ligação",
                                                    c("identity","log","inverse"))),
                                column(2,br(),
                                       checkboxInput(inputId = 'proporcao', label = 'Propor\u00E7\u00E3o Binomial',
                                                     value = FALSE),
                                       checkboxInput(inputId = 'interacao', label = 'Intera\u00E7\u00E3o',
                                                     value = FALSE),
                                       br(),uiOutput("ui.action"))
                            )),
                          fluidRow( box(width = 4 , actionButton("add", "Adicionar Interacao")
                                        #actionButton('rem','remover interacao')
                                        ) ),
                          
                          fluidRow(
                            
                            box(width = 12, solidHeader = TRUE,
                                column(width = 6,
                                       h4("Estimativa dos Par\u00E2metros"),     #br(),
                                       tableOutput("conteudo")),
                                column(width = 6,                     #br(),br(),
                                       h4("Tabela de An\u00E1lise de Vari\u00E2ncia"), #br(),
                                       tableOutput("anova1")) )   
                          ),
                          fluidRow(
                            column(width = 12,
                                   box(width = 3, solidHeader = TRUE,
                                       #uiOutput("myConditionalPanel2")
                                       selectInput("escolhas","Função de Ligação",
                                                                      c("identity","log","inverse") , 
                                                                      selected = 'identity' , multiple = TRUE)
                                       ),
                                   box(width = 12, solidHeader = TRUE,
                                       plotlyOutput("curva_ligacao")
                                       #,
                                       #verbatimTextOutput("teste")
                                       ))
                          )  
                  ),
                  
                  tabItem(tabName = "residuos",
                          fluidRow(
                            box(width = 6, solidHeader = TRUE,
                                fluidRow( column(6, checkboxInput(inputId = 'halfnorm', label = 'Half-Normal',
                                                                  value = TRUE) ) ,
                                          column(6, selectInput('tipo_residuo' , label = 'Tipo de Residuo',
                                                                choices = c('deviance','pearson','response','working','simple','student','standard')))),
                                fluidRow(plotlyOutput("residuos_hnp",height = "355px"))
                            ),
                            box(width = 6, solidHeader = TRUE,
                                plotlyOutput("res_autoplot2")) ),
                          
                          fluidRow(
                            box(width = 6, solidHeader = TRUE,
                                plotlyOutput("res_autoplot3")),
                            box(width = 6, solidHeader = TRUE,
                                plotlyOutput("res_autoplot4")) )
                  ),
                  
                  tabItem(tabName = "livro",
                           tags$iframe(style = "height:calc(100vh - 80px); width:100%; scrolling=yes", 
                                       src = "livro.pdf"))
                          #tags$a(href = "livro.pdf" , download = "livro.pdf"))
                )
              )
)
