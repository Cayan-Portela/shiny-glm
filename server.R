shinyServer(function(input, output, session) {
  
  
  dados_pre <- eventReactive(input$carregar_dados_pre,{
      dd <- data.table::fread(file=paste0('dados/',input$dados_pre,'.txt'), header=TRUE,stringsAsFactors = TRUE)
      return(as_data_frame(dd))
  })
  
  dados <- reactive({
    
    file1 <- input$file

    #if(is.null(file1) & is.null(dados_pre())) return(NULL)
    
    if( !is.null(file1) ) {
      dd <- data.table::fread(file=file1$datapath, sep = input$sep, dec = input$dec,
                              header=input$header,stringsAsFactors = input$stringAsFactors) 
      
    } else {
      dd <- dados_pre()
    }
    
    if(input$nova_var != "") {
       
            mutate_aspas = function(df, s){
              q = quote(mutate(df, z = s))
              eval(parse(text=sub("z = s", s, deparse(q)) ))
            }
      
            dd <- mutate_aspas(dd , input$nova_var)
           
    } else {
    #    dd <- data.table::fread(file=file1$datapath, sep = input$sep, dec = input$dec,
    #                            header=input$header,stringsAsFactors = input$stringAsFactors)
         dd <- dd
     }

    return(as_data_frame(dd))
    
  })
  
  dados2 <- reactive({
    
    if(is.null(dados())) {return(NULL)} else {
      
      df <- dados() %>% mutate(Observacao = 1:nrow(dados())) }
    
    if( length(values()$d$key) > 0 | length(values()$s$key) > 0) {
      
      obs     <- unique( c( values()$d$key , values()$s$key ) ) 
      keep    <- df [ setdiff(df$Observacao , as.numeric(obs)) ,]
      exclude <- df [ as.numeric(obs) , ]
      
      lista <- list(keep = keep,
                    exclude = exclude) 
      
    } else {  
      
      lista <- list(keep    = df ,
                    exclude = NULL)  }
    
    return(lista)
  })
  
  output$dependente <- renderUI({
    df <- dados()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("dependentes","Variável resposta",items,
                multiple = input$proporcao)
  })
  
  output$independentes <- renderUI({
    df <- dados()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("independentes","Variáveis preditoras",items,
                multiple=TRUE)
  })
  
  observeEvent(input$add, {
    insertUI(
      selector = "#add",
      where = "afterEnd",
      ui = tags$div( selectInput(paste0("txt", input$add),
                       "Interacao",names(dados()),
                       multiple=TRUE
                       ) )
    ) 
  })
  
  
  # observeEvent(input$rem , {
  #    removeUI(
  #      selector = paste0("div:has(> #txt",1:input$add,")" ),
  #      immediate = TRUE #, multiple = TRUE
  #    )
  # 
  #   print(paste0("txt", input$add))
  # })
  
  output$table <- DT::renderDataTable({
    if(is.null(dados()) ) return(NULL)
    dados() 
  })
  
  output$strR <- renderPrint({
    if(is.null(dados())) return(NULL)
    str(dados())
   # print(getwd())
  })
  
  values <- reactive({
    
    valores <- list(d = event_data("plotly_click"),
                    s = event_data("plotly_selected"))
    return(valores)
  })
  
  modelo <- eventReactive(input$action, {   
    
    dd <- dados2()
    df <- dd$keep
    
    if(is.null(df)) {return(NULL)} 
    
    if(input$add>=1) {
      
      qnt_int <- as.numeric( input$add )
      #c2 <- list()
      int <- list()
      teste <- list()
      print(qnt_int)
      for (i in 1:qnt_int){
        if ( is.null(input[[paste0("txt",i)]]) ) {
          teste[[i]] <- ""
        } else {
            teste[[i]] <- input[[paste0("txt",i)]] }
        
        int[[i]] <-  paste( teste[[i]], collapse = "*" )
        print(int[[i]])
      }
      
      int2 <- int[sapply(int , function(x) x != "")]
print(int2)
      lado_direito <- paste(c(input$independentes,unlist(int2)),collapse = "+")
      
    } else {
      
      lado_direito <- paste(input$independentes,collapse = "+")
      
    }
    
    if (input$proporcao==TRUE){
      
      sucesso   <- df[,input$dependentes[1]]
      total     <- df[,input$dependentes[2]]
      Proporcao <- as.matrix(cbind( sucesso, total - sucesso ))
      formula   <- as.formula(paste("Proporcao"," ~ ",lado_direito))
      glm(formula , family = do.call(input$familia , list(link = input$ligacao) ),data=df)
      }
    else {
      
      formula <- as.formula(paste(input$dependentes," ~ ",lado_direito))
      glm(formula,family = do.call(input$familia , list(link = input$ligacao) ),data=df) 
      }
  })
  
  output$conteudo <- renderTable({
    input$action
    isolate({
      mod <- modelo()
      if (is.null(input$independentes)) return(NULL)
      ab <- summary(mod)$coefficients
      colnames(ab) <- c("Estimativa","Erro Padrão","T-valor","P-valor")
      return( ab )
    })
  }, rownames = TRUE , digits = 3)
  
  output$anova1 <- renderTable({
    input$action
    isolate({
      mod <- modelo()
      if(is.null(mod)) return(NULL)
      
      if ( !(mod$family$family %in% c('binomial','poisson')) ){
        an <- car::Anova(mod , test = "F") 
      } else{
        an <- car::Anova(mod)
      }
      #attr(an,"names") <- c( "g.l." , "Soma Q." , "Q. Médio" , "Est." , "P-valor" )
      return( an  )
    })
  }, rownames = TRUE)
  
  output$descritiva_eixox1 <- renderUI({
    df <- dados()
    if(is.null(df)) return(NULL)
    else {
      items = names(df)
      names(items) = items
      textInput("eixo_x1" , "Eixo X" , value = "Digite a variavel")
    }
  })
  
  output$descritiva_eixoy1 <- renderUI({
    df <- dados()
    if(is.null(df)) return(NULL)
    else {
      items = names(df)
      names(items) = items
      textInput("eixo_y1" , "Eixo Y" , value = "Digite a variavel")
    }
  })
  
  output$descritiva_grupo1 <- renderUI({
    df <- dados()
    if(is.null(df)) return(NULL)
    else {
      items = names(df)
      names(items) = items
      selectInput("grupo1"  , "Grupo"  , c("NULL",items) , multiple = FALSE)
    }
  })
  
  output$descritiva_eixox2 <- renderUI({
    df <- dados()
    if(is.null(df)) return(NULL)
    else {
      items = names(df)
      names(items) = items
      textInput("eixo_x2" , "Eixo X" , value = "Digite a variavel" )
    }
  })
  
  output$descritiva_eixoy2 <- renderUI({
    df <- dados()
    if(is.null(df)) return(NULL)
    else {
      items = names(df)
      names(items) = items
      textInput("eixo_y2" , "Eixo Y" , value = "Digite a variavel" )
    }
  })
  
  output$descritiva_grupo2 <- renderUI({
    df <- dados()
    if(is.null(df)) return(NULL)
    else {
      items = names(df)
      names(items) = items
      selectInput("grupo2"  , "Grupo"  , c("NULL",items) , multiple = FALSE)
    }
  })
  
  output$descritiva_eixox3 <- renderUI({
    df <- dados()
    if(is.null(df)) return(NULL)
    else {
      items = names(df)
      names(items) = items
      selectInput("eixo_x3"  , "Eixo X"  , items , multiple = FALSE)
    }
  })
  
  output$descritiva_eixoy3 <- renderUI({
    df <- dados()
    if(is.null(df)) return(NULL)
    else {
      items = names(df)
      names(items) = items
      selectInput("eixo_y3"  , "Eixo Y"  , items , multiple = FALSE)
    }
  })
  
  output$descritiva_grupo3 <- renderUI({
    df <- dados()
    if(is.null(df)) return(NULL)
    else {
      items = names(df)
      names(items) = items
      selectInput("grupo3"  , "Grupo"  , c(items) , multiple = FALSE)
    }
  })
  
  output$descritiva_histo <- renderUI({
    df <- dados()
    if(is.null(df)) return(NULL)
    else {
      items = names(df)
      names(items) = items
      selectInput("descr_hist"  , "Eixo Y"  , items , multiple = FALSE)
    }
  })
  
  output$grafico_dispersao <- renderPlotly({
    df <- dados()
    if(is.null(df) | input$eixo_x1=="Digite a variavel" | input$eixo_y1=="Digite a variavel") return(NULL) #|  sum(( c(input$eixo_x1,input$eixo_y1) %in% names(df))) < 2  ) return(NULL) #| input$eixo_x1=="Digite a variavel" | input$eixo_y1=="Digite a variavel")  return(NULL)
    else {
      p <- ggplot(df, aes_string(x = input$eixo_x1, y = input$eixo_y1, fill = input$grupo1)) +
        geom_point(shape = 21 , size = 3) +
        ggtitle("") +
        labs(x = input$eixo_x1, y = input$eixo_y1, fill = input$grupo1) +
        theme(legend.position = "bottom", legend.direction = "horizontal",
              axis.line = element_line(size=1, colour = "black"),
              panel.grid.major = element_line(colour = "#d3d3d3"),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(), panel.background = element_blank(),
              plot.title = element_text(size = 14, family = "Tahoma", face = "bold",hjust = 0.5),
              text=element_text(family="Tahoma"),
              axis.text.x=element_text(colour="black", size = 9),
              axis.text.y=element_text(colour="black", size = 9))
      
      ggplotly(p) %>%  config(displayModeBar = F)
    } 
   
  })
  
  output$var_summary <- renderUI({
    df <- dados()
    if(is.null(df)) return(NULL)
    else {
      items = names(df)
      names(items) = items
      selectInput("variaveis"  , "Resumo das Variaveis"  , items , multiple = TRUE) }
  })
  
  output$summary_ <- renderTable({
    df <- dados()
    #if (is.null(df)) return(NULL)
    if (is.null(input$variaveis)) return(NULL)
    else{
      return(  xtable::xtable( summary( df[ , input$variaveis ]  ) )   )
    }
  })
  
  output$grafico_plotly<- renderPlotly({
    df <- dados()
    if(is.null(df)) return(NULL)
    else {  
      plot_ly(data = df, x = df[[input$eixo_x3]] , y = df[[input$eixo_y3]],
              color = df[[input$grupo3]], type = input$tipo_de_grafico) %>%
        config(displayModeBar = F)
    }
  })
  
  output$grafico_histograma <- renderPlotly({
    df <- dados()
    if(is.null(df)) return(NULL)
    else {  #attach(df)
      p <- ggplot(df, aes_string(x=input$descr_hist) ) +
        geom_histogram(aes(y=..count..),
                       position="identity" , alpha = 0.2 , fill = "blue" , bins = input$numero_bins) +
        ylab("Frequência") +
        theme_pander()
      
      ggplotly(p) %>% config(displayModeBar = F)
    }
  })
  
#  output$curva_ligacao <- renderPlotly({

     dados_curva <- reactive({
       
     df       <- dados2()
     mod      <- modelo()
     preditos <- list()
     #print(head(df))
     if(is.null(df)) {return(NULL)}

     if (input$proporcao==TRUE){
       
       sucesso   <- df$keep[,input$dependentes[1]]
       total     <- df$keep[,input$dependentes[2]]
       Proporcao <- as.matrix(cbind( sucesso, total - sucesso ))

       formula   <- as.formula(paste("Proporcao"," ~ ",input$independentes[1]))

        p <- ggplot(data = df$keep ,
                    aes_string( x = input$independentes ,
                                y = paste0(input$dependentes[1],"/",input$dependentes[2]),
                                key = "Observacao")) +
         geom_point (data = df$exclude,
                     shape = 21, fill = NA, color = "black", alpha = 0.25) +
         geom_point (data = df$keep)

       #familia3 <- get(input$familia, mode = "function" , envir = parent.frame() )

     } else {

       formula <- as.formula(paste(input$dependentes," ~ ",input$independentes))
 #      indice <- ifelse( is.numeric(input$independentes[1]) , 1 , which(is.numeric(input$independentes))[1] )
       p <- ggplot(df$keep , aes_string(  x = input$independentes[1] ,
                                          y = input$dependentes ,
                                          key = "Observacao")) +
         geom_point (data = df$exclude,
                     shape = 21, fill = NA, color = "black", alpha = 0.25) +
         geom_point (data = df$keep )

       #familia3 <- get(input$familia, mode = "function" , envir = parent.frame() )
     }

     plist <- list()
     links <- input$escolhas

     plist[[1]] <- p
     n_link_mais1 <- length(input$escolhas) + 1
     
     escolhas_lig <- input$escolhas
     fam <- input$familia
     for(i in 2:n_link_mais1 ) {
       plist[[i]] <- plist[[i-1]]  + stat_smooth(method = "glm" , n = 400,
                                                 method.args = list(do.call( fam , list(link = escolhas_lig[i-1])))  ,# list(family = familia3(link=input$escolhas[i-1] )),
                                                 se = FALSE  , aes(colour = escolhas_lig[i-1]) )
     }

     valores3 <- map(plist, ggplot_build) %>%
                 map("data") %>%
                 pluck(length(input$escolhas)+1) %>%
                 `[`(-1) %>%
                 `[`(-1)
     
      for(i in 1:length(input$escolhas)){
        valores3[[i]] <- valores3[[i]] %>%
                         mutate(Link = input$escolhas[i]) %>%
                         dplyr::select(colour , x , y , Link)
      }

     dados_plot <- do.call( rbind , valores3 )
     dados_plot <- dados_plot %>% mutate(Observacao = 1:nrow(dados_plot))

     p2 <- p + geom_line( data = dados_plot , 
                          aes(x = x , y = y, color = Link ,label = Link,
                              key = Observacao)) +
       theme_minimal()
     return(p2)
     
      # for (i in 1:length(input$escolhas)){
      # 
      #   family <- get(input$familia, mode = "function" , envir = parent.frame() )
      #   modelo <- glm(formula,family = family(link = input$escolhas[i]),data=df$keep)
      # 
      #   preditos[[i]] <- predict( modelo, type = "response") %>%
      #     as.data.frame() %>%
      #     mutate( Funcao = input$escolhas[i] ,
      #             X      = df$keep[[input$independentes[1]]])
      # 
      # }
     
     # for (i in 1:length(input$escolhas)) {
     #   p <- p + geom_line(data = preditos[[i]] ,
     #                      aes(x = X , y = . , colour = Funcao )) }

     # print(head(preditos[[1]]))
     # print(str(preditos[[1]]))
     # print(str(df$keep))
     # print(preditos[[1]])

     #ggplotly(p2)  %>% layout(dragmode = "select") %>% config(displayModeBar = F)
   })

     
     output$curva_ligacao <- renderPlotly({ ggplotly(dados_curva(),
                                                     tooltip = c("x","y","label")) %>%
                                            layout(dragmode = "select") %>%
                                            config(displayModeBar = F) })

  
  output$residuos_hnp <- renderPlotly({
    
    mod <- modelo()
    if(is.null(mod)){return(NULL)}
    
    mod$family$link <- input$ligacao
    
    ab <-  hnp::hnp(mod , print.on = TRUE , paint.out = TRUE, resid.type = input$tipo_residuo,
                    ylab = "Resíduos" , xlab = "Quantis distribuição normal" , conf=.95,
                    main = "Envelope com 95% de confiança", halfnormal = input$halfnorm)
    
    dados_hnp <- data.frame( X = ab$x,
                             Residuos = ab$residuals,
                             Min = ab$lower,
                             Med = ab$median,
                             Max = ab$upper)
    
    #d     <- event_data("plotly_click")
    #s     <- event_data("plotly_selected")
    
    if ( length(values()$d) || length(values()$s) ) {
      
      dados_hnp <- dados_hnp %>% mutate(Observacao = 1:nrow(dados_hnp))
      
      obs     <- unique( c( values()$d$key , values()$s$key ) ) 
      keep    <- dados_hnp [ setdiff(dados_hnp$Observacao , as.numeric(obs)) ,]
      exclude <- dados_hnp [ as.numeric(obs) , ]
      #df      <- keep 
      
      p <- ggplot(dados_hnp , 
                  aes( x = X , y = Residuos, key = Observacao)) +  
        geom_point(data = keep) + 
        geom_point (data = exclude,
                    shape = 21, fill = NA, color = "black", alpha = 0.25) +
        geom_line(aes( x = X , y = Med) , linetype = 2) +
        geom_line(aes( x = X , y = Min) , linetype = 1 , colour = "firebrick") +
        geom_line(aes( x = X , y = Max) , linetype = 1 , colour = "firebrick") + 
        ggtitle("") +
        theme_minimal()
      
      ggplotly(p) %>% layout(dragmode = "select") %>% config(displayModeBar = F)
      
    } else {
      
      dados_hnp <- dados_hnp %>% mutate(Observacao = 1:nrow(dados_hnp))
      
      p <- ggplot(dados_hnp , 
                  aes( x = X , y = Residuos, key = Observacao)) +  geom_point() +
        geom_line(aes( x = X , y = Med) , linetype = 2) +
        geom_line(aes( x = X , y = Min) , linetype = 1 , colour = "firebrick") +
        geom_line(aes( x = X , y = Max) , linetype = 1 , colour = "firebrick") + 
        ggtitle("") +
        theme_minimal()
      
      ggplotly(p) %>% layout(dragmode = "select") %>% config(displayModeBar = F)
      
    }
  })
  
  residuos_tab <- reactive({
    
    df  <- dados2()
    mod <- modelo()  
    influentes <- influence.measures(mod)
    cook <- which(colnames(influentes$infmat)=="cook.d")
    
    if(is.null(mod)){return(NULL)}
    
    dados_res <- data.frame( Std           = rstandard(mod),
                             Ajustados     = mod$fitted.values,
                             Cook          = as.numeric(influentes$infmat[,cook]),
                             Hat           = hatvalues(mod))
    
    dados_res <- dados_res %>% mutate(Observacao = 1:nrow(dados_res))
    
    return(dados_res)
    
  })
  
  output$res_autoplot2 <- renderPlotly({
    
    df <- dados2()
    mod <- modelo()
    #ab   <- autoplot(mod , which = c(1,3,4,5))
    dados_res <- residuos_tab()
    
    #ab[[2]]$labels$x  <- "Valores Ajustados"
    #ab[[2]]$labels$y  <- "Resíduos Padronizados"
    keep <- dados_res[df$keep$Observacao,]
    exclude <- dados_res[df$exclude$Observacao,]
    
    p2 <- ggplot(dados_res, aes(x = Ajustados , y = Std , key = Observacao)) + 
      geom_point(data = keep) +
      geom_point (data = exclude , 
                  shape = 21, fill = NA, color = "black", alpha = 0.25) +
      geom_hline(yintercept = c(-2,2) , colour = "firebrick",linetype = "dashed") +
      scale_y_continuous(breaks = -4:4) + theme_minimal()
    
    ggplotly(p2) %>% layout(dragmode = "select") %>% config(displayModeBar = F)
  })
  
  output$res_autoplot3 <- renderPlotly({
    
    df <- dados2()
    mod <- modelo()
    
    dados_res <- residuos_tab()
    
    keep <- dados_res[df$keep$Observacao,]
    exclude <- dados_res[df$exclude$Observacao,]
    
    p2 <- ggplot(dados_res, aes(x = Observacao , y = Cook , key = Observacao)) + 
          geom_segment(aes(x = Observacao , xend = Observacao, y = 0 , yend = Cook),
                       size = 0.75 , color = "grey") +
          geom_point(data = keep,color = "black" , size = 2 , alpha = 0.8) +
          geom_point(data = exclude , color = "gray" , fill = NA , shape = 21) +
          ylab("Distância de Cook") +
          theme_minimal()
      
      #   geom_bar(data = keep,stat = "identity") +
      # geom_bar(data = exclude , stat ="identity",#shape = 21,
      #          fill = NA, color = "gray", alpha = 0.25) + theme_minimal()
      # 
    
    #geom_hline(yintercept = c(-2,2) , colour = "firebrick",linetype = "dashed") +
    #scale_y_continuous(breaks = -4:4) + theme_minimal()
    
    ggplotly(p2) %>% layout(dragmode = "select") %>% config(displayModeBar = F)
    
    ######------------------##########------------######
    #mod <- modelo()
    #ab   <- autoplot(mod , which = c(1,3,4,5))
    
    #ab[[3]]$labels$title <- "Distância de Cook"
    #ab[[3]]$labels$x     <- "Observação"
    #ab[[3]]$labels$y     <- "Distância de Cook"
    
    #ggplotly(ab[[3]] + #geom_hline(yintercept = 1, colour = "firebrick",linetype=) +
    #           theme_minimal()) %>% config(displayModeBar = F) %>%
    #                                layout(dragmode = "select")
  })
  
  output$res_autoplot4 <- renderPlotly({
    mod <- modelo()
    df <- dados2()
    dados_res <- residuos_tab()
    
    keep <- dados_res[df$keep$Observacao,]
    exclude <- dados_res[df$exclude$Observacao,]
    #lev <- hatvalues(mod)
    #obs <- 1:nrow(mod$data)
    linha_corte <- 2 * (length(mod$coefficients) +1)/ nrow(mod$data)
    
    p <- ggplot(data = dados_res , aes( x = Observacao , y = Hat , key = Observacao)) +
      geom_point(data = keep) +
      geom_point (data = exclude , 
                  shape = 21, fill = NA, color = "black", alpha = 0.25) +
      #geom_linerange(aes(ymin = 0 , ymax = max(lev))) + 
      ggtitle("") + xlab("Observacao") + ylab("Leverage") +
      geom_hline(yintercept = linha_corte , colour = "firebrick",
                 linetype = "dashed") + theme_minimal()
    
    ggplotly(p) %>% config(displayModeBar = F) %>% layout(dragmode = "select")
  })
  
  
  output$ui.action <- renderUI({
    #if (is.null(input$file)) return(NULL)
    actionButton("action", "Rodar novo modelo")
  })
  
  observe({
    if ( input$familia == 'gaussian') {
      updateSelectInput(session,inputId =  "ligacao", 
                        choices = c("identity","log","inverse") , 
                        selected = 'identity')
    } else if(input$familia == 'binomial') {
      updateSelectInput(session, inputId = "ligacao", 
                        choices =  c("logit","probit","cauchit","log","cloglog"),
                        selected = 'logit')
    } else if(input$familia == 'Gamma'){
      updateSelectInput(session, "ligacao", 
                        choices =  c("inverse","identity","log") ,
                        selected = 'inverse')
    } else if(input$familia == 'inverse.gaussian') {
      updateSelectInput(session, inputId = "ligacao", 
                        choices = c("1/mu^2","inverse","log"),
                        selected = '1/mu^2')
    } else if(input$familia == 'poisson'){
      updateSelectInput(session, inputId = "ligacao", 
                        choices = c("log","identity","sqrt"),
                        selected = 'log')
    } else if(input$familia=='quasipoisson'){
      updateSelectInput(session, inputId = "ligacao", 
                        choices = c("log") , selected = 'log')
    } else {
      updateSelectInput(session, inputId = "ligacao", 
                        choices = c("logit") , selected = 'logit' )
    }
  })
  
  # output$myConditionalPanel = renderUI({
  #   if(input$familia=="binomial") {
  #     conditionalPanel(
  #       condition = "input.familia == 'binomial'",
  #       selectInput("ligacao","Função de Ligação",
  #                   c("logit","probit","cauchit","log","cloglog")))  }
  #   
  #   else if(input$familia=="gaussian") {
  #     conditionalPanel(
  #       condition = "input.familia=='gaussian",
  #       selectInput("ligacao","Função de Ligação",
  #                   c("identity","log","inverse")))  }
  #   
  #   else if(input$familia=="Gamma") {
  #     conditionalPanel(
  #       condition = "input.familia=='Gamma",
  #       selectInput("ligacao","Função de Ligação",
  #                   c("inverse","identity","log")))  }
  #   
  #   else if (input$familia=="inverse.gaussian"){
  #     conditionalPanel(
  #       condition = "input.familia=='inverse.gaussian",
  #       selectInput("ligacao","Função de Ligação",
  #                   c("1/mu^2","inverse","indentity","log")))}
  #   
  #   else if (input$familia=="poisson"){
  #     conditionalPanel(
  #       condition = "input.familia=='poisson'",
  #       selectInput("ligacao","Função de Ligação",
  #                   c("log","identity","sqrt")))}
  #   
  #   else if(input$familia=="quasipoisson"){
  #     conditionalPanel(
  #       condition = "input.familia=='quasipoisson",
  #       selectInput("ligacao","Função de Ligação",
  #                   c("log")))}
  #   else { 
  #     conditionalPanel(
  #       condition = "input.familia=='quasibinomial",
  #       selectInput("ligacao","Função de Ligação",
  #                   c("logit")))  }
  # })
  
  observe({
    if ( input$familia == 'gaussian') {
      updateSelectInput(session,inputId =  "escolhas", 
                        choices = c("identity","log","inverse") , 
                        selected = 'identity')
    } else if(input$familia == 'binomial') {
      updateSelectInput(session, inputId = "escolhas", 
                        choices =  c("logit","probit","cauchit","log","cloglog"),
                        selected = 'logit')
    } else if(input$familia == 'Gamma'){
      updateSelectInput(session, "escolhas", 
                        choices =  c("inverse","identity","log") ,
                        selected = 'inverse')
    } else if(input$familia == 'inverse.gaussian') {
      updateSelectInput(session, inputId = "escolhas", 
                        choices = c("1/mu^2","inverse","log"),
                        selected = '1/mu^2')
    } else if(input$familia == 'poisson'){
      updateSelectInput(session, inputId = "escolhas", 
                        choices = c("log","identity","sqrt"),
                        selected = 'log')
    } else if(input$familia=='quasipoisson'){
      updateSelectInput(session, inputId = "escolhas", 
                        choices = c("log") , selected = 'log')
    } else {
      updateSelectInput(session, inputId = "escolhas", 
                        choices = c("logit") , selected = 'logit' )
    }
  })
  
#  output$myConditionalPanel2 = renderUI({
    
 #   req(input$familia)
    
    
  #   if(input$familia=="binomial") {
  #     conditionalPanel(
  #       condition = "input.familia == 'binomial'",
  #       selectInput("escolhas","Função de Ligação", 
  #                   c("logit","probit","cauchit","log","cloglog") ,
  #                   selected = input$familia,multiple = TRUE) )  
  # }
  #   
  #   else if(input$familia=="gaussian")  {
  #     conditionalPanel(
  #       condition = "input.familia=='gaussian'",
  #       selectInput("escolhas","Função de Ligação",
  #                   c("identity","log","inverse") , 
  #                   selected = input$ligacao , multiple = TRUE))  
  # }
  #   
  #   else if(input$familia=="Gamma") {
  #     conditionalPanel(
  #       condition = "input.familia=='Gamma'",
  #       selectInput("escolhas","Função de Ligação",
  #                   c("inverse","identity","log") ,
  #                   selected = input$ligacao , multiple = TRUE))  }
  # 
  #   else if (input$familia=="inverse.gaussian"){
  #     conditionalPanel(
  #       condition = "input.familia=='inverse.gaussian'",
  #       selectInput("escolhas","Função de Ligação",
  #                   c("1/mu^2","inverse","indentity","log") ,
  #                   selected = input$ligacao , multiple = TRUE))}
  # 
  #   else if (input$familia=="poisson"){
  #     conditionalPanel(
  #       condition = "input.familia=='poisson'",
  #       selectInput("escolhas","Função de Ligação",
  #                   c("log","identity","sqrt") ,
  #                   selected = input$ligacao , multiple = TRUE))}
  # 
  #   else if(input$familia=="quasipoisson"){
  #     conditionalPanel(
  #       condition = "input.familia=='quasipoisson'",
  #       selectInput("escolhas","Função de Ligação",
  #                   c("log") , selected = input$ligacao ,multiple = TRUE))}
  # 
  #   else {
  #     conditionalPanel(
  #       condition = "input.familia=='quasibinomial'",
  #       selectInput("escolhas","Função de Ligação",
  #                   c("logit") , selected = input$ligacao , multiple = TRUE))  }
  # })
  
})
