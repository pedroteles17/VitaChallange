# Libraries ----
library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(scales)
library(plotly)
library(readxl)
library(DT)
library(zoo)

stat_ret <- function(ret_port, ind, rf) {
  
  # Cumulative Return
  ret_period <- function(ret) {
    prod(ret + 1)^(252 / length(ret)) - 1
  }
  
  # CAPM
  regres <- lm(I(ret_port - rf) ~ I(ind - rf))
  # Get regression coefficients
  regres_coef <- summary(regres)$coefficients
  
  alpha <- (regres_coef[1, 1] + 1) ^ 252 - 1
  beta <- regres_coef[2, 1]
  
  cumulative_ret <- ret_period(ret_port)
  # Portfolio volatility
  vol <- sd(ret_port) * sqrt(252)
  
  port_excess_ret <- ret_period(ret_port - rf)
  
  # Update the Sharpe Ratio so its annualized
  SR <- port_excess_ret / vol
  
  sortino <- port_excess_ret / (sd(ret_port[ret_port < 0]) * sqrt(252))
  
  results <- data.frame(c(
    cumulative_ret, vol, 
    SR, sortino,
    beta, alpha
  )) 
  
  rownames(results) <- c(
    "Retorno Anual. (%)",
    "Desv. Pad. (%)", 
    "Índice Sharpe",
    "Índice Sortino",
    "Beta", "Alfa (%)"
  )
  
  results[c(1, 2, 6), 1] <- round(results[c(1, 2, 6), 1]*100, 2)
  results[c(3, 4, 5), 1] <- round(results[c(3, 4, 5), 1], 2)
  
  return(results)
}

# Initialize Data ----
## Portfolio Acao
dat <- readRDS(file = "all_hyper_ports.rds")

index_rf <- readRDS(file = "index_rf.rds")

# fator
fator_list <- as.list(c('multiple', 'profit', 'mom', 'size', 'value', 'vol'))
names(fator_list) <- c('Combinação de todos', 'Lucratividade', 'Momentum', 'Tamanho', 'Valor', 'Volatilidade' )

fator_name_plot <- c('Todos', 'Lucrat.', 'Mom.', 'Tamanho', 'Valor', 'Volat.')
names(fator_name_plot) <-  c('multiple', 'profit', 'mom', 'size', 'value', 'vol')

# tipo_peso
tipo_peso_list <- as.list(c('ew', 'hrp'))
names(tipo_peso_list) <- c('Equal Weighted', 'Hierarquical Risk Parity')

tipo_peso_name_plot <- c('EW', 'HRP')
names(tipo_peso_name_plot) <-  c('ew', 'hrp')

# tipo_port
tipo_port_list <- as.list(c('lo', 'ls'))
names(tipo_port_list) <- c('Long Only', 'Long & Short')

## Fundos
fundos <- readRDS(file = "fundos.rds")

# nome_fundo
nome_fundo_list <- c('ARX K2 Prev.', 'AZ Quest Altro', 'Oaktree Glob. Cred. USD', 'PIMCO Income Dólar', 'Quasar Latam Bonds', 'Gávea Macro', 'Kadima Prev Multimerc.', 'Kapitalo K10 Prev.', 'Kinea Prev. Atlas', 'Vinland Macro', 'AF Global Bonds', 'ARX Elbrus', 'ARX Everest', 'Trend DI')
names(nome_fundo_list) <- c(unique(fundos$Fundo))

fundos$Fundo <- unlist(lapply(fundos$Fundo, function(x) nome_fundo_list[x]))

names(nome_fundo_list) <- nome_fundo_list

# tipo_dado
tipo_dado_list <- list('Cota', 'PL', 'capt_liq', 'Captacao', 'Resgate')
names(tipo_dado_list) <- c('Cota', 'Patrimônio Líquido', 'Captação Líquida', 'Captação', 'Resgate')

# benchmark
benchmark_list <- list('index', 'rf')
names(benchmark_list) <- c('IBX', 'CDI')

benchmark_list2 <- list('IBX', 'CDI')
names(benchmark_list2) <- c('index', 'rf')


# UI ----
ui <- dashboardPage(
  
  #theme = bs_theme(version = 4, bootswatch = "minty"),
  skin = "blue",
  
  #### Header ----
  dashboardHeader(
    title = "Vita Challange",
    titleWidth = 350
  ),
  #### Sidebar ----
  dashboardSidebar(
    
    width = 350,
    br(),
    h4("Selecione os Inputs aqui", style = "padding-left:20px"),
    
    uiOutput("sidebar")
    
  ),
  #### Body ----
  dashboardBody(
    tabsetPanel(
      type = "tabs",
      id = "tab_selected",
      tabPanel(
        title = "Ações",
        plotlyOutput("plot_stock_data"),
        DTOutput('stats_table')
      ),
      tabPanel(
        title = "Fundos",
        plotlyOutput("plot_funds_data"),
        DTOutput('stats_funds_table')
      )
    )
  )
)

server <- function(input, output) {
  
  # _________________________ -----
  # 1 - Data Cleaning Functions ----
  # 01. get_data_stocks() ----
  get_data_stocks <- reactive({
    
    req(input$tipo_port, input$date_range)
    
    # Preparamos o tipo de indice a ser comparado dependendo se é long only ou long short
    if(input$tipo_port == 'lo'){
      index_rf_user <- index_rf %>%
        dplyr::filter(Date >= input$date_range[1] & Date <= input$date_range[2]) %>%
        select(index) %>%
        set_names('IBX')
    } else if (input$tipo_port == 'ls'){
      index_rf_user <- index_rf %>%
        dplyr::filter(Date >= input$date_range[1] & Date <= input$date_range[2]) %>%
        select(rf) %>%
        set_names('CDI')
    }
    
    # Difinimos a especificacao do portfolio
    nome_port <- paste(input$fator, input$hold_period, '0.3', sep = '_')
    nome_port <- unlist(lapply(nome_port, function(x) paste(x, input$tipo_peso, input$tipo_port, sep = '_')))
    
    # Nome linhas grafico
    nome_linhas_graph <- unlist(lapply(fator_name_plot[input$fator], function(x) paste(x, tipo_peso_name_plot[input$tipo_peso])))
    
    # Selecionamos os dados
    dat_user <- dat %>%
      dplyr::select(Date, all_of(nome_port)) %>%
      set_names(c('Date', nome_linhas_graph)) %>%
      dplyr::filter(Date >= input$date_range[1] & Date <= input$date_range[2]) %>%
      bind_cols(index_rf_user)
    
    dat_user[, -1] <- apply(dat_user[, -1], 2, function(x) cumprod(x + 1))
    dat_user[, -1] <- apply(dat_user[, -1], 2, function(x) x / x[1])
    
    dat_user <- dat_user %>%
      pivot_longer(!Date, names_to = 'Port', values_to = 'Retorno')
    
  })
  
  # 02. get_funds_data() ----
  get_funds_data <- reactive({
    req(input$date_range_funds, input$tipo_dado)
    
    dados_fundos <- fundos %>%
      dplyr::filter(Data >= input$date_range_funds[1] & Data <= input$date_range_funds[2]) %>% 
      dplyr::filter(Fundo %in% input$nome_fundo) %>% 
      dplyr::select(Data, Fundo, input$tipo_dado) %>% 
      set_names('Data', 'Fundo', 'Info') %>%
      mutate(Data = as.Date(Data)) %>%
      arrange(Data) 
    
    if(input$tipo_dado == 'Cota'){
      indice_rf_fundos <- index_rf %>% 
        dplyr::filter(Date >= min(dados_fundos$Data) & Date <= input$date_range_funds[2]) %>%
        mutate(index = cumprod(1 + index), rf = cumprod(1 + rf)) %>% 
        dplyr::select(Date, input$benchmark) %>%
        set_names('Data', benchmark_list2[input$benchmark])
      
      dados_fundos <- merge(indice_rf_fundos[, 1, drop = FALSE], dados_fundos, by = 'Data')
      
      data_unica <- data.frame(Data = as.Date(unique(dados_fundos$Data)))
      
      indice_rf_fundos <- merge(data_unica, indice_rf_fundos, by = 'Data') %>%
        pivot_longer(!Data, names_to = 'Fundo', values_to = 'Info')
      
      dados_fundos <- rbind(dados_fundos, indice_rf_fundos)
      
      dados_fundos <- dados_fundos %>% 
        pivot_wider(names_from = 'Fundo', values_from = 'Info')
    
      dados_fundos[,-1] <- lapply(dados_fundos[,-1], function(x) ifelse(is.na(x), NA, x / x[min(which(!is.na(x)))]))
      
      dados_fundos <- dados_fundos %>% 
        pivot_longer(!Data, names_to = 'Fundo', values_to = 'Info')
    } else if (input$tipo_dado == 'capt_liq'){
      dados_fundos <- dados_fundos %>% 
        pivot_wider(names_from = 'Fundo', values_from = 'Info')
      
      dados_fundos[,-1] <- lapply(dados_fundos[,-1], function(x) cumsum(ifelse(is.na(x), 0, x)) + x*0)
      
      dados_fundos <- dados_fundos %>% 
        pivot_longer(!Data, names_to = 'Fundo', values_to = 'Info')
    }
    
    dados_fundos <- dados_fundos
    
  })
  
  # 03. get_stocks_stats() ----
  get_stocks_stats <- reactive({
    dados_stats <- get_data_stocks()
    
    dados_stats <- dados_stats %>% 
      pivot_wider(names_from = Port, values_from = Retorno)
    
    dados_stats[, -1] <- apply(dados_stats[, -1], 2, function(a) append(0, diff(a)/a[-length(a)]))
    
    ind_rf_data <- index_rf %>% 
      dplyr::filter(Date >= input$date_range[1] & Date <= input$date_range[2])
    
    panel <- lapply(dados_stats[, -1], function(x) stat_ret(x, ind_rf_data$index, ind_rf_data$rf))
    panel <- do.call("cbind", panel) 
    colnames(panel) <- colnames(dados_stats)[-1]
    
    panel[c((nrow(panel)-1), nrow(panel)), ncol(panel)] <- NA
    
    panel <- panel %>% 
      rownames_to_column() %>% 
      pivot_longer(!rowname, names_to = "col1", values_to = "col2") %>% 
      pivot_wider(names_from = "rowname", values_from = "col2")
    
    colnames(panel)[1] <- 'Portfólio'
    
    panel <- panel
  })
  
  # 04. get_funds_stats() ----
  get_funds_stats <- reactive({
    dados_stats <- get_funds_data()
    
    dados_stats <- dados_stats %>% 
      pivot_wider(names_from = Fundo, values_from = Info)
    
    dados_stats[, -1] <- apply(dados_stats[, -1], 2, function(x) ifelse(is.na(x), NA, append(NA, diff(x)/x[-length(x)])))
    
    ind_rf_data <- index_rf %>% 
      dplyr::filter(Date >= min(dados_stats$Data) & Date <= max(dados_stats$Data))
    
    panel <- lapply(dados_stats[, -1], function(x) stat_ret(x[min(which(!is.na(x))):max(which(!is.na(x)))], ind_rf_data$index[min(which(!is.na(x))):max(which(!is.na(x)))], ind_rf_data$rf[min(which(!is.na(x))):max(which(!is.na(x)))]))
    panel <- do.call("cbind", panel) 
    colnames(panel) <- colnames(dados_stats)[-1]
    
    if('CDI' %in% colnames(panel)){
      panel[['CDI']][c(2, 3, 4)] <- NA
    }
    
    panel <- panel %>% 
      rownames_to_column() %>% 
      pivot_longer(!rowname, names_to = "col1", values_to = "col2") %>% 
      pivot_wider(names_from = "rowname", values_from = "col2") %>%
      dplyr::select(!c('Beta', 'Alfa (%)'))
    
    colnames(panel)[1] <- 'Fundo/Índice'
    
    panel <- panel
  })
  
  # _________________________ -----
  
  # 2 - Plotting Data ----
  
  # 02.Stock Plot ----
  output$plot_stock_data <- renderPlotly({
    
    ggplotly(
      ggplot(get_data_stocks(), aes(x=Date, y=Retorno, group=Port)) +
        geom_line(aes(color=Port))+
        xlab('') + ylab('') + 
        theme_classic()
    )
    
  })
  
  output$plot_funds_data <- renderPlotly({
    
    ggplotly(
      ggplot(get_funds_data(), aes(x=Data, y=Info, group=Fundo)) +
        geom_line(aes(color=Fundo))+
        xlab('') + ylab('') + 
        theme_classic()
    )
    
  })
  
  # _________________________ -----
  
  # 3 SELECT Inputs ----
  
  # 03.A fator ----
  output$fator <- renderUI({
    selectInput(
      inputId = "fator", 
      label = strong("Qual fator de risco?", style = "font-family: 'arial'; font-si28pt"),
      multiple = TRUE,
      choices =  fator_list,
      selected = fator_list[1]
    )
  })
  
  # 03.B tipo_peso ----
  output$tipo_peso <- renderUI({
    selectInput(
      inputId = "tipo_peso",
      label = strong("Qual o método para definir os pesos?", style = "font-family: 'arial'; font-si28pt"),
      multiple = TRUE,
      choices = tipo_peso_list,
      selected = tipo_peso_list[length(tipo_peso_list)]
    )
  })
  
  # 03.C date_range ----
  output$date_range <- renderUI({
    dateRangeInput(
      inputId = "date_range",
      label = "Qual a data de início e fim?",
      start = "2003-01-01",
      end   = "2021-12-31"
    )
  })
  
  # 03.D tipo_port ----
  output$tipo_port <- renderUI({
    selectInput(
      inputId = "tipo_port",
      label = strong("Long Only ou Long & Short?", style = "font-family: 'arial'; font-si28pt"),
      choices = tipo_port_list,
      selected = tipo_port_list[1]
    )
  })
  
  # 03.E hold_period ----
  output$hold_period <- renderUI({
    selectInput(
      inputId = "hold_period",
      label = strong("Rebalancear a cada quantos meses?", style = "font-family: 'arial'; font-si28pt"),
      choices = c(1, 3, 6, 12),
      selected = 1
    )
  })
  
  # 03.E stats_table ----
  output$stats_table <- DT::renderDT({
    get_stocks_stats()
  }, options = list(pageLength = 5))
  
  # 03.E stats_funds_table ----
  output$stats_funds_table <- DT::renderDT({
    req(input$tipo_dado)
    if(input$tipo_dado == 'Cota'){
      get_funds_stats() 
    }
  }, options = list(pageLength = 5))
  
  # 03.G nome_fundo ----
  output$nome_fundo <- renderUI({
    selectInput(
      inputId = "nome_fundo", 
      label = strong("Qual o fundo?", style = "font-family: 'arial'; font-si28pt"),
      multiple = TRUE,
      choices =  nome_fundo_list,
      selected = nome_fundo_list[1]
    )
  })
  
  # 03.H tipo_dado ----
  output$tipo_dado <- renderUI({
    selectInput(
      inputId = "tipo_dado", 
      label = strong("Qual o dado?", style = "font-family: 'arial'; font-si28pt"),
      choices =  tipo_dado_list,
      selected = tipo_dado_list[1]
    )
  })
  
  # 03.I benchmark ----
  output$benchmark <- renderUI({
    selectInput(
      inputId = "benchmark", 
      label = strong("Qual o benchmark?", style = "font-family: 'arial'; font-si28pt"),
      multiple = TRUE,
      choices =  benchmark_list,
      selected = benchmark_list[1]
    )
  })
  
  # 03.J date_range_funds ----
  output$date_range_funds <- renderUI({
    dateRangeInput(
      inputId = "date_range_funds",
      label = "Qual a data de início e fim?",
      start = "2015-01-01",
      end   = "2021-12-31"
    )
  })
  
  # 4 UI Sidebar Output ----
  output$sidebar <- renderUI({
    if( input$tab_selected == "Ações"){
      div(
        uiOutput("fator"),
        uiOutput("tipo_peso"),
        uiOutput("date_range"),
        uiOutput("tipo_port"),
        uiOutput("hold_period")
      )
    } else if ( input$tab_selected == "Fundos" ) {
      div(
        uiOutput("nome_fundo"),
        uiOutput("tipo_dado"),
        uiOutput("benchmark"),
        uiOutput("date_range_funds")
      )
    }
  })
  
}

shinyApp(ui, server)




