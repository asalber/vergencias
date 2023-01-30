# Carga de librerías
library(shiny)
library(tidyverse)
library(knitr)
library(kableExtra)
library(gridExtra)

# Colores
color1 <- "#00BFC4"
color2 <- "#F8766D"
color3 <- "#7CAE00"
color4 <- "#C77CFF"

# Carga de datos
# Conjunto de datos original
df <- read_csv("../data/datos.preprocesados.csv") %>%
  mutate(ID = as.factor(ID))
# Conjunto de datos sin medidas repetidas
df2 <- read_csv("../data/datos.preprocesados.sin.medidas.repetidas.csv") %>%
  mutate(ID = as.factor(ID))

# Pacientes con 3 o más revisones
ids <- df %>%
  filter(REVISION == 3) %>%
  pull(ID)

# Variables
vars.vergencias <- c("VHL.BT1", "VHL.BT2", "VHL.BN1", "VHL.BN2", "VHC.BT1", "VHC.BT2", "VHC.BT3", "VHC.BN1", "VHC.BN2", "VHC.BN3")
vars.forias <- c("FVL", "FVC", "FLC")
vars.dep <- c("EME.D", "EME.I", "HIP.D", "HIP.I", "MIO.D", "MIO.I", "AST.D", "AST.I", "PRE.D", "PRE.I")
vars.dep.evol <- paste0(vars.dep, ".EVOL")
vars.ind <- c(vars.vergencias, vars.forias)
vars.ind.evol <- paste0(vars.ind, ".EVOL")
vars <- c(vars.ind, vars.dep)

df2.dep <- df2 %>% select(vars.dep)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Vergencias"),
  navbarPage(
    title = NULL,
    # Página de evolución
    tabPanel(
      "Distribuciones",
      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(
          varSelectInput("var1", "Variable", df %>% select(order(colnames(df))) %>% select(where(is.numeric))),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          h3("Histograma"),
          plotOutput("distPlot"),
          h3("Contraste de normalidad"),
          textOutput("normalidad"),
          h3("Tabla de frecuencias"),
          htmlOutput("freqTable")
        )
      )
    ),

    # Página de evolución
    tabPanel(
      "Evolución pacientes",
      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(
          varSelectInput("vardep", "Variable Dependiente", df2 %>% select(vars.dep)),
          varSelectInput("varind", "Variable Independiente", df2 %>% select(vars.ind)),
          selectInput("id", "Id Paciente", setNames(ids, ids)),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("evolPlot"),
        )
      )
    ),

    # Página de regresión
    tabPanel(
      "Regresión simple",
      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(
          varSelectInput("vardep", "Variable Dependiente", df2 %>% select(vars.dep.evol)),
          varSelectInput("varind", "Variable Independiente", df2 %>% select(vars.ind.evol)),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("scatterPlot"),
          textOutput("R2")
        )
      )
    )
  )
)

# Lógica del servidor
server <- function(input, output) {
  output$distPlot <- renderPlot({
    # Dibujo del histograma
    ggplot(df, aes(x = !!input$var1)) +
      geom_histogram(fill = color1)
  })
  
  output$normalidad <- renderText({
    p <- shapiro.test(df[[input$var1]])$p.value
    paste("p-valor =", round(p,3))
  })

  output$freqTable <- reactive({
    kable(count(df, !!input$var1)) %>%
      kable_styling(bootstrap_options = c("hover", "striped"), full_width = F)
  })

  output$evolPlot <- renderPlot({
    # Dibujo del diagrama de evolución
    p1 <- df %>%
      filter(ID == input$id) %>%
      ggplot(aes(x = REVISION, y = !!input$vardep, group = 1)) +
      geom_line(col = color1)
    p2 <- df %>%
      filter(ID == input$id) %>%
      ggplot(aes(x = REVISION, y = !!input$varind, group = 1)) +
      geom_line(col = color2)
    grid.arrange(p1, p2, nrow = 2)
  })

  output$scatterPlot <- renderPlot({
    # Dibujo del diagrama de dispersión
    ggplot(df2, aes(x = !!input$varind, y = !!input$vardep)) +
      geom_point() +
      geom_smooth(method = "lm")
  })


  output$R2 <- renderText({
    r2 <- cor(df2[[input$varind]], df2[[input$vardep]])^2
    paste("R² =", r2)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
