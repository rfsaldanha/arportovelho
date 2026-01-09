# Packages
library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(kunakapir)
library(vchartr)
library(leaflet)

device_id <- "0324240376"

# Interface
ui <- page_navbar(
  tags$head(includeHTML("google-analytics.html")),

  title = "Qualidade do Ar - Porto Velho",
  theme = bs_theme(bootswatch = "shiny"),

  # Logo
  tags$head(
    tags$script(
      HTML(
        '$(document).ready(function() {
             $(".navbar .container-fluid")
               .append("<img id = \'myImage\' src=\'pin_obs_horizontal.png\' align=\'right\' height = \'65px\'>"  );
            });'
      )
    ),
    tags$style(
      HTML(
        '@media (max-width:992px) { #myImage { position: fixed; right: 10%; top: 0.5%; }}'
      )
    )
  ),

  # Translation
  tags$script(
    HTML(
      "
      $(document).ready(function() {
        // Change the text 'Expand' in all tooltips
        $('.card.bslib-card bslib-tooltip > div').each(function() {
          if ($(this).text().includes('Expand')) {
            $(this).text('Expandir');
          }
        });
  
        // Use MutationObserver to change the text 'Close'
        var observer = new MutationObserver(function(mutations) {
          $('.bslib-full-screen-exit').each(function() {
            if ($(this).html().includes('Close')) {
              $(this).html($(this).html().replace('Close', 'Fechar'));
            }
          });
        });
  
        // Observe all elements with the class 'card bslib-card'
        $('.card.bslib-card').each(function() {
          observer.observe(this, { 
            attributes: true, 
            attributeFilter: ['data-full-screen'] 
          });
        });
      });
    "
    )
  ),

  # Map page
  nav_panel(
    title = "Painel",

    # Card
    card(
      full_screen = TRUE,
      layout_sidebar(
        sidebar = sidebar(
          open = "closed",
          selectInput(
            inputId = "indicators",
            label = "Indicadores",
            choices = NULL,
            multiple = TRUE
          ),
          numericInput(
            inputId = "readings",
            label = "Últimas leituras",
            min = 1,
            max = 4000,
            step = 1,
            value = 500
          ),
          tags$caption("A estação realiza uma leitura a cada 10 minutos.")
        ),
        vchartOutput(outputId = "graph")
      )
    )
  ),

  # About page
  nav_panel(
    title = "Sobre o projeto",
    accordion(
      multiple = FALSE,
      accordion_panel(
        "Sobre",
        p(
          "Estação de qualidade do ar instalada na Fiocruz Rondônia, em Porto Velho."
        ),
      ),
      accordion_panel(
        "Localização da estação",
        leafletOutput(outputId = "map"),
        class = "p-0"
      ),
      accordion_panel(
        "Indicadores disponíveis",
        p(
          "A estação apresenta os seguintes indicadores, atualizados continuamente em intervalos de 10 minutos."
        ),
        tags$div(
          tags$ul(
            tags$li(
              "•	Concentrações de material particulado (PM1, PM2.5, PM4 e PM10)"
            ),
            tags$li("Concentração de CO"),
            tags$li("Concentração de NO2"),
            tags$li("Concentração de O3"),
            tags$li("Concentração de SO2"),
            tags$li("Contagem total de partículas (TPC)"),
            tags$li("Total de partículas suspensas (TSP)"),
            tags$li("Concentração de compostos orgânicos voláteis (VOC)"),
            tags$li("Índice de qualidade do ar (AQI)"),
            tags$li("Ponto de orvalho (Dew point)"),
            tags$li("Umidade do ar (Humidity index)"),
            tags$li("Pressão do ar (Pressure)"),
            tags$li("Temperatura do ar (Temp ext)"),
            tags$li("Índice de calor (Heat index)"),
            tags$li("Velocidade do vento (média, máxima e rajadas)"),
            tags$li(
              "Variáveis de controle do equipamento: temperatura do sensor, bateria, carga e sinal"
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  updateSelectInput(
    inputId = "indicators",
    choices = list_elements(device_id = device_id) |> unlist(),
    selected = c("PM2.5", "O3 GCc")
  )

  output$graph <- renderVchart({
    req(input$indicators)
    req(input$readings)

    # Refresh every 1 minute
    invalidateLater(millis = 600000, session = session)

    # Get data
    res <- get_reads_multiple_until(
      device_id = device_id,
      elements_ids = input$indicators,
      ts = Sys.time(),
      number = input$readings
    )

    # Adjust
    res <- res |>
      arrange(ts) |>
      mutate(ts = as_datetime(ts))

    # Plot
    vchart(res) |>
      v_line(
        aes(x = ts, y = value, color = sensor_tag),
        lineLabel = list(visible = TRUE)
      ) |>
      v_scale_y_continuous(min = 0) |>
      v_specs_legend(visible = FALSE)
  })

  output$map <- renderLeaflet({
    res <- kunakapir::read_info(device_id = device_id)

    leaflet() |>
      addTiles() |>
      addMarkers(
        lng = as.numeric(res$location$longitude),
        lat = as.numeric(res$location$latitude)
      )
  })
}

shinyApp(ui, server)
