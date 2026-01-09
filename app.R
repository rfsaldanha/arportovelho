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
          "A iniciativa apresentada durante a COP30 está diretamente vinculada ao desenvolvimento da Plataforma FioAres, um sistema integrado de monitoramento da qualidade do ar e vigilância em saúde, atualmente implantado em cinco cidades da Amazônia: Rio Branco, Porto Velho, Manaus, Belém e Santarém."
        ),
        p(
          "A FioAres é desenvolvida pela Fundação Oswaldo Cruz – Fiocruz Piauí, com apoio técnico e operacional do Observatório de Clima e Saúde e da Plataforma de Ciência de Dados Aplicada à Saúde (PCDaS/ICICT-Fiocruz), e apoio institucional do Ministério do Meio Ambiente e Mudanças no Clima (MMA). "
        ),
        p(
          "No território amazônico, a plataforma conta com um conjunto estratégico de parceiros locais, incluindo a Universidade Federal do Pará (UFPA), a Universidade Federal de Rondônia (UNIR), a Universidade do Estado do Amazonas (UEA), a Fiocruz Rondônia, por meio do Centro de Clima e Saúde de Rondônia (CCSRO), e o Instituto Leônidas & Maria Deane (ILMD/Fiocruz Amazônia). Esses parceiros contribuem diretamente para a operação, manutenção e produção de pesquisa aplicada sobre a qualidade do ar e seus impactos na saúde, fortalecendo a atuação territorial e colaborativa da plataforma na região. "
        ),
        p(
          "A COP30 representa uma oportunidade fundamental para ampliar a visibilidade da FioAres e demonstrar seu potencial como ferramenta de monitoramento local, contínuo e em tempo real de diversos poluentes atmosféricos, contribuindo para a vigilância ambiental e para a comunicação de riscos em áreas altamente vulneráveis aos efeitos das mudanças climáticas e das queimadas."
        ),
        p(
          "Para esta ação durante a COP30, foi instalada uma estação compacta Kunak Air, cedida em parceria com a ACOEM Brasil/JCTM, possibilitando a apresentação ao vivo dos indicadores de qualidade do ar monitorados durante o evento. A instalação, realizada em cooperação com o CENSIPAM, a UFPA, o Observatório de Clima e Saúde e a PCDaS, demonstra o compromisso da FioAres em fortalecer a vigilância da qualidade do ar na Amazônia e apoiar a tomada de decisão baseada em evidências."
        ),
        p("Apoio:"),
        img(
          src = "footer.png",
          width = "600px",
          align = "center"
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
