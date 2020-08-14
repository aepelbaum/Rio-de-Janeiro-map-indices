#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
# https://cran.r-project.org/web/packages/geobr/vignettes/intro_to_geobr.html
library(geobr)
library(shiny)

#Lê CSV com indices dos municípios do Rio de Janeiro
df_indice <- read_csv2("../data/dados-rj.csv", col_names = TRUE,
                       locale = locale(encoding = "ISO-8859-1"), col_types = NULL) %>% 
  rename(code_muni = codigo_municipio) %>% 
  mutate(area_log=log1p(area)) %>% 
  mutate(densidade_demografica_2010_log=log1p(densidade_demografica_2010)) %>% 
  mutate(escolaridade_2010_log=log1p(escolaridade_2010)) %>% 
  mutate(idh_municipal_2010_log=log1p(idh_municipal_2010)) %>% 
  mutate(mortalidade_infantil_mortes_por_mil_nascimentos_2017_log=log1p(mortalidade_infantil_mortes_por_mil_nascimentos_2017)) %>% 
  mutate(populacao_log=log1p(populacao)) 

df_rj <- read_municipality( code_muni = "RJ", year= 2018) %>% 
  mutate(name_muni=str_replace_all(name_muni, " De "," de ")) %>% 
  mutate(name_muni=str_replace_all(name_muni, " Do "," do ")) %>% 
  mutate(name_muni=str_replace_all(name_muni, " Dos "," dos "))

df_municipios_rj <- df_rj$name_muni %>% fct_inorder()

sf_rj_indice <- df_rj %>% 
  left_join(df_indice, by = "code_muni")

# Define UI for application that draws a map
ui <- fluidPage(

    # Application title
    titlePanel("Índices dos municípios do Rio de Janeiro"),

    sidebarLayout(
      sidebarPanel(
        radioButtons("indice", label = "Selecione",
                     choices = list("Área" = "area",
                                    "Densidade demográfica" = "densidade_demografica_2010",
                                    "Escolaridade" = "escolaridade_2010",
                                    "IDH" = "idh_municipal_2010",
                                    "Mortalidade infantil (mortes por mil nascimentos)" = "mortalidade_infantil_mortes_por_mil_nascimentos_2017",
                                    "População" = "populacao"), 
                     selected = "area"),
        checkboxInput("log", label = "Log", value = FALSE, width = NULL)
      ),
      
      
      mainPanel(
        plotOutput("drawMap")
      )
    )
)

# Define server logic required to draw a map
server <- function(input, output) {

    output$drawMap <- renderPlot({
        fill_var <-  input$indice
        if (input$log)
          fill_var  <-  str_c(fill_var, "_log")
        if(input$indice == "area") {
          fill_label <-  "Área"
          high_color <-  "yellow"
        }  
        if(input$indice == "densidade_demografica_2010") {
          fill_label <-  "Densidade demográfica"
          high_color <-  "green"
    }
        if(input$indice == "escolaridade_2010") {
          fill_label <-  "Escolaridade"
          high_color <-  "blue"
        }  
        if(input$indice == "idh_municipal_2010") {
          fill_label <-  "IDH"
          high_color <-  "purple"
        }
        if(input$indice == "mortalidade_infantil_mortes_por_mil_nascimentos_2017") {
          fill_label <-  "Mortalidade infantil (mortes por mil nascimentos)"
          high_color <-  "red"
        }  
        if(input$indice == "populacao") {
          fill_label <-  "População"
          high_color <-  "pink"
        }  
        sf_rj_indice %>%
                  ggplot() +
                  geom_sf(aes_string(fill = fill_var), color= "black", size=.15) +
                  scale_fill_continuous(type = "gradient", low = "white", high = high_color, name = fill_label, label = scales::comma) +
                  coord_sf(datum = NA) +
                  theme(legend.position = "bottom", legend.direction = "vertical")
                  
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
