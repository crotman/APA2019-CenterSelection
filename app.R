#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




library(shiny)
library(leaflet)
library(htmltools)
library(DT)
library(leafem)
library(sf)
library(tidyverse)
library(bit64)
library(ggmap)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Seleção de centros: uma boa solução"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textAreaInput("pontos",
                      label = "Locais",
                      value = "Rua Pinheiro Guimaraes 115 - Botafogo - Rio de Janeiro\nAvenida Pasteur 458, Rio de Janeiro\nAvenida Rio Branco 1, Rio de Janeiro\nPraça Saens Peña, Rio de Janeiro\nEstrada dos Bandeirantes, Rio de Janeiro\nAvenida das Américas 666, Rio de Janeiro",
                      rows = 20
                      
                      ),
            numericInput("k", "Número de centros", value = 3),
            checkboxInput("dist_real", "Distância de carro",value = FALSE ),
            actionButton("vai", "Vai!")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("mapa_tab1", height = 600),
            div(dataTableOutput("tabela_centros"),style = "font-size:70%"),
            div(dataTableOutput("tabela_distancias"),style = "font-size:70%")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    
    chama_mapdist <- function(from_param, to_param)
    {
        
        mapdist(
            from = from_param, 
            to =  to_param,
            mode = "driving",
            output = "simple"
        ) %>% 
        select(km)

    }

    
    
    enderecos_com_coordenadas <- eventReactive(input$vai,{


        tibble(endereco = unlist(str_split(input$pontos, "\n"))) %>% 
            mutate_geocode(endereco) %>% 
            mutate(id = row_number())
  
        
    })
    
    k <- eventReactive(input$vai,{
        
        input$k    
        
        
    })
    
    distancia_real <- eventReactive(input$vai,{
      
      input$dist_real    
      
      
    })
    

    distancias <- reactive({
     
        df1 <- enderecos_com_coordenadas() %>%
            rename(endereco_1 = endereco,
                   lat_1 = lat,
                   lon_1 = lon,
                   id_1 = id
                   )

        df2 <- enderecos_com_coordenadas() %>%
            rename(endereco_2 = endereco,
                   lat_2 = lat,
                   lon_2 = lon,
                   id_2 = id
            )


        dfcross <- df1 %>%
            crossing(df2)

        if(distancia_real())        
        {
          resposta <- map2_df(dfcross$endereco_1, dfcross$endereco_2, chama_mapdist ) %>%
              bind_cols(dfcross)
        }
        else
        {

          dist_tratada_1 <- dfcross %>%   
            st_as_sf(coords = c("lon_1", "lat_1"), crs ="+proj=longlat +datum=WGS84" ) %>%
            st_transform(crs = "+proj=robin +datum=WGS84") %>% 
            rename(geometry_1 = geometry  ) %>% 
            select(geometry_1)
          
          dist_tratada_2 <- dfcross %>%   
            st_as_sf(coords = c("lon_2", "lat_2"), crs ="+proj=longlat +datum=WGS84" ) %>%
            st_transform(crs = "+proj=robin +datum=WGS84") %>% 
            rename(geometry_2 = geometry  ) %>% 
            select(geometry_2)
          
          
          junta <- bind_cols(dist_tratada_1, dist_tratada_2 ) %>% 
            mutate(km = as.numeric( st_distance(geometry_1, geometry_2, by_element = TRUE))/1000) %>% 
            select(km)

          resposta <- dfcross %>%
            bind_cols(junta)
          
          
                    
        }

        resposta
      
        # write_csv(resposta,"c:/temp/cache_distancias.csv")

        # read_csv("c:/temp/cache_distancias.csv")        

    })
        

    centros <- reactive({
        
        print("oi")

        distancias <- distancias()
        
        centros <- sample_n(enderecos_com_coordenadas(), 1) %>% 
          mutate(passo = 1)
        
        candidatos <- enderecos_com_coordenadas() %>% 
          anti_join(centros, by = c("id"))

        n_centros <- 1

        while(n_centros < k())
        {
            s <- distancias %>%
                inner_join(candidatos, by = c("id_1" = "id")  ) %>% 
                semi_join(centros, by = c("id_2" = "id")) %>% 
                mutate(id = id_1) %>%
                group_by(id_1) %>%
                mutate(min_distancia_cada_cidade_centro = min(km)) %>%
                filter(km == min_distancia_cada_cidade_centro ) %>%
                ungroup() %>%
                mutate(max_distancia_cidades_centro = max(km)) %>%
                filter(km == max_distancia_cidades_centro ) %>%
                select(names(enderecos_com_coordenadas())) %>% 
                mutate(passo = n_centros + 1 ) %>% 
                identity()
            
            centros <- bind_rows(centros, s)
            n_centros = n_centros + 1
            
            candidatos <- enderecos_com_coordenadas() %>% 
              anti_join(centros, by = c("id"))

        }
        
        centros
        
        
    })
    
    
    
        
    output$mapa_tab1 <- renderLeaflet({
        leaflet(data = enderecos_com_coordenadas()) %>% 
            addProviderTiles(providers$Esri.WorldStreetMap) %>% 
            addCircleMarkers( ~lon, ~lat, radius = 3) %>% 
            addCircleMarkers(data = centros(),~lon, ~lat, color = "red",  label = ~as.character(passo), labelOptions = labelOptions(noHide = TRUE), radius = 3   )
    })
    


    output$tabela_centros <- renderDataTable({
      
      centros() %>% 
        select(endereco) %>% 
        datatable(centros(), autoHideNavigation = FALSE, width = 200)    
      
    })
    
    output$tabela_distancias <- renderDataTable({
      distancias() %>% 
        select(endereco_1, endereco_2, km) %>% 
        datatable( autoHideNavigation = FALSE, width = 200)    
      
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)







