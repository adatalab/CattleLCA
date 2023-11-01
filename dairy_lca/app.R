library(shiny)
library(tidyverse)
library(reactable)
library(sparkline)
library(shinycssloaders)
library(highcharter)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("젖소 농장 온실가스 산출 프로토타입"),
  
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          width = 2,
          tabsetPanel(
            tabPanel("우군",
                     numericInput("drying", label = "건유우 사육두수:", value = 10),
                     numericInput("dairy", label = "착유우 사육두수:", value = 80),
                     numericInput("heifer", label = "미경산우 사육두수:", value = 31),
                     numericInput("growing", label = "육성우 사육두수", value = 13),
                     numericInput("calve", label = "송아지 사육두수", value = 12),
                     numericInput("drying_bw", label = "건유우 체중(kg):", value = 700),
                     numericInput("dairy_bw", label = "착유우 체중(kg):", value = 650),
                     numericInput("heifer_bw", label = "미경산우 체중(kg):", value = 473),
                     numericInput("growing_bw", label = "육성우 체중(kg):", value = 320),
                     numericInput("calve_bw", label = "송아지 체중(kg):", value = 167),
                     actionButton("go", label = "Calcultate")         
                     ),
            tabPanel("유성분",
                     numericInput("milk_yield", label = "평균유량(kg/d):", value = 32),
                     numericInput("milk_fat", label = "유지방함량(%):", value = 3.5),
                     numericInput("milk_protein", label = "유단백함량(%):", value = 3.0)
                     ),
            tabPanel("사료",
                     numericInput("drying_tdn", label = "건유우 TDN(%):", value = 74.04),
                     numericInput("dairy_tdn", label = "착유우 TDN(%):", value = 65.19),
                     numericInput("heifer_tdn", label = "미경산우 TDN(%):", value = 57.75),
                     numericInput("growing_tdn", label = "육성우 TDN(%)", value = 57.75),
                     numericInput("calve_tdn", label = "송아지 TDN(%)", value = 66.15),
                     numericInput("drying_cp", label = "건유우 CP(%):", value = 12.32),
                     numericInput("dairy_cp", label = "착유우 CP(%):", value = 15.22),
                     numericInput("heifer_cp", label = "미경산우 CP(%):", value = 14.24),
                     numericInput("growing_cp", label = "육성우 CP(%)", value = 14.24),
                     numericInput("calve_cp", label = "송아지 CP(%)", value = 15.46),
                     numericInput("feed_co2eq_total", label = "Total Feed CO2eq/year/head", value = 4360.044502)
            ),
            tabPanel("에너지",
                     # numericInput("farm_head", label = "총사육두수", value = 1),
                     numericInput("co2eq_electr", label = "연간 두당 전기사용량(Kwh/head/year)", value = 1378.48),
                     numericInput("co2eq_gasoline", label = "연간 두당 가솔린 사용량(L/head/year)", value = 0),
                     numericInput("co2eq_disel", label = "연간 두당 디젤 사용량(L/head/year)", value = 29.41),
                     numericInput("ef_electr", label = "Electr EF (kg co2eq/KWh)", value = 0.4954),
                     numericInput("ef_gasoline", label = "Gasoline EF (kg co2eq/L)", value = 2.178878),
                     numericInput("ef_co2_disel", label = "Disel EF (kg co2eq/L)", value = 2.596127),
                     numericInput("ef_n2o_disel", label = "Disel EF (kg n2o/L)", value = 0.001007)
            ),
            tabPanel("Parameters",
                     prettyRadioButtons(inputId = "ch4_gwp", label = "CH4 GWP", choices = c(28.0, 29.8), inline = TRUE, status = "danger", fill = TRUE),
                     prettyRadioButtons(inputId = "n2o_gwp", label = "N2O GWP", choices = c(265, 273), inline = TRUE, status = "danger", fill = TRUE),
                     numericInput("cfi", label = "Cfi:", value = 0.322),
                     colorSelectorInput("color", "Fill Color", choices = viridisLite::viridis(n = 10), selected = "#FDE725FF")
                     )
          )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          width = 10,
          fluidRow(
            column(8, highchartOutput("plot1") %>% shinycssloaders::withSpinner(type = 8)),
            column(4, highchartOutput("plot2"))
          ),
          fluidRow(
            column(12, reactableOutput("table") %>% shinycssloaders::withSpinner(type = 8))
          ),
          br(),
          verbatimTextOutput("message"),
          br(),
          downloadButton('downloadData', 'Download All Data')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # function 
  dairy_ghg <- function() {
    
    # df <- dairyCattle::dairy_example %>% filter(검정일 == "2018-05-19")
    
    df1 <- tibble(
      category = c("dry", "milking", "heifer", "growing", "calve"),
      number_of_animals = c(input$drying, input$dairy, input$heifer, input$growing, input$calve),
      milk_yield = c(0, input$milk_yield, 0, 0, 0),
      milk_fat = c(0, input$milk_fat, 0, 0, 0),
      mature_bw = 680,
      bw = c(input$drying_bw, input$dairy_bw, input$heifer_bw, input$growing_bw, input$calve_bw),
      adg = c(0.17, 0, 0.8, 0.8, 0.79),
      ge_mcal_kg = 4.33,
      # tdn_percent = c(74.04, 65.19, 57.75, 57.75, 66.15),
      tdn_percent = c(input$drying_tdn, input$dairy_tdn, input$heifer_tdn, input$growing_tdn, input$calve_tdn),
      de_mcal_kg = tdn_percent*0.04409,
      de_percent = de_mcal_kg/ge_mcal_kg*100,
      cfi = c(0.322, 0.386, 0.322, 0.322, 0.322),
      c_pregnancy = c(0.1, 0.1, 0, 0, 0),
      Ym = c(6.3, 5.8, 6.3, 6.3, 6.3),
      NEm_mj_day = cfi*bw^0.75,
      NEg_mj_day = 22.02*(bw/(0.8*mature_bw))^0.75*adg^1.097,
      NEl_mj_day = milk_yield*(1.47+0.4*milk_fat),
      NEp_mj_day = c_pregnancy*NEm_mj_day,
      rem = (1.123-(4.092*(10^-3)*de_percent)+(1.126*(10^-5)*(de_percent^2))-(25.4/de_percent)),
      reg = (1.164-(5.16*(10^-3)*de_percent)+(1.308*(10^-5)*(de_percent^2))-(37.4/de_percent)),
      ge_mj_day = (((NEm_mj_day + NEl_mj_day + NEp_mj_day)/rem)+(NEg_mj_day/reg))/(de_percent/100),
      enteric_ch4_year = ((ge_mj_day*(Ym/100))*365)/55.65,
      total_enteric_ch4 = enteric_ch4_year*number_of_animals
    )
    
    # manure
    df1 <- df1 %>% 
      mutate(
        # ch4 from manure
        ue_ge = 0.04*ge_mj_day,
        ash_percent = c(5.9, 8.8, 9.1, 9.1, 9.4),
        vs_kg_day_animal = ((ge_mj_day*(1-(de_percent/100)+ue_ge/100)*(((1-(ash_percent/100))/18.45)))),
        awms = c(29, 21, 21, 21, 21),
        b0 = c(0.18, 0.24, 0.24, 0.24, 0.24),
        mcf = 0.02,
        manure_ch4_year =(vs_kg_day_animal*365)*(b0*0.67*(mcf/100)*awms),
        total_manure_ch4 = manure_ch4_year*number_of_animals,
        # n20 from manure (direct)
        cp_percent = c(input$drying_cp, input$dairy_cp, input$heifer_cp, input$growing_cp, input$calve_cp),
        awms_n2o = 0.21,
        ncdg = 0, 
        ef3 = 0.1,
        n_intake = (ge_mj_day/18.45)*((cp_percent/100)/6.25),
        milk_prot = c(0, input$milk_protein, 0, 0, 0),
        n_retention = (milk_yield*(milk_prot/100))/6.38,
        nex_year = n_intake*(1-n_retention)*365,
        total_n2o_direct = ((((number_of_animals*nex_year)*awms_n2o)+ncdg)*ef3)*(44/28),
        # n2o from manure (indirect)
        frac_gas = c(0.45, 0.3, 0.45, 0.45, 0.45),
        n_volatilization = (((nex_year)*awms_n2o) + 0)*frac_gas,
        ef4 = 0.1,
        n2o_indirect = (n_volatilization*ef4)*(44/28),
        total_n2o_indirect = n2o_indirect*number_of_animals
      )
    
    # energy
    df1 <- df1 %>% 
      mutate(
        electricity_khw_year_head = 1378.48,
        diesel_l_year_head = 29.41,
        electr_ef = input$ef_electr,
        diesel_ef_co2 = input$ef_co2_disel,
        diesel_ef_n2o = input$ef_n2o_disel,
        total_electr_co2eq = electricity_khw_year_head*electr_ef*number_of_animals,
        total_diesel_co2eq = diesel_l_year_head*diesel_ef_co2*number_of_animals,
        total_diesel_n2o = diesel_l_year_head*diesel_ef_n2o*number_of_animals
      )
    
    # result
    df1 <- df1 %>% 
      mutate(
        ch4_gwp = input$ch4_gwp %>% as.numeric(),
        n2o_gwp = input$n2o_gwp %>% as.numeric(),
      ) %>% 
      mutate(
        enteric_co2eq_day = total_enteric_ch4*ch4_gwp/365,
        manure_co2eq_day = (total_manure_ch4*ch4_gwp + total_n2o_direct*n2o_gwp + total_n2o_indirect*n2o_gwp)/365,
        energy_co2eq_day = (total_electr_co2eq + total_diesel_co2eq)/365,
        feed_co2eq_day = (input$feed_co2eq_total/365)*number_of_animals,
        sum_co2eq_day = enteric_co2eq_day + manure_co2eq_day + energy_co2eq_day + feed_co2eq_day
      ) %>% 
      mutate(total_milk = number_of_animals*milk_yield) %>% 
      #mutate all numeric values to 2 decimals
      mutate_if(is.numeric, round, 2)
    
    return(df1)
    
  }
    
   coef <- reactive({dairy_ghg()})
    
   output$message <- renderText({
     
     
     paste0("목장에서 일일 평균 ", input$milk_yield, "kg의 우유를 생산하는 ", input$dairy,"두의 착유우가 총 ", input$milk_yield*input$dairy, "kg의 우유를 생산합니다. ", "\n이 목장의 일일 탄소배출량은 ", sum(coef()$sum_co2eq_day, na.rm = TRUE), "kg CO2eq/day이며, 따라서 우유 1kg 당 약",
            round(sum(coef()$sum_co2eq_day, na.rm = TRUE)/(sum(coef()$total_milk, na.rm = TRUE)), 3), "kg CO2eq의 탄소를 배출합니다."
            )
     
   })
     
   
   output$downloadData <- downloadHandler(
     filename = function() {
       paste('data-', Sys.Date(), '.xlsx', sep='')
     },
     content = function(file) {
       rio::export(coef(), file)
     }
   )
     
    output$table <- renderReactable({

        # input$go
        coef1 <- coef() %>% 
          select(category, number_of_animals, milk_yield, enteric_co2eq_day, manure_co2eq_day, energy_co2eq_day, feed_co2eq_day, sum_co2eq_day)
        names(coef1) <- c("category", "n", "milk_yield", "enteric", "manure", "energy", "feed", "sum") 
        reactable(coef1)

    })
    
    output$plot1 <- renderHighchart({
      coef1 <- coef() %>% 
        select(category, number_of_animals, milk_yield, enteric_co2eq_day, manure_co2eq_day, energy_co2eq_day, feed_co2eq_day, sum_co2eq_day)
      names(coef1) <- c("category", "n", "milk_yield", "enteric", "manure", "energy", "feed", "sum") 
      
      milking <- coef1 %>% 
        filter(category == "milking") %>% 
        select(enteric, manure, energy, feed) %>% 
        pivot_longer(cols = c(enteric, manure, energy, feed), names_to = "name", values_to = "value") %>% 
        list_parse2()
      
      coef1 %>%
        hchart(hcaes(x = category, y = sum, group = category, color = category), type = "column") %>%
        hc_title(text = "사육단계별 온실가스(CO2eq) 배출량")
    })
    
    
    output$plot2 <- renderHighchart({
      
      coef1 <- coef() %>% 
        select(enteric_co2eq_day, manure_co2eq_day, energy_co2eq_day, feed_co2eq_day)
      names(coef1) <- c("enteric", "manure", "energy", "feed")
      
      coef1 %>% pivot_longer(cols = 1:4) %>% 
        group_by(name) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>% 
        hchart(hcaes(x = name, y = value), type = "pie") %>%
        hc_colors(c("#FFCF9D", "#FFB000", "#F5F5DC", "#004225")) %>% 
        #add title
        hc_title(text = "발생원별 온실가스(CO2eq) 배출량")
    
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
