library(shiny)
library(tidyverse)
library(reactable)
library(hanwoo)
library(sparkline)
library(shinycssloaders)
library(highcharter)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("한우 거세우 온실가스 산출 프로토타입"),
  
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          width = 2,
          tabsetPanel(
            tabPanel("이력제정보",
                     textInput("cattle", label = "Cattle ID:", value = "002116755679"),
                     numericInput("calving_bw", label = "생시체중(kg)", value = 25),
                     actionButton("go", label = "Calcultate")         
                     ),
            tabPanel("사료정보",
                     numericInput("tdn_calf", label = "송아지 TDN (%)", value = 69.4),
                     numericInput("tdn_growing", label = "육성우 TDN (%)", value = 68.7),
                     numericInput("tdn_early_fattening", label = "비육전기 TDN (%)", value = 70.8),
                     numericInput("tdn_late_fattening", label = "비육후기 TDN (%)", value = 72.7),
                     numericInput("feed_co2eq_total", label = "Total Feed CO2eq", value = 13282.84)
            ),
            tabPanel("에너지",
                     numericInput("farm_head", label = "총사육두수", value = 1),
                     numericInput("co2eq_electr", label = "연간 농장 전기사용량(Kwh/year)", value = 640),
                     numericInput("co2eq_gasoline", label = "연간 농장 가솔린 사용량(L/year)", value = 0),
                     numericInput("co2eq_disel", label = "연간 농장 디젤 사용량(L/year)", value = 20.4),
                     numericInput("ef_electr", label = "Electr EF (kg co2eq/KWh)", value = 0.4954),
                     numericInput("ef_gasoline", label = "Gasoline EF (kg co2eq/L)", value = 2.178878),
                     numericInput("ef_co2_disel", label = "Disel EF (kg co2eq/L)", value = 2.596127),
                     numericInput("ef_n2o_disel", label = "Disel EF (kg n2o/L)", value = 0.001007),
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
            column(8, plotOutput("plot") %>% shinycssloaders::withSpinner(type = 8)),
            column(4, highchartOutput("pie") %>% shinycssloaders::withSpinner(type = 8))
          ),
          br(),
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

    # hanwoo_key setting
    key_encoding <- "PLEASE_ADD_YOUR_API_KEY_FROM_DATA.GO.KR"
    key_decoding <- "PLEASE_ADD_YOUR_API_KEY_FROM_DATA.GO.KR"
    
    # function
    hanwoo_ghg <- function(cattle, key_encoding, key_decoding){
        
        # import api data
        hanwoo <- hanwoo_info(cattle, key_encoding, key_decoding)
        weight <- hanwoo$quality_info$weight/0.6 %>% as.numeric()
        month <- hanwoo$quality_info$birthmonth %>% as.numeric()
        day <- month*(365/12)
        breed <- hanwoo$quality_info$judgeBreedNm
        sex <- hanwoo$quality_info$judgeSexNm
        
        # coefficient
        ch4_gwp <- input$ch4_gwp %>% as.numeric()
        n2o_gwp <- input$n2o_gwp %>% as.numeric()
        cfi <- input$cfi
        ge_mcal_per_kg <- 4.32
        bw_calving_kg <- input$calving_bw
        adg = (weight-bw_calving_kg) / day
        energy_kg_co2eq_head_per_day <- 375/365
        feed_co2eq_day <- input$feed_co2eq_total / day
        
        # coefficient2 (energy)
        farm_head <- input$farm_head %>% as.numeric()
        co2eq_electr <- input$co2eq_electr %>% as.numeric()
        co2eq_gasoline <- input$co2eq_gasoline %>% as.numeric()
        co2eq_disel <- input$co2eq_disel %>% as.numeric()
        ef_electr <- input$ef_electr %>% as.numeric()
        ef_gasoline <- input$ef_gasoline %>% as.numeric()
        ef_co2_disel <- input$ef_co2_disel %>% as.numeric()
        ef_n2o_disel <- input$ef_n2o_disel %>% as.numeric()
        
        energy_kg_co2eq_head_per_day <- ((co2eq_electr*ef_electr + co2eq_gasoline*ef_gasoline + co2eq_disel*ef_co2_disel + co2eq_disel*ef_n2o_disel)/farm_head)/365
        
        
        # mean tdn
        if(sex == "거세") {
            # tdn
            coef <- tibble(
                cattle = cattle, 
                category = c("calf", "growing", "early fattening", "late fattening"),
                tdn_percent = c(input$tdn_calf, input$tdn_growing, input$tdn_early_fattening, input$tdn_late_fattening)
            )
            # tdn to de_percent
            coef <- coef %>% mutate(de_percent = ((tdn_percent * 0.04409) / ge_mcal_per_kg) * 100)
            # Ym
            coef <- coef %>% mutate(Ym = ifelse(de_percent >= 72, 4, 6.3))
            # REM & REG
            coef <- coef %>% 
                mutate(
                    rem = (1.123-4.092*(10^(-3))*de_percent) + (1.126*10^(-5))*(de_percent^2)-(25.4/de_percent),
                    reg = (1.164-(0.00516*de_percent)) + (0.00001308*(de_percent^2)-(37.4/de_percent))
                )
            
            # BW
            coef <- coef %>% mutate(matureBW = weight, adg = adg)
            bw_kg <- c(bw_calving_kg + (adg * 2.5 * (365/12)), 
                       bw_calving_kg + (adg * 9.5 * (365/12)),
                       bw_calving_kg + (adg * 17.5 * (365/12)),
                       bw_calving_kg + (adg * (22 + (month-22)/2) * (365/12))
            )
            coef <- coef %>% mutate(bw_kg = bw_kg)
            
            #NEM & NEG
            coef <- coef %>% mutate(
                nem = bw_kg^0.75 * cfi,
                neg = 22.02 * (bw_kg/matureBW)^0.75 * adg^1.097
            )
            
            #GE
            coef <- coef %>% mutate(ge = (nem/rem) + (neg/reg) / (de_percent/100))
            
            # tier2
            days_of_growing <- c(6*(365/12), 8*(365/12), 8*(365/12), (month-22)*(365/12))
            
            coef <- coef %>% mutate(enteric_ch4_kg_head_per_day = (ge * (Ym/100))/55.65) 
            coef <- coef %>% mutate(days = days_of_growing)
            coef <- coef %>% mutate(enteric_ch4_kg_head_per_growing = enteric_ch4_kg_head_per_day * days)
            coef <- coef %>% mutate(enteric_co2eq_cumsum = cumsum(enteric_ch4_kg_head_per_growing*ch4_gwp))
            
            # carcass traits
            coef <- coef %>% mutate(qgrade = hanwoo$quality_info$qgrade[1])
            coef <- coef %>% mutate(wgrade = hanwoo$quality_info$wgrade[1])
            coef <- coef %>% mutate(grade = paste0(qgrade, wgrade))
            coef <- coef %>% mutate(rea = hanwoo$quality_info$rea[1])
            coef <- coef %>% mutate(backfat = hanwoo$quality_info$backfat[1])

            
            # co2 from feed
            coef <- coef %>% 
              mutate(
                feed_co2eq_kg_head_per_growing = feed_co2eq_day * days,
                feed_co2eq_cumsum = cumsum(feed_co2eq_kg_head_per_growing)
                )
            
            # ch4 from manure
            ash_percent <- c(9.33, 9.79, 9.96, 10)
            awms <- 1.0
            b0 <- 0.18
            mcf_percent <- 4
            
            coef <- coef %>% mutate(
                ue_ge = 0.04*ge,
                ash_percent = ash_percent,
                vs_kg_per_day_per_animal = ((ge*(1-(de_percent/100))+(ue_ge/100))*(((1-ash_percent/100)/18.45))), # 18.45..? 평균 VS 함량?
                manure_ch4_kg_head_per_growing = (vs_kg_per_day_per_animal*days)*(b0*0.67*(mcf_percent/100)*awms),
                manure_co2eq_ch4_cumsum = cumsum(manure_ch4_kg_head_per_growing*ch4_gwp)
            )
            
            # direct n2o from manure
            cp_percent <- c(16.3, 14.1, 13.7, 13.5)
            ncdg <- 0
            ef3 <- 0.01
            
            coef <- coef %>% mutate(n_intake_kg_n_per_animal_per_day = (ge/18.45)*((cp_percent/100)/(6.25)))
            coef <- coef %>% mutate(n_retention_kg_n_per_animal_per_day = ((adg*(((268-((7.03*neg)/adg))/1000))/6.25)))
            coef <- coef %>% mutate(nex_kg_per_day = n_intake_kg_n_per_animal_per_day - n_retention_kg_n_per_animal_per_day)
            coef <- coef %>% mutate(n2o_direct_emmision_head_per_growing = (((nex_kg_per_day*awms)+ncdg)*ef3)*(44/28)*days)
            coef <- coef %>% mutate(manure_co2eq_direct_n2o_cumsum = cumsum(n2o_direct_emmision_head_per_growing)*n2o_gwp)
            
            # indirenct n2o from manure
            frac_gas_mt <- 0.22
            ef4 <- 0.01
            frac_leaching <- 0
            ef5 <- 0.0075
            
            coef <- coef %>% mutate(n_volatilization_kg_day = ((((nex_kg_per_day)*awms)+ncdg)*frac_gas_mt))
            coef <- coef %>% mutate(n2o_from_volatilization_kg_per_day = (n_volatilization_kg_day*ef4)*44/28)
            coef <- coef %>% mutate(n2o_indirect_emmision_head_per_growing = n2o_from_volatilization_kg_per_day*day)
            coef <- coef %>% mutate(manure_co2eq_indirect_n2o_cumsum = cumsum(n2o_indirect_emmision_head_per_growing)*n2o_gwp)
            
            
            # energy
            coef <- coef %>% mutate(energy_kg_co2eq_head_per_growing = days*energy_kg_co2eq_head_per_day)
            coef <- coef %>% mutate(energy_co2eq_cumsum = cumsum(energy_kg_co2eq_head_per_growing))
            
            
            coef_selected <- coef %>% select(cattle, category, matureBW, bw_kg, ends_with("cumsum")) %>% 
                mutate(sum = enteric_co2eq_cumsum + 
                         feed_co2eq_cumsum +
                         manure_co2eq_ch4_cumsum + 
                         manure_co2eq_direct_n2o_cumsum + 
                         manure_co2eq_indirect_n2o_cumsum + 
                         energy_co2eq_cumsum)

            # summary
            coef <- coef %>% 
                mutate(sum = enteric_co2eq_cumsum +
                         feed_co2eq_cumsum +
                         manure_co2eq_ch4_cumsum + 
                         manure_co2eq_direct_n2o_cumsum + 
                         manure_co2eq_indirect_n2o_cumsum + 
                         energy_co2eq_cumsum)
            co2eq <- coef_selected %>% slice(4) %>% pull(sum) %>% sum()
            co2eq_per_meat <-  round(co2eq/(hanwoo$quality_info$weight/0.6), 2)
            
        }
        
        return(coef)
    }
    
    coef <- reactive({hanwoo_ghg(input$cattle, key_encoding, key_decoding) })
    
    output$message <- renderText({
      
      
      paste0(
        input$cattle, " 한우의 생애주기 탄소배출량은 총 ", round(coef()$sum[[4]], 1), "kg CO2eq 입니다.",
        "\n", "이 한우의 생체중은 약 ", round(coef()$matureBW[[1]], 1), "kg 입니다(도체율 60% 기준). 따라서 kg 당 탄소배출량은 약 ", round(coef()$sum[[4]]/coef()$matureBW[[1]], 1), "kg CO2eq 로 예상됩니다."
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
        input$go
        coef() %>% select(cattle, category, matureBW, bw_kg, days, ends_with("cumsum")) %>% 
            mutate(sum = enteric_co2eq_cumsum + 
                     feed_co2eq_cumsum +
                     manure_co2eq_ch4_cumsum + 
                     manure_co2eq_direct_n2o_cumsum + 
                     manure_co2eq_indirect_n2o_cumsum + 
                     energy_co2eq_cumsum,
                   manure = manure_co2eq_ch4_cumsum + manure_co2eq_direct_n2o_cumsum + manure_co2eq_indirect_n2o_cumsum
                   ) %>% 
            rename(
              enteric = enteric_co2eq_cumsum,
              feed = feed_co2eq_cumsum,
              energy = energy_co2eq_cumsum
            ) %>% 
            select(cattle, category, days, bw_kg, enteric, feed, manure, energy, sum) %>%
            reactable(resizable = TRUE, 
                      striped = TRUE,
                      defaultPageSize = 50,
                      defaultColDef = colDef(
                          headerStyle = list(background = "#f7f7f8"),
                          format = colFormat(digits = 1),
                          footer = function(values) {
                              if (!is.numeric(values)) return()
                              sparkline(values, type = "box", width = 100, height = 30)
                          }
                      ),
                      columns = list(
                        cattle = colDef(cell = function(value) {
                          htmltools::tags$a(href = paste0("https://mtrace.go.kr/mtracesearch/cattleNoSearch.do?btsProgNo=0109008401&btsActionMethod=SELECT&cattleNo=", value), target = "_blank", value)
                        })
                      )
            )
        
    })
    
    
    output$plot <- renderPlot({
        
        input$go
        coef() %>% 
            group_by(cattle) %>% 
            mutate(
                days_of_growing = cumsum(days),
                cumsum = round(sum, 1),
                carcassBW = round(matureBW*0.6, 0),
                month = sum(days)/(365/12),
                sum = max(sum),
                per_beef_kg = round(sum/(matureBW*0.6), 2),
                enteric = round(max(enteric_co2eq_cumsum), 2),
                feed = round(max(feed_co2eq_cumsum), 2),
                manure = round(max(manure_co2eq_ch4_cumsum + manure_co2eq_direct_n2o_cumsum + manure_co2eq_indirect_n2o_cumsum), 2),
                energy = round(max(energy_co2eq_cumsum), 2)
            ) %>% 
            ungroup() %>% 
            ggplot(aes(days_of_growing, cumsum)) +
            geom_area(aes(fill = per_beef_kg), fill = input$color) +
            geom_point(size = 4) +
            geom_line(size = 1) +
            geom_text(aes(label = cumsum, family = "NanumGothic"), color = "grey30", size = 5, vjust = 2) +
            geom_text(aes(x = 150, y = 12000, hjust = 0, label = paste0("Carcass Weight = ", carcassBW, "kg", "\n", "Month = ", month, "\n", "kg CO2eq/kg Beef = ", per_beef_kg, "\n", "Grade = ", grade, "\n", "REA = ", rea, " cm2", "\n", "Backfat = ", backfat, " mm"))) +
            geom_text(aes(x = 600, y = 2550, hjust = 0, 
                          label = paste0("Enteric = ", enteric, "\n", "Feed = ", feed, "\n", "Manure = ", manure, "\n", "Energy = ", energy)), color = "grey30") +
            ggthemes::theme_clean(base_family = "NanumGothic") +
            facet_wrap(.~cattle) +
            labs(
                title = "한우 거세우 개체별 온실가스 배출량 평가 결과", 
                # subtitle = "(장내배출 + 분뇨 + 에너지 유래 온실가스)",
                caption = "*축산물이력제 및 품질평가 API 데이터 기반 추정치",
                x = "Days of growing", y = "Cumulated CO2eq kg/head"
            ) +
            theme(
                legend.position = "right"
            )
        
    })
    
    output$pie <- renderHighchart({
      
      input$go
      coef() %>% select(cattle, category, ends_with("cumsum")) %>% 
        filter(category == "late fattening") %>% 
        mutate(
          enteric = enteric_co2eq_cumsum, 
          feed = feed_co2eq_cumsum,
          manure = manure_co2eq_ch4_cumsum + manure_co2eq_direct_n2o_cumsum + manure_co2eq_indirect_n2o_cumsum,
          energy = energy_co2eq_cumsum
        ) %>% 
        select(cattle, category, enteric:energy) %>% 
        pivot_longer(cols = enteric:energy, names_to = "from") %>% 
        hchart("pie", hcaes(x = from, y = value)) %>% 
        hc_colors(c("#FFCF9D", "#FFB000", "#F5F5DC", "#004225"))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
