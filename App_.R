# ===================================================================
# App R: Dashboard Urban Heat Island (UHI) dengan Sinkronisasi Model
# ===================================================================

# 1. MEMUAT PUSTAKA
library(shiny)
library(shinythemes)
library(leaflet)
library(dplyr)
library(readr)
library(ggplot2)
library(DT)
library(broom)
library(tidyr)

# 2. MEMUAT SEMUA FILE DATA
tidak_ada_file <- function() stop("Pastikan file dalam folder 'data/' tersedia dan namanya sesuai!")
kepadatan_raw <- tryCatch(read_csv("data/kepadatan.csv"), error = function(e) tidak_ada_file())
ndvi_data <- tryCatch(read_csv("data/ndvi.csv"), error = function(e) tidak_ada_file())
suhu_data <- tryCatch(read_csv("data/suhu.csv"), error = function(e) tidak_ada_file())
tutupan_data <- tryCatch(read_csv("data/tutupan_lahan.csv"), error = function(e) tidak_ada_file())

# 3. MERAPIKAN DATA
kepadatan_long <- kepadatan_raw %>%
    pivot_longer(cols = `2011`:`2024`, names_to = "tahun", values_to = "kepadatan") %>%
    mutate(tahun = as.numeric(tahun)) %>%
    select(Kode_Kab = kodekab, nmkab, tahun, kepadatan)

data_lingkungan <- suhu_data %>%
    left_join(ndvi_data, by = c("Kab_Kota", "Kode_Kab", "provinsi", "X", "Y", "tahun")) %>%
    left_join(tutupan_data, by = c("Kab_Kota", "Kode_Kab", "provinsi", "X", "Y", "tahun"))

combined_data <- data_lingkungan %>%
    left_join(kepadatan_long, by = c("Kode_Kab", "tahun")) %>%
    na.omit() %>%
    mutate(
        landcover = as.numeric(landcover),
        Kab_Kota = as.factor(Kab_Kota)
    )

# ===================================================================
# UI
# ===================================================================
ui <- fluidPage(
    theme = shinytheme("cerulean"),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    navbarPage(
        "Dashboard Urban Heat Island (UHI)",

        # TAB 1
        tabPanel(
            "Peta UHI & Heatmap",
            sidebarLayout(
                sidebarPanel(
                    h3("Visualisasi Spasial UHI"),
                    selectInput("selected_year_map", "Pilih Tahun:",
                        choices = sort(unique(combined_data$tahun)),
                        selected = max(combined_data$tahun)
                    ),
                    downloadButton("downloadDataPeta", "Download Data Peta")
                ),
                mainPanel(leafletOutput("heatmap", height = "600px"))
            )
        ),

        # TAB 2 - Eksplorasi
        tabPanel(
            "Eksplorasi Regresi",
            sidebarLayout(
                sidebarPanel(
                    selectInput("tahunEksplorasi", "Pilih Tahun:", choices = sort(unique(combined_data$tahun)), selected = max(combined_data$tahun)),
                    selectInput("tipeModel", "Pilih Tipe Model:", choices = c("Linier", "Kuadratik", "Kubik"))
                ),
                mainPanel(
                    h4(textOutput("namaModel")),
                    plotOutput("plotEksplorasi")
                )
            )
        ),

        # TAB 3 - Analisis Regresi
        tabPanel(
            "Analisis Regresi",
            sidebarLayout(
                sidebarPanel(
                    h3("Model Regresi Linier"),
                    selectInput("selected_year_model", "Pilih Tahun:", choices = sort(unique(combined_data$tahun)), selected = max(combined_data$tahun)),
                    checkboxGroupInput("model_vars", "Variabel Independen:",
                        choices = c("NDVI" = "ndvi", "Kepadatan Penduduk" = "kepadatan", "Tutupan Lahan" = "landcover"),
                        selected = c("ndvi", "kepadatan", "landcover")
                    )
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel(
                            "Ringkasan Model",
                            verbatimTextOutput("modelSummaryOutput"),
                            h4("Persamaan Regresi"),
                            uiOutput("regressionEquation")
                        ),
                        tabPanel(
                            "Kesesuaian & Diagnostik",
                            div(style = "overflow-x: auto", tableOutput("glanceOutput")),
                            plotOutput("diagnosticPlot")
                        )
                    )
                )
            )
        ),

        # TAB 4 - Statistik
        tabPanel(
            "Data & Statistik",
            tabsetPanel(
                tabPanel("Statistik Deskriptif", verbatimTextOutput("descriptiveStats")),
                tabPanel("Data Mentah", DT::dataTableOutput("rawData"), downloadButton("downloadRawData", "Download Semua Data"))
            )
        )
    )
)

# ===================================================================
# SERVER
# ===================================================================
server <- function(input, output, session) {
    # Reactive: Simpan jenis model dari eksplorasi
    jenis_model <- reactiveVal("Linier")
    observeEvent(input$tipeModel, jenis_model(input$tipeModel))

    # DATA UNTUK PETA
    map_data <- reactive({
        combined_data %>% filter(tahun == input$selected_year_map)
    })

    output$heatmap <- renderLeaflet({
        leaflet(map_data()) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addCircleMarkers(
                lng = ~X, lat = ~Y,
                radius = 6,
                color = ~ colorNumeric("YlOrRd", suhu)(suhu),
                fillOpacity = 0.7,
                popup = ~ paste("<b>", Kab_Kota, "</b><br>", "Suhu:", round(suhu, 2), "<br>NDVI:", round(ndvi, 2), "<br>Kepadatan:", round(kepadatan, 2), "<br>Tutupan:", landcover)
            )
    })

    output$downloadDataPeta <- downloadHandler(
        filename = function() paste0("data_uhi_", input$selected_year_map, ".csv"),
        content = function(file) write.csv(map_data(), file, row.names = FALSE)
    )

    # EKPLORASI VISUAL
    eksplorasi_data <- reactive({
        combined_data %>% filter(tahun == input$tahunEksplorasi)
    })

    output$plotEksplorasi <- renderPlot({
        ggplot(eksplorasi_data(), aes(x = ndvi, y = suhu)) +
            {
                if (input$tipeModel == "Linier") {
                    geom_smooth(method = "lm", formula = y ~ x)
                } else if (input$tipeModel == "Kuadratik") {
                    geom_smooth(method = "lm", formula = y ~ poly(x, 2))
                } else {
                    geom_smooth(method = "lm", formula = y ~ poly(x, 3))
                }
            } +
            geom_point(alpha = 0.4) +
            theme_minimal()
    })

    # NAMA MODEL REGRESI
    output$namaModel <- renderText({
        # Pastikan input$tipeModel tidak kosong untuk menghindari error
        req(input$tipeModel)

        # Menggunakan switch untuk memilih teks yang tepat berdasarkan input
        model_text <- switch(input$tipeModel,
            "Linier"    = "Model Regresi Linier",
            "Kuadratik" = "Model Regresi Kuadratik",
            "Kubik"     = "Model Regresi Kubik"
        )

        # Mengembalikan teks yang sudah dipilih
        return(model_text)
    })

    # MODEL REGRESI MENGIKUTI EKSPLORASI
    model_data <- reactive({
        combined_data %>% filter(tahun == input$selected_year_model)
    })

    model_fit <- reactive({
        req(input$model_vars)
        vars <- input$model_vars
        tipe <- jenis_model()

        predictors <- paste(vars, collapse = " + ")

        formula_text <- switch(tipe,
            "Linier" = paste("suhu ~", predictors),
            "Kuadratik" = paste("suhu ~", paste0("poly(", vars, ",2)", collapse = " + ")),
            "Kubik" = paste("suhu ~", paste0("poly(", vars, ",3)", collapse = " + "))
        )
        lm(as.formula(formula_text), data = model_data())
    })

    # OUTPUT MODEL REGRESI
    output$modelSummaryOutput <- renderPrint({
        summary(model_fit())
    })

    output$glanceOutput <- renderTable({
        glance(model_fit())
    })

    output$diagnosticPlot <- renderPlot({
        par(mfrow = c(2, 2))
        plot(model_fit())
        par(mfrow = c(1, 1))
    })

    # PERSAMAAN REGRESI
    output$regressionEquation <- renderUI({
        mod <- tidy(model_fit())
        eq <- paste0("suhu = ", round(mod$estimate[1], 3))
        for (i in 2:nrow(mod)) {
            est <- round(mod$estimate[i], 3)
            sign <- ifelse(est >= 0, " + ", " - ")
            eq <- paste0(eq, sign, abs(est), "*", mod$term[i])
        }
        HTML(paste0("<code>", eq, "</code>"))
    })

    # STATISTIK DESKRIPTIF & DATA MENTAH
    output$descriptiveStats <- renderPrint({
        summary(combined_data)
    })

    output$rawData <- DT::renderDataTable({
        datatable(combined_data, options = list(pageLength = 10, scrollX = TRUE))
    })

    output$downloadRawData <- downloadHandler(
        filename = function() "semua_data_uhi.csv",
        content = function(file) write.csv(combined_data, file, row.names = FALSE)
    )
}

# JALANKAN APP
shinyApp(ui, server)
