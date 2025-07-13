# ===================================================================
# APLIKASI SHINY: DASHBOARD URBAN HEAT ISLAND (UHI)
# Versi Final dengan Dokumentasi Lengkap
#
# Penjelasan:
# Aplikasi ini dirancang untuk menganalisis dan memvisualisasikan data
# Urban Heat Island. Fitur utamanya meliputi:
# 1. Peta interaktif dengan pewarnaan dinamis (suhu/tutupan lahan).
# 2. Analisis eksplorasi hubungan bivariat dengan visualisasi cerdas.
# 3. Model regresi linier berganda yang valid secara statistik.
# 4. Pop-up video tutorial untuk pengguna baru.
# 5. Tabel data mentah yang interaktif dan dapat diunduh.
# ===================================================================


# ===================================================================
# 1. PENYIAPAN (SETUP)
# ===================================================================
# Penjelasan:
# Memuat semua pustaka (packages) yang dibutuhkan oleh aplikasi.
# Pastikan semua pustaka ini sudah terinstal di lingkungan R Anda.
# -------------------------------------------------------------------

library(shiny) # Kerangka kerja utama aplikasi web
library(shinythemes) # Tema visual untuk mempercantik UI
library(leaflet) # Untuk membuat peta interaktif
library(dplyr) # Untuk manipulasi dan transformasi data
library(readr) # Untuk membaca file CSV dengan cepat dan efisien
library(ggplot2) # Untuk membuat plot statistik yang indah dan fleksibel
library(DT) # Untuk menampilkan tabel data yang interaktif
library(broom) # Untuk merapikan output model statistik
library(tidyr) # Untuk merapikan data (misal: pivot_longer)
library(lmtest) # Untuk uji Breusch-Pagan, Durbin-Watson
library(car) # Untuk uji VIF (Multikolinearitas)


# ===================================================================
# 2. MEMUAT & MEMBERSIHKAN DATA (DATA LOADING & CLEANING)
# ===================================================================
# Penjelasan:
# Bagian ini bertanggung jawab untuk memuat semua file data mentah,
# melakukan transformasi (seperti reshaping), menggabungkan data,
# dan membersihkannya agar siap untuk dianalisis dan divisualisasikan.
# -------------------------------------------------------------------

# Memuat 4 file data utama. Aplikasi akan berhenti jika salah satu file tidak ditemukan.
tryCatch(
    {
        kepadatan_raw <- read_csv("data/kepadatan.csv")
        ndvi_data <- read_csv("data/ndvi.csv")
        suhu_data <- read_csv("data/suhu.csv")
        tutupan_data <- read_csv("data/tutupan_lahan.csv")
    },
    error = function(e) {
        stop("KRITIS: Gagal memuat file CSV. Pastikan semua file (kepadatan, ndvi, suhu, tutupan_lahan) ada di dalam folder 'data/'.")
    }
)

# Mengubah data 'kepadatan' dari format 'wide' ke 'long' agar bisa digabung berdasarkan tahun.
kepadatan_long <- kepadatan_raw %>%
    pivot_longer(cols = `2011`:`2024`, names_to = "tahun", values_to = "kepadatan") %>%
    mutate(tahun = as.numeric(tahun)) %>%
    select(Kode_Kab = kodekab, nmkab, tahun, kepadatan)

# Menggabungkan data suhu, ndvi, dan tutupan lahan yang sudah memiliki format 'long'.
data_lingkungan <- suhu_data %>%
    left_join(ndvi_data, by = c("Kab_Kota", "Kode_Kab", "provinsi", "X", "Y", "tahun")) %>%
    left_join(tutupan_data, by = c("Kab_Kota", "Kode_Kab", "provinsi", "X", "Y", "tahun"))

# Menggabungkan semua data menjadi satu dataset utama dan melakukan pembersihan akhir.
combined_data <- data_lingkungan %>%
    left_join(kepadatan_long, by = c("Kode_Kab", "tahun")) %>%
    na.omit() %>% # Menghapus baris yang memiliki nilai kosong (NA)
    mutate(
        Kab_Kota = as.factor(Kab_Kota),

        # Perlakuan variabel 'landcover' berdasarkan informasi bahwa nilainya adalah indeks rata-rata.
        # Ini adalah langkah yang paling penting secara statistik.
        # Langkah 1: Bulatkan nilai indeks ke angka integer terdekat untuk menentukan kategori dominan.
        # Langkah 2: Ubah hasil pembulatan menjadi 'factor' agar R menganggapnya sebagai KATEGORI, bukan angka.
        # Langkah 3: Beri label pada setiap level faktor agar output lebih mudah dibaca.
        landcover = recode_factor(round(landcover, 0),
            `0` = "Tidak.Terkasifikasi",
            `1` = "Hutan.Tropis",
            `2` = "Hutan.Sedang",
            `3` = "Hutan.Konifer",
            `4` = "Semak.Belukar",
            `5` = "Padang.Rumput",
            `6` = "Lahan.Basah/Padi",
            `7` = "Pertanian",
            `8` = "Perkebunan",
            `9` = "Area.Terbangun",
            `10` = "Tanah.Kosong",
            `11` = "Gurun/Pasir",
            `12` = "Salju/Es",
            `13` = "Perairan",
            `14` = "Awan/Bayangan"
        )
    )

# Palet warna untuk tutupan lahan. Nama harus cocok persis dengan label di atas.
land_cover_colors <- c(
    "Tidak.Terkasifikasi" = "#FFFFFF",
    "Hutan.Tropis"        = "#006400",
    "Hutan.Sedang"        = "#228B22",
    "Hutan.Konifer"       = "#008000",
    "Semak.Belukar"       = "#6B8E23",
    "Padang.Rumput"       = "#7CFC00",
    "Lahan.Basah/Padi"    = "#ADFF2F",
    "Pertanian"           = "#FFFF00",
    "Perkebunan"          = "#DAA520",
    "Area.Terbangun"      = "#FF0000",
    "Tanah.Kosong"        = "#D2B48C",
    "Gurun/Pasir"         = "#FFE4B5",
    "Salju/Es"            = "#F0F8FF",
    "Perairan"            = "#00BFFF",
    "Awan/Bayangan"       = "#D3D3D3"
)


# ===================================================================
# 3. ANTARMUKA PENGGUNA (USER INTERFACE - UI)
# ===================================================================
# Penjelasan:
# Bagian ini mendefinisikan seluruh tata letak dan tampilan visual
# dari aplikasi Shiny. Semua elemen input (tombol, slider, dropdown) dan
# elemen output (plot, peta, tabel) dideklarasikan di sini.
# -------------------------------------------------------------------

ui <- fluidPage(
    theme = shinytheme("cerulean"),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    navbarPage(
        "Dashboard Urban Heat Island (UHI)",

        # TAB 1: Peta Interaktif
        tabPanel(
            "Peta UHI",
            sidebarLayout(
                sidebarPanel(
                    h3("Visualisasi Spasial UHI"),
                    p("Peta ini menampilkan distribusi titik data secara geografis."),
                    selectInput("selected_year_map", "Pilih Tahun:", choices = sort(unique(combined_data$tahun)), selected = max(combined_data$tahun)),
                    radioButtons("map_color_by", "Warnai Peta Berdasarkan:",
                        choices = c("Suhu Permukaan" = "suhu", "Tutupan Lahan" = "landcover"),
                        selected = "suhu"
                    ),
                    downloadButton("downloadDataPeta", "Download Data Peta (.csv)")
                ),
                mainPanel(leafletOutput("heatmap", height = "70vh"))
            )
        ),

        # TAB 2: Eksplorasi Hubungan
        tabPanel(
            "Eksplorasi Hubungan",
            sidebarLayout(
                sidebarPanel(
                    h3("Eksplorasi Hubungan Bivariat"),
                    p("Visualisasikan hubungan antara satu variabel independen (X) dengan suhu (Y)."),
                    selectInput("year_explore", "Pilih Tahun:", choices = sort(unique(combined_data$tahun)), selected = max(combined_data$tahun)),
                    selectInput("x_var", "Pilih Variabel Independen (X):", choices = c("NDVI" = "ndvi", "Kepadatan Penduduk" = "kepadatan", "Tutupan Lahan" = "landcover")),
                    # UI untuk pilihan model akan muncul secara dinamis dari server
                    uiOutput("model_type_ui")
                ),
                mainPanel(
                    plotOutput("explorePlot"),
                    br(),
                    h4("Perbandingan Statistik Model"),
                    tableOutput("compareModelStats")
                )
            )
        ),

        # TAB 3: Analisis Regresi Linier Berganda
        tabPanel(
            "Analisis Regresi",
            sidebarLayout(
                sidebarPanel(
                    h3("Model Regresi Linier Berganda"),
                    p("Bangun model untuk menjelaskan Suhu (LST) berdasarkan beberapa prediktor."),
                    selectInput("selected_year_model", "Pilih Tahun:", choices = sort(unique(combined_data$tahun)), selected = max(combined_data$tahun)),
                    checkboxGroupInput("model_vars", "Pilih Variabel Independen:", choices = c("NDVI" = "ndvi", "Kepadatan Penduduk" = "kepadatan", "Tutupan Lahan" = "landcover"), selected = c("ndvi", "kepadatan", "landcover"))
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel(
                            "Ringkasan Model",
                            verbatimTextOutput("modelSummaryOutput"),
                            hr(),
                            h4("Persamaan Regresi"),
                            uiOutput("regressionEquation")
                        ),
                        tabPanel(
                            "Kesesuaian & Diagnostik",
                            h4("Statistik Kesesuaian Model"),
                            div(style = "overflow-x: auto;", tableOutput("glanceOutput")),
                            hr(),
                            h4("Uji Statistik Asumsi Klasik"),
                            verbatimTextOutput("assumptionTestsOutput"),
                            hr(),
                            h4("Plot Diagnostik Asumsi Klasik"),
                            plotOutput("diagnosticPlot", height = "600px")
                        )
                    )
                )
            )
        ),

        # TAB 4: Data & Statistik Deskriptif
        tabPanel(
            "Data & Statistik",
            tabsetPanel(
                tabPanel("Statistik Deskriptif", verbatimTextOutput("descriptiveStats")),
                tabPanel("Data Mentah", DT::dataTableOutput("rawData"), downloadButton("downloadRawData", "Download Semua Data (.csv)"))
            )
        )
    )
)

# ===================================================================
# 4. LOGIKA SERVER (SERVER LOGIC)
# ===================================================================
# Penjelasan:
# Ini adalah "otak" dari aplikasi. Semua perhitungan, analisis,
# pembuatan plot, dan logika interaktif terjadi di sini. Server menerima
# input dari UI, memprosesnya, dan mengirimkan kembali outputnya.
# -------------------------------------------------------------------

server <- function(input, output, session) {
    ## BAGIAN POP-UP TUTORIAL
    # Penjelasan: Menampilkan pop-up video tutorial saat aplikasi pertama kali dimuat.
    # `once = TRUE` memastikan ini hanya berjalan satu kali per sesi pengguna.
    # PENTING: Ganti link `src` di bawah dengan link 'embed' dari video YouTube Anda.
    observeEvent(TRUE,
        {
            showModal(modalDialog(
                title = "Selamat Datang! Tonton Tutorial Singkat Ini",
                HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/Lsv4wg9YU8Y?si=jgcfsOx0pc3cMUI5&amp;controls=0" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>'),
                easyClose = TRUE,
                footer = modalButton("Tutup dan Mulai Eksplorasi")
            ))
        },
        once = TRUE
    )


    ## BAGIAN DATA REACTIVE
    # Penjelasan: Membuat subset data berdasarkan input pengguna. Ini lebih efisien
    # karena data hanya difilter ulang saat input yang relevan berubah.
    # Input: Pilihan tahun dari masing-masing tab.
    # Output: Data frame yang sudah terfilter untuk digunakan di plot, peta, dan model.
    map_data <- reactive({
        combined_data %>% filter(tahun == input$selected_year_map)
    })
    model_data <- reactive({
        combined_data %>% filter(tahun == input$selected_year_model)
    })
    explore_data <- reactive({
        req(input$year_explore, input$x_var)
        combined_data %>%
            filter(tahun == input$year_explore) %>%
            select(suhu, x_var = !!sym(input$x_var)) # `!!sym()` adalah cara canggih untuk menggunakan string sebagai nama kolom
    })


    ## BAGIAN PETA INTERAKTIF (TAB 1)
    output$heatmap <- renderLeaflet({
        req(map_data(), input$map_color_by)
        base_map <- leaflet(map_data()) %>% addProviderTiles(providers$CartoDB.Positron)

        if (input$map_color_by == "suhu") {
            pal <- colorNumeric("YlOrRd", domain = map_data()$suhu)
            base_map %>%
                addCircleMarkers(
                    lng = ~X, lat = ~Y, radius = 7, color = ~ pal(suhu), stroke = FALSE, fillOpacity = 0.7,
                    popup = ~ paste("<b>Suhu:</b>", round(suhu, 2), "°C<br><b>Tutupan:</b>", landcover)
                ) %>%
                addLegend("bottomright", pal = pal, values = ~suhu, title = "Suhu (°C)", opacity = 1)
        } else {
            pal <- colorFactor(palette = land_cover_colors, domain = map_data()$landcover)
            base_map %>%
                addCircleMarkers(
                    lng = ~X, lat = ~Y, radius = 7, color = ~ pal(landcover), stroke = FALSE, fillOpacity = 0.7,
                    popup = ~ paste("<b>Suhu:</b>", round(suhu, 2), "°C<br><b>Tutupan:</b>", landcover)
                ) %>%
                addLegend("bottomright", pal = pal, values = ~landcover, title = "Tutupan Lahan", opacity = 1)
        }
    })
    output$downloadDataPeta <- downloadHandler(
        filename = function() paste0("data_peta_uhi_", input$selected_year_map, ".csv"),
        content = function(file) write.csv(map_data(), file, row.names = FALSE)
    )


    ## BAGIAN EKSPLORASI HUBUNGAN (TAB 2)
    # Penjelasan: Bagian ini membuat visualisasi dan perbandingan model secara dinamis
    # berdasarkan pilihan variabel X oleh pengguna.
    output$model_type_ui <- renderUI({
        req(explore_data())
        if (is.numeric(explore_data()$x_var)) {
            checkboxGroupInput("model_type", "Tampilkan Garis Model:", choices = c("Linier", "Kuadratik", "Kubik"), selected = "Linier", inline = TRUE)
        } else {
            p("Plot Boxplot ditampilkan untuk variabel kategorikal.")
        }
    })

    output$explorePlot <- renderPlot({
        df <- explore_data()
        req(df)
        if (is.numeric(df$x_var)) { # Scatterplot untuk variabel numerik
            p <- ggplot(df, aes(x = x_var, y = suhu)) +
                geom_point(alpha = 0.5, color = "steelblue") +
                labs(x = input$x_var, y = "Suhu (°C)", title = paste("Hubungan Suhu vs", input$x_var)) +
                theme_minimal(base_size = 14)
            if (!is.null(input$model_type)) {
                if ("Linier" %in% input$model_type) p <- p + geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#28a745", linetype = "dashed")
                if ("Kuadratik" %in% input$model_type) p <- p + geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "#fd7e14")
                if ("Kubik" %in% input$model_type) p <- p + geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), se = FALSE, color = "#dc3545")
            }
            print(p)
        } else { # Boxplot untuk variabel kategorikal/faktor
            ggplot(df, aes(x = x_var, y = suhu)) +
                geom_boxplot(aes(fill = x_var), show.legend = FALSE, alpha = 0.8) +
                geom_jitter(width = 0.1, alpha = 0.2) +
                labs(x = input$x_var, y = "Suhu (°C)", title = paste("Distribusi Suhu berdasarkan", input$x_var)) +
                theme_minimal(base_size = 14)
        }
    })

    output$compareModelStats <- renderTable(
        {
            df <- explore_data()
            req(df)
            if (!is.numeric(df$x_var) || is.null(input$model_type)) {
                return(NULL)
            }
            stats <- data.frame()
            y <- df$suhu
            x <- df$x_var
            if ("Linier" %in% input$model_type) {
                m <- lm(y ~ x)
                stats <- rbind(stats, data.frame(Model = "Linier", R.squared = summary(m)$r.squared, AIC = AIC(m)))
            }
            if ("Kuadratik" %in% input$model_type) {
                m <- lm(y ~ x + I(x^2))
                stats <- rbind(stats, data.frame(Model = "Kuadratik", R.squared = summary(m)$r.squared, AIC = AIC(m)))
            }
            if ("Kubik" %in% input$model_type) {
                m <- lm(y ~ x + I(x^2) + I(x^3))
                stats <- rbind(stats, data.frame(Model = "Kubik", R.squared = summary(m)$r.squared, AIC = AIC(m)))
            }
            if (nrow(stats) > 0) stats else NULL
        },
        striped = TRUE,
        hover = TRUE,
        bordered = TRUE,
        width = "100%"
    )


    ## BAGIAN ANALISIS REGRESI (TAB 3)
    # Penjelasan: Ini adalah inti dari analisis statistik. Membangun model regresi linier berganda
    # berdasarkan variabel yang dipilih pengguna dan menyajikan hasilnya.
    model_fit <- reactive({
        req(input$model_vars, nrow(model_data()) > 1) # Memastikan ada variabel & data
        formula_text <- paste("suhu ~", paste(input$model_vars, collapse = " + "))
        lm(as.formula(formula_text), data = model_data())
    })

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

    output$regressionEquation <- renderUI({
        tidy_model <- tidy(model_fit())
        eq <- paste0("suhu = ", round(tidy_model$estimate[1], 4))
        for (i in 2:nrow(tidy_model)) {
            term <- tidy_model$term[i]
            estimate <- round(tidy_model$estimate[i], 4)
            sign <- ifelse(estimate >= 0, " + ", " - ")
            eq <- paste0(eq, sign, abs(estimate), " * ", term)
        }
        HTML(paste0("<code>", eq, "</code>"))
    })


    ## BAGIAN DATA & STATISTIK (TAB 4)
    # Penjelasan: Menyediakan akses ke data mentah dan statistik deskriptif dasarnya.
    output$descriptiveStats <- renderPrint({
        summary(combined_data)
    })
    output$rawData <- DT::renderDataTable({
        DT::datatable(combined_data, options = list(pageLength = 10, scrollX = TRUE), filter = "top")
    })
    output$downloadRawData <- downloadHandler(
        filename = function() "semua_data_uhi.csv",
        content = function(file) write.csv(combined_data, file, row.names = FALSE)
    )
}

## BAGIAN UJI STATISTIK INFERENSIA ASUMSI KLASIK
# Penjelasan: Blok ini menjalankan beberapa uji hipotesis formal untuk memeriksa
# asumsi-asumsi penting dari model regresi linier. Hasilnya disajikan dengan
# interpretasi untuk memudahkan pemahaman.
# Input: model_fit()
# Output: Teks terformat yang berisi hasil dan interpretasi uji statistik.
output$assumptionTestsOutput <- renderPrint({
    # Pastikan model sudah ada sebelum menjalankan tes
    req(model_fit())
    model <- model_fit()

    # --- 1. Uji Normalitas Residual (Shapiro-Wilk) ---
    # H0: Residual berdistribusi normal.
    # Jika p-value < 0.05, kita menolak H0, artinya asumsi normalitas TIDAK terpenuhi.
    cat("--- 1. Uji Normalitas Residual (Shapiro-Wilk) ---\n")
    sw_test <- shapiro.test(residuals(model))
    print(sw_test)
    cat("\nInterpretasi:\n")
    cat("H0: Residual berdistribusi normal.\n")
    if (sw_test$p.value < 0.05) {
        cat("Hasil: p-value < 0.05. Asumsi normalitas TIDAK terpenuhi.\n\n")
    } else {
        cat("Hasil: p-value >= 0.05. Asumsi normalitas terpenuhi.\n\n")
    }

    # --- 2. Uji Homoskedastisitas (Breusch-Pagan) ---
    # H0: Varians residual adalah konstan (homoskedastisitas).
    # Jika p-value < 0.05, kita menolak H0, artinya terjadi heteroskedastisitas.
    cat("-- 2. Uji Homoskedastisitas (Breusch-Pagan) --\n")
    bp_test <- bptest(model)
    print(bp_test)
    cat("\nInterpretasi:\n")
    cat("H0: Homoskedastisitas (varians residual konstan).\n")
    if (bp_test$p.value < 0.05) {
        cat("Hasil: p-value < 0.05. Terdeteksi masalah Heteroskedastisitas.\n\n")
    } else {
        cat("Hasil: p-value >= 0.05. Asumsi homoskedastisitas terpenuhi.\n\n")
    }

    # --- 3. Uji Autokorelasi (Durbin-Watson) ---
    # H0: Tidak ada autokorelasi (residual saling bebas).
    # Nilai DW antara 0-4. Aturan umum: nilai mendekati 2 berarti tidak ada autokorelasi.
    # Jika p-value < 0.05, artinya ada autokorelasi.
    cat("--- 3. Uji Autokorelasi (Durbin-Watson) ---\n")
    dw_test <- dwtest(model)
    print(dw_test)
    cat("\nInterpretasi:\n")
    cat("H0: Tidak ada autokorelasi (residual independen).\n")
    if (dw_test$p.value < 0.05) {
        cat("Hasil: p-value < 0.05. Terdeteksi masalah autokorelasi.\n\n")
    } else {
        cat("Hasil: p-value >= 0.05. Tidak ada bukti adanya autokorelasi.\n\n")
    }

    # --- 4. Uji Multikolinearitas (Variance Inflation Factor - VIF) ---
    # VIF mengukur seberapa besar peningkatan varians dari koefisien regresi
    # akibat korelasi antar variabel independen.
    # Aturan umum: VIF > 5 atau VIF > 10 menunjukkan adanya multikolinearitas.
    cat("--- 4. Uji Multikolinearitas (Variance Inflation Factor - VIF) ---\n")
    # VIF hanya bisa dihitung jika ada lebih dari 1 variabel independen
    if (length(coef(model)) > 2) {
        vif_values <- vif(model)
        print(vif_values)
        cat("\nInterpretasi:\n")
        cat("VIF > 5 atau 10 sering dianggap sebagai indikasi adanya multikolinearitas yang perlu diwaspadai.\n")
        if (any(vif_values > 10)) {
            cat("Hasil: Ditemukan setidaknya satu variabel dengan VIF > 10.\n")
        } else if (any(vif_values > 5)) {
            cat("Hasil: Ditemukan setidaknya satu variabel dengan VIF > 5 (indikasi awal).\n")
        } else {
            cat("Hasil: Tidak ada indikasi kuat adanya masalah multikolinearitas (semua VIF < 5).\n")
        }
    } else {
        cat("VIF tidak dapat dihitung karena hanya ada satu variabel independen dalam model.\n")
    }
})

# ===================================================================
# 5. MENJALANKAN APLIKASI (RUN APPLICATION)
# ===================================================================
shinyApp(ui = ui, server = server)
