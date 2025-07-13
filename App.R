# ===================================================================
# 1. PENYIAPAN (SETUP)
# ===================================================================
# Penjelasan:
# Memuat semua pustaka (packages) yang dibutuhkan oleh aplikasi.
# Pastikan semua pustaka ini sudah terinstal di lingkungan R Anda.
# -------------------------------------------------------------------

library(shiny)
library(shinythemes)
library(leaflet)
library(dplyr)
library(readr)
library(ggplot2)
library(DT)
library(broom)
library(tidyr)
library(lmtest)
library(car)
library(sandwich)
library(ggfortify)


# ===================================================================
# 2. MEMUAT & MEMBERSIHKAN DATA (DATA LOADING & CLEANING)
# ===================================================================
# Penjelasan:
# Bagian ini bertanggung jawab untuk memuat semua file data mentah,
# melakukan transformasi (seperti reshaping), menggabungkan data,
# dan membersihkannya agar siap untuk dianalisis dan divisualisasikan.
# -------------------------------------------------------------------

tryCatch({
  kepadatan_raw <- read_csv("data/kepadatan.csv")
  ndvi_data     <- read_csv("data/ndvi.csv")
  suhu_data     <- read_csv("data/suhu.csv")
  tutupan_data  <- read_csv("data/tutupan_lahan.csv")
}, error = function(e) {
  stop("KRITIS: Gagal memuat file CSV. Pastikan semua file (kepadatan, ndvi, suhu, tutupan_lahan) ada di dalam folder 'data/'.")
})

kepadatan_long <- kepadatan_raw %>%
  pivot_longer(cols = c(`2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`, `2021`, `2022`, `2023`, `2024`), names_to = "tahun", values_to = "kepadatan") %>%
  mutate(tahun = as.numeric(tahun)) %>%
  select(Kode_Kab = kodekab, tahun, kepadatan)

data_lingkungan <- suhu_data %>%
  left_join(ndvi_data, by = c("Kab_Kota", "Kode_Kab", "provinsi", "X", "Y", "tahun")) %>%
  left_join(tutupan_data, by = c("Kab_Kota", "Kode_Kab", "provinsi", "X", "Y", "tahun"))

combined_data <- data_lingkungan %>%
  left_join(kepadatan_long, by = c("Kode_Kab", "tahun")) %>%
  na.omit() %>%
  mutate(
    Kab_Kota = as.factor(Kab_Kota),
    landcover = recode_factor(round(landcover, 0),
                              `0` = "Perairan", `1` = "Hutan.Jarum.Hijau.Sepanjang.Tahun", `2` = "Hutan.Daun.Lebar.Sepanjang.Tahun",
                              `3` = "Hutan.Jarum.Gugur", `4` = "Hutan.Daun.Lebar.Gugur", `5` = "Hutan.Campuran",
                              `6` = "Semak.Rapat", `7` = "Semak.Terbuka", `8` = "Savana.Berpepohonan", `9` = "Savana",
                              `10` = "Padang.Rumput", `11` = "Lahan.Basah.Permanen", `12` = "Lahan.Pertanian",
                              `13` = "Kawasan.Perkotaan/Terbangun", `14` = "Campuran.Pertanian.dan.Vegetasi.Alami",
                              `15` = "Salju.dan.Es", `16` = "Gersang.atau.Minim.Vegetasi", `255` = "Tidak.Ada.Data"
    )
  )

land_cover_colors <- c(
  "Perairan" = "#476BA1", 
  "Hutan.Jarum.Hijau.Sepanjang.Tahun" = "#397D49",
  "Hutan.Daun.Lebar.Sepanjang.Tahun" = "#88B053", 
  "Hutan.Jarum.Gugur" = "#397D49",
  "Hutan.Daun.Lebar.Gugur" = "#DAE049", 
  "Hutan.Campuran" = "#C3AA69", 
  "Semak.Rapat" = "#A9A9A9",
  "Semak.Terbuka" = "#DCD939", 
  "Savana.Berpepohonan" = "#AB6C28", 
  "Savana" = "#B76031",
  "Padang.Rumput" = "#D9903D", 
  "Lahan.Basah.Permanen" = "#91AF40", 
  "Lahan.Pertanian" = "#111149",
  "Kawasan.Perkotaan/Terbangun" = "#DEC5C5", 
  "Campuran.Pertanian.dan.Vegetasi.Alami" = "#D1D182",
  "Salju.dan.Es" = "#ffffff", 
  "Gersang.atau.Minim.Vegetasi" = "#CDBBB3", 
  "Tidak.Ada.Data" = "#000000"
)

year_range <- range(combined_data$tahun, na.rm = TRUE)
# ===================================================================
# 3. ANTARMUKA PENGGUNA (USER INTERFACE - UI)
# ===================================================================
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  # Background: Awan + Hujan
  div(id = "sky-bg"),
  
  # Style & Script untuk latar belakang
  tags$head(
    tags$style(HTML("
       /* --- Sticky Navbar --- */
      .navbar {
        position: fixed;
        top: 0;
        width: 100%;
        z-index: 9999;
        backdrop-filter: blur(10px);
        background: rgba(255, 255, 255, 0.7) !important;
        border-bottom: 1px solid rgba(200, 200, 200, 0.3);
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.05);
      }
  
      /* Offset main content so it's not under navbar */
      .navbar + .container, 
      .navbar + .container-fluid {
        margin-top: 70px; /* Sesuaikan dengan tinggi navbar */
      }
  
      /* Navbar brand */
      .navbar .navbar-brand {
        font-weight: 700;
        color: #0077b6 !important;
        font-size: 20px;
      }
  
      /* Navbar links */
      .navbar-nav > li > a {
        color: #333 !important;
        font-weight: 600;
        transition: color 0.3s;
      }
  
      .navbar-nav > li > a:hover {
        color: #ffffff !important;
      }
  
      .navbar-nav > .active > a,
      .navbar-nav > .active > a:hover {
        color: #0077b6 !important;
        border-bottom: 2px solid #0077b6;
        background-color: transparent !important;
      }
      /* Sky background container */
        #sky-bg {
          position: fixed;
          top: 0; left: 0;
          width: 100vw;
          height: 100vh;
          z-index: -10;
          background: linear-gradient(to bottom, #a3d5ff 0%, #ffffff 100%);
          overflow: hidden;
        }
      
      /* Hover fill-to-stroke effect */
      .btn {
        transition: all 0.3s ease;
        border-radius: 8px;
        font-weight: 600;
        background-color: #ffffff;
        color: #007acc;
        border: 2px solid transparent;
      }
      
      .btn:hover {
        background-color: transparent;
        border-color: #007acc;
        color: #005999;
      }
      
      /* Glassmorphism Card */
        .well, .panel, .card, .glass-box {
          background: rgba(255, 255, 255, 0.5);
          backdrop-filter: blur(12px);
          -webkit-backdrop-filter: blur(12px);
          border-radius: 16px;
          border: 1px solid rgba(255, 255, 255, 0.3);
          box-shadow: 0 8px 32px 0 rgba(0, 0, 0, 0.1);
        }
      ")),
    
    tags$script(HTML("
       // Efek hujan (canvas background)
       document.addEventListener('DOMContentLoaded', () => {
         const canvas = document.createElement('canvas');
         canvas.id = 'rain-canvas';
         canvas.style.position = 'fixed';
         canvas.style.top = 0;
         canvas.style.left = 0;
         canvas.style.width = '100%';
         canvas.style.height = '100%';
         canvas.style.zIndex = '-1';
         canvas.style.pointerEvents = 'none';
         document.body.appendChild(canvas);
         
         const ctx = canvas.getContext('2d');
         let width, height;
         let drops = [];
         
         function init() {
           width = canvas.width = window.innerWidth;
           height = canvas.height = window.innerHeight;
           drops = Array.from({length: 150}, () => ({
             x: Math.random() * width,
             y: Math.random() * height,
             length: 10 + Math.random() * 20,
             speed: 2 + Math.random() * 3
           }));
         }
         
         function animate() {
           ctx.clearRect(0, 0, width, height);
           ctx.strokeStyle = 'rgba(0, 180, 255, 0.25)';
           ctx.lineWidth = 1.2;
           for (let drop of drops) {
             ctx.beginPath();
             ctx.moveTo(drop.x, drop.y);
             ctx.lineTo(drop.x, drop.y + drop.length);
             ctx.stroke();
             drop.y += drop.speed;
             if (drop.y > height) {
               drop.y = -drop.length;
               drop.x = Math.random() * width;
             }
           }
           requestAnimationFrame(animate);
         }
         
         window.addEventListener('resize', init);
         init();
         animate();
       });
       "))
  ),
  
  navbarPage(
    "Dashboard Urban Heat Island (UHI)",
    tabPanel("Overview",
             fluidRow(
               # Kolom Kiri: Deskripsi & Ringkasan Statistik
               column(7,
                      h3("Selamat Datang di Dasbor Urban Heat Island"),
                      p("Gunakan dasbor ini untuk eksplorasi dan analisis data suhu permukaan perkotaan. Anda dapat memulai dengan data bawaan atau mengunggah data Anda sendiri pada panel di sebelah kanan."),
                      hr(),
                      h4("Ringkasan Data Bawaan"),
                      
                      # slider rentang tahun
                      sliderInput("overview_year_range", # ID baru untuk input
                                  "Pilih Rentang Tahun untuk Ringkasan & Tren:",
                                  min = year_range[1],
                                  max = year_range[2],
                                  value = year_range, # Nilai defaultnya adalah seluruh rentang tahun
                                  step = 1,
                                  sep = ""), # Menghilangkan koma pemisah ribuan (misal: 2,015 -> 2015)
                      
                      # Output untuk tabel 5 kota terpanas
                      h5("5 Kota dengan Rata-rata Suhu Tertinggi (pada rentang tahun terpilih)"),
                      tableOutput("top_5_cities_table"),
                      uiOutput("interpretasi_top_cities"),
                      hr(),
                      # Output untuk plot tren suhu nasional
                      h5("Tren Rata-rata Suhu Tahunan Nasional (pada rentang tahun terpilih)"),
                      plotOutput("national_trend_plot", height = "250px"),
                      uiOutput("interpretasi_national_trend")
               ),
               # Kolom Kanan: Input Data Pengguna
               column(5,
                      wellPanel(
                        h4("Analisis Data Anda Sendiri"),
                        p("Unggah file CSV Anda untuk dianalisis. Pastikan file Anda memiliki kolom wajib: 'suhu', 'ndvi', 'kepadatan', 'landcover', 'tahun', 'X', 'Y', dan 'Kab_Kota'."),
                        fileInput("user_file", "Pilih File CSV:",
                                  multiple = FALSE,
                                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                        actionButton("run_user_data_analysis", "Gunakan Data Saya", icon = icon("cogs")),
                        actionButton("reset_to_default_data", "Gunakan Data Bawaan", icon = icon("undo"))
                      )
               )
             )
    ),
    tabPanel("Peta UHI",
             sidebarLayout(
               sidebarPanel(
                 h3("Visualisasi Spasial UHI"),
                 p("Peta ini menampilkan distribusi titik data secara geografis."),
                 selectInput("selected_year_map", "Pilih Tahun:", choices = NULL),
                 radioButtons("map_color_by", "Warnai Peta Berdasarkan:",
                              choices = c("Suhu Permukaan" = "suhu", "Tutupan Lahan" = "landcover"),
                              selected = "suhu"),
                 downloadButton("downloadDataPeta", "Download Data Peta (.csv)"),
                 hr(),
                 h4("Interpretasi Peta UHI"),
                 uiOutput("map_interpretation")
               ),
               mainPanel(leafletOutput("heatmap", height = "70vh"))
             )
    ),
    tabPanel("Eksplorasi Hubungan",
             sidebarLayout(
               sidebarPanel(
                 h3("Eksplorasi Hubungan Bivariat"),
                 p("Visualisasikan hubungan antara satu variabel independen (X) dengan suhu (Y)."),
                 selectInput("year_explore", "Pilih Tahun:", choices = NULL), # Pastikan ini juga sudah dinamis seperti yang kita atur sebelumnya
                 selectInput("x_var", "Pilih Variabel Independen (X):", choices = c("NDVI" = "ndvi", "Kepadatan Penduduk" = "kepadatan", "Tutupan Lahan" = "landcover")),
                 uiOutput("model_type_ui") # Ini adalah checkbox Linier/Kuadratik/Kubik
               ),
               mainPanel(
                 plotOutput("explorePlot"),
                 br(),
                 h4("Interpretasi Hubungan"),
                 uiOutput("explore_interpretation"),
                 uiOutput("model_comparison_section")
               )
             )
    ),
    tabPanel("Analisis Regresi",
             sidebarLayout(
               sidebarPanel(
                 h3("Model Regresi Linier Berganda"),
                 p("Bangun model untuk menjelaskan Suhu (LST) berdasarkan beberapa prediktor."),
                 selectInput("selected_year_model", "Pilih Tahun:", choices = NULL, selected = max(combined_data$tahun)),
                 checkboxGroupInput("model_vars", "Pilih Variabel Independen:", choices = c("NDVI" = "ndvi", "Kepadatan Penduduk" = "kepadatan", "Tutupan Lahan" = "landcover"), selected = c("ndvi", "kepadatan")),
                 hr(),
                 h4("Tindakan Perbaikan Asumsi"),
                 selectInput("transform_y", "1. Transformasi Variabel Suhu (Y):",
                             choices = c("Tidak Ada", "Logaritma (log)" = "log", "Akar Kuadrat (sqrt)" = "sqrt")),
                 strong("2. Gunakan Robust Standard Errors"),
                 checkboxInput("use_robust_se", "Ya", value = FALSE),
                 hr(),
                 downloadButton("download_report", "Unduh Laporan Analisis (PDF)")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Ringkasan Model",
                            verbatimTextOutput("modelSummaryOutput"),
                            uiOutput("regressionEquation"),
                            hr(),
                            h4("Interpretasi Hasil Model"),
                            uiOutput("model_interpretation")
                   ),
                   tabPanel("Kesesuaian & Diagnostik",
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
    tabPanel("Data & Statistik",
             tabsetPanel(
               tabPanel("Statistik Deskriptif", 
                        h4("Ringkasan Statistik Data Aktif"),
                        verbatimTextOutput("descriptiveStats"),
                        hr(), # Pemisah
                        h4("Interpretasi Statistik Deskriptif"), # Judul untuk interpretasi
                        uiOutput("descriptive_stats_interpretation") # Output untuk interpretasi dinamis
               ),
               tabPanel("Data Mentah", DT::dataTableOutput("rawData"), downloadButton("downloadRawData", "Download Semua Data (.csv)"))
             )
    ),
    tabPanel("About",
             fluidRow(
               column(8, offset = 2,
                      wellPanel(
                        h3("Tentang Dashboard UHI"),
                        p("Dashboard ini dirancang sebagai alat interaktif untuk memvisualisasikan dan menganalisis fenomena Urban Heat Island (UHI) di berbagai kabupaten/kota di Indonesia. Pengguna dapat menjelajahi data spasial, hubungan antar variabel, dan membangun model regresi untuk memahami faktor-faktor yang memengaruhi suhu permukaan perkotaan."),
                        hr(),
                        h4("Informasi Data & Sumber"),
                        HTML("
                          <ul>
                            <li><b>Judul Data Set:</b> Data Urban Heat Island (UHI) Indonesia</li>
                            <li><b>Rentang Tahun Data:</b> 2015 - 2024</li>
                            <li><b>Cakupan Geografis:</b> Seluruh Kabupaten/Kota di Indonesia</li>
                            <li><b>Sumber Data Mentah:</b>
                                  <ul>
                                    <li><b>Suhu Permukaan (LST), NDVI, dan Tutupan Lahan:</b> Berasal dari citra satelit Landsat 8 OLI/TIRS, diolah menggunakan Google Earth Engine.</li>
                                    <li><b>Kepadatan Penduduk:</b> Berasal dari WorldPop Open Spatial Demographic Data and Research.</li>
                                  </ul>
                            </li>
                          </ul>
                        "),
                        
                        h4("Deskripsi Variabel Kunci"),
                        tableOutput("metadata_variables_table"),
                        
                        hr(),
                        
                        h4("Penulis"),
                        HTML("
                          <ul>
                            <li><b>Tim Pembuat Dashboard:</b> 
                                <ul>
                                  <li>Anggita Cristin Meylani</li>
                                  <li>Muhammad Muhlis Aditya Nur Wahid</li>
                                  <li>Safira Inayah</li>
                                </ul>
                            <li><b>Afiliasi:</b> Politeknik Statistika STIS</li>
                          </ul>
                        "),
                        
                        hr(),
                        
                        h4("Kutipan yang Disarankan"),
                        p("Ketika menggunakan data atau analisis dari dashboard ini dalam publikasi, mohon kutip sumber data asli (Landsat 8 OLI/TIRS, WorldPop) dan merujuk pada dashboard ini sebagai alat analisis yang digunakan.")
                      )
               )
             )
    )
  )
)

# ===================================================================
# 4. LOGIKA SERVER (SERVER LOGIC)
# ===================================================================
server <- function(input, output, session) {
  
  # WADAH REAKTIF UNTUK DATA
  # Secara default, isinya adalah data bawaan aplikasi.
  # Jika pengguna mengunggah file, isinya akan kita ganti.
  data_aktif <- reactiveVal(combined_data)
  
  ## BAGIAN LOGIKA UNTUK UNGGAH DATA PENGGUNA
  observeEvent(input$user_file, {
    # Saat file diunggah, baca file tersebut
    tryCatch({
      df <- read_csv(input$user_file$datapath)
      # Simpan sementara di reactiveVal
      user_data_temp <- reactiveVal(df)
      
      # Tampilkan notifikasi
      showNotification("File berhasil diunggah. Klik 'Gunakan Data Saya' untuk memulai analisis.", type = "success")
      
      # Saat tombol 'Gunakan Data Saya' diklik, ganti data_aktif
      observeEvent(input$run_user_data_analysis, {
        # Lakukan validasi kolom di sini jika perlu
        required_cols <- c("suhu", "ndvi", "kepadatan", "landcover", "tahun", "X", "Y", "Kab_Kota")
        if(all(required_cols %in% names(user_data_temp()))){
          data_aktif(user_data_temp())
          showNotification("Analisis sekarang menggunakan data Anda.", type = "message", duration = 5)
        } else {
          showNotification("Error: Data Anda tidak memiliki semua kolom yang dibutuhkan.", type = "error", duration = 10)
        }
      })
      
    }, error = function(e) {
      showNotification(paste("Error saat membaca file:", e$message), type = "error", duration = 10)
    })
  })
  
  # Logika untuk tombol reset
  observeEvent(input$reset_to_default_data, {
    data_aktif(combined_data)
    showNotification("Analisis telah kembali menggunakan data bawaan.", type = "warning", duration = 5)
  })
  
  observeEvent(TRUE, {
    showModal(modalDialog(
      title = "Selamat Datang! Tonton Tutorial Singkat Ini",
      HTML('
      <div style="text-align: center;">
        <iframe width="560" height="315" src="https://www.youtube.com/embed/GDHM19emZn8" title="Video Tutorial Penggunaan Dashboard" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>
        <br><br>
        <a href="UserGuide_UHI.pdf" target="_blank" class="btn btn-primary">📄 Buka Panduan Pengguna (PDF)</a>
      </div>
    '),
      easyClose = TRUE,
      footer = modalButton("Tutup dan Mulai Eksplorasi")
    ))
  })
  
  map_data      <- reactive({ req(data_aktif()); data_aktif() %>% filter(tahun == input$selected_year_map) })
  model_data_filtered <- reactive({ req(data_aktif()); data_aktif() %>% filter(tahun == input$selected_year_model) })
  
  explore_data  <- reactive({
    req(input$year_explore, input$x_var, data_aktif())
    data_aktif() %>%
      filter(tahun == input$year_explore) %>%
      select(suhu, x_var = !!sym(input$x_var))
  })
  
  data_compare_model_stats <- reactive({
    df <- explore_data()
    req(df) # Pastikan explore_data ada
    
    # Pastikan variabel X adalah numerik untuk perbandingan model
    if (!is.numeric(df$x_var) || is.null(input$model_type) || length(input$model_type) == 0) {
      return(NULL) # Kembalikan NULL jika tidak relevan untuk perbandingan model
    }
    
    stats <- data.frame(Model = character(0), R_Squared = numeric(0), AIC = numeric(0))
    y <- df$suhu
    x <- df$x_var
    
    if ("Linier" %in% input$model_type) {
      m <- lm(y ~ x)
      stats <- rbind(stats, data.frame(Model = "Linier", R_Squared = summary(m)$r.squared, AIC = AIC(m)))
    }
    if ("Kuadratik" %in% input$model_type) {
      m <- lm(y ~ x + I(x^2))
      stats <- rbind(stats, data.frame(Model = "Kuadratik", R_Squared = summary(m)$r.squared, AIC = AIC(m)))
    }
    if ("Kubik" %in% input$model_type) {
      m <- lm(y ~ x + I(x^2) + I(x^3))
      stats <- rbind(stats, data.frame(Model = "Kubik", R_Squared = summary(m)$r.squared, AIC = AIC(m)))
    }
    
    if (nrow(stats) > 0) {
      colnames(stats) <- c("Model", "R Squared", "AIC") # Beri nama kolom yang konsisten
      return(stats)
    } else {
      return(NULL)
    }
  })
  
  ## BAGIAN LOGIKA UNTUK RINGKASAN STATISTIK DI OVERVIEW
  overview_data_filtered <- reactive({
    req(input$overview_year_range, data_aktif())
    data_aktif() %>%
      filter(tahun >= input$overview_year_range[1] & tahun <= input$overview_year_range[2])
  })
  
  # Tabel 5 kota terpanas
  output$top_5_cities_table <- renderTable({
    overview_data_filtered() %>%
      group_by(Kab_Kota) %>%
      summarise(Suhu_Rata_Rata = mean(suhu, na.rm = TRUE)) %>%
      slice_max(order_by = Suhu_Rata_Rata, n = 5) %>%
      arrange(desc(Suhu_Rata_Rata)) %>%
      mutate(Suhu_Rata_Rata = sprintf("%.2f °C", Suhu_Rata_Rata))
  }, striped = TRUE, hover = TRUE)
  
  output$interpretasi_top_cities <- renderUI({
    df_filtered <- overview_data_filtered()
    if (nrow(df_filtered) == 0) return(NULL)
    
    top_city_data <- df_filtered %>%
      group_by(Kab_Kota) %>%
      summarise(Suhu_Rata_Rata = mean(suhu, na.rm = TRUE)) %>%
      slice_max(order_by = Suhu_Rata_Rata, n = 1) %>%
      ungroup() # Penting untuk ungroup sebelum menggunakan $
    
    if (nrow(top_city_data) > 0) {
      top_city_name <- top_city_data$Kab_Kota[1]
      top_city_suhu <- sprintf("%.2f", top_city_data$Suhu_Rata_Rata[1])
      HTML(paste0(
        "<p><b>Interpretasi:</b><br>Kota/kabupaten dengan rata-rata suhu permukaan tertinggi pada periode ",
        input$overview_year_range[1], " - ", input$overview_year_range[2],
        " adalah <b>", top_city_name, "</b> dengan suhu rata-rata <b>", top_city_suhu, " °C</b>. ",
        "Hal ini mengindikasikan bahwa ", top_city_name, " mungkin mengalami efek Urban Heat Island (UHI) yang lebih intens dibandingkan wilayah lain dalam data ini pada rentang tahun tersebut. Suhu tinggi ini dapat disebabkan oleh konsentrasi bangunan, material non-reflektif, dan minimnya vegetasi."
      ))
    } else {
      return(NULL)
    }
  })
  
  # Plot tren suhu nasional
  output$national_trend_plot <- renderPlot({
    plot_data <- overview_data_filtered() %>%
      group_by(tahun) %>%
      summarise(Suhu_Nasional_Rata_Rata = mean(suhu, na.rm = TRUE))
    ggplot(plot_data, aes(x = tahun, y = Suhu_Nasional_Rata_Rata)) +
      geom_line(color = "tomato", size = 1.2) +
      geom_point(color = "tomato", size = 3, shape = 21, fill = "white", stroke = 1.5) +
      geom_text(aes(label = round(Suhu_Nasional_Rata_Rata, 2)), vjust = -1.5, size = 4) +
      labs(x = "Tahun", y = "Rata-rata Suhu (°C)") +
      theme_minimal(base_size = 14) +
      expand_limits(y = min(plot_data$Suhu_Nasional_Rata_Rata) + 1.25)
  })
  
  output$interpretasi_national_trend <- renderUI({
    plot_data <- overview_data_filtered() %>%
      group_by(tahun) %>%
      summarise(Suhu_Nasional_Rata_Rata = mean(suhu, na.rm = TRUE)) %>%
      arrange(tahun) # Pastikan data terurut berdasarkan tahun
    
    if (nrow(plot_data) < 2) return(NULL) # Perlu minimal 2 tahun untuk tren
    
    first_year_data <- plot_data %>% filter(tahun == min(plot_data$tahun))
    last_year_data <- plot_data %>% filter(tahun == max(plot_data$tahun))
    
    suhu_awal <- sprintf("%.2f", first_year_data$Suhu_Nasional_Rata_Rata[1])
    suhu_akhir <- sprintf("%.2f", last_year_data$Suhu_Nasional_Rata_Rata[1])
    
    perubahan_suhu <- last_year_data$Suhu_Nasional_Rata_Rata[1] - first_year_data$Suhu_Nasional_Rata_Rata[1]
    interpretasi_teks <- ""
    
    if (abs(perubahan_suhu) < 0.1) { # Threshold untuk dianggap stabil
      interpretasi_teks <- "relatif stabil"
    } else if (perubahan_suhu > 0) {
      interpretasi_teks <- paste0("peningkatan sekitar ", sprintf("%.2f", perubahan_suhu), " °C")
    } else {
      interpretasi_teks <- paste0("penurunan sekitar ", sprintf("%.2f", abs(perubahan_suhu)), " °C")
    }
    
    HTML(paste0(
      "<p><b>Interpretasi:</b><br>Tren rata-rata suhu tahunan nasional menunjukkan adanya <b>", interpretasi_teks, "</b> dari tahun <b>", # Perhatikan <b> dan </b>
      min(plot_data$tahun), "</b> hingga <b>", max(plot_data$tahun), "</b>. ", # Perhatikan <b> dan </b>
      "Rata-rata suhu berubah dari sekitar <b>", suhu_awal, " °C</b> menjadi <b>", suhu_akhir, " °C</b>. ",
      "Pola ini dapat memberikan wawasan tentang dampak perubahan iklim regional atau efek urbanisasi yang meluas terhadap suhu permukaan di Indonesia selama periode tersebut."
    ))
  })
  
  output$heatmap <- renderLeaflet({
    req(map_data(), input$map_color_by)
    base_map <- leaflet(map_data()) %>% addProviderTiles(providers$CartoDB.Positron)
    if (input$map_color_by == "suhu") {
      pal <- colorNumeric("YlOrRd", domain = map_data()$suhu)
      base_map %>%
        addCircleMarkers(lng = ~X, lat = ~Y, radius = 7, color = ~pal(suhu), stroke = FALSE, fillOpacity = 0.7,
                         popup = ~paste("<b>Suhu:</b>", round(suhu, 2), "°C<br><b>Tutupan:</b>", landcover)) %>%
        addLegend("bottomright", pal = pal, values = ~suhu, title = "Suhu (°C)", opacity = 1)
    } else {
      pal <- colorFactor(palette = land_cover_colors, domain = map_data()$landcover)
      base_map %>%
        addCircleMarkers(lng = ~X, lat = ~Y, radius = 7, color = ~pal(landcover), stroke = FALSE, fillOpacity = 0.7,
                         popup = ~paste("<b>Suhu:</b>", round(suhu, 2), "°C<br><b>Tutupan:</b>", landcover)) %>%
        addLegend("bottomright", pal = pal, values = ~landcover, title = "Tutupan Lahan", opacity = 1)
    }
  })
  
  # Sinkronkan pilihan tahun untuk peta dan model dengan data_aktif()
  observeEvent(data_aktif(), {
    current_data <- data_aktif()
    if (nrow(current_data) > 0) {
      available_years <- sort(unique(current_data$tahun))
      
      # Update pilihan tahun untuk Peta UHI
      updateSelectInput(session, "selected_year_map",
                        choices = available_years,
                        selected = max(available_years, na.rm = TRUE)) # Pilih tahun terbaru yang valid
      
      # Update pilihan tahun untuk Analisis Regresi
      updateSelectInput(session, "selected_year_model",
                        choices = available_years,
                        selected = max(available_years, na.rm = TRUE))
      
      # Update pilihan tahun untuk Eksplorasi Hubungan
      updateSelectInput(session, "year_explore",
                        choices = available_years,
                        selected = max(available_years, na.rm = TRUE))
    } else {
      # Jika data_aktif kosong, reset pilihan tahun
      updateSelectInput(session, "selected_year_map", choices = NULL, selected = NULL)
      updateSelectInput(session, "selected_year_model", choices = NULL, selected = NULL)
      updateSelectInput(session, "year_explore", choices = NULL, selected = NULL)
    }
  }, ignoreNULL = FALSE, ignoreInit = FALSE) # Penting untuk dijalankan pada inisialisasi
  
  output$map_interpretation <- renderUI({
    req(map_data(), input$map_color_by) # Pastikan data peta dan pilihan pewarnaan tersedia
    
    df_map <- map_data() # Ambil data yang sedang ditampilkan di peta
    
    if (nrow(df_map) == 0) {
      return(HTML("<p>Tidak ada data yang tersedia untuk tahun yang dipilih. Silakan pilih tahun lain atau unggah data baru.</p>"))
    }
    
    interpretasi_teks <- ""
    
    # Interpretasi Umum Peta (bisa tetap statis atau sedikit diperkaya)
    interpretasi_teks <- paste0(
      "<p>Peta ini menampilkan distribusi spasial titik-titik data di berbagai kota/kabupaten pada tahun <b>", input$selected_year_map, "</b>.</p>"
    )
    
    # --- Interpretasi Berdasarkan Suhu ---
    if (input$map_color_by == "suhu") {
      suhu_max <- sprintf("%.2f", max(df_map$suhu, na.rm = TRUE))
      suhu_min <- sprintf("%.2f", min(df_map$suhu, na.rm = TRUE))
      suhu_avg <- sprintf("%.2f", mean(df_map$suhu, na.rm = TRUE))
      
      # Identifikasi jenis tutupan lahan di suhu tertinggi (misalnya, top 1% atau 5 titik terpanas)
      top_suhu_landcover <- df_map %>%
        arrange(desc(suhu)) %>%
        head(5) %>% # Ambil 5 titik terpanas
        count(landcover, sort = TRUE) %>%
        slice(1) # Ambil tutupan lahan yang paling sering muncul di 5 titik terpanas
      
      if (nrow(top_suhu_landcover) > 0) {
        dominan_hot_lc <- top_suhu_landcover$landcover[1]
      } else {
        dominan_hot_lc <- "Tidak diketahui"
      }
      
      interpretasi_teks <- paste0(interpretasi_teks,
                                  "<p><b>Fokus: Suhu Permukaan (LST)</b></p>",
                                  "<p>Pada tahun <b>", input$selected_year_map, "</b>, suhu permukaan rata-rata adalah <b>", suhu_avg, " °C</b>. ",
                                  "Rentang suhu yang teramati di peta ini adalah dari <b>", suhu_min, " °C</b> (area terdingin) hingga <b>", suhu_max, " °C</b> (area terpanas).</p>",
                                  "<p>Titik-titik berwarna merah/oranye gelap mengindikasikan area dengan suhu tinggi. Observasi menunjukkan bahwa di antara 5 titik terpanas, jenis tutupan lahan yang paling sering teridentifikasi adalah <b>", dominan_hot_lc, "</b>. Hal ini seringkali terjadi di kawasan perkotaan yang padat, dimana material bangunan menyerap dan menyimpan panas lebih banyak, serta minimnya vegetasi.</p>",
                                  "<p><b>Indikasi UHI:</b> Jika Anda melihat konsentrasi tinggi titik-titik merah gelap di area yang Anda kenali sebagai pusat perkotaan atau kawasan industri dibandingkan dengan area pedesaan/vegetasi di sekitarnya, ini merupakan bukti kuat adanya fenomena Urban Heat Island (UHI).</p>"
      )
    }
    # --- Interpretasi Berdasarkan Tutupan Lahan ---
    else { # input$map_color_by == "landcover"
      
      # Hitung rata-rata suhu untuk kategori tutupan lahan tertentu
      # (misalnya, Kawasan Perkotaan/Terbangun vs Hutan/Perairan)
      suhu_urban <- df_map %>%
        filter(landcover == "Kawasan.Perkotaan/Terbangun") %>%
        summarise(avg_suhu = mean(suhu, na.rm = TRUE)) %>%
        pull(avg_suhu)
      suhu_forest <- df_map %>%
        filter(landcover == "Hutan.Jarum.Hijau.Sepanjang.Tahun" | landcover == "Hutan.Daun.Lebar.Sepanjang.Tahun") %>%
        summarise(avg_suhu = mean(suhu, na.rm = TRUE)) %>%
        pull(avg_suhu)
      
      interpretasi_teks <- paste0(interpretasi_teks,
                                  "<p><b>Fokus: Tutupan Lahan</b></p>",
                                  "<p>Pada tahun <b>", input$selected_year_map, "</b>, peta ini menampilkan distribusi berbagai jenis tutupan lahan. Setiap warna mewakili kategori tutupan lahan yang berbeda (lihat legenda untuk detail).</p>"
      )
      
      if (!is.null(suhu_urban) && !is.nan(suhu_urban) && !is.null(suhu_forest) && !is.nan(suhu_forest)) {
        interpretasi_teks <- paste0(interpretasi_teks,
                                    "<p>Sebagai contoh, rata-rata suhu di <b>Kawasan Perkotaan/Terbangun</b> sekitar <b>", sprintf("%.2f", suhu_urban), " °C</b>, sementara rata-rata suhu di area <b>Hutan</b> sekitar <b>", sprintf("%.2f", suhu_forest), " °C</b>. Perbedaan suhu ini menyoroti peran penting tutupan lahan dalam memengaruhi suhu permukaan.</p>",
                                    "<p><b>Koneksi dengan UHI:</b> Secara umum, area dengan tutupan lahan yang didominasi oleh permukaan kedap air (seperti 'Kawasan Perkotaan/Terbangun') cenderung memiliki suhu permukaan yang lebih tinggi. Sebaliknya, area dengan vegetasi padat ('Hutan') atau perairan cenderung memiliki efek pendinginan. Hal ini merupakan mekanisme kunci pembentukan UHI.</p>"
        )
      } else {
        interpretasi_teks <- paste0(interpretasi_teks, "<p>Data untuk perbandingan suhu antara jenis tutupan lahan tertentu (misalnya perkotaan vs hutan) tidak lengkap untuk tahun ini.</p>")
      }
    }
    
    HTML(interpretasi_teks)
  })
  
  output$downloadDataPeta <- downloadHandler(
    filename = function() paste0("data_peta_uhi_", input$selected_year_map, ".csv"),
    content = function(file) write.csv(map_data(), file, row.names = FALSE)
  )
  
  output$model_type_ui <- renderUI({
    req(explore_data())
    if (is.numeric(explore_data()$x_var)) {
      checkboxGroupInput("model_type", "Tampilkan Garis Model:", choices = c("Linier", "Kuadratik", "Kubik"), selected = "Linier", inline = TRUE)
    } else {
      p("Plot Boxplot ditampilkan untuk variabel kategorikal.")
    }
  })
  output$explorePlot <- renderPlot({
    df <- explore_data(); req(df)
    if (is.numeric(df$x_var)) {
      p <- ggplot(df, aes(x = x_var, y = suhu)) +
        geom_point(alpha = 0.5, color = "steelblue") +
        labs(x = input$x_var, y = "Suhu (°C)", title = paste("Hubungan Suhu vs", input$x_var)) + theme_minimal(base_size = 14)
      if (!is.null(input$model_type)) {
        if ("Linier" %in% input$model_type) p <- p + geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "#28a745", linetype = "dashed")
        if ("Kuadratik" %in% input$model_type) p <- p + geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "#fd7e14")
        if ("Kubik" %in% input$model_type) p <- p + geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), se = FALSE, color = "#dc3545")
      }
      print(p)
    } else {
      ggplot(df, aes(x = x_var, y = suhu)) +
        geom_boxplot(aes(fill = x_var), show.legend = FALSE, alpha = 0.8) +
        geom_jitter(width = 0.1, alpha = 0.2) +
        labs(x = input$x_var, y = "Suhu (°C)", title = paste("Distribusi Suhu berdasarkan", input$x_var)) +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Memutar label X agar tidak tumpang tindih
    }
  })
  
  output$compareModelStats <- renderTable({
    data_compare_model_stats() # Cukup panggil reaktif yang baru dibuat
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", digits = 4)
  
  model_data_transformed <- reactive({
    df <- model_data_filtered()
    if (input$transform_y == "log") {
      if(any(df$suhu <= 0)) return(NULL)
      df$suhu <- log(df$suhu)
    } else if (input$transform_y == "sqrt") {
      df$suhu <- sqrt(df$suhu)
    }
    return(df)
  })
  model_fit <- reactive({
    
    df <- model_data_transformed()
    
    # Tambahkan pemeriksaan di sini
    if (length(input$model_vars) == 0) {
      # Jika tidak ada variabel yang dipilih, kembalikan NULL
      # Ini akan menyebabkan req(model_fit()) di tempat lain untuk berhenti
      # Atau Anda bisa mengembalikan pesan error yang lebih spesifik
      return(NULL) # Penting: kembalikan NULL
    }
    
    # Pastikan df juga tidak NULL atau kosong
    req(df) 
    
    formula_text <- paste("suhu ~", paste(input$model_vars, collapse = " + "))
    
    # Coba untuk membuat model, tangani error jika terjadi
    tryCatch({
      lm(as.formula(formula_text), data = df)
    }, error = function(e) {
      showNotification(paste("Error saat membangun model:", e$message), type = "error", duration = 5)
      return(NULL) # Kembalikan NULL jika ada error
    })
  })
  
  output$descriptiveStats <- renderPrint({
    req(data_aktif()) 
    summary(data_aktif()) 
  })
  
  output$rawData <- DT::renderDataTable({
    req(data_aktif())
    DT::datatable(data_aktif(), options = list(pageLength = 10, scrollX = TRUE), filter = "top", rownames = FALSE)
  })
  
  output$downloadRawData <- downloadHandler(
    filename = function() "semua_data_uhi.csv",
    content = function(file) {
      req(data_aktif())
      write.csv(data_aktif(), file, row.names = FALSE)}
  )
  
  # Membuat string persamaan regresi
  regression_equation_string <- reactive({
    req(model_fit())
    y_name <- if (input$transform_y == "log") "log(suhu)" else if (input$transform_y == "sqrt") "sqrt(suhu)" else "suhu"
    tidy_model <- tidy(model_fit())
    
    # Memastikan model memiliki intercept sebelum melanjutkan
    if (!"(Intercept)" %in% tidy_model$term) return("Model tidak memiliki intercept.")
    
    eq <- paste0(y_name, " = ", round(tidy_model$estimate[1], 4))
    
    # Filter untuk mendapatkan prediktor saja
    predictors <- tidy_model %>% filter(term != "(Intercept)")
    if (nrow(predictors) > 0) {
      for (i in 1:nrow(predictors)) {
        term <- predictors$term[i]
        estimate <- round(predictors$estimate[i], 4)
        sign <- ifelse(estimate >= 0, " + ", " - ")
        eq <- paste0(eq, sign, abs(estimate), " * ", term)
      }
    }
    return(eq)
  })
  
  # Membuat teks hasil uji asumsi
  assumption_tests_string <- reactive({
    req(model_fit())
    model <- model_fit()
    
    # capture.output() akan "menangkap" semua yang dicetak oleh cat() dan print()
    # dan menyimpannya sebagai vektor karakter.
    output_lines <- capture.output({
      
      # --- Uji 1: Normalitas ---
      cat("--- 1. Uji Normalitas Residual (Kolmogorov-Smirnov) ---\n")
      tryCatch({
        res_std <- rstandard(model)
        ks_test <- ks.test(res_std, "pnorm")
        print(ks_test)
        cat("\nInterpretasi:\n"); cat("H0: Residual mengikuti distribusi normal.\n")
        if (ks_test$p.value < 0.05) cat("Hasil: p-value < 0.05. Asumsi normalitas TIDAK terpenuhi.\n\n")
        else cat("Hasil: p-value >= 0.05. Asumsi normalitas terpenuhi.\n\n")
      }, error = function(e) cat("ERROR:", e$message, "\n\n"))
      
      # --- Uji 2: Homoskedastisitas ---
      cat("-- 2. Uji Homoskedastisitas (Breusch-Pagan) --\n")
      tryCatch({
        bp_test <- bptest(model); print(bp_test)
        cat("\nInterpretasi:\n"); cat("H0: Homoskedastisitas (varians residual konstan).\n")
        if (bp_test$p.value < 0.05) cat("Hasil: p-value < 0.05. Terdeteksi masalah Heteroskedastisitas.\n\n")
        else cat("Hasil: p-value >= 0.05. Asumsi homoskedastisitas terpenuhi.\n\n")
      }, error = function(e) cat("ERROR:", e$message, "\n\n"))
      
      # --- Uji 3: Autokorelasi ---
      cat("--- 3. Uji Autokorelasi (Durbin-Watson) ---\n")
      tryCatch({
        dw_test <- dwtest(model); print(dw_test)
        cat("\nInterpretasi:\n"); cat("H0: Tidak ada autokorelasi (residual independen).\n")
        if (dw_test$p.value < 0.05) cat("Hasil: p-value < 0.05. Terdeteksi masalah autokorelasi.\n\n")
        else cat("Hasil: p-value >= 0.05. Tidak ada bukti adanya autokorelasi.\n\n")
      }, error = function(e) cat("ERROR:", e$message, "\n\n"))
      
      # --- Uji 4: Multikolinearitas ---
      cat("--- 4. Uji Multikolinearitas (Variance Inflation Factor - VIF) ---\n")
      tryCatch({
        # PENTING: Gunakan input$model_vars untuk mengecek jumlah prediktor yang dipilih
        # daripada all.vars(formula(model))[-1] yang mungkin bermasalah jika ada variabel dummy
        if (length(input$model_vars) > 1) { # Pastikan ada lebih dari 1 prediktor yang dipilih
          vif_values <- vif(model)
          print(vif_values)
          cat("\nInterpretasi:\n"); cat("VIF > 5 atau 10 sering dianggap sebagai indikasi multikolinearitas. Nilai VIF tertinggi: ", sprintf("%.2f", max(vif_values)), "\n")
        } else {
          cat("VIF tidak dapat dihitung karena hanya ada satu atau kurang variabel independen yang dipilih.\n")
        }
      }, error = function(e) cat("ERROR saat menghitung VIF:", e$message, "\n"))
    })
    
    # Menggabungkan semua baris menjadi satu blok teks tunggal
    paste(output_lines, collapse = "\n")
  })
  
  output$regressionEquation <- renderUI({
    HTML(paste0("<code>", regression_equation_string(), "</code>"))
  })
  
  output$assumptionTestsOutput <- renderPrint({
    cat(assumption_tests_string())
  })
  
  output$modelSummaryOutput <- renderPrint({
    req(model_fit())
    model <- model_fit()
    cat("=================================================================\n")
    if (input$transform_y != "Tidak Ada") {
      cat(paste("CATATAN: Variabel dependen (suhu) telah ditransformasi menggunakan:", input$transform_y, "\n"))
    }
    if (input$use_robust_se) {
      cat("MENAMPILKAN HASIL DENGAN ROBUST STANDARD ERRORS (HC1)\n")
      cat("=================================================================\n")
      print(coeftest(model, vcov. = vcovHC(model, type = "HC1")))
    } else {
      cat("MENAMPILKAN HASIL REGRESI STANDAR (OLS)\n")
      cat("=================================================================\n")
      print(summary(model))
    }
  })
  
  output$glanceOutput <- renderTable({ 
    req(model_fit())
    glance(model_fit()) })
  
  output$diagnosticPlot <- renderPlot({
    req(model_fit())
    autoplot(model_fit(), which = 1:4, label.size = 3, ncol = 2) + theme_minimal(base_size = 13)
  })
  
  ## REPORT
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("Laporan-Regresi-UHI-", Sys.Date(), ".pdf")
    },
    content = function(file) {
      id <- showNotification("Menyiapkan laporan, mohon tunggu...", duration = NULL, type = "message")
      on.exit(removeNotification(id))
      
      # Menangkap output model summary sebagai teks, lalu membersihkannya
      summary_obj <- if(input$use_robust_se) {
        coeftest(model_fit(), vcov. = vcovHC(model_fit(), type = "HC1"))
      } else {
        summary(model_fit())
      }
      
      # Siapkan semua parameter yang akan dikirim ke R Markdown
      params <- list(
        model_object = model_fit(), # Kirim objek model lm() secara langsung
        use_robust_se = input$use_robust_se,
        glance_df = glance(model_fit()),
        diagnostic_plot = autoplot(model_fit(), which = 1:4, label.size = 3, ncol = 2) + theme_minimal(base_size = 13),
        selected_year = input$selected_year_model,
        selected_vars = input$model_vars,
        transform_y = input$transform_y
      )
      
      # Render file Rmd menjadi PDF
      # Menyalin template ke folder sementara untuk menghindari masalah izin tulis
      temp_report <- file.path(tempdir(), "report-template.Rmd")
      file.copy("report-template.Rmd", temp_report, overwrite = TRUE)
      rmarkdown::render("report-template.Rmd", 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  # REAKTIF UNTUK INTERPRETASI MODEL
  model_interpretation_string <- reactive({
    req(model_fit())
    model <- model_fit()
    
    summary_model <- if (input$use_robust_se) {
      coeftest(model, vcov. = vcovHC(model, type = "HC1"))
    } else {
      summary(model)
    }
    
    # --- Bagian R-squared ---
    glance_data <- glance(model)
    r_squared <- sprintf("%.3f", glance_data$r.squared)
    adj_r_squared <- sprintf("%.3f", glance_data$adj.r.squared)
    
    y_name_display <- if (input$transform_y == "log") "logaritma suhu" else if (input$transform_y == "sqrt") "akar kuadrat suhu" else "suhu"
    
    model_type_text <- ""
    interpreted_r_squared <- ""
    
    if (length(input$model_vars) == 1) {
      model_type_text <- "linier sederhana"
      interpreted_r_squared <- paste0("memiliki nilai R-squared sebesar <b>", r_squared, "</b>. Ini berarti sekitar <b>", round(as.numeric(r_squared) * 100, 1), "%</b> dari variasi pada ", y_name_display, " dapat dijelaskan oleh variabel prediktor yang Anda pilih.")
    } else {
      model_type_text <- "linier berganda"
      interpreted_r_squared <- paste0("memiliki nilai Adjusted R-squared sebesar <b>", adj_r_squared, "</b> (R-squared: ", r_squared, "). Ini berarti sekitar <b>", round(as.numeric(adj_r_squared) * 100, 1), "%</b> dari variasi pada ", y_name_display, " dapat dijelaskan oleh kombinasi variabel prediktor yang Anda pilih, setelah disesuaikan untuk jumlah prediktor.")
    }
    
    interpretasi <- paste0(
      "<p><b>Interpretasi Model Regresi (Tahun ", input$selected_year_model, "):</b></p>",
      "<p>Model regresi ", model_type_text, " yang Anda bangun, dengan variabel dependen <b>", y_name_display, "</b>, ",
      interpreted_r_squared, "</p>"
    )
    
    # --- Bagian Koefisien dan Signifikansi ---
    interpretasi <- paste0(interpretasi, "<p><b>Dampak Variabel Independen:</b></p><ul>")
    tidy_summary <- tidy(summary_model)
    
    # Cek apakah ada koefisien selain intercept (untuk menghindari error jika model gagal)
    if (nrow(tidy_summary) > 0 && "(Intercept)" %in% tidy_summary$term) {
      for (i in 1:nrow(tidy_summary)) {
        term <- tidy_summary$term[i]
        estimate <- sprintf("%.4f", tidy_summary$estimate[i])
        p_value <- tidy_summary$p.value[i]
        
        # Skip intercept untuk interpretasi dampak
        if (term == "(Intercept)") next
        
        significance_text <- if (p_value < 0.001) {
          "sangat signifikan secara statistik (p < 0.001)"
        } else if (p_value < 0.01) {
          "sangat signifikan secara statistik (p < 0.01)"
        } else if (p_value < 0.05) {
          "signifikan secara statistik (p < 0.05)"
        } else if (p_value < 0.1) {
          "signifikan secara statistik pada tingkat 10% (p < 0.1)"
        } else {
          "tidak signifikan secara statistik (p >= 0.1)"
        }
        
        direction <- if (as.numeric(estimate) > 0) "peningkatan" else "penurunan"
        
        # Mapping nama variabel agar lebih user-friendly
        display_term <- switch(term,
                               "ndvi" = "NDVI",
                               "kepadatan" = "kepadatan penduduk",
                               "landcoverPerairan" = "landcover Perairan",
                               "landcoverHutan.Jarum.Hijau.Sepanjang.Tahun" = "landcover Hutan Jarum Hijau",
                               "landcoverHutan.Daun.Lebar.Sepanjang.Tahun" = "landcover Hutan Daun Lebar",
                               "landcoverHutan.Jarum.Gugur" = "landcover Hutan Jarum Gugur",
                               "landcoverHutan.Daun.Lebar.Gugur" = "landcover Hutan Daun Lebar Gugur",
                               "landcoverHutan.Campuran" = "landcover Hutan Campuran",
                               "landcoverSemak.Rapat" = "landcover Semak Rapat",
                               "landcoverSemak.Terbuka" = "landcover Semak Terbuka",
                               "landcoverSavana.Berpepohonan" = "landcover Savana Berpepohonan",
                               "landcoverSavana" = "landcover Savana",
                               "landcoverPadang.Rumput" = "landcover Padang Rumput",
                               "landcoverLahan.Basah.Permanen" = "landcover Lahan Basah Permanen",
                               "landcoverLahan.Pertanian" = "landcover Lahan Pertanian",
                               "landcoverKawasan.Perkotaan/Terbangun" = "landcover Kawasan Perkotaan/Terbangun",
                               "landcoverCampuran.Pertanian.dan.Vegetasi.Alami" = "landcover Campuran Pertanian dan Vegetasi Alami",
                               "landcoverSalju.dan.Es" = "landcover Salju dan Es",
                               "landcoverGersang.atau.Minim.Vegetasi" = "landcover Gersang atau Minim Vegetasi",
                               "landcoverTidak.Ada.Data" = "landcover Tidak Ada Data",
                               term # default jika tidak ada di switch
        )
        
        reference_phrase <- ""
        # Cek apakah 'term' dimulai dengan 'landcover' (indikasi variabel dummy dari faktor)
        if (startsWith(term, "landcover")) {
          reference_phrase <- " (dibandingkan dengan kategori referensi)"
        }
        
        interpretasi <- paste0(interpretasi,
                               "<li><b>", display_term, ":</b> Koefisien sebesar <b>", estimate, "</b> (p-value = ", sprintf("%.3f", p_value), ") menunjukkan ", significance_text, ". ",
                               "Ini berarti setiap peningkatan satu unit pada ", display_term, reference_phrase, " terkait dengan ", direction, " suhu sebesar ", abs(as.numeric(estimate)), " °C, dengan variabel lain konstan.",
                               "</li>"
        )
      }
    } else {
      interpretasi <- paste0(interpretasi, "<li>Tidak ada variabel independen yang signifikan atau model tidak dapat di-fit dengan benar.</li>")
    }
    interpretasi <- paste0(interpretasi, "</ul>")
    
    # --- Bagian Asumsi Model ---
    # Di sini kita akan langsung menghitung hasil uji asumsi
    # agar lebih mudah diinterpretasi secara terstruktur
    
    interpretasi <- paste0(interpretasi, "<p><b>Catatan Mengenai Asumsi Model:</b></p><ul>")
    
    # 1. Normalitas Residual
    normalitas_status <- tryCatch({
      res_std <- rstandard(model)
      ks_test <- ks.test(res_std, "pnorm")
      if (ks_test$p.value < 0.05) "Tidak terpenuhi" else "Terpenuhi"
    }, error = function(e) "ERROR (tidak dapat diuji)")
    interpretasi <- paste0(interpretasi, "<li>Uji Normalitas Residual: <b>", normalitas_status, "</b>. Ini menunjukkan sisaan model ", ifelse(normalitas_status == "Terpenuhi", "berdistribusi normal.", "mungkin tidak berdistribusi normal, yang dapat memengaruhi validitas inferensi statistik."), "</li>")
    
    # 2. Homoskedastisitas
    homoskedastisitas_status <- tryCatch({
      bp_test <- bptest(model)
      if (bp_test$p.value < 0.05) "Tidak terpenuhi" else "Terpenuhi"
    }, error = function(e) "ERROR (tidak dapat diuji)")
    interpretasi <- paste0(interpretasi, "<li>Uji Homoskedastisitas (Breusch-Pagan): <b>", homoskedastisitas_status, "</b>. Varians sisaan model ", ifelse(homoskedastisitas_status == "Terpenuhi", "konstan.", "mungkin tidak konstan (heteroskedastisitas terdeteksi), yang dapat menyebabkan standar error menjadi bias. Penggunaan Robust Standard Errors <b>direkomendasikan</b> dalam kasus ini."), "</li>")
    
    # 3. Autokorelasi
    autokorelasi_status <- tryCatch({
      dw_test <- dwtest(model)
      if (dw_test$p.value < 0.05) "Terdeteksi" else "Tidak terdeteksi"
    }, error = function(e) "ERROR (tidak dapat diuji)")
    interpretasi <- paste0(interpretasi, "<li>Uji Autokorelasi (Durbin-Watson): <b>", autokorelasi_status, "</b>. Residual model menunjukkan ", ifelse(autokorelasi_status == "Tidak terdeteksi", "independen.", "autokorelasi, yang berarti pengamatan tidak independen. Ini sering terjadi pada data spasial atau deret waktu dan dapat mengarah pada standar error yang bias."), "</li>")
    
    # 4. Multikolinearitas (VIF)
    # Di sini kita akan menghitung VIF secara langsung
    if (length(input$model_vars) > 1) { # VIF hanya relevan jika ada >1 prediktor
      vif_status <- tryCatch({
        vif_values <- vif(model)
        max_vif <- max(vif_values)
        if (max_vif >= 5) { # Umumnya threshold 5 atau 10
          paste0("Ada indikasi <b>multikolinearitas</b> di antara prediktor (VIF tertinggi: ", sprintf("%.2f", max_vif), "). Ini dapat membuat estimasi koefisien menjadi tidak stabil.")
        } else {
          "Tidak ada bukti multikolinearitas yang signifikan."
        }
      }, error = function(e) paste0("ERROR saat menghitung VIF: ", e$message))
      interpretasi <- paste0(interpretasi, "<li>Uji Multikolinearitas (VIF): ", vif_status, "</li>")
    } else {
      interpretasi <- paste0(interpretasi, "<li>Uji Multikolinearitas (VIF): Tidak dapat dihitung karena hanya ada satu variabel independen.</li>")
    }
    
    interpretasi <- paste0(interpretasi, "</ul>")
    
    HTML(interpretasi)
  })
  
  
  output$model_interpretation <- renderUI({
    if (is.null(model_fit())) {
      # Tampilkan pesan jika tidak ada variabel terpilih ATAU model gagal
      if (length(input$model_vars) == 0) {
        return(HTML("<p>Pilih setidaknya satu variabel independen untuk membangun model regresi dan melihat interpretasinya.</p>"))
      } else {
        # Pesan generik jika model gagal karena alasan lain
        return(HTML("<p>Tidak dapat membuat model regresi dengan variabel yang dipilih. Periksa data atau kombinasi variabel.</p>"))
      }
    }
    model_interpretation_string() # Hanya jalankan jika model_fit() tidak NULL
  })
  
  explore_interpretation_string <- reactive({
    req(explore_data(), input$x_var)
    df <- explore_data()
    
    if (nrow(df) == 0) {
      return(HTML("<p>Tidak ada data yang tersedia untuk tahun yang dipilih. Silakan sesuaikan pilihan Anda.</p>"))
    }
    
    interpretasi_teks <- paste0("<p><b>Interpretasi Hubungan (Tahun ", input$year_explore, ")</b></p>")
    
    if (is.numeric(df$x_var)) { # Jika X adalah NDVI atau Kepadatan Penduduk
      kor <- cor(df$suhu, df$x_var, use = "pairwise.complete.obs")
      kor_formatted <- sprintf("%.3f", kor)
      
      interpretasi_teks <- paste0(interpretasi_teks,
                                  "<p>Grafik ini menunjukkan sebaran titik data untuk Suhu Permukaan (Y) terhadap <b>", input$x_var, "</b> (X). Koefisien korelasi Pearson antara kedua variabel ini adalah <b>", kor_formatted, "</b>.</p>"
      )
      
      if (input$x_var == "ndvi") {
        if (kor < 0) {
          interpretasi_teks <- paste0(interpretasi_teks,
                                      "<p>Nilai korelasi negatif yang teramati (", kor_formatted, ") mengindikasikan bahwa semakin tinggi nilai NDVI (yang berarti vegetasi semakin lebat), cenderung semakin rendah suhu permukaan. Ini konsisten dengan peran vegetasi sebagai pendingin alami dalam mengurangi efek panas.</p>"
          )
        } else {
          interpretasi_teks <- paste0(interpretasi_teks,
                                      "<p>Korelasi (", kor_formatted, ") antara NDVI dan suhu permukaan ini tidak menunjukkan hubungan negatif yang kuat seperti yang umum diharapkan. Ada kemungkinan faktor lain atau variasi spasial yang memengaruhi pola ini.</p>"
          )
        }
      } else if (input$x_var == "kepadatan") {
        if (kor > 0) {
          interpretasi_teks <- paste0(interpretasi_teks,
                                      "<p>Korelasi positif yang teramati (", kor_formatted, ") menunjukkan bahwa semakin tinggi kepadatan penduduk, cenderung semakin tinggi pula suhu permukaan. Temuan ini mendukung fenomena Urban Heat Island (UHI), di mana area padat penduduk dan terbangun umumnya lebih panas.</p>"
          )
        } else {
          interpretasi_teks <- paste0(interpretasi_teks,
                                      "<p>Korelasi (", kor_formatted, ") antara kepadatan penduduk dan suhu permukaan ini tidak menunjukkan hubungan positif yang kuat seperti yang diharapkan. Perlu dipertimbangkan faktor-faktor lain yang mungkin memengaruhi suhu di area padat penduduk.</p>"
          )
        }
      }
      
      # Interpretasi berdasarkan model yang dipilih (Linear, Quadratic, Cubic)
      if (!is.null(input$model_type) && length(input$model_type) > 0) {
        interpretasi_teks <- paste0(interpretasi_teks, "<p>Garis model yang Anda pilih memvisualisasikan potensi tren:</p><ul>")
        
        if ("Linier" %in% input$model_type) {
          m_linier <- lm(suhu ~ x_var, data = df)
          p_val_linier <- tidy(m_linier) %>% filter(term == "x_var") %>% pull(p.value)
          signif_linier <- if (p_val_linier < 0.05) "signifikan secara statistik" else "tidak signifikan secara statistik"
          
          interpretasi_teks <- paste0(interpretasi_teks,
                                      "<li>Garis Linier: Menggambarkan hubungan lurus. Hubungan ini secara ", signif_linier, " (p-value: ", sprintf("%.3f", p_val_linier), ").</li>")
        }
        if ("Kuadratik" %in% input$model_type) {
          m_quad <- lm(suhu ~ x_var + I(x_var^2), data = df)
          # Periksa signifikansi salah satu atau kedua koefisien x_var dan I(x_var^2)
          p_val_quad_x <- tidy(m_quad) %>% filter(term == "x_var") %>% pull(p.value)
          p_val_quad_x2 <- tidy(m_quad) %>% filter(term == "I(x_var^2)") %>% pull(p.value)
          signif_quad <- if (p_val_quad_x < 0.05 || p_val_quad_x2 < 0.05) "signifikan secara statistik" else "tidak signifikan secara statistik"
          interpretasi_teks <- paste0(interpretasi_teks,
                                      "<li>Garis Kuadratik: Menggambarkan hubungan melengkung (kurva). Hubungan ini secara ", signif_quad, ".</li>")
        }
        if ("Kubik" %in% input$model_type) {
          m_cubic <- lm(suhu ~ x_var + I(x_var^2) + I(x_var^3), data = df)
          # Periksa signifikansi salah satu atau lebih koefisien
          p_val_cub_x <- tidy(m_cubic) %>% filter(term == "x_var") %>% pull(p.value)
          p_val_cub_x2 <- tidy(m_cubic) %>% filter(term == "I(x_var^2)") %>% pull(p.value)
          p_val_cub_x3 <- tidy(m_cubic) %>% filter(term == "I(x_var^3)") %>% pull(p.value)
          signif_cub <- if (p_val_cub_x < 0.05 || p_val_cub_x2 < 0.05 || p_val_cub_x3 < 0.05) "signifikan secara statistik" else "tidak signifikan secara statistik"
          interpretasi_teks <- paste0(interpretasi_teks,
                                      "<li>Garis Kubik: Menggambarkan hubungan dengan beberapa belokan. Hubungan ini secara ", signif_cub, ".</li>")
        }
        interpretasi_teks <- paste0(interpretasi_teks, "</ul>")
      }
      
    } else { # Jika X adalah Tutupan Lahan (Kategorikal)
      avg_suhu_per_lc <- df %>%
        group_by(x_var) %>%
        summarise(avg_suhu = mean(suhu, na.rm = TRUE)) %>%
        arrange(desc(avg_suhu)) # Urutkan dari yang terpanas
      
      interpretasi_teks <- paste0(interpretasi_teks,
                                  "<p>Grafik ini menampilkan distribusi suhu permukaan berdasarkan setiap kategori tutupan lahan. Anda dapat mengamati perbedaan suhu rata-rata antar kategori:</p><ul>"
      )
      
      for (i in 1:nrow(avg_suhu_per_lc)) {
        lc_name <- avg_suhu_per_lc$x_var[i]
        avg_temp <- sprintf("%.2f", avg_suhu_per_lc$avg_suhu[i])
        interpretasi_teks <- paste0(interpretasi_teks, "<li><b>", lc_name, ":</b> Rata-rata suhu <b>", avg_temp, " °C</b></li>")
      }
      interpretasi_teks <- paste0(interpretasi_teks, "</ul>")
      
      if (nrow(avg_suhu_per_lc) > 1) {
        hottest_lc <- avg_suhu_per_lc$x_var[1]
        coldest_lc <- avg_suhu_per_lc$x_var[nrow(avg_suhu_per_lc)]
        
        interpretasi_teks <- paste0(interpretasi_teks,
                                    "<p>Secara umum, kategori tutupan lahan <b>", hottest_lc, "</b> cenderung memiliki suhu rata-rata tertinggi, sementara <b>", coldest_lc, "</b> adalah yang terendah. Pola ini menyoroti bagaimana karakteristik permukaan lahan sangat memengaruhi suhu lokal dan berkontribusi pada pembentukan UHI.</p>"
        )
      }
    }
    
    HTML(interpretasi_teks)
  })
  
  output$explore_interpretation <- renderUI({
    explore_interpretation_string()
  })
  
  model_comparison_interpretation_string <- reactive({
    df_stats <- data_compare_model_stats() # Sekarang panggil reaktif yang baru
    
    if (is.null(df_stats) || nrow(df_stats) == 0) {
      return(HTML("<p>Pilih setidaknya satu model untuk melihat perbandingan statistik.</p>"))
    }
    
    interpretasi_komparasi <- paste0("<p><b>Interpretasi Perbandingan Model:</b></p>")
    
    # Temukan model dengan AIC terendah (umumnya lebih baik)
    best_aic_model <- df_stats %>%
      arrange(AIC) %>%
      slice(1)
    
    # Temukan model dengan R-Squared tertinggi (umumnya lebih baik)
    best_r_squared_model <- df_stats %>%
      arrange(desc(`R Squared`)) %>%
      slice(1)
    
    interpretasi_komparasi <- paste0(interpretasi_komparasi,
                                     "<p>Berdasarkan kriteria <b>Akaike Information Criterion (AIC)</b>, model <b>", best_aic_model$Model, "</b> (AIC: ", sprintf("%.2f", best_aic_model$AIC), ") adalah yang paling sesuai, karena nilai AIC yang lebih rendah menunjukkan model yang lebih baik dalam menyeimbangkan kesesuaian data dan kompleksitas.</p>",
                                     "<p>Adapun berdasarkan <b>R-squared</b>, model <b>", best_r_squared_model$Model, "</b> (R-squared: ", sprintf("%.3f", best_r_squared_model$`R Squared`), ") adalah yang paling baik dalam menjelaskan proporsi variasi suhu yang dapat dijelaskan oleh variabel independen.</p>"
    )
    
    if (best_aic_model$Model == best_r_squared_model$Model) {
      interpretasi_komparasi <- paste0(interpretasi_komparasi,
                                       "<p>Dalam kasus ini, model <b>", best_aic_model$Model, "</b> direkomendasikan karena memberikan keseimbangan terbaik antara kecocokan model dan kompleksitasnya berdasarkan kedua kriteria.</p>")
    } else {
      interpretasi_komparasi <- paste0(interpretasi_komparasi,
                                       "<p>Perbedaan antara model terbaik berdasarkan AIC dan R-squared menyarankan bahwa pilihan model mungkin bergantung pada prioritas analisis Anda (prediksi vs. parsimoni).</p>")
    }
    
    HTML(interpretasi_komparasi)
  })
  
  output$model_comparison_interpretation <- renderUI({
    model_comparison_interpretation_string()
  })
  
  output$model_comparison_section <- renderUI({
    req(input$x_var, explore_data()) # Pastikan x_var dan data tersedia
    
    df <- explore_data()
    
    if (is.numeric(df$x_var)) {
      # Jika x_var numerik, tampilkan seluruh bagian perbandingan model
      tagList(
        h4("Perbandingan Statistik Model"),
        tableOutput("compareModelStats"),
        uiOutput("model_comparison_interpretation")
      )
    } else {
      NULL
    }
  })
  
  descriptive_stats_interpretation_string <- reactive({
    req(data_aktif())
    df <- data_aktif()
    
    interpretasi_teks <- ""
    
    # 1. Informasi Umum Data Set
    n_rows <- nrow(df)
    n_cols <- ncol(df)
    min_tahun <- min(df$tahun, na.rm = TRUE)
    max_tahun <- max(df$tahun, na.rm = TRUE)
    
    interpretasi_teks <- paste0(interpretasi_teks,
                                "<p>Dataset ini mencakup <b>", n_rows, "</b> observasi (titik lokasi/waktu) dan <b>", n_cols, "</b> variabel, dengan data yang dikumpulkan dari tahun <b>", min_tahun, "</b> hingga <b>", max_tahun, "</b>.</p>"
    )
    
    # 2. Interpretasi Suhu (LST)
    if ("suhu" %in% names(df)) {
      suhu_min <- sprintf("%.2f", min(df$suhu, na.rm = TRUE))
      suhu_max <- sprintf("%.2f", max(df$suhu, na.rm = TRUE))
      suhu_mean <- sprintf("%.2f", mean(df$suhu, na.rm = TRUE))
      suhu_sd <- sprintf("%.2f", sd(df$suhu, na.rm = TRUE))
      
      interpretasi_teks <- paste0(interpretasi_teks,
                                  "<p><b>Suhu Permukaan (LST):</b> Rata-rata suhu permukaan teramati adalah <b>", suhu_mean, " °C</b>. Suhu bervariasi dari <b>", suhu_min, " °C</b> hingga <b>", suhu_max, " °C</b> (dengan standar deviasi: ", suhu_sd, "). Rentang suhu yang cukup lebar ini mengindikasikan adanya perbedaan suhu yang signifikan antar lokasi atau waktu, yang dapat menjadi indikator potensi fenomena Urban Heat Island (UHI).</p>"
      )
    }
    
    # 3. Interpretasi NDVI
    if ("ndvi" %in% names(df)) {
      ndvi_min <- sprintf("%.2f", min(df$ndvi, na.rm = TRUE))
      ndvi_max <- sprintf("%.2f", max(df$ndvi, na.rm = TRUE))
      ndvi_mean <- sprintf("%.2f", mean(df$ndvi, na.rm = TRUE))
      
      interpretasi_teks <- paste0(interpretasi_teks,
                                  "<p><b>NDVI (Normalized Difference Vegetation Index):</b> Rata-rata nilai NDVI adalah <b>", ndvi_mean, "</b>, berkisar antara <b>", ndvi_min, "</b> (vegetasi sangat jarang atau tidak ada) dan <b>", ndvi_max, "</b> (vegetasi sangat lebat). Nilai NDVI yang lebih tinggi umumnya berasosiasi dengan area yang lebih sejuk.</p>"
      )
    }
    
    # 4. Interpretasi Kepadatan Penduduk
    if ("kepadatan" %in% names(df)) {
      kepadatan_min <- sprintf("%.0f", min(df$kepadatan, na.rm = TRUE))
      kepadatan_max <- sprintf("%.0f", max(df$kepadatan, na.rm = TRUE))
      kepadatan_mean <- sprintf("%.0f", mean(df$kepadatan, na.rm = TRUE))
      
      interpretasi_teks <- paste0(interpretasi_teks,
                                  "<p><b>Kepadatan Penduduk:</b> Rata-rata kepadatan penduduk adalah <b>", kepadatan_mean, " orang/km²</b>, dengan kisaran dari <b>", kepadatan_min, "</b> hingga <b>", kepadatan_max, " orang/km²</b>. Area dengan kepadatan penduduk yang tinggi seringkali menjadi indikator kawasan perkotaan yang rentan terhadap pembentukan UHI.</p>"
      )
    }
    
    # 5. Interpretasi Tutupan Lahan (Kategori Paling Dominan)
    if ("landcover" %in% names(df) && is.factor(df$landcover)) {
      # Menghitung frekuensi setiap kategori tutupan lahan
      lc_counts <- df %>%
        count(landcover) %>%
        arrange(desc(n))
      
      if (nrow(lc_counts) > 0) {
        most_dominant_lc <- lc_counts$landcover[1]
        most_dominant_n <- lc_counts$n[1]
        most_dominant_perc <- sprintf("%.1f", (most_dominant_n / n_rows) * 100)
        
        interpretasi_teks <- paste0(interpretasi_teks,
                                    "<p><b>Tutupan Lahan:</b> Kategori tutupan lahan yang paling dominan dalam dataset ini adalah <b>", most_dominant_lc, "</b>, yang menyumbang sekitar <b>", most_dominant_perc, "%</b> dari total observasi. Distribusi tutupan lahan ini sangat memengaruhi pola suhu permukaan yang diamati di berbagai lokasi.</p>"
        )
      }
    }
    
    HTML(interpretasi_teks)
  })
  
  output$descriptive_stats_interpretation <- renderUI({
    descriptive_stats_interpretation_string()
  })
  
  output$metadata_variables_table <- renderTable({
    # Buat data.frame statis untuk metadata variabel
    data.frame(
      Variabel = c("suhu (LST)", "ndvi", "kepadatan", "landcover", "tahun", "X (Longitude)", "Y (Latitude)", "Kab_Kota", "Kode_Kab", "provinsi"),
      Deskripsi = c(
        "Suhu Permukaan Darat (°C).",
        "Normalized Difference Vegetation Index, mengukur kehijauan vegetasi (tanpa unit, -1.0 hingga +1.0).",
        "Kepadatan Penduduk (orang/km²).",
        "Kategori penggunaan/penutupan lahan (misal: Kawasan Perkotaan/Terbangun, Hutan, Perairan).",
        "Tahun observasi data.",
        "Koordinat bujur dari titik observasi (°).",
        "Koordinat lintang dari titik observasi (°).",
        "Nama Kabupaten/Kota.",
        "Kode unik untuk Kabupaten/Kota.",
        "Nama Provinsi."
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
}

# ===================================================================
# 5. MENJALANKAN APLIKASI (RUN APPLICATION)
# ===================================================================
shinyApp(ui = ui, server = server)