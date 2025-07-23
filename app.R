# ===================================================================
# SOVIA-3MED Dashboard - Social Vulnerability Analysis
# ===================================================================

# === LOAD LIBRARIES ===
# === LOAD LIBRARIES ===
library(shiny)
library(bslib)
library(DT)
library(sf)
library(dplyr)
library(classInt)
library(ggplot2)
# library(showtext)              # ❌ Nonaktifkan: tidak didukung di shinyapps.io
library(shinycssloaders)
library(grid)
library(gridExtra)
library(leaflet)
library(rmapshaper)
library(openxlsx)
# library(webshot)              # ❌ Nonaktifkan: butuh PhantomJS (tidak tersedia di server shinyapps.io)
# library(mapview)              # ❌ Nonaktifkan: banyak dependensi berat yang tidak cocok di shinyapps.io
library(car)
library(lmtest)
library(MASS)
library(plotly)
library(moments)
# library(officer)              # ❌ Nonaktifkan: Word export tidak stabil di shinyapps.io
# library(flextable)            # ❌ Nonaktifkan: bergantung pada officer
library(nortest)
library(tseries)
library(tidyr)                  # Untuk pivot_longer
library(tibble)                 # Untuk rownames_to_column
# library(rsconnect)            # ❌ Tidak perlu di dalam app; hanya digunakan di lokal untuk deploy


# === 1. Load library yang diperlukan ===
library(sf)
library(rmapshaper)


# === BACA DAN SEDERHANAKAN GEOJSON ===
# PERBAIKAN: Gunakan data sample jika file tidak ada
# === LOAD GEO DATA DARI FILE YANG SUDAH DIKOMPRES (RDS) ===
geo_data <- readRDS("data/geoindonesia.rds")

# PERBAIKAN: Pastikan geo_data memiliki kolom yang diperlukan
geo_data <- geo_data %>%
  mutate(
    kode = as.numeric(substr(as.character(kdprov), 1, 1)),
    region = case_when(
      kode %in% c(1, 2) ~ "Sumatra",
      kode == 3 ~ "Jawa",
      kode == 5 ~ "Bali & Nusa Tenggara",
      kode == 6 ~ "Kalimantan",
      kode == 7 ~ "Sulawesi",
      kode %in% c(8, 9) ~ "Maluku & Papua",
      TRUE ~ "Lainnya"
    )
  )

# Path ke file data (SESUAIKAN DENGAN LOKASI FILE ANDA)
DATA_PATH <- "data/geoindonesia.geojson"

# Definisi variabel untuk dropdown dengan label yang user-friendly
VARIABLE_CHOICES <- list(
  "Persentase penduduk usia di bawah 5 tahun" = "CHILDREN",
  "Persentase penduduk perempuan" = "FEMALE",
  "Persentase penduduk usia di atas 65 tahun" = "ELDERLY",
  "Persentase rumah tangga dengan kepala keluarga perempuan" = "FHEAD",
  "Rata-rata jumlah anggota rumah tangga" = "FAMILYSIZE",
  "Persentase rumah tangga tanpa listrik" = "NOELECTRIC",
  "Persentase penduduk ≥15 tahun dengan pendidikan rendah" = "LOWEDU",
  "Pertumbuhan penduduk (persentase)" = "GROWTH",
  "Persentase penduduk miskin" = "POVERTY",
  "Persentase buta huruf" = "ILLITERATE",
  "Persentase rumah tangga yang tidak mendapat pelatihan kebencanaan" = "NOTRAINING",
  "Persentase rumah tangga yang tinggal di area rawan bencana" = "DPRONE",
  "Persentase rumah tangga yang menyewa rumah" = "RENTED",
  "Persentase rumah tangga tanpa sistem saluran pembuangan" = "NOSEWER",
  "Persentase rumah tangga yang menggunakan air ledeng" = "TAPWATER",
  "Jumlah penduduk" = "POPULATION"
)

# Nama-nama indikator dalam Bahasa Indonesia
nama_indikator <- c(
  LOWEDU = "Pendidikan Rendah",
  POVERTY = "Kemiskinan",
  ILLITERATE = "Buta Huruf",
  DPRONE = "Tinggal di Daerah Rawan",
  NOTRAINING = "Tanpa Pelatihan Bencana",
  NOELECTRIC = "Tanpa Akses Listrik",
  CHILDREN = "Balita",
  FEMALE = "Perempuan",
  ELDERLY = "Lansia",
  FHEAD = "Kepala Keluarga Perempuan",
  FAMILYSIZE = "Jumlah Anggota Rumah Tangga",
  NOSEWER = "Tanpa Drainase",
  RENTED = "Rumah Sewa",
  TAPWATER = "Air Ledeng",
  POPULATION = "Jumlah Penduduk",
  GROWTH = "Pertumbuhan Penduduk"
)

indikator_labels <- list(
  CHILDREN    = "Persentase Balita",
  FEMALE      = "Persentase Perempuan",
  ELDERLY     = "Persentase Lansia",
  FHEAD       = "Persentase Kepala Rumah Tangga Perempuan",
  FAMILYSIZE  = "Rata-rata Jumlah Anggota Rumah Tangga",
  NOELECTRIC  = "Persentase Rumah Tangga tanpa Listrik",
  LOWEDU      = "Persentase Penduduk Berpendidikan Rendah",
  GROWTH      = "Laju Pertumbuhan Penduduk",
  POVERTY     = "Persentase Penduduk Miskin",
  ILLITERATE  = "Persentase Penduduk Buta Huruf",
  NOTRAINING  = "Persentase Rumah Tangga tanpa Pelatihan Bencana",
  DPRONE      = "Persentase Rumah Tangga di Daerah Rawan Bencana",
  RENTED      = "Persentase Rumah Tangga Mengontrak",
  NOSEWER     = "Persentase Rumah Tangga tanpa Saluran Pembuangan",
  TAPWATER    = "Persentase Rumah Tangga Menggunakan Air Ledeng",
  POPULATION  = "Jumlah Penduduk"
)

# Klasifikasi dan deskripsi
sub_analisis_choices <- list(
  "Dampak" = c("POPULATION", "NOTRAINING", "DPRONE", "FAMILYSIZE", "NOSEWER"),
  "Mitigasi" = c("LOWEDU", "POVERTY", "ILLITERATE", "DPRONE", "NOTRAINING", "NOELECTRIC"),
  "Evakuasi" = c("CHILDREN", "FEMALE", "ELDERLY", "NOTRAINING", "FHEAD")
)

deskripsi_var <- list(
  POPULATION = "Jumlah penduduk. Semakin besar, maka dampak bencana berpotensi luas.",
  NOTRAINING = "Rumah tangga tanpa pelatihan kebencanaan. Semakin tinggi, semakin rendah kesiapsiagaan.",
  DPRONE = "Tinggal di daerah rawan bencana. Semakin tinggi, semakin besar risiko.",
  FAMILYSIZE = "Rata-rata jumlah anggota rumah tangga. Makin besar, makin kompleks evakuasi.",
  NOSEWER = "Tanpa saluran pembuangan. Rentan terhadap penyebaran penyakit pasca bencana.",
  LOWEDU = "Pendidikan rendah. Tantangan besar dalam penyuluhan mitigasi.",
  POVERTY = "Kemiskinan. Rentan secara ekonomi dalam kesiapsiagaan bencana.",
  ILLITERATE = "Buta huruf. Hambatan utama dalam memahami informasi keselamatan.",
  NOELECTRIC = "Tanpa listrik. Keterbatasan akses informasi dan peringatan dini.",
  CHILDREN = "Balita. Sangat rentan dalam evakuasi dan pertolongan.",
  FEMALE = "Perempuan. Mungkin memiliki keterbatasan akses atau mobilitas.",
  ELDERLY = "Lansia. Rentan secara fisik dan sosial saat bencana.",
  FHEAD = "Kepala keluarga perempuan. Bisa mengalami beban ganda dan kerentanan sosial."
)

# Definisikan vektor warna tetap untuk setiap region
region_colors <- c(
  "Sumatra" = "#FFB347",
  "Jawa" = "#77DD77",
  "Bali & Nusa Tenggara" = "#779ECB",
  "Kalimantan" = "#FF6961",
  "Sulawesi" = "#CBAACB",
  "Maluku & Papua" = "#FDFD96"
)

# Fungsi untuk normalisasi 0-1
scale01 <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Fungsi untuk menghitung indeks pembobotan
hitung_indeks <- function(data, sub_analisis) {
  data_calc <- data %>% st_drop_geometry()
  
  if (sub_analisis == "Dampak") {
    data_calc <- data_calc %>%
      mutate(
        POPULATION_norm = scale01(POPULATION),
        RISK_norm = scale01(NOTRAINING + DPRONE),
        indeks_dampak = 0.3 * (0.6 * POPULATION_norm + 0.4 * RISK_norm)
      )
    return(data_calc %>% dplyr::select(nmkab, nmprov, region, indeks_dampak) %>% arrange(desc(indeks_dampak)))
    
  } else if (sub_analisis == "Mitigasi") {
    data_calc <- data_calc %>%
      mutate(
        EDU_norm = scale01(LOWEDU + ILLITERATE),
        POVERTY_norm = scale01(POVERTY),
        TRAINING_norm = scale01(NOTRAINING),
        INFRA_norm = scale01(NOELECTRIC + DPRONE),
        indeks_mitigasi = 0.3 * (0.4 * EDU_norm + 0.3 * POVERTY_norm + 0.2 * TRAINING_norm + 0.1 * INFRA_norm)
      )
    return(data_calc %>% dplyr::select(nmkab, nmprov, region, indeks_mitigasi) %>% arrange(desc(indeks_mitigasi)))
    
  } else if (sub_analisis == "Evakuasi") {
    data_calc <- data_calc %>%
      mutate(
        AGE_norm = scale01(CHILDREN + ELDERLY),
        GENDER_norm = scale01(FEMALE + FHEAD),
        TRAINING_norm = scale01(NOTRAINING),
        indeks_evakuasi = 0.3 * (0.3 * AGE_norm + 0.2 * GENDER_norm + 0.1 * TRAINING_norm)
      )
    return(data_calc %>% dplyr::select(nmkab, nmprov, region, indeks_evakuasi) %>% arrange(desc(indeks_evakuasi)))
  }
}

# Fungsi untuk membuat interpretasi lengkap
create_interpretation <- function(test_result, test_type, variable_name = "", alpha = 0.05) {
  interpretation <- list()
  
  if (test_type == "shapiro") {
    interpretation$hypothesis <- paste0(
      "H₀: Data berdistribusi normal\n",
      "H₁: Data tidak berdistribusi normal"
    )
    interpretation$decision_rule <- paste0("Tolak H₀ jika p-value < α = ", alpha)
    interpretation$test_statistic <- paste0("W = ", round(test_result$statistic, 6))
    interpretation$p_value <- format(test_result$p.value, scientific = TRUE)
    interpretation$decision <- ifelse(test_result$p.value < alpha, "Tolak H₀", "Gagal tolak H₀")
    interpretation$conclusion <- ifelse(test_result$p.value < alpha,
                                        paste0("Pada taraf signifikansi ", alpha, ", terdapat cukup bukti untuk menyatakan bahwa data ", variable_name, " tidak berdistribusi normal."),
                                        paste0("Pada taraf signifikansi ", alpha, ", tidak terdapat cukup bukti untuk menyatakan bahwa data ", variable_name, " tidak berdistribusi normal.")
    )
  } else if (test_type == "levene") {
    interpretation$hypothesis <- paste0(
      "H₀: Variansi antar kelompok homogen\n",
      "H₁: Variansi antar kelompok tidak homogen"
    )
    interpretation$decision_rule <- paste0("Tolak H₀ jika p-value < α = ", alpha)
    interpretation$test_statistic <- paste0("F = ", round(test_result$`F value`[1], 6))
    interpretation$degrees_freedom <- paste0("df1 = ", test_result$Df[1], ", df2 = ", test_result$Df[2])
    interpretation$p_value <- format(test_result$`Pr(>F)`[1], scientific = TRUE)
    interpretation$decision <- ifelse(test_result$`Pr(>F)`[1] < alpha, "Tolak H₀", "Gagal tolak H₀")
    interpretation$conclusion <- ifelse(test_result$`Pr(>F)`[1] < alpha,
                                        paste0("Pada taraf signifikansi ", alpha, ", terdapat cukup bukti untuk menyatakan bahwa variansi ", variable_name, " antar kelompok tidak homogen."),
                                        paste0("Pada taraf signifikansi ", alpha, ", tidak terdapat cukup bukti untuk menyatakan bahwa variansi ", variable_name, " antar kelompok tidak homogen.")
    )
  }
  
  return(interpretation)
}

metadata_variables <- data.frame(
  Variable = c("DISTRICTCODE", "CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", 
               "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", "NOTRAINING", 
               "DPRONE", "RENTED", "NOSEWER", "TAPWATER", "POPULATION"),
  Label = c("Kode Distrik", "Balita", "Perempuan", "Lansia", "Kepala Rumah Tangga Perempuan", 
            "Jumlah Anggota Keluarga", "Rumah Tangga Tanpa Listrik", "Pendidikan Rendah", "Pertumbuhan Penduduk", 
            "Kemiskinan", "Buta Huruf", "Tidak Mengikuti Pelatihan", "Rawan Bencana", "Menyewa Rumah", 
            "Tanpa Saluran Pembuangan", "Sumber Air Ledeng", "Jumlah Penduduk"),
  Description = c("Kode wilayah/kecamatan", 
                  "Persentase penduduk usia di bawah lima tahun",
                  "Persentase penduduk perempuan",
                  "Persentase penduduk usia 65 tahun ke atas",
                  "Persentase rumah tangga dengan kepala rumah tangga perempuan",
                  "Rata-rata jumlah anggota rumah tangga di suatu distrik",
                  "Persentase rumah tangga yang tidak menggunakan listrik sebagai sumber penerangan",
                  "Persentase penduduk usia 15 tahun ke atas dengan pendidikan rendah",
                  "Persentase perubahan jumlah penduduk",
                  "Persentase penduduk miskin",
                  "Persentase penduduk yang tidak bisa membaca dan menulis",
                  "Persentase rumah tangga yang tidak mendapatkan pelatihan kebencanaan",
                  "Persentase rumah tangga yang tinggal di daerah rawan bencana",
                  "Persentase rumah tangga yang menyewa rumah",
                  "Persentase rumah tangga tanpa sistem saluran pembuangan",
                  "Persentase rumah tangga yang menggunakan air ledeng",
                  "Jumlah total penduduk"),
  `Data Type` = c("Karakter", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik",
                  "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik",
                  "Numerik", "Numerik", "Numerik", "Numerik", "Integer"),
  `Scale of Measurement` = c("Nominal", "Rasio", "Rasio", "Rasio", "Rasio", "Rasio",
                             "Rasio", "Rasio", "Rasio", "Rasio", "Rasio", "Rasio",
                             "Rasio", "Rasio", "Rasio", "Rasio", "Rasio"),
  Unit = c("-", "%", "%", "%", "%", "Orang/rumah tangga", "%", "%", "%", "%", "%", "%",
           "%", "%", "%", "%", "Orang"),
  stringsAsFactors = FALSE
)


#============================ UI ============================
ui <- navbarPage(
  title = div(
    class = "navbar-title-container",
    div(class = "main-title", "SOVIA-3MED Dashboard"),
    div(class = "sub-title", "Social Vulnerability Analysis - Mitigasi, Evakuasi, dan Dampak")
  ),
  
  # PERBAIKAN: Load CSS eksternal dengan benar
  header = tags$head(
    # CSS Eksternal - PRIORITAS UTAMA
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    
    # CSS Internal minimal - hanya untuk loading spinner
    tags$style(HTML("
        body {
          zoom: 0.8;
        }    
    
      /* Loading Spinner - tidak ada di CSS eksternal */
      .loading-overlay {
        position: fixed !important;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: rgba(255,255,255,0.8);
        z-index: 9999;
        display: flex;
        justify-content: center;
        align-items: center;
      }
      
      .loading-spinner {
        border: 4px solid #f3f3f3;
        border-top: 4px solid #3498db;
        border-radius: 50%;
        width: 40px;
        height: 40px;
        animation: spin 1s linear infinite;
      }
      
      @keyframes spin {
        0% { transform: rotate(0deg); }
        100% { transform: rotate(360deg); }
      }
      
      /* Pastikan background image terlihat */
      .container-fluid {
        background: transparent !important;
      }
      
      .tab-content {
        background: transparent !important;
      }
    "))
  ),
  
  #============================ BERANDA ============================
  tabPanel("BERANDA",
      div(id = "tab_beranda",
           tags$head(
             tags$script(src = "https://html2canvas.hertzen.com/dist/html2canvas.min.js"),
             tags$script(HTML("
                function downloadBeranda() {
                  html2canvas(document.getElementById('tab_beranda')).then(function(canvas) {
                    var link = document.createElement('a');
                    link.download = 'halaman_beranda.png';
                    link.href = canvas.toDataURL();
                    link.click();
                  });
                }
            
                $(document).on('click', '#download_beranda_img', function() {
                  downloadBeranda();
                });
              "))
           ),
           
           br(),
           
           # === JUDUL UTAMA ===
           fluidRow(
             column(12, align = "center",
                    h2(style = "color:white; font-weight:700; margin-top:20px;", "SOVIA-3MED Dashboard"),
                    h4(style = "color:#e0e0e0; font-weight:400;",
                       "Social Vulnerability Analysis: Mitigasi, Evakuasi, dan Dampak")
             )
           ),
           br(),
           
           # === LATAR BELAKANG & GALERI ===
           fluidRow(
             column(1),
             column(6,
                    wellPanel(
                      style = "color:#2b2d38; min-height: 630px;",
                      h3("Latar Belakang", style = "text-align: center; font-weight:600;"),
                      p("Indonesia merupakan salah satu negara dengan tingkat kerentanan bencana alam yang sangat tinggi, baik dari segi frekuensi kejadian maupun dampak sosial-ekonomi yang ditimbulkan. Namun, kerentanan terhadap bencana tidak hanya ditentukan oleh faktor alam, melainkan juga oleh kerentanan sosial masyarakat, seperti tingkat pendidikan, kemiskinan, kesiapsiagaan, dan karakteristik demografi."),
                      h4("Kondisi dan Tantangan Sosial"),
                      p("Wilayah padat penduduk seperti Sumatra dan Jawa menghadapi risiko tinggi akibat kombinasi populasi besar dan kesiapan bencana yang masih terbatas. Kelompok rentan seperti anak-anak, lansia, dan perempuan memerlukan perhatian khusus dalam proses evakuasi dan penanganan pascabencana. Temuan dari data SUSENAS 2017 mengungkap tantangan signifikan ini dan memperkuat urgensi kebijakan yang berbasis data dan keadilan sosial."),
                      h4("Tujuan Dashboard"),
                      p("Dashboard ini dibangun untuk menyajikan analisis spasial kerentanan sosial terhadap bencana berbasis data dari SUSENAS 2017 dan peta spasial Indonesia. Tujuan utama dashboard ini adalah untuk:"),
                      tags$ul(style = "margin-left: 25px; padding-left: 10px; list-style-type: disc;",
                              tags$li("Mengidentifikasi wilayah-wilayah di Indonesia yang paling rentan secara sosial terhadap bencana."),
                              tags$li("Memberikan dasar data yang kuat untuk perencanaan pembangunan wilayah dan mitigasi bencana yang berbasis data."),
                              tags$li("Membantu pengambil kebijakan dalam menentukan wilayah prioritas intervensi, baik dalam konteks edukasi mitigasi, kesiapan tanggap darurat, maupun alokasi sumber daya.")
                      )
                    )
             ),
             column(4,
                    wellPanel(
                      style = "min-height: 630px;",
                      h3("Galeri Indonesia", style ="color:#2b2d38; font-weight:600; text-align: center;"),
                      fluidRow(
                        lapply(1:6, function(i) {
                          column(6, img(src = paste0("foto", i, ".jpg"), width = "100%", height = "100%",
                                        style = "object-fit:cover; border-radius:10px; margin-bottom:15px;"))
                        })
                      )
                    )
             ),
             column(1)
           ),
           
           # === PENJELASAN FITUR ===
           fluidRow(
             column(1),
             column(10,
                    wellPanel(
                      h3("Fitur Dashboard", style = "color:#2b2d38; font-weight:600; text-align:center;"),
                      p("Dashboard SOVIA-3MED menyediakan berbagai fitur analisis yang dapat membantu dalam pengambilan keputusan terkait mitigasi bencana.",
                        style = "color:#2b2d38; text-align:center; margin-bottom:25px;"),
                      
                      # Card fitur dalam bentuk 2 baris x 3 kolom
                      fluidRow(
                        # Baris 1
                        column(4,
                               div(class = "feature-card",
                                   h5("📊 Eksplorasi Data", style = "font-weight:600; margin-bottom:15px;"),
                                   p("Visualisasi data interaktif dengan grafik, tabel, dan peta tematik untuk memahami pola kerentanan sosial di berbagai wilayah Indonesia.",
                                     style = "font-size:14px; line-height:1.4; text-align:center !important;")
                               )
                        ),
                        column(4,
                               div(class = "feature-card",
                                   h5("🧮 Statistik Inferensia", style = "font-weight:600; margin-bottom:15px;"),
                                   p("Uji hipotesis, perbandingan rata-rata, uji proporsi, dan ANOVA untuk analisis mendalam perbedaan antar wilayah dan kelompok.",
                                     style = "font-size:14px; line-height:1.4; text-align:center !important;")
                               )
                        ),
                        column(4,
                               div(class = "feature-card",
                                   h5("📈 Regresi Linear", style = "font-weight:600; margin-bottom:15px;"),
                                   p("Analisis regresi berganda lengkap dengan uji asumsi untuk memahami faktor-faktor yang mempengaruhi kerentanan sosial.",
                                     style = "font-size:14px; line-height:1.4; text-align:center !important;")
                               )
                        )
                      ),
                      
                      # Baris 2
                      fluidRow(
                        column(4,
                               div(class = "feature-card",
                                   h5("⚙️ Manajemen Data", style = "font-weight:600; margin-bottom:15px;"),
                                   p("Fasilitas untuk kategorisasi data kontinyu, transformasi variabel, dan preprocessing data sesuai kebutuhan analisis.",
                                     style = "font-size:14px; line-height:1.4; text-align:center !important;")
                               )
                        ),
                        column(4,
                               div(class = "feature-card",
                                   h5("✅ Uji Asumsi", style = "font-weight:600; margin-bottom:15px;"),
                                   p("Uji normalitas dan homogenitas data sebagai prasyarat analisis statistik parametrik dengan interpretasi yang lengkap.",
                                     style = "font-size:14px; line-height:1.4; text-align:center !important;")
                               )
                        ),
                        column(4,
                               div(class = "feature-card",
                                   h5("💾 Export Hasil", style = "font-weight:600; margin-bottom:15px;"),
                                   p("Semua hasil analisis, grafik, dan tabel dapat diunduh dalam format JPG, PDF, Word, dan Excel untuk dokumentasi dan presentasi.",
                                     style = "font-size:14px; line-height:1.4; text-align:center !important;")
                               )
                        )
                      )
                    )
             ),
             column(1)
           ),
           
           
           # === FOKUS ANALISIS ===
           fluidRow(
             column(1),
             column(10,
                    wellPanel(
                      h3("Fokus Analisis", style = "color:#2b2d38; font-weight:600; text-align:center;"),
                      br(),
                      fluidRow(
                        column(4,
                               div(class = "card",
                                   h5("DAMPAK", style = "color:#2b2d38; text-align:center;font-weight:600;"),
                                   img(src = "dampak.jpg", width = "100%", height = "180px",
                                       style = "object-fit:cover; border-radius:12px;"),
                                   br(),
                                   h5("Wilayah dengan Populasi Besar tetapi Tidak Siap Menghadapi Bencana", style = "color:#2b2d38; text-align:center; font-weight:600;"),
                                   p("Menentukan daerah yang bila terkena bencana berpotensi menimbulkan korban massal karena jumlah penduduk besar, rawan bencana, dan tidak siap.",
                                     style = "color:#2b2d38; text-align:center !important;font-weight:400;")
                               )
                        ),
                        column(4,
                               div(class = "card",
                                   h5("MITIGASI", style = "color:#2b2d38; text-align:center;font-weight:600;"),
                                   img(src = "mitigasi.jpg", width = "100%", height = "180px",
                                       style = "object-fit:cover; border-radius:12px;"),
                                   br(),
                                   h5("Wilayah Prioritas untuk Penyebaran Informasi Mitigasi Bencana", style = "color:#2b2d38; text-align:center;font-weight:600;"),
                                   p("Menentukan wilayah yang paling membutuhkan penyuluhan dan pelatihan mitigasi bencana.",
                                     style = "color:#2b2d38; text-align:center !important;font-weight:400;")
                               )
                        ),
                        column(4,
                               div(class = "card",
                                   h5("EVAKUASI", style = "color:#2b2d38; text-align:center;font-weight:600;"),
                                   img(src = "evakuasi.jpg", width = "100%", height = "180px",
                                       style = "object-fit:cover; border-radius:12px;"),
                                   br(),
                                   h5("Wilayah yang Harus Diprioritaskan Saat Terjadi Bencana", style = "color:#2b2d38; text-align:center;font-weight:600;"),
                                   p("Menentukan daerah yang memiliki penduduk sangat rentan secara fisik dan sosial, yang kemungkinan besar tidak dapat menyelamatkan diri secara mandiri.",
                                     style = "color:#2b2d38; text-align:center !important;font-weight:400;")
                               )
                        )
                      )
                    )
             ),
             column(1)
           ),
           
           # === METADATA ===
           fluidRow(
             column(1),
             column(10,
                    wellPanel(
                      h3("Metadata Dashboard", style = "color:#2b2d38; font-weight:600; text-align:center;"),
                      
                      # METADATA STRUKTURAL
                      h4("Metadata Struktural", style = "color:#2b2d38; font-weight:500; margin-top:25px;"),
                      div(style = "overflow-x: auto; margin: 15px 0;",
                          DT::dataTableOutput("metadata_table")
                      ),
                      div(style = "text-align: center; margin: 20px 0;",
                          downloadButton("download_metadata_csv", "📥 UNDUH Metadata (CSV)", 
                                         class = "btn-primary", style = "margin-right: 10px;"),
                          downloadButton("download_metadata_excel", "📥 UNDUH Metadata (Excel)", 
                                         class = "btn-success")
                      ),
                      
                      # METADATA REFERENSI
                      h4("Metadata Referensi", style = "color:#2b2d38; font-weight:500; margin-top:30px;"),
                      p("Dokumentasi lengkap mengenai sumber dan struktur variabel dalam dashboard:",
                        style = "color:#2b2d38; margin-bottom:15px; text-align:center;"),
                      div(style = "text-align: center;",
                          downloadButton("download_metadata_reference", "📄 UNDUH Metadata Referensi (PDF)",
                                         class = "btn-info")
                      )
                    )
             ),
             column(1)
           ),
           
           
           # === TENTANG SAYA ===
           fluidRow(
             column(1),
             column(
               10,
               wellPanel(
                 h3("Tentang Saya", style = "color:#2b2d38; font-weight:600; text-align:center;"),
                 fluidRow(
                   column(
                     3,
                     div(style = "text-align:center;",
                         img(
                           src = "foto_saya.jpg",
                           width = "180px",
                           style = "border-radius:5%; max-height:260px; margin-bottom:10px;  border: 2px solid white;"
                         ),
                         h4("Nur Na'imah Ma'ruf", style = "margin-top:10px; font-weight:700; color:#2b2d38;")
                     )
                   ),
                   column(
                     9,
                     div(
                       style = "text-align:justify; padding-right: 50px; color:#2b2d38;font-weight:550px; font-size: 15px",
                       p("Saya Nur Na'imah Ma'ruf, mahasiswa semester 4 di Politeknik Statistika STIS, jurusan Komputasi Statistik yang memiliki ketertarikan khusus pada bidang sains data, terutama dalam penerapan visualisasi data dan analisis berbasis R. Ketertarikan saya pada bidang sains data mendorong saya untuk terus belajar dan mengeksplorasi bagaimana data dapat dimanfaatkan untuk memberikan informasi yang bermakna dan mendukung pengambilan keputusan."),
                       p("Dashboard ini merupakan proyek tugas Ujian Akhir Semester (UAS) dari mata kuliah Komputasi Statistik. Dalam proses pembuatannya, saya menerapkan berbagai konsep yang telah dipelajari selama perkuliahan, seperti manipulasi data, visualisasi interaktif, analisis spasial, serta pengembangan aplikasi menggunakan R Shiny. Melalui dashboard ini, saya berharap pengguna dapat lebih mudah memahami informasi yang kompleks secara visual, dan memperoleh wawasan yang lebih mendalam mengenai kondisi sosial di Indonesia.")
                     )
                   )
                 )
               )
             ),
             column(1)
           ),
           
          div(style = "text-align:center;",
              actionButton("download_beranda_img", "Unduh Halaman Beranda")
          )
          
           
      )      
  ),
  
  #============================ EKSPLORASI DATA ============================
  tabPanel(
    "EKSPLORASI DATA",
    
    # Loading Overlay
    conditionalPanel(
      condition = "$('html').hasClass('shiny-busy')",
      div(
        class = "loading-overlay",
        div(
          div(class = "loading-spinner"),
          h4("Memproses data...", style = "margin-top: 20px; color: #666;")
        )
      )
    ),
    
    # Main Content Layout
    fluidRow(
      column(
        width = 3,
        
        # Panel Filter Utama
        wellPanel(
          h4("🎛️ Panel Kontrol"),
          
          # Dropdown Variabel
          selectInput(
            inputId = "selected_variable",
            label = "Pilih Variabel untuk Analisis:",
            choices = VARIABLE_CHOICES,
            selected = "CHILDREN",
            width = "100%"
          ),
          
          # Tombol Refresh Data
          actionButton(
            inputId = "refresh_data",
            label = "🔄 Refresh Data",
            class = "btn btn-outline-primary btn-sm",
            style = "margin-top: 10px;"
          )
        ),
        
        # Panel Informasi Variabel
        wellPanel(
          h5("📋 Informasi Variabel"),
          verbatimTextOutput("variable_info"),
          
          h5("📊 Status Data"),
          verbatimTextOutput("data_status"),
          
          # Area Notifikasi Custom
          uiOutput("notification_area")
        )
      ),

      column(
        width = 9,
        
        # Panel Statistik Deskriptif
        wellPanel(
          fluidRow(
            column(
              width = 8,
              h4("📈 Statistik Deskriptif")
            ),
            column(
              width = 4,
              div(
                style = "text-align: right;",
                downloadButton(
                  outputId = "download_table_docx",
                  label = "📄 Unduh DOCX",
                  class = "btn btn-primary btn-sm"
                )
              )
            )
          ),
          
          DT::dataTableOutput("descriptive_stats"),
          
          # Progress Bar untuk Loading
          conditionalPanel(
            condition = "output.stats_loading",
            div(
              class = "progress",
              style = "margin-top: 15px;",
              div(
                class = "progress-bar progress-bar-striped progress-bar-animated",
                style = "width: 100%",
                "Memuat statistik..."
              )
            )
          )
        ),
        
        # Panel Visualisasi
        fluidRow(
          # Boxplot Panel
          column(
            width = 6,
            wellPanel(
              h5("📦 Boxplot"),
              
              plotlyOutput("boxplot", height = "300px"),
              
              div(
                style = "margin-top: 15px;",
                h6("💡 Interpretasi:"),
                textOutput("boxplot_interpretation"),
                
                div(
                  style = "margin-top: 10px;",
                  downloadButton("download_boxplot_png", "🖼️ PNG", class = "btn btn-info btn-sm"),
                  downloadButton("download_boxplot_docx", "📄 DOCX", class = "btn btn-success btn-sm")
                )
              )
            )
          ),
          
          # Histogram Panel
          column(
            width = 6,
            wellPanel(
              h5("📊 Histogram"),
              
              plotlyOutput("histogram", height = "300px"),
              
              div(
                style = "margin-top: 15px;",
                h6("💡 Interpretasi:"),
                textOutput("histogram_interpretation"),
                
                div(
                  style = "margin-top: 10px;",
                  downloadButton("download_histogram_png", "🖼️ PNG", class = "btn btn-info btn-sm"),
                  downloadButton("download_histogram_docx", "📄 DOCX", class = "btn btn-success btn-sm")
                )
              )
            )
          )
        ),
        
        # Panel Peta
        wellPanel(
          fluidRow(
            column(
              width = 8,
              h4("🗺️ Peta Sebaran Geografis")
            ),
            column(
              width = 4,
              div(
                style = "text-align: right;",
                downloadButton(
                  outputId = "download_map_docx",
                  label = "📄 Unduh Peta DOCX",
                  class = "btn btn-warning btn-sm"
                )
              )
            )
          ),
          
          leafletOutput("map", height = "450px"),
          
          div(
            style = "margin-top: 15px;",
            h6("💡 Interpretasi Peta:"),
            textOutput("map_interpretation")
          )
        ),
        
        # Download handler untuk halaman eksplorasi
        fluidRow(
          column(12, align = "center",
                 br(),
                 actionButton("download_eksplorasi", "Unduh Halaman Eksplorasi",
                 style = "margin-bottom: 20px; display: block; margin-left: auto; margin-right: auto;")
                 
          )
        )
      )
    
    )
  ),
  
  #============================ MANAJEMEN DATA ============================
  tabPanel("MANAJEMEN DATA",
           # === HEADER ===
           fluidRow(
             column(12,
                    h2("Manajemen Data dan Visualisasi Spasial",
                       style = "color:white; text-align:center; margin-top:20px; font-family:'poppins';")
             )
           ),
           
           # === FILTER ===
           fluidRow(
             column(12,
                    wellPanel(
                      fluidRow(
                        column(3,
                               selectInput("subkategori", "Pilih Sub Analisis",
                                           choices = names(sub_analisis_choices),
                                           selected = "Dampak",
                                           selectize = FALSE,
                                           width = "100%")
                        ),
                        column(3,
                               uiOutput("indikator_ui")
                        ),
                        column(3,
                               selectInput("filter_region", "Filter Kepulauan:",
                                           choices = c("Semua", unique(geo_data$region)),
                                           selected = "Semua")
                        ),
                        column(3,
                               br(),
                               downloadButton("download_semua", "Download Semua",
                                              class = "btn-success", style = "width:100%;")
                        )
                      )
                    )
             )
           ),
           
           # === BOXPLOT DAN STATISTIK DESKRIPTIF ===
           fluidRow(
             column(6,
                    wellPanel(
                      h4("Distribusi Data per Wilayah", style = "text-align:center; font-weight:bold;"),
                      plotOutput("boxplot_mgmt", height = "400px") %>% withSpinner(color = "#0dc5c1"),
                      div(style = "text-align:center; margin-top:10px;",
                          downloadButton("unduh_boxplot", "Unduh Grafik", class = "btn-primary")
                      )
                    )
             ),
             column(6,
                    wellPanel(
                      h4("Statistik Deskriptif", style = "font-weight:bold; text-align:center;"),
                      div(style = "text-align: center;",
                          div(style = "display: inline-block;",
                              tableOutput("statistik_deskriptif")
                          )
                      ),
                      
                      br(),
                      h5("Interpretasi Distribusi", style = "font-weight:bold;"),
                      textOutput("interpretasi_boxplot"),
                      div(style = "text-align:center; margin-top:10px;",
                          downloadButton("unduh_interpretasi_boxplot", "Unduh Interpretasi", class = "btn-info")
                      )
                    )
             )
           ),
           
           # === PETA, JENKS, DAN PRIORITAS VARIABEL ===
           fluidRow(
             column(6,
                    wellPanel(
                      h4("Peta Kerentanan Sosial", style = "text-align:center; font-weight:bold;"),
                      leafletOutput("peta_kerentanan", height = "400px") %>% withSpinner(color = "#0dc5c1"),
                      div(style = "text-align:center; margin-top:10px;",
                          downloadButton("unduh_peta", "🗺️ Unduh Peta", class = "btn-primary")
                      )
                    )
             ),
             column(3,
                    wellPanel(
                      h5("Klasifikasi Jenks Natural Breaks", style = "font-weight:bold; text-align:center;"),
                      tableOutput("jenks_table"),
                      br(),
                      h5("Deskripsi Variabel", style = "font-weight:bold;"),
                      textOutput("deskripsi_variabel")
                    )
             ),
             column(3,
                    wellPanel(
                      h5("5 Distrik Prioritas (Variabel)", style = "font-weight:bold; text-align:center;"),
                      tableOutput("prioritas_distrik"),
                      br(),
                      h5("Interpretasi Peta", style = "font-weight:bold;"),
                      textOutput("interpretasi_peta")
                    )
             )
           ),
           
           # === PRIORITAS BERDASARKAN INDEKS PEMBOBOTAN ===
           fluidRow(
             column(12,
                    wellPanel(
                      h4("Distrik Prioritas Berdasarkan Indeks Pembobotan Sub-Analisis",
                         style = "font-weight:bold; text-align:center;"),
                      br(),
                      fluidRow(
                        column(6,
                               h5("Penjelasan Indeks Pembobotan", style = "font-weight:bold; text-align:center;"),
                               textOutput("penjelasan_indeks"),
                               br(),
                               h5("Formula Indeks", style = "font-weight:bold;"),
                               verbatimTextOutput("formula_indeks"),
                               br(),
                               h5("Peta Hasil Pengindeksan", style = "font-weight:bold; text-align:center;"),
                               plotOutput("peta_hasil_indeks", height = "400px"),
                               div(style = "text-align:center; margin-top:10px;",
                                   downloadButton("unduh_peta_indeks", "📊 UNDUH Peta (.png)", 
                                                  class = "btn-success")
                               )
                        ),
                        column(6,
                               h5("15 Distrik Teratas Berdasarkan Indeks", style = "font-weight:bold; text-align:center;"),
                               DT::dataTableOutput("tabel_indeks_prioritas"),
                               div(style = "text-align:center; margin-top:15px;",
                                   downloadButton("unduh_indeks_prioritas", "Unduh Tabel Indeks", class = "btn-success")
                               )
                        )
                      ),
                      
                    )
             )
           ),
           
           # === TABEL DATA LENGKAP ===
           fluidRow(
             column(12,
                    wellPanel(
                      h4("Tabel Data Lengkap", style = "font-weight:bold; text-align:center;"),
                      DT::dataTableOutput("tabel_lengkap"),
                      div(style = "text-align:center; margin-top:15px;",
                          downloadButton("unduh_tabel_lengkap", "Unduh Tabel Excel", class = "btn-success")
                      )
                    )
             )
           ),
           
           # Download handler untuk halaman manajemen data
           fluidRow(
             column(12, align = "center",
                    br(),
                    actionButton("download_manajemen_complete", 
                                 "Unduh Halaman Manajemen Data", 
                                 class = "btn-info btn-lg",
                                 icon = icon("info-circle"))
             )
           )
  ),
  
  #============================ UJI ASUMSI ============================
  tabPanel("UJI ASUMSI",
           fluidRow(
             column(12,
                    h2("Uji Asumsi Normalitas dan Homogenitas",
                       style = "color:white; text-align:center; margin-top:20px; font-family:'poppins';")
             )
           ),
           
           fluidRow(
             column(12,
                    wellPanel(
                      fluidRow(
                        column(3,
                               selectInput("asumsi_subkategori", "Pilih Sub Analisis",
                                           choices = names(sub_analisis_choices),
                                           selected = "Dampak")
                        ),
                        column(3,
                               uiOutput("asumsi_variabel_ui")
                        ),
                        column(3,
                               selectInput("asumsi_filter_region", "Filter Kepulauan (Untuk Uji Normalitas):",
                                           choices = c("Nasional", unique(geo_data$region)),
                                           selected = "Nasional"),
                               div(style = "font-size:11px; color:#666; margin-top:5px;",
                                   "* Uji homogenitas selalu menggunakan data nasional")
                        ),
                        column(3,
                               br(),
                               actionButton("run_asumsi", "Jalankan Uji Asumsi",
                                            class = "btn-primary", style = "width:100%;")
                        )
                      )
                    )
             )
           ),
           
           # Grafik QQ Plot dan Histogram untuk Normalitas
           fluidRow(
             column(6,
                    wellPanel(
                      h4("QQ Plot untuk Uji Normalitas", style = "text-align:center; font-weight:bold;"),
                      div(style = "font-size:12px; color:#666; text-align:center; margin-bottom:10px;",
                          "Plot berdasarkan filter kepulauan yang dipilih"),
                      plotOutput("qq_plot", height = "350px"),
                      h5("Interpretasi QQ Plot:"),
                      verbatimTextOutput("interpretasi_qq_plot"),
                      div(
                        style = "display: flex; justify-content: center; gap: 10px; margin-top: 10px;",
                        downloadButton("download_qq_plot", "UNDUH QQ Plot", class = "btn-primary"),
                        downloadButton("download_qq_plot_docx", "📄 UNDUH QQ Plot (.docx)", class = "btn-success")
                      )
                      
                    )
             ),
             column(6,
                    wellPanel(
                      h4("Histogram dengan Kurva Normal", style = "text-align:center; font-weight:bold;"),
                      div(style = "font-size:12px; color:#666; text-align:center; margin-bottom:10px;",
                          "Plot berdasarkan filter kepulauan yang dipilih"),
                      plotOutput("histogram_normal", height = "350px"),
                      h5("Interpretasi Histogram:"),
                      verbatimTextOutput("interpretasi_histogram"),
                      div(style = "text-align:center; margin-top:10px;",
                          downloadButton("download_histogram_normal", "UNDUH Histogram", class = "btn-primary"),
                          downloadButton("download_histogram_docx", "UNDUH Histogram (.docx)", class = "btn-success")
                      )
                    )
             )
           ),
           
           fluidRow(
             column(6,
                    wellPanel(
                      h4("Uji Normalitas", style = "text-align:center; font-weight:bold;"),
                      div(style = "font-size:12px; color:#666; text-align:center; margin-bottom:10px;",
                          "Menggunakan data sesuai filter kepulauan yang dipilih"),
                      h5("Langkah-langkah Pengujian:"),
                      verbatimTextOutput("langkah_normalitas"),
                      h5("Hasil Uji:"),
                      verbatimTextOutput("hasil_normalitas"),
                      h5("Interpretasi:"),
                      textOutput("interpretasi_normalitas"),
                      div(style = "text-align:center; margin-top:10px;",
                          downloadButton("download_normalitas", "UNDUH Hasil Normalitas", class = "btn-info")
                      )
                    )
             ),
             column(6,
                    wellPanel(
                      h4("Uji Homogenitas", style = "text-align:center; font-weight:bold;"),
                      div(style = "font-size:12px; color:#666; text-align:center; margin-bottom:10px;",
                          "Membandingkan variansi antar kepulauan (data nasional)"),
                      h5("Langkah-langkah Pengujian:"),
                      verbatimTextOutput("langkah_homogenitas"),
                      h5("Hasil Uji:"),
                      verbatimTextOutput("hasil_homogenitas"),
                      h5("Interpretasi:"),
                      textOutput("interpretasi_homogenitas"),
                      div(style = "text-align:center; margin-top:10px;",
                          downloadButton("download_homogenitas", "UNDUH Hasil Homogenitas", class = "btn-info")
                      )
                    )
             )
           ),
           
           # Download handler untuk halaman uji asumsi
           fluidRow(
             column(12, align = "center",
                    br(),
                    actionButton("download_asumsi_complete", "UNDUH Halaman Uji Asumsi Lengkap", 
                                   class = "btn-success btn-lg")
             )
           )
  ),
  #============================ INFERENSIA ============================
  tabPanel("INFERENSIA",
           fluidRow(
             column(12,
                    h2("Uji Statistik Inferensia", 
                       style = "color:white; text-align:center; margin-top:20px; font-family:'poppins';")
             )
           ),
           
           fluidRow(
             column(3,
                    wellPanel(
                      selectInput("sub_analysis", "Pilih Sub Analisis", choices = c(
                        "Penyebaran Informasi Mitigasi",
                        "Prioritas Pertolongan Saat Bencana",
                        "Padat Penduduk Tidak Siap Bencana"
                      )),
                      uiOutput("var_selector"),
                      selectInput("jenis_uji", "Jenis Uji", choices = c(
                        "Uji t Satu Sampel",
                        "Uji t Dua Sampel",
                        "Uji Proporsi Satu Kelompok",
                        "Uji Proporsi Dua Kelompok",
                        "Uji Variansi Satu Kelompok",
                        "Uji Variansi Dua Kelompok",
                        "ANOVA (Satu Arah)"
                      )),
                      conditionalPanel(
                        condition = "input.jenis_uji == 'Uji t Satu Sampel'",
                        numericInput("nilai_hipotesis", "Nilai Hipotesis (μ₀)", value = 50)
                      ),
                      conditionalPanel(
                        condition = "input.jenis_uji == 'Uji Proporsi Satu Kelompok'",
                        numericInput("nilai_proporsi", "Nilai Hipotesis Proporsi (p₀)", value = 0.5)
                      ),
                      conditionalPanel(
                        condition = "input.jenis_uji == 'Uji Variansi Satu Kelompok'",
                        numericInput("varian_hipotesis", "Variansi Hipotesis (σ²₀)", value = 1)
                      ),
                      conditionalPanel(
                        condition = "input.jenis_uji == 'Uji t Dua Sampel' || input.jenis_uji == 'Uji Proporsi Dua Kelompok' || input.jenis_uji == 'Uji Variansi Dua Kelompok'",
                        selectInput("wilayah1", "Pilih Wilayah 1", choices = NULL),
                        selectInput("wilayah2", "Pilih Wilayah 2", choices = NULL)
                      ),
                      actionButton("run_test", "Jalankan Uji", class = "btn-primary")
                    )
             ),
             column(9,
                    wellPanel(
                      h3("Langkah-langkah Pengujian"),
                      uiOutput("langkah_pengujian"),
                      h3("Output Uji Statistik"),
                      verbatimTextOutput("hasil_uji"),
                      h3("Interpretasi Hasil"),
                      uiOutput("interpretasi"),
                      div(style = "text-align:center; margin-top:15px;",
                          downloadButton("download_inferensia_hasil", "📄 UNDUH Hasil Uji", class = "btn-success")
                      )
                    )
             )
           ),
           
           # Download handler untuk halaman inferensia
           fluidRow(
             column(12, align = "center",
                    br(),
                    downloadButton("download_inferensia_complete", "📄 UNDUH Halaman Inferensia Lengkap", 
                                   class = "btn-success btn-lg")
             )
           )
  ),
  
  #============================ REGRESI ============================
  tabPanel("REGRESI",
           fluidRow(
             column(12,
                    h2("Analisis Regresi dengan Uji Asumsi Klasik",
                       style = "color:white; text-align:center; margin-top:20px; font-family:'poppins';")
             )
           ),
           
           # 1. Panel Kontrol/Filter
           fluidRow(
             column(12,
                    wellPanel(
                      fluidRow(
                        column(4,
                               selectInput("sub_regresi", "Pilih Sub Analisis untuk Regresi:",
                                           choices = names(sub_analisis_choices),
                                           selected = "Dampak")
                        ),
                        column(4,
                               selectInput("metode_regresi", "Metode Regresi:",
                                           choices = c("Multiple Linear Regression" = "lm",
                                                       "Stepwise Regression" = "step"),
                                           selected = "lm")
                        ),
                        column(4,
                               br(),
                               actionButton("run_regresi", "Jalankan Analisis Regresi",
                                            class = "btn-primary", style = "width:100%;")
                        )
                      )
                    )
             )
           ),
           
           # 2. Interpretasi Model dan Metode
           fluidRow(
             column(6,
                    wellPanel(
                      h4("Informasi Model", style = "text-align:center; font-weight:bold;"),
                      verbatimTextOutput("info_model")
                    )
             ),
             column(6,
                    wellPanel(
                      h4("Penjelasan Metode", style = "text-align:center; font-weight:bold;"),
                      verbatimTextOutput("penjelasan_metode")
                    )
             )
           ),
           
           # 3. Uji Asumsi Klasik (tanpa grafik)
           fluidRow(
             column(6,
                    wellPanel(
                      h4("1. Uji Normalitas Residual", style = "text-align:center; font-weight:bold;"),
                      verbatimTextOutput("uji_normalitas_residual")
                    )
             ),
             column(6,
                    wellPanel(
                      h4("2. Uji Multikolinearitas (VIF)", style = "text-align:center; font-weight:bold;"),
                      verbatimTextOutput("uji_multikolinearitas")
                    )
             )
           ),
           
           fluidRow(
             column(6,
                    wellPanel(
                      h4("3. Uji Heteroskedastisitas", style = "text-align:center; font-weight:bold;"),
                      verbatimTextOutput("uji_heteroskedastisitas")
                    )
             ),
             column(6,
                    wellPanel(
                      h4("4. Uji Autokorelasi", style = "text-align:center; font-weight:bold;"),
                      verbatimTextOutput("uji_autokorelasi")
                    )
             )
           ),
           
           # 4. Regresi dan Goodness of Fit
           fluidRow(
             column(6,
                    wellPanel(
                      h4("Ringkasan Model Regresi", style = "text-align:center; font-weight:bold;"),
                      verbatimTextOutput("summary_regresi")
                    )
             ),
             column(6,
                    wellPanel(
                      h4("Goodness of Fit", style = "text-align:center; font-weight:bold;"),
                      verbatimTextOutput("goodness_of_fit")
                    )
             )
           ),
           
           # 5. Interpretasi Lengkap
           fluidRow(
             column(12,
                    wellPanel(
                      h4("Interpretasi Lengkap Model Regresi", style = "text-align:center; font-weight:bold;"),
                      uiOutput("interpretasi_regresi_lengkap")
                    )
             )
           ),
           
           # Download handler untuk halaman regresi lengkap
           fluidRow(
             column(12, align = "center",
                    br(),
                    downloadButton("download_regresi_complete", "📄 UNDUH Halaman Regresi Lengkap", 
                                   class = "btn-success btn-lg")
             )
           )
  ),
  
  #============================ DOWNLOAD ============================
  
  tabPanel(
    "📥 UNDUH HASIL",
    
    # Header
    fluidRow(
      column(12,
             h2("Pusat Download Dashboard SOVIA-3MED",
                style = "color:white; text-align:center; margin-top:20px; font-family:'poppins';")
      )
    ),
    
    br(),
    
    # === DOWNLOAD PER HALAMAN ===
    fluidRow(
      column(12,
             div(class = "well",
                 style = "background-color: rgba(255,255,255,0.9); border-radius: 10px;",
                 
                 h3("📊 Download Per Halaman", 
                    style = "color: #2b2d38; font-family: 'poppins';"),
                 
                 hr(),
                 
                 # Row 1: Beranda & Eksplorasi
                 fluidRow(
                   column(6,
                          div(class = "panel",
                              style = "border: 1px solid #3a3c48;",
                              div(class = "panel-heading",
                                  style = "background-color: #2b2d38;",
                                  h4("🏠 BERANDA", class = "panel-title", style = "color: #ffffff;")),
                              div(class = "panel-body",
                                  p("Download ringkasan dashboard dan overview data"),
                                  downloadButton("download_beranda", 
                                                 "📥 Download Beranda",
                                                 class = "btn btn-primary btn-block")
                              )
                          )
                   ),
                   column(6,
                          div(class = "panel",
                              style = "border: 1px solid #3a3c48;",
                              div(class = "panel-heading",
                                  style = "background-color: #3a3c48;",
                                  h4("📈 EKSPLORASI DATA", class = "panel-title", style = "color: #ffffff;")),
                              div(class = "panel-body",
                                  p("Download grafik eksplorasi dan statistik deskriptif"),
                                  downloadButton("download_eksplorasi", 
                                                 "📥 Download Eksplorasi",
                                                 class = "btn btn-info btn-block")
                              )
                          )
                   )
                 ),
                 
                 br(),
                 
                 # Row 2: Manajemen & Uji Asumsi
                 fluidRow(
                   column(6,
                          div(class = "panel",
                              style = "border: 1px solid #3a3c48;",
                              div(class = "panel-heading",
                                  style = "background-color: #4e4e4e;",
                                  h4("🔧 MANAJEMEN DATA", class = "panel-title", style = "color: #ffffff;")),
                              div(class = "panel-body",
                                  p("Download data yang telah diproses dan dibersihkan"),
                                  downloadButton("download_manajemen", 
                                                 "📥 Download Manajemen",
                                                 class = "btn btn-success btn-block")
                              )
                          )
                   ),
                   column(6,
                          div(class = "panel",
                              style = "border: 1px solid #3a3c48;",
                              div(class = "panel-heading",
                                  style = "background-color: #2b2d38;",
                                  h4("✅ UJI ASUMSI", class = "panel-title", style = "color: #ffffff;")),
                              div(class = "panel-body",
                                  p("Download hasil uji normalitas, homogenitas, dan asumsi lainnya"),
                                  downloadButton("download_asumsi", 
                                                 "📥 Download Uji Asumsi",
                                                 class = "btn btn-warning btn-block")
                              )
                          )
                   )
                 ),
                 
                 br(),
                 
                 # Row 3: Inferensia & Regresi
                 fluidRow(
                   column(6,
                          div(class = "panel",
                              style = "border: 1px solid #3a3c48;",
                              div(class = "panel-heading",
                                  style = "background-color: #3a3c48;",
                                  h4("🔬 INFERENSIA", class = "panel-title", style = "color: #ffffff;")),
                              div(class = "panel-body",
                                  p("Download hasil uji hipotesis dan analisis inferensial"),
                                  downloadButton("download_inferensia", 
                                                 "📥 Download Inferensia",
                                                 class = "btn btn-danger btn-block")
                              )
                          )
                   ),
                   column(6,
                          div(class = "panel",
                              style = "border: 1px solid #3a3c48;",
                              div(class = "panel-heading",
                                  style = "background-color: #4e4e4e;",
                                  h4("📊 REGRESI", class = "panel-title", style = "color: #ffffff;")),
                              div(class = "panel-body",
                                  p("Download model regresi dan hasil analisis prediktif"),
                                  downloadButton("download_regresi", 
                                                 "📥 Download Regresi",
                                                 class = "btn btn-default btn-block")
                              )
                          )
                   )
                 )
             )
      )
    ),
    
    br(),
    
    # === DOWNLOAD KOMBINASI ===
    fluidRow(
      column(12,
             div(class = "well",
                 style = "background-color: rgba(255,255,255,0.9); border-radius: 10px;",
                 
                 h3("📦 Download Kombinasi", 
                    style = "color: #2b2d38; font-family: 'poppins';"),
                 
                 hr(),
                 
                 fluidRow(
                   column(12,
                          div(class = "panel",
                              style = "border: 2px solid #2b2d38;",
                              div(class = "panel-heading",
                                  style = "background-color: #2b2d38;",
                                  h4("🎯 DOWNLOAD LENGKAP", 
                                     class = "panel-title",
                                     style = "color: #ffffff;")),
                              div(class = "panel-body",
                                  style = "text-align: center;",
                                  h5("Semua Hasil Analisis", 
                                     style = "color: #2b2d38; margin-bottom: 15px;"),
                                  br(),
                                  downloadButton("download_all", 
                                                 "📥 Download Semua Halaman",
                                                 class = "btn btn-primary btn-lg",
                                                 style = "width: 80%; font-size: 16px;")
                              )
                          )
                   )
                 )
             )
      )
    ),
    
    br(),
    
    # === INFORMASI DOWNLOAD ===
    fluidRow(
      column(12,
             div(class = "well",
                 style = "background-color: rgba(255,255,255,0.9); border-radius: 10px;",
                 
                 h3("ℹ️ Informasi Download", 
                    style = "color: #2b2d38; font-family: 'poppins';"),
                 
                 hr(),
                 
                 div(class = "alert alert-info",
                     h4("📁 Format File:", style = "color: #2b2d38;"),
                     tags$ul(
                       tags$li("Grafik: JPG/PNG (resolusi tinggi)"),
                       tags$li("Tabel & Hasil: CSV/Excel"),
                       tags$li("Interpretasi: PDF/Word"),
                       tags$li("Paket lengkap: ZIP")
                     )
                 ),
                 
                 div(class = "alert alert-warning",
                     h4("⚠️ Catatan Penting:", style = "color: #2b2d38;"),
                     tags$ul(
                       tags$li("Pastikan sudah menjalankan analisis di setiap halaman sebelum download"),
                       tags$li("File download akan berisi hasil analisis terbaru"),
                       tags$li("Ukuran file bervariasi tergantung kompleksitas analisis")
                     )
                 )
             )
      )
    ),
    
    # Progress indicator (hidden by default)
    div(id = "download_progress", 
        style = "display: none;",
        div(class = "alert alert-info text-center",
            h4("🔄 Memproses Download..."),
            div(class = "progress",
                div(class = "progress-bar progress-bar-striped active",
                    style = "width: 100%")
            ),
            p(id = "download_status", "Mohon tunggu, file sedang diproses...")
        )
    )
  ),
  
  #============================ FOOTER ============================
  tags$footer(
    style = "background-color:#2b2d38; color:white; padding:20px 0; margin-top:40px;",
    div(
      class = "container text-center",
      style = "text-align:center;",
      p("© 2025 Nur Na'imah Ma'ruf | Politeknik Statistika STIS")
    )
  )
)

#============================ SERVER ============================
server <- function(input, output, session) {
  # Load distance matrix
  distance_matrix <- read.csv("data/distance.csv", 
                              row.names = 1, check.names = FALSE)
  
  # Load distance matrix 
  distance_long <- NULL
  tryCatch({
    distance_raw <- read.csv("data/distance.csv", 
                             check.names = FALSE, stringsAsFactors = FALSE)
    
    identifiers <- distance_raw[, 1]
    distance_matrix <- distance_raw[, -1]
    distance_matrix[] <- lapply(distance_matrix, function(x) as.numeric(as.character(x)))
    
    min_distances <- apply(distance_matrix, 1, function(row) {
      valid_distances <- row[!is.na(row) & row > 0]
      if(length(valid_distances) > 0) min(valid_distances) else NA
    })
    
    distance_long <- data.frame(
      V1 = as.character(identifiers),
      jarak_ke_pusat = min_distances,
      stringsAsFactors = FALSE
    )
    distance_long <- distance_long[!is.na(distance_long$jarak_ke_pusat), ]
    
  }, error = function(e) {
    distance_long <<- NULL
    cat("Distance matrix tidak tersedia, menggunakan peta standar\n")
  })
  
  #============================ BERANDA ============================
  
  #== UNDUHAN PDF METADATA ==
  output$unduh_metadata <- downloadHandler(
    filename = function() {
      "Metadata_Dashboard_SOVIA3MED.pdf"
    },
    content = function(file) {
      if (file.exists("www/Metadata_Dashboard_SOVIA3MED.pdf")) {
        file.copy("www/Metadata_Dashboard_SOVIA3MED.pdf", file)
      } else {
        writeLines(c(
          "METADATA DASHBOARD SOVIA-3MED",
          "==============================",
          "",
          "Sumber Data: SUSENAS 2017 - BPS Indonesia",
          "Jumlah Variabel: 16 indikator kerentanan sosial",
          "Cakupan: 511 Kabupaten/Kota di Indonesia",
          "Fokus Analisis: Mitigasi, Evakuasi, dan Dampak Bencana",
          "",
          "Variabel Utama:",
          "- CHILDREN: Persentase balita",
          "- ELDERLY: Persentase lansia", 
          "- POVERTY: Persentase penduduk miskin",
          "- LOWEDU: Persentase pendidikan rendah",
          "- NOTRAINING: Persentase tanpa pelatihan bencana",
          "- DPRONE: Persentase tinggal di daerah rawan",
          "- POPULATION: Jumlah penduduk",
          "",
          "Dashboard ini dibuat untuk tugas UAS Komputasi Statistik",
          "Politeknik Statistika STIS - 2025"
        ), file)
      }
    }
  )
  
  #== TABEL METADATA INTERAKTIF ==
  output$metadata_table <- DT::renderDataTable({
    DT::datatable(
      metadata_variables,
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        #buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = c(3, 4, 5)))
      ),
      rownames = FALSE,
      class = 'table-striped table-hover',
      escape = FALSE
    )
  })
  
  #== UNDUHAN METADATA (CSV) ==
  output$download_metadata_csv <- downloadHandler(
    filename = function() {
      paste("SOVIA3MED_Metadata_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(metadata_variables, file, row.names = FALSE)
    }
  )
  
  #== UNDUHAN METADATA (EXCEL) ==
  output$download_metadata_excel <- downloadHandler(
    filename = function() {
      paste("SOVIA3MED_Metadata_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(metadata_variables, file)
    }
  )
  
  #== UNDUHAN METADATA REFERENCE ==
  output$download_metadata_reference <- downloadHandler(
    filename = function() {
      "Metadata_Dashboard_SOVIA3MED.pdf"
    },
    content = function(file) {
      file.copy("www/Metadata_Dashboard_SOVIA3MED.pdf", file)
    }
  )
  
  #== UNDUHAN USER GUIDE (PDF) ==
  output$download_user_guide <- downloadHandler(
    filename = function() {
      "Panduan Pengguna.pdf"
    },
    content = function(file) {
      file.copy("www/Panduan Pengguna.pdf", file)
    }
  )
  
  #== UNDUHAN DOKUMEN NARASI BERANDA ==
  output$download_beranda_complete <- downloadHandler(
    filename = function() {
      paste0("SOVIA3MED_Beranda_Lengkap_", Sys.Date(), ".docx")
    },
    content = function(file) {
      content <- paste(
        "SOVIA-3MED DASHBOARD",
        "===================",
        "Social Vulnerability Analysis: Mitigasi, Evakuasi, dan Dampak",
        "",
        "LATAR BELAKANG",
        "===============",
        "Indonesia merupakan salah satu negara dengan tingkat kerentanan bencana alam yang sangat tinggi...",
        "",
        "KONDISI DAN TANTANGAN SOSIAL",
        "============================",
        "Wilayah padat penduduk seperti Sumatra dan Jawa menghadapi risiko tinggi...",
        "",
        "TUJUAN DASHBOARD",
        "================",
        "1. Mengidentifikasi wilayah-wilayah di Indonesia yang paling rentan...",
        "2. Memberikan dasar data yang kuat...",
        "3. Membantu pengambil kebijakan...",
        "",
        "FOKUS ANALISIS",
        "==============",
        "1. DAMPAK: Wilayah dengan Populasi Besar tetapi Tidak Siap Menghadapi Bencana",
        "2. MITIGASI: Wilayah Prioritas untuk Penyebaran Informasi Mitigasi Bencana",
        "3. EVAKUASI: Wilayah yang Harus Diprioritaskan Saat Terjadi Bencana",
        "",
        "INFORMASI DASHBOARD",
        "===================",
        "Data Source: SUSENAS 2017 - BPS Indonesia",
        "Coverage: 511 Kabupaten/Kota di Indonesia",
        "Variables: 16 Indikator Kerentanan Sosial",
        "Analysis Focus: Mitigasi, Evakuasi, dan Dampak Bencana",
        "",
        "TENTANG PEMBUAT",
        "===============",
        "Nur Na'imah Ma'ruf",
        "Mahasiswa Semester 4 Politeknik Statistika STIS",
        "Jurusan Komputasi Statistik",
        "",
        "Dashboard ini merupakan proyek tugas Ujian Akhir Semester (UAS)...",
        "",
        paste("Tanggal Generate:", Sys.time()),
        "Dashboard SOVIA-3MED - 2025"
      )
      writeLines(content, file)
    }
  )
  
  
  #============================ EKSPLORASI DATA ============================
  notification_msg <- reactiveVal("")
  notification_type <- reactiveVal("info")
  
  # Fungsi helper untuk notifikasi
  show_notification <- function(message, type = "info") {
    notification_msg(message)
    notification_type(type)
    
    # Auto-clear notification setelah 5 detik
    invalidateLater(5000)
    notification_msg("")
  }
  
  # Render area notifikasi
  output$notification_area <- renderUI({
    req(notification_msg())
    
    if (notification_msg() != "") {
      div(
        class = paste0("alert alert-", notification_type()),
        HTML(paste0(
          "<strong>",
          switch(notification_type(),
                 "info" = "ℹ️ Info:",
                 "success" = "✅ Sukses:",
                 "warning" = "⚠️ Peringatan:",
                 "danger" = "❌ Error:"
          ),
          "</strong> ", notification_msg()
        ))
      )
    }
  })
  
  # Cache untuk data geografis (load sekali saja)
  geo_data_cache <- reactiveVal(geo_data)  # PERBAIKAN: Langsung gunakan geo_data yang sudah dimuat
  data_loading_status <- reactiveVal(FALSE)
  
  # Refresh data handler
  observeEvent(input$refresh_data, {
    show_notification("🔄 Data berhasil di-refresh!", "success")
  })
  
  # Reactive untuk akses data yang sudah di-cache
  geo_data_reactive <- reactive({
    geo_data_cache()
  })
  
  output$data_status <- renderText({
    data <- geo_data_reactive()
    if (is.null(data)) {
      "❌ Data tidak tersedia"
    } else {
      paste0(
        "✅ Data siap\n",
        "📍 Jumlah wilayah: ", nrow(data), "\n",
        "📊 Variabel tersedia: ", ncol(data) - 1, "\n", # -1 untuk geometry column
        "📅 Terakhir dimuat: ", Sys.time()
      )
    }
  })
  
  output$variable_info <- renderText({
    var_name <- input$selected_variable
    var_label <- names(VARIABLE_CHOICES)[VARIABLE_CHOICES == var_name]
    
    paste0(
      "📋 Variabel: ", var_label, "\n",
      "🔤 Kode: ", var_name, "\n",
      "📈 Tipe: ", ifelse(var_name == "POPULATION", "Absolut", "Persentase")
    )
  })
  
  # Debounced reactive untuk mengurangi pemrosesan berulang
  selected_variable_debounced <- reactive({
    input$selected_variable
  }) %>% debounce(500) # Wait 500ms after last change
  
  # Data untuk analisis dengan caching dan validasi yang robust
  analysis_data <- reactive({
    req(geo_data_reactive(), selected_variable_debounced())
    
    data <- geo_data_reactive()
    var_name <- selected_variable_debounced()
    
    # Validasi variabel exists
    if (!var_name %in% names(data)) {
      show_notification(
        paste("Variabel", var_name, "tidak ditemukan dalam dataset"),
        "warning"
      )
      return(NULL)
    }
    
    # Extract dan clean data
    values <- data[[var_name]]
    
    # Convert ke numeric jika diperlukan
    if (!is.numeric(values)) {
      values <- suppressWarnings(as.numeric(values))
    }
    
    # Remove NA dan infinite values
    values <- values[is.finite(values)]
    
    # Validasi data availability
    if (length(values) == 0) {
      show_notification(
        "Tidak ada data valid untuk variabel ini",
        "warning"
      )
      return(NULL)
    }
    
    return(values)
  })
  
  # Statistik deskriptif dengan caching
  descriptive_stats_data <- reactive({
    req(analysis_data())
    
    values <- analysis_data()
    
    # Hitung statistik dengan error handling
    tryCatch({
      # PERBAIKAN: Gunakan fungsi skewness yang aman
      skew_val <- tryCatch({
        moments::skewness(values)
      }, error = function(e) {
        # Hitung skewness manual jika library tidak tersedia
        n <- length(values)
        mean_val <- mean(values)
        sd_val <- sd(values)
        sum((values - mean_val)^3) / (n * sd_val^3)
      })
      
      stats <- data.frame(
        `📊 Statistik` = c(
          "Jumlah Data", "Rata-rata", "Median", "Std. Deviasi",
          "Minimum", "Maksimum", "Kuartil 1", "Kuartil 3",
          "Range", "Skewness", "Koef. Variasi"
        ),
        `🔢 Nilai` = c(
          format(length(values), big.mark = ","),
          round(mean(values), 3),
          round(median(values), 3),
          round(sd(values), 3),
          round(min(values), 3),
          round(max(values), 3),
          round(quantile(values, 0.25), 3),
          round(quantile(values, 0.75), 3),
          round(max(values) - min(values), 3),
          round(skew_val, 3),
          paste0(round((sd(values)/mean(values)) * 100, 2), "%")
        ),
        check.names = FALSE
      )
      
      return(stats)
      
    }, error = function(e) {
      show_notification(
        paste("Error menghitung statistik:", e$message),
        "danger"
      )
      return(NULL)
    })
  })
  
  # Statistik Deskriptif Table
  output$descriptive_stats <- DT::renderDataTable({
    req(descriptive_stats_data())
    
    DT::datatable(
      descriptive_stats_data(),
      options = list(
        dom = 't',
        pageLength = 15,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = 1)
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    )
  })
  
  # Boxplot dengan caching
  boxplot_reactive <- reactive({
    req(analysis_data())
    
    values <- analysis_data()
    var_label <- names(VARIABLE_CHOICES)[VARIABLE_CHOICES == input$selected_variable]
    
    data.frame(values = values, variable = var_label)
  })
  
  output$boxplot <- renderPlotly({
    req(boxplot_reactive())
    
    plot_data <- boxplot_reactive()
    var_label <- plot_data$variable[1]
    
    p <- ggplot(plot_data, aes(y = values)) +
      geom_boxplot(
        fill = "#3498db",
        alpha = 0.7,
        color = "#2c3e50",
        outlier.color = "#e74c3c",
        outlier.size = 2
      ) +
      labs(
        title = paste("Boxplot:", var_label),
        y = var_label,
        caption = "Outlier ditandai dengan titik merah"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()
      )
    
    ggplotly(p, tooltip = c("y")) %>%
      config(displayModeBar = FALSE)
  })
  
  # Histogram dengan caching
  histogram_reactive <- reactive({
    req(analysis_data())
    
    values <- analysis_data()
    var_label <- names(VARIABLE_CHOICES)[VARIABLE_CHOICES == input$selected_variable]
    
    data.frame(values = values, variable = var_label)
  })
  
  output$histogram <- renderPlotly({
    req(histogram_reactive())
    
    plot_data <- histogram_reactive()
    var_label <- plot_data$variable[1]
    
    # Optimal bin calculation
    n_bins <- min(30, max(10, ceiling(sqrt(length(plot_data$values)))))
    
    p <- ggplot(plot_data, aes(x = values)) +
      geom_histogram(
        bins = n_bins,
        fill = "#e74c3c",
        alpha = 0.7,
        color = "#c0392b",
        boundary = 0
      ) +
      geom_vline(
        aes(xintercept = mean(values)),
        color = "#2c3e50",
        linetype = "dashed",
        size = 1,
        alpha = 0.8
      ) +
      labs(
        title = paste("Histogram:", var_label),
        x = var_label,
        y = "Frekuensi",
        caption = "Garis putus-putus menunjukkan rata-rata"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      config(displayModeBar = FALSE)
  })
  
  # Peta dengan optimasi performa
  output$map <- renderLeaflet({
    req(geo_data_reactive(), input$selected_variable)
    
    data <- geo_data_reactive()
    var_name <- input$selected_variable
    
    # Validasi variabel
    if (!var_name %in% names(data)) {
      return(
        leaflet() %>%
          addTiles() %>%
          addControl(
            "❌ Variabel tidak ditemukan",
            position = "topright",
            className = "alert alert-danger"
          )
      )
    }
    
    # Prepare data untuk peta
    values <- data[[var_name]]
    values[!is.finite(values)] <- NA
    
    # Color palette
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = values,
      na.color = "#808080",
      reverse = FALSE
    )
    
    # Labels untuk popup
    var_label <- names(VARIABLE_CHOICES)[VARIABLE_CHOICES == var_name]
    region_name <- ifelse("nmkab" %in% names(data), data$nmkab, data$kdprov)
    
    labels <- sprintf(
      "<div style='font-size: 12px;'>
        <strong>🏛️ %s</strong><br/>
        <strong>📊 %s:</strong> %s<br/>
        <em>Klik untuk detail lebih lanjut</em>
      </div>",
      region_name,
      var_label,
      ifelse(is.na(values), "Data tidak tersedia",
             ifelse(var_name == "POPULATION",
                    format(values, big.mark = ","),
                    paste0(round(values, 2), "%")))
    ) %>% lapply(htmltools::HTML)
    
    # Render peta dengan optimasi
    leaflet(data, options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(opacity = 0.8)
      ) %>%
      addPolygons(
        fillColor = ~pal(values),
        weight = 1,
        opacity = 0.8,
        color = "white",
        dashArray = "1",
        fillOpacity = 0.7,
        smoothFactor = 0.5,
        highlight = highlightOptions(
          weight = 3,
          color = "#2c3e50",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list(
            "font-weight" = "normal",
            "padding" = "8px 12px",
            "background" = "rgba(255,255,255,0.9)",
            "border-radius" = "6px",
            "box-shadow" = "0 2px 4px rgba(0,0,0,0.2)"
          ),
          textsize = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~values,
        opacity = 0.8,
        title = HTML(paste0("<strong>", var_label, "</strong>")),
        position = "bottomright",
        labFormat = labelFormat(
          suffix = ifelse(var_name == "POPULATION", "", "%"),
          big.mark = ","
        )
      ) %>%
      setView(lng = 118, lat = -2, zoom = 5)
  })
  
  output$boxplot_interpretation <- renderText({
    req(analysis_data())
    
    values <- analysis_data()
    q1 <- quantile(values, 0.25)
    q3 <- quantile(values, 0.75)
    iqr <- q3 - q1
    outliers <- length(values[values < (q1 - 1.5*iqr) | values > (q3 + 1.5*iqr)])
    
    interpretation <- paste0(
      "Distribusi menunjukkan median ", round(median(values), 2),
      " dengan rentang interkuartil ", round(q1, 2), " - ", round(q3, 2),
      ". Terdapat ", outliers, " outlier",
      ifelse(outliers != 1, "", ""),
      " yang mengindikasikan ",
      ifelse(outliers > length(values) * 0.05,
             "adanya wilayah dengan karakteristik yang sangat berbeda.",
             "distribusi yang relatif normal.")
    )
    
    return(interpretation)
  })
  
  output$histogram_interpretation <- renderText({
    req(analysis_data())
    
    values <- analysis_data()
    # PERBAIKAN: Gunakan skewness yang aman
    skew_val <- tryCatch({
      moments::skewness(values)
    }, error = function(e) {
      n <- length(values)
      mean_val <- mean(values)
      sd_val <- sd(values)
      sum((values - mean_val)^3) / (n * sd_val^3)
    })
    cv <- (sd(values)/mean(values)) * 100
    
    distribution_type <- case_when(
      abs(skew_val) < 0.5 ~ "relatif simetris",
      skew_val > 0.5 ~ "miring ke kanan (positif)",
      skew_val < -0.5 ~ "miring ke kiri (negatif)",
      TRUE ~ "mendekati normal"
    )
    
    variability <- case_when(
      cv < 15 ~ "variabilitas rendah",
      cv < 30 ~ "variabilitas sedang",
      TRUE ~ "variabilitas tinggi"
    )
    
    interpretation <- paste0(
      "Distribusi ", distribution_type, " dengan rata-rata ", round(mean(values), 2),
      " dan ", variability, " (CV = ", round(cv, 1), "%). ",
      "Skewness = ", round(skew_val, 3), " menunjukkan ",
      ifelse(abs(skew_val) < 0.5,
             "distribusi yang cukup seimbang.",
             "adanya kecenderungan konsentrasi data pada satu sisi.")
    )
    
    return(interpretation)
  })
  
  output$map_interpretation <- renderText({
    req(geo_data_reactive(), input$selected_variable)
    
    var_label <- names(VARIABLE_CHOICES)[VARIABLE_CHOICES == input$selected_variable]
    values <- analysis_data()
    
    if (is.null(values)) return("Data tidak tersedia untuk interpretasi.")
    
    # Analisis distribusi spasial sederhana
    high_threshold <- quantile(values, 0.75, na.rm = TRUE)
    low_threshold <- quantile(values, 0.25, na.rm = TRUE)
    
    interpretation <- paste0(
      "Peta menunjukkan distribusi spasial ", var_label, " di Indonesia. ",
      "Wilayah dengan warna merah/oranye (nilai ≥ ", round(high_threshold, 2), ") ",
      "menunjukkan konsentrasi tinggi, sedangkan wilayah biru (nilai ≤ ", round(low_threshold, 2), ") ",
      "menunjukkan konsentrasi rendah. Pola spasial ini dapat mengindikasikan ",
      "adanya klaster geografis dengan karakteristik sosial-ekonomi yang serupa, ",
      "yang berguna untuk perencanaan kebijakan regional."
    )
    
    return(interpretation)
  })
  
  # Helper function untuk generate plot
  generate_boxplot <- function() {
    req(boxplot_reactive())
    
    plot_data <- boxplot_reactive()
    var_label <- plot_data$variable[1]
    
    ggplot(plot_data, aes(y = values)) +
      geom_boxplot(
        fill = "#3498db",
        alpha = 0.7,
        color = "#2c3e50",
        outlier.color = "#e74c3c",
        outlier.size = 2
      ) +
      labs(
        title = paste("Boxplot:", var_label),
        y = var_label,
        caption = "Outlier ditandai dengan titik merah"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        text = element_text(size = 12)
      )
  }
  
  generate_histogram <- function() {
    req(histogram_reactive())
    
    plot_data <- histogram_reactive()
    var_label <- plot_data$variable[1]
    n_bins <- min(30, max(10, ceiling(sqrt(length(plot_data$values)))))
    
    ggplot(plot_data, aes(x = values)) +
      geom_histogram(
        bins = n_bins,
        fill = "#e74c3c",
        alpha = 0.7,
        color = "#c0392b",
        boundary = 0
      ) +
      geom_vline(
        aes(xintercept = mean(values)),
        color = "#2c3e50",
        linetype = "dashed",
        size = 1,
        alpha = 0.8
      ) +
      labs(
        title = paste("Histogram:", var_label),
        x = var_label,
        y = "Frekuensi",
        caption = "Garis putus-putus menunjukkan rata-rata"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        text = element_text(size = 12)
      )
  }
  
  # Download handlers dengan error handling yang lebih baik
  output$download_boxplot_png <- downloadHandler(
    filename = function() {
      paste0("boxplot_", input$selected_variable, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      tryCatch({
        p <- generate_boxplot()
        ggsave(file, p, width = 10, height = 6, dpi = 300, bg = "white")
        show_notification("✅ Boxplot PNG berhasil diunduh!", "success")
      }, error = function(e) {
        show_notification(paste("❌ Error download PNG:", e$message), "danger")
      })
    }
  )
  
  output$download_histogram_png <- downloadHandler(
    filename = function() {
      paste0("histogram_", input$selected_variable, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      tryCatch({
        p <- generate_histogram()
        ggsave(file, p, width = 10, height = 6, dpi = 300, bg = "white")
        show_notification("✅ Histogram PNG berhasil diunduh!", "success")
      }, error = function(e) {
        show_notification(paste("❌ Error download PNG:", e$message), "danger")
      })
    }
  )
  
  output$download_boxplot_docx <- downloadHandler(
    filename = function() {
      paste0("boxplot_", input$selected_variable, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      tryCatch({
        var_label <- names(VARIABLE_CHOICES)[VARIABLE_CHOICES == input$selected_variable]
        
        doc_content <- paste(
          "ANALISIS BOXPLOT",
          "================",
          "",
          paste("Variabel:", var_label),
          "",
          "Interpretasi:",
          isolate(output$boxplot_interpretation()),
          "",
          "Statistik Deskriptif:",
          paste(capture.output(print(isolate(descriptive_stats_data()))), collapse = "\n"),
          "",
          "Dashboard SOVIA-3MED - 2025",
          sep = "\n"
        )
        
        writeLines(doc_content, file)
        show_notification("✅ Boxplot DOCX berhasil diunduh!", "success")
      }, error = function(e) {
        show_notification(paste("❌ Error download DOCX:", e$message), "danger")
      })
    }
  )
  
  output$download_histogram_docx <- downloadHandler(
    filename = function() {
      paste0("histogram_", input$selected_variable, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      tryCatch({
        var_label <- names(VARIABLE_CHOICES)[VARIABLE_CHOICES == input$selected_variable]
        
        doc_content <- paste(
          "ANALISIS HISTOGRAM",
          "==================",
          "",
          paste("Variabel:", var_label),
          "",
          "Interpretasi:",
          isolate(output$histogram_interpretation()),
          "",
          "Statistik Deskriptif:",
          paste(capture.output(print(isolate(descriptive_stats_data()))), collapse = "\n"),
          "",
          "Dashboard SOVIA-3MED - 2025",
          sep = "\n"
        )
        
        writeLines(doc_content, file)
        show_notification("✅ Histogram DOCX berhasil diunduh!", "success")
      }, error = function(e) {
        show_notification(paste("❌ Error download DOCX:", e$message), "danger")
      })
    }
  )
  
  output$download_table_docx <- downloadHandler(
    filename = function() {
      paste0("statistik_deskriptif_", input$selected_variable, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      tryCatch({
        req(descriptive_stats_data())
        
        var_label <- names(VARIABLE_CHOICES)[VARIABLE_CHOICES == input$selected_variable]
        stats <- descriptive_stats_data()
        
        doc_content <- paste(
          "STATISTIK DESKRIPTIF",
          "====================",
          "",
          paste("Variabel:", var_label),
          paste("Tanggal Analisis:", Sys.Date()),
          "",
          paste(capture.output(print(stats)), collapse = "\n"),
          "",
          "Keterangan:",
          paste0("Analisis statistik deskriptif untuk variabel ", var_label, " ",
                 "menunjukkan karakteristik distribusi data dari ", nrow(geo_data_reactive()), " ",
                 "wilayah di Indonesia. Data ini dapat digunakan untuk memahami ",
                 "pola sebaran dan variabilitas indikator demografi di tingkat regional."),
          "",
          "Dashboard SOVIA-3MED - 2025",
          sep = "\n"
        )
        
        writeLines(doc_content, file)
        show_notification("✅ Tabel statistik DOCX berhasil diunduh!", "success")
      }, error = function(e) {
        show_notification(paste("❌ Error download tabel:", e$message), "danger")
      })
    }
  )
  
  output$download_map_docx <- downloadHandler(
    filename = function() {
      paste0("peta_sebaran_", input$selected_variable, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      tryCatch({
        var_label <- names(VARIABLE_CHOICES)[VARIABLE_CHOICES == input$selected_variable]
        
        doc_content <- paste(
          "ANALISIS PETA SEBARAN GEOGRAFIS",
          "===============================",
          "",
          paste("Variabel:", var_label),
          paste("Tanggal Analisis:", Sys.Date()),
          "",
          "Deskripsi Peta:",
          paste0("Peta ini menampilkan distribusi spasial variabel ", var_label, " ",
                 "di seluruh wilayah Indonesia. Visualisasi menggunakan gradasi warna ",
                 "dari biru (nilai rendah) hingga merah (nilai tinggi) untuk memudahkan ",
                 "identifikasi pola geografis."),
          "",
          "Interpretasi Pola Spasial:",
          isolate(output$map_interpretation()),
          "",
          "Dashboard SOVIA-3MED - 2025",
          sep = "\n"
        )
        
        writeLines(doc_content, file)
        show_notification("✅ Laporan peta DOCX berhasil diunduh!", "success")
      }, error = function(e) {
        show_notification(paste("❌ Error download peta:", e$message), "danger")
      })
    }
  )
  
  observeEvent(input$download_eksplorasi, {
  showModal(modalDialog(
    title = "Petunjuk Unduh Halaman",
    HTML("<p>Untuk mengunduh halaman ini sebagai PDF:</p>
         <ol>
           <li>Tekan <strong>Ctrl + P</strong> (atau <strong>Cmd + P</strong> di Mac)</li>
           <li>Pilih <strong>Save as PDF</strong> pada pilihan printer</li>
           <li>Klik <strong>Save</strong></li>
         </ol>"),
    easyClose = TRUE,
    footer = modalButton("Tutup")
  ))
})

  
  
  #============================ MANAJEMEN DATA ============================
  
  # UI indikator
  output$indikator_ui <- renderUI({
    selectInput("indikator", "Pilih Indikator",
                choices = sub_analisis_choices[[input$subkategori]],
                width = "100%")
  })
  
  # Reactive untuk data terfilter
  data_filtered <- reactive({
    req(input$indikator)
    data_base <- geo_data %>%
      filter(!is.na(.data[[input$indikator]])) %>%
      mutate(region = factor(region, levels = c(
        "Sumatra", "Jawa", "Bali & Nusa Tenggara",
        "Kalimantan", "Sulawesi", "Maluku & Papua"
      )))
    
    if (input$filter_region != "Semua") {
      data_base <- data_base %>% filter(region == input$filter_region)
    }
    
    return(data_base)
  })
  
  # Interpretasi Boxplot
  output$interpretasi_boxplot <- renderText({
    req(input$indikator)
    indikator <- input$indikator
    label <- indikator_labels[[indikator]]
    
    df <- data_filtered() %>%
      st_drop_geometry() %>%
      group_by(region) %>%
      summarise(mean_value = mean(.data[[indikator]], na.rm = TRUE), .groups = "drop")
    
    tertinggi <- df %>% filter(mean_value == max(mean_value)) %>% pull(region)
    terendah <- df %>% filter(mean_value == min(mean_value)) %>% pull(region)
    rerata <- mean(df$mean_value, na.rm = TRUE)
    
    paste0(
      "Rata-rata ", label, " di Indonesia adalah ", round(rerata, 2), ". ",
      "Wilayah dengan nilai ", label, " tertinggi adalah ", tertinggi, ", ",
      "sedangkan terendah terdapat di ", terendah, ". ",
      "Hal ini menunjukkan adanya disparitas regional yang perlu mendapat perhatian khusus dalam perencanaan mitigasi bencana."
    )
  })
  
  # Boxplot untuk Manajemen Data
  output$boxplot_mgmt <- renderPlot({
    req(input$indikator)
    indikator <- input$indikator
    label <- indikator_labels[[indikator]]
    df <- data_filtered()
    
    ggplot(df, aes(x = region, y = .data[[indikator]], fill = region)) +
      geom_boxplot(color = "black", outlier.color = "red", outlier.size = 2) +
      scale_fill_manual(values = region_colors) +
      theme_minimal(base_family = "sans") +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
        axis.title = element_text(size = 10),
        legend.position = "none"
      ) +
      ggtitle(paste("Sebaran", label, "per Wilayah")) +
      xlab("Wilayah") +
      ylab(label)
  })
  
  # Peta Kerentanan
  output$peta_kerentanan <- renderLeaflet({
    req(input$indikator)
    
    var_name <- input$indikator
    df <- data_filtered()
    
    # Hitung breaks Jenks (buang NA dulu)
    jenks <- classIntervals(
      df[[var_name]][!is.na(df[[var_name]])],
      n = 5,
      style = "jenks"
    )
    breaks <- jenks$brks
    
    # Warna dan label kategori manual
    warna_manual_vec <- c(
      "#ffffb2",  # Sangat Rendah
      "#fecc5c",  # Rendah
      "#fd8d3c",  # Sedang
      "#f03b20",  # Tinggi
      "#bd0026"   # Sangat Tinggi
    )
    
    label_manual <- c("Sangat Rendah", "Rendah", "Sedang", "Tinggi", "Sangat Tinggi")
    
    # Buat kategori (ordered factor!)
    df$kategori <- cut(
      df[[var_name]],
      breaks = breaks,
      include.lowest = TRUE,
      labels = label_manual,
      ordered_result = TRUE
    )
    
    # Buang NA (karena tidak bisa diwarnai)
    df <- df[!is.na(df$kategori), ]
    
    # Palet warna manual
    pal <- colorFactor(
      palette = warna_manual_vec,
      domain = label_manual
    )
    
    leaflet(df) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(kategori),
        weight = 1,
        color = "white",
        fillOpacity = 0.7,
        label = ~lapply(
          paste0(
            "<b>", nmkab, " (", nmprov, ")</b><br>",
            nama_indikator[[var_name]], ": ",
            round(get(var_name), 2), "<br>Kategori: ", kategori
          ),
          htmltools::HTML
        ),
        highlight = highlightOptions(weight = 2, color = "black", fillOpacity = 0.9)
      ) %>%
      addLegend(
        position = "bottomright",
        colors = warna_manual_vec,
        labels = label_manual,
        title = nama_indikator[[var_name]],
        opacity = 0.7
      ) %>%
      addScaleBar(position = "bottomleft")
  })
  
  
  
  # Interpretasi Peta
  output$interpretasi_peta <- renderText({
    req(input$indikator, input$subkategori)
    
    indikator <- input$indikator
    nama <- nama_indikator[[indikator]]
    sub <- input$subkategori
    
    base_text <- paste0("Peta menunjukkan distribusi spasial ", nama, " di Indonesia. ")
    
    if (sub == "Mitigasi") {
      specific_text <- "Wilayah dengan nilai tinggi memerlukan prioritas dalam program penyuluhan dan pelatihan mitigasi bencana untuk meningkatkan kesiapsiagaan masyarakat."
    } else if (sub == "Evakuasi") {
      specific_text <- "Wilayah dengan nilai tinggi menunjukkan kerentanan demografis yang memerlukan perhatian khusus dalam perencanaan jalur evakuasi dan sistem peringatan dini."
    } else if (sub == "Dampak") {
      specific_text <- "Wilayah dengan nilai tinggi berpotensi mengalami dampak bencana yang lebih besar, sehingga memerlukan infrastruktur dan sistem tanggap darurat yang lebih kuat."
    }
    
    paste0(base_text, specific_text)
  })
  
  # Tabel Jenks
  output$jenks_table <- renderTable({
    req(input$indikator)
    values <- data_filtered()[[input$indikator]]
    jenks <- classIntervals(values, n = 5, style = "jenks")
    data.frame(
      Kelas = c("Sangat Rendah", "Rendah", "Sedang", "Tinggi", "Sangat Tinggi"),
      Rentang = paste(round(jenks$brks[-6], 2), "-", round(jenks$brks[-1], 2))
    )
  })
  
  # Top 5 distrik prioritas berdasarkan variabel
  output$prioritas_distrik <- renderTable({
    req(input$indikator)
    
    result <- data_filtered() %>%
      st_drop_geometry() %>%
      filter(!is.na(.data[[input$indikator]])) %>%
      arrange(desc(.data[[input$indikator]])) %>%
      slice_head(n = 5) %>%
      select(nmkab, nmprov, all_of(input$indikator)) %>%
      rename(
        Distrik = nmkab,
        Provinsi = nmprov,
        Nilai = all_of(input$indikator)
      ) %>%
      mutate(Nilai = round(Nilai, 2))
    
    return(result)
  })
  
  # Deskripsi Variabel
  output$deskripsi_variabel <- renderText({
    req(input$indikator)
    deskripsi_var[[input$indikator]]
  })
  
  # Statistik Deskriptif
  output$statistik_deskriptif <- renderTable({
    req(input$indikator)
    values <- data_filtered()[[input$indikator]]
    data.frame(
      Statistik = c("Minimum", "Q1", "Median", "Mean", "Q3", "Maximum", "Std Dev"),
      Nilai = c(
        round(min(values, na.rm = TRUE), 2),
        round(quantile(values, 0.25, na.rm = TRUE), 2),
        round(median(values, na.rm = TRUE), 2),
        round(mean(values, na.rm = TRUE), 2),
        round(quantile(values, 0.75, na.rm = TRUE), 2),
        round(max(values, na.rm = TRUE), 2),
        round(sd(values, na.rm = TRUE), 2)
      )
    )
  })
  
  # Tabel Indeks Prioritas (YANG SEHARUSNYA ADA!)
  output$tabel_indeks_prioritas <- DT::renderDataTable({
    req(input$subkategori)
    
    indeks_data <- hitung_indeks(geo_data, input$subkategori) %>%
      slice(1:15)
    
    hasil <- data.frame(
      Ranking = 1:15,
      Distrik = indeks_data$nmkab,
      Provinsi = indeks_data$nmprov,
      Region = indeks_data$region,
      Indeks = round(indeks_data[[paste0("indeks_", tolower(input$subkategori))]], 4)
    )
    
    hasil
  }, options = list(pageLength = 15, scrollX = TRUE, searching = FALSE))
  
  # Penjelasan Indeks
  output$penjelasan_indeks <- renderText({
    req(input$subkategori)
    
    if (input$subkategori == "Dampak") {
      "Indeks Dampak menggabungkan faktor jumlah penduduk (60%) dan tingkat kerentanan berdasarkan kurangnya pelatihan dan lokasi rawan bencana (40%). Semakin tinggi indeks, semakin besar potensi dampak bencana di wilayah tersebut."
    } else if (input$subkategori == "Mitigasi") {
      "Indeks Mitigasi mempertimbangkan tingkat pendidikan rendah (40%), kemiskinan (30%), kurangnya pelatihan (20%), dan keterbatasan infrastruktur (10%). Semakin tinggi indeks, semakin membutuhkan program mitigasi yang intensif."
    } else if (input$subkategori == "Evakuasi") {
      "Indeks Evakuasi fokus pada kelompok rentan berdasarkan usia (30%), gender dan kepemimpinan keluarga (20%), serta kurangnya pelatihan (10%). Semakin tinggi indeks, semakin membutuhkan perhatian khusus dalam perencanaan evakuasi."
    }
  })
  
  # Formula Indeks
  output$formula_indeks <- renderText({
    req(input$subkategori)
    
    if (input$subkategori == "Dampak") {
      "Indeks Dampak = 0.3 × (0.6 × Population_norm + 0.4 × (Notraining + Dprone)_norm)"
    } else if (input$subkategori == "Mitigasi") {
      "Indeks Mitigasi = 0.3 × (0.4 × (LowEdu + Illiterate)_norm + 0.3 × Poverty_norm + 0.2 × Notraining_norm + 0.1 × (Noelectric + Dprone)_norm)"
    } else if (input$subkategori == "Evakuasi") {
      "Indeks Evakuasi = 0.3 × (0.3 × (Children + Elderly)_norm + 0.2 × (Female + Fhead)_norm + 0.1 × Notraining_norm)"
    }
  })
  
  # Tabel Data Lengkap
  output$tabel_lengkap <- DT::renderDataTable({
    req(input$indikator)
    data_filtered() %>%
      st_drop_geometry() %>%
      dplyr::select(nmkab, nmprov, region, all_of(input$indikator)) %>%
      arrange(desc(.data[[input$indikator]])) %>%
      rename(
        `Nama Distrik` = nmkab,
        Provinsi = nmprov,
        Region = region
      ) %>%
      rename_with(~nama_indikator[[input$indikator]], all_of(input$indikator))
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # Download handler untuk boxplot
  output$unduh_boxplot <- downloadHandler(
    filename = function() {
      paste0("boxplot_", input$indikator, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(input$indikator)
      
      # Buat plot boxplot
      indikator <- input$indikator
      label <- indikator_labels[[indikator]]
      df <- data_filtered()
      
      p <- ggplot(df, aes(x = region, y = .data[[indikator]], fill = region)) +
        geom_boxplot(color = "black", outlier.color = "red", outlier.size = 2) +
        scale_fill_manual(values = region_colors) +
        theme_minimal(base_family = "sans") +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
          axis.title = element_text(size = 10),
          legend.position = "none"
        ) +
        ggtitle(paste("Sebaran", label, "per Wilayah")) +
        xlab("Wilayah") +
        ylab(label)
      
      # Simpan plot
      ggsave(file, p, width = 10, height = 6, device = "png", dpi = 300)
    }
  )
  
  # Download handler untuk interpretasi boxplot
  output$unduh_interpretasi_boxplot <- downloadHandler(
    filename = function() {
      paste0("interpretasi_boxplot_", input$indikator, "_", Sys.Date(), ".txt")
    },
    content = function(file) {
      req(input$indikator)
      
      # Buat interpretasi terlebih dahulu
      indikator <- input$indikator
      label <- indikator_labels[[indikator]]
      
      df <- data_filtered() %>%
        st_drop_geometry() %>%
        group_by(region) %>%
        summarise(mean_value = mean(.data[[indikator]], na.rm = TRUE), .groups = "drop")
      
      tertinggi <- df %>% filter(mean_value == max(mean_value)) %>% pull(region)
      terendah <- df %>% filter(mean_value == min(mean_value)) %>% pull(region)
      rerata <- mean(df$mean_value, na.rm = TRUE)
      
      interpretasi_text <- paste0(
        "Rata-rata ", label, " di Indonesia adalah ", round(rerata, 2), ". ",
        "Wilayah dengan nilai ", label, " tertinggi adalah ", tertinggi, ", ",
        "sedangkan terendah terdapat di ", terendah, ". ",
        "Hal ini menunjukkan adanya disparitas regional yang perlu mendapat perhatian khusus dalam perencanaan mitigasi bencana."
      )
      
      # Buat konten interpretasi
      content <- paste(
        "INTERPRETASI DISTRIBUSI DATA",
        "============================",
        "",
        paste("Indikator:", label),
        paste("Sub Analisis:", input$subkategori),
        paste("Filter Wilayah:", input$filter_region),
        paste("Tanggal Analisis:", Sys.Date()),
        "",
        "INTERPRETASI:",
        "=============",
        interpretasi_text,
        "",
        "DESKRIPSI VARIABEL:",
        "===================",
        deskripsi_var[[input$indikator]],
        "",
        "Dashboard SOVIA-3MED - 2025",
        "Politeknik Statistika STIS"
      )
      
      writeLines(content, file)
    }
  )
  
  create_index_map <- function(sub_analisis) {
    # Hitung indeks
    indeks_data <- hitung_indeks(geo_data, sub_analisis)
    
    # Gabungkan dengan data geografis
    map_data <- geo_data %>%
      left_join(indeks_data, by = "nmkab")
    
    # Tentukan kolom indeks dan judul
    index_col <- switch(sub_analisis,
                        "Dampak" = "indeks_dampak",
                        "Mitigasi" = "indeks_mitigasi",
                        "Evakuasi" = "indeks_evakuasi")
    
    title <- paste("Peta Indeks", sub_analisis)
    
    # Buat peta
    ggplot(map_data) +
      geom_sf(aes(fill = .data[[index_col]]), color = "white", size = 0.1) +
      scale_fill_gradientn(
        colours = c("#ffffcc", "#ffeda0", "#feb24c", "#fd8d3c", "#f03b20", "#bd0026"),
        name = "Indeks\nKerentanan",
        labels = function(x) sprintf("%.3f", x)
      ) +
      labs(title = title,
           subtitle = "Semakin tinggi nilai, semakin rentan",
           caption = "Sumber: Dashboard SOVIA-3MED") +
      theme_void() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        plot.caption = element_text(hjust = 0.5, size = 8),
        legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        legend.key.height = unit(0.3, "cm")
      )
  }
  
  
  # Tabel analisis gabungan indeks dan jarak
  # Tabel analisis gabungan (AMAN)
  output$analisis_jarak_indeks <- DT::renderDataTable({
    req(input$subkategori)
    
    if(is.null(distance_long)) {
      # Jika tidak ada data jarak, tampilkan pesan
      data.frame(
        Pesan = "Data jarak tidak tersedia",
        Info = "Menggunakan analisis berdasarkan indeks saja"
      )
    } else {
      # Kode analisis gabungan seperti biasa
      indeks_data <- hitung_indeks(geo_data, input$subkategori)
      
      # ... kode analisis lainnya
    }
  }, options = list(pageLength = 10, scrollX = TRUE))  

  
  # Output untuk peta hasil indeks
  output$peta_hasil_indeks <- renderPlot({
    req(input$subkategori)
    create_index_map(input$subkategori)
  })
  
  # Download handler untuk peta indeks
  output$unduh_peta_indeks <- downloadHandler(
    filename = function() {
      paste0("Peta_Indeks_", input$subkategori, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(input$subkategori)
      
      # Buat plot dengan resolusi tinggi
      p <- create_index_map(input$subkategori)
      
      ggsave(file, p, 
             width = 12, height = 8, 
             dpi = 300, device = "png",
             bg = "white")
    }
  )
  
  # Download handler untuk peta (sebagai gambar)
  output$unduh_peta <- downloadHandler(
    filename = function() {
      paste0("peta_kerentanan_", input$indikator, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Buat plot peta menggunakan ggplot untuk export
      req(input$indikator)
      
      var_name <- input$indikator
      filtered_data <- data_filtered()
      
      # Hitung breaks dengan jenks
      jenks <- classIntervals(
        filtered_data[[var_name]][!is.na(filtered_data[[var_name]])],
        n = 5,
        style = "jenks"
      )
      breaks <- jenks$brks
      
      # Kategori klasifikasi jenks
      filtered_data$kategori <- cut(
        filtered_data[[var_name]],
        breaks = breaks,
        include.lowest = TRUE,
        labels = c("Sangat Rendah", "Rendah", "Sedang", "Tinggi", "Sangat Tinggi")
      )
      
      # Buat plot dengan ggplot
      p <- ggplot(filtered_data) +
        geom_sf(aes(fill = kategori), color = "white", size = 0.1) +
        scale_fill_brewer(type = "seq", palette = "YlOrRd", name = nama_indikator[[var_name]]) +
        theme_void() +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          legend.position = "bottom",
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8)
        ) +
        ggtitle(paste("Peta Kerentanan:", nama_indikator[[var_name]]))
      
      ggsave(file, p, width = 12, height = 8, device = "png", dpi = 300)
    }
  )
  
  # Download handler untuk tabel indeks prioritas
  output$unduh_indeks_prioritas <- downloadHandler(
    filename = function() {
      paste0("indeks_prioritas_", input$subkategori, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(input$subkategori)
      
      # Data indeks lengkap (semua distrik)
      indeks_data <- hitung_indeks(geo_data, input$subkategori)
      
      hasil_lengkap <- data.frame(
        Ranking = 1:nrow(indeks_data),
        Distrik = indeks_data$nmkab,
        Provinsi = indeks_data$nmprov,
        Region = indeks_data$region,
        Indeks = round(indeks_data[[paste0("indeks_", tolower(input$subkategori))]], 4)
      )
      
      # Tambahan sheet dengan penjelasan
      penjelasan_sheet <- data.frame(
        Informasi = c(
          "Sub Analisis",
          "Tanggal Analisis", 
          "Total Distrik",
          "Penjelasan Indeks",
          "Formula Indeks"
        ),
        Nilai = c(
          input$subkategori,
          as.character(Sys.Date()),
          nrow(hasil_lengkap),
          isolate({
            if (input$subkategori == "Dampak") {
              "Indeks Dampak menggabungkan faktor jumlah penduduk (60%) dan tingkat kerentanan berdasarkan kurangnya pelatihan dan lokasi rawan bencana (40%). Semakin tinggi indeks, semakin besar potensi dampak bencana di wilayah tersebut."
            } else if (input$subkategori == "Mitigasi") {
              "Indeks Mitigasi mempertimbangkan tingkat pendidikan rendah (40%), kemiskinan (30%), kurangnya pelatihan (20%), dan keterbatasan infrastruktur (10%). Semakin tinggi indeks, semakin membutuhkan program mitigasi yang intensif."
            } else if (input$subkategori == "Evakuasi") {
              "Indeks Evakuasi fokus pada kelompok rentan berdasarkan usia (30%), gender dan kepemimpinan keluarga (20%), serta kurangnya pelatihan (10%). Semakin tinggi indeks, semakin membutuhkan perhatian khusus dalam perencanaan evakuasi."
            }
          }),
          isolate({
            if (input$subkategori == "Dampak") {
              "Indeks Dampak = 0.3 × (0.6 × Population_norm + 0.4 × (Notraining + Dprone)_norm)"
            } else if (input$subkategori == "Mitigasi") {
              "Indeks Mitigasi = 0.3 × (0.4 × (LowEdu + Illiterate)_norm + 0.3 × Poverty_norm + 0.2 × Notraining_norm + 0.1 × (Noelectric + Dprone)_norm)"
            } else if (input$subkategori == "Evakuasi") {
              "Indeks Evakuasi = 0.3 × (0.3 × (Children + Elderly)_norm + 0.2 × (Female + Fhead)_norm + 0.1 × Notraining_norm)"
            }
          })
        )
      )
      
      # Buat workbook dengan multiple sheets
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Ranking_Indeks")
      openxlsx::addWorksheet(wb, "Penjelasan")
      
      openxlsx::writeData(wb, "Ranking_Indeks", hasil_lengkap)
      openxlsx::writeData(wb, "Penjelasan", penjelasan_sheet)
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # Download handler untuk tabel data lengkap
  output$unduh_tabel_lengkap <- downloadHandler(
    filename = function() {
      paste0("data_lengkap_", input$indikator, "_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(input$indikator)
      
      tabel_data <- data_filtered() %>%
        st_drop_geometry() %>%
        dplyr::select(nmkab, nmprov, region, all_of(input$indikator)) %>%
        arrange(desc(.data[[input$indikator]])) %>%
        rename(
          `Nama Distrik` = nmkab,
          Provinsi = nmprov,
          Region = region
        ) %>%
        rename_with(~nama_indikator[[input$indikator]], all_of(input$indikator))
      
      # Tambahan sheet dengan statistik deskriptif
      values <- data_filtered()[[input$indikator]]
      statistik_sheet <- data.frame(
        Statistik = c("Minimum", "Q1", "Median", "Mean", "Q3", "Maximum", "Std Dev", "Total Observasi"),
        Nilai = c(
          round(min(values, na.rm = TRUE), 2),
          round(quantile(values, 0.25, na.rm = TRUE), 2),
          round(median(values, na.rm = TRUE), 2),
          round(mean(values, na.rm = TRUE), 2),
          round(quantile(values, 0.75, na.rm = TRUE), 2),
          round(max(values, na.rm = TRUE), 2),
          round(sd(values, na.rm = TRUE), 2),
          length(values[!is.na(values)])
        )
      )
      
      # Informasi tambahan
      info_sheet <- data.frame(
        Informasi = c(
          "Indikator",
          "Sub Analisis",
          "Filter Wilayah",
          "Tanggal Export",
          "Deskripsi Variabel"
        ),
        Nilai = c(
          nama_indikator[[input$indikator]],
          input$subkategori,
          input$filter_region,
          as.character(Sys.Date()),
          deskripsi_var[[input$indikator]]
        )
      )
      
      # Buat workbook
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Data_Lengkap")
      openxlsx::addWorksheet(wb, "Statistik_Deskriptif")
      openxlsx::addWorksheet(wb, "Informasi")
      
      openxlsx::writeData(wb, "Data_Lengkap", tabel_data)
      openxlsx::writeData(wb, "Statistik_Deskriptif", statistik_sheet)
      openxlsx::writeData(wb, "Informasi", info_sheet)
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # Download handler untuk semua (komprehensif)
  output$download_semua <- downloadHandler(
    filename = function() {
      paste0("SOVIA3MED_Manajemen_Data_Semua_", input$subkategori, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(input$indikator, input$subkategori)
      
      # Data lengkap
      tabel_data <- data_filtered() %>%
        st_drop_geometry() %>%
        dplyr::select(nmkab, nmprov, region, all_of(input$indikator)) %>%
        arrange(desc(.data[[input$indikator]])) %>%
        rename(
          Nama_Distrik = nmkab,
          Provinsi = nmprov,
          Region = region
        ) %>%
        rename_with(~gsub("[^A-Za-z0-9_]", "_", nama_indikator[[input$indikator]]), all_of(input$indikator))
      
      write.csv(tabel_data, file, row.names = FALSE)
    }
  )
  
  
  observeEvent(input$download_manajemen_complete, {
    showModal(modalDialog(
      title = "Petunjuk Unduh Halaman",
      HTML("<p>Untuk mengunduh halaman ini sebagai PDF:</p>
         <ol>
           <li>Tekan <strong>Ctrl + P</strong> (atau <strong>Cmd + P</strong> di Mac)</li>
           <li>Pilih <strong>Save as PDF</strong> pada pilihan printer</li>
           <li>Klik <strong>Save</strong></li>
         </ol>"),
      easyClose = TRUE,
      footer = modalButton("Tutup")
    ))
  })
  
  #============================ UJI ASUMSI ============================
  
  # UI variabel untuk uji asumsi
  output$asumsi_variabel_ui <- renderUI({
    req(input$asumsi_subkategori)
    vars <- sub_analisis_choices[[input$asumsi_subkategori]]
    selectInput("asumsi_variabel", "Pilih Variabel:",
                choices = setNames(vars, nama_indikator[vars]))
  })
  
  # Reactive values untuk uji asumsi
  asumsi_results <- reactiveValues(
    normalitas = NULL,
    homogenitas = NULL,
    data_filtered = NULL
  )
  
  # Jalankan uji asumsi
  observeEvent(input$run_asumsi, {
    req(input$asumsi_variabel)
    
    # Reset hasil sebelumnya
    asumsi_results$normalitas <- NULL
    asumsi_results$homogenitas <- NULL
    asumsi_results$data_filtered <- NULL
    
    tryCatch({
      # Filter data berdasarkan pilihan untuk UJI NORMALITAS
      if (input$asumsi_filter_region == "Nasional") {
        data_normalitas <- geo_data %>%
          st_drop_geometry() %>%
          filter(!is.na(.data[[input$asumsi_variabel]]))
      } else {
        data_normalitas <- geo_data %>%
          st_drop_geometry() %>%
          filter(region == input$asumsi_filter_region) %>%
          filter(!is.na(.data[[input$asumsi_variabel]]))
      }
      
      # Data untuk UJI HOMOGENITAS selalu menggunakan data NASIONAL
      data_homogenitas <- geo_data %>%
        st_drop_geometry() %>%
        filter(!is.na(.data[[input$asumsi_variabel]]))
      
      # Validasi data normalitas
      if (nrow(data_normalitas) == 0) {
        showNotification("Data tidak tersedia untuk kombinasi pilihan ini!", type = "error")
        return()
      }
      
      asumsi_results$data_filtered <- data_normalitas
      var_name <- input$asumsi_variabel
      
      # Validasi variabel
      if (!var_name %in% names(data_normalitas)) {
        showNotification("Variabel tidak ditemukan dalam data!", type = "error")
        return()
      }
      
      values <- data_normalitas[[var_name]]
      values <- values[!is.na(values)]  # Hapus NA
      
      if (length(values) < 3) {
        showNotification("Data terlalu sedikit untuk uji statistik!", type = "error")
        return()
      }
      
      # Uji Normalitas (Shapiro-Wilk) - menggunakan data sesuai filter
      if (length(values) <= 5000) {
        asumsi_results$normalitas <- shapiro.test(values)
      } else {
        # Sample untuk data besar
        sample_values <- sample(values, 5000)
        asumsi_results$normalitas <- shapiro.test(sample_values)
      }
      
      # Uji Homogenitas (Levene's Test) - SELALU menggunakan data NASIONAL
      if (length(unique(data_homogenitas$region)) > 1) {
        # Pastikan ada data untuk setiap region
        region_counts <- table(data_homogenitas$region)
        valid_regions <- names(region_counts[region_counts >= 2])
        
        if (length(valid_regions) > 1) {
          data_homogen <- data_homogenitas %>% filter(region %in% valid_regions)
          asumsi_results$homogenitas <- car::leveneTest(data_homogen[[var_name]], data_homogen$region)
        } else {
          asumsi_results$homogenitas <- NULL
        }
      } else {
        asumsi_results$homogenitas <- NULL
      }
      
      showNotification("Uji asumsi berhasil dijalankan!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Langkah-langkah uji normalitas
  output$langkah_normalitas <- renderText({
    req(input$asumsi_variabel)
    
    paste(
      "1. Hipotesis:",
      "   H₀: Data berdistribusi normal",
      "   H₁: Data tidak berdistribusi normal",
      "",
      "2. Taraf Signifikansi: α = 0.05",
      "",
      "3. Statistik Uji: Shapiro-Wilk Test",
      "",
      "4. Kriteria Keputusan:",
      "   Tolak H₀ jika p-value < α",
      "",
      "5. Kesimpulan berdasarkan p-value",
      sep = "\n"
    )
  })
  
  # Langkah-langkah uji homogenitas
  output$langkah_homogenitas <- renderText({
    paste(
      "1. Hipotesis:",
      "   H₀: Variansi antar kepulauan homogen",
      "   H₁: Variansi antar kepulauan tidak homogen",
      "",
      "2. Taraf Signifikansi: α = 0.05",
      "",
      "3. Statistik Uji: Levene's Test",
      "",
      "4. Data: Membandingkan semua kepulauan (data nasional)",
      "",
      "5. Kriteria Keputusan:",
      "   Tolak H₀ jika p-value < α",
      "",
      "6. Kesimpulan berdasarkan p-value",
      sep = "\n"
    )
  })
  
  # Output hasil normalitas
  output$hasil_normalitas <- renderText({
    if (is.null(asumsi_results$normalitas)) {
      "Belum ada hasil uji. Klik 'Jalankan Uji Asumsi' untuk memulai."
    } else {
      result <- asumsi_results$normalitas
      paste0(
        "Shapiro-Wilk Test for Normality\n",
        "================================\n",
        "W = ", round(result$statistic, 6), "\n",
        "p-value = ", format(result$p.value, scientific = TRUE, digits = 4), "\n",
        "n = ", length(asumsi_results$data_filtered[[input$asumsi_variabel]][!is.na(asumsi_results$data_filtered[[input$asumsi_variabel]])])
      )
    }
  })
  
  # Output hasil homogenitas
  output$hasil_homogenitas <- renderText({
    if (is.null(asumsi_results$homogenitas)) {
      if (is.null(asumsi_results$data_filtered)) {
        "Belum ada hasil uji. Klik 'Jalankan Uji Asumsi' untuk memulai."
      } else {
        "Uji homogenitas tidak dapat dilakukan.\nTidak cukup data untuk perbandingan antar kepulauan."
      }
    } else {
      result <- asumsi_results$homogenitas
      paste0(
        "Levene's Test for Homogeneity of Variance\n",
        "=========================================\n",
        "Membandingkan variansi antar kepulauan (data nasional)\n",
        "F = ", round(result$`F value`[1], 6), "\n",
        "df1 = ", result$Df[1], ", df2 = ", result$Df[2], "\n",
        "p-value = ", format(result$`Pr(>F)`[1], scientific = TRUE, digits = 4), "\n"
      )
    }
  })
  
  # Interpretasi normalitas
  output$interpretasi_normalitas <- renderText({
    if (is.null(asumsi_results$normalitas)) {
      "Interpretasi akan muncul setelah uji dijalankan."
    } else {
      result <- asumsi_results$normalitas
      alpha <- 0.05
      
      keputusan <- ifelse(result$p.value < alpha, "Tolak H₀", "Gagal tolak H₀")
      
      paste0(
        "Keputusan: ", keputusan, "\n\n",
        "Interpretasi: ",
        if (result$p.value < alpha) {
          paste0("Pada taraf signifikansi ", alpha, ", terdapat cukup bukti untuk menyatakan bahwa data tidak berdistribusi normal (p-value = ", format(result$p.value, scientific = TRUE, digits = 4), " < ", alpha, ").")
        } else {
          paste0("Pada taraf signifikansi ", alpha, ", tidak terdapat cukup bukti untuk menyatakan bahwa data tidak berdistribusi normal (p-value = ", format(result$p.value, scientific = TRUE, digits = 4), " ≥ ", alpha, ").")
        }
      )
    }
  })
  
  # Interpretasi homogenitas
  output$interpretasi_homogenitas <- renderText({
    if (is.null(asumsi_results$homogenitas)) {
      if (is.null(asumsi_results$data_filtered)) {
        "Interpretasi akan muncul setelah uji dijalankan."
      } else {
        "Interpretasi tidak tersedia. Tidak cukup data untuk perbandingan antar kepulauan."
      }
    } else {
      result <- asumsi_results$homogenitas
      alpha <- 0.05
      
      keputusan <- ifelse(result$`Pr(>F)`[1] < alpha, "Tolak H₀", "Gagal tolak H₀")
      
      paste0(
        "Keputusan: ", keputusan, "\n\n",
        "Interpretasi: ",
        if (result$`Pr(>F)`[1] < alpha) {
          paste0("Pada taraf signifikansi ", alpha, ", terdapat cukup bukti untuk menyatakan bahwa variansi antar kepulauan tidak homogen (p-value = ", format(result$`Pr(>F)`[1], scientific = TRUE, digits = 4), " < ", alpha, ").")
        } else {
          paste0("Pada taraf signifikansi ", alpha, ", tidak terdapat cukup bukti untuk menyatakan bahwa variansi antar kepulauan tidak homogen (p-value = ", format(result$`Pr(>F)`[1], scientific = TRUE, digits = 4), " ≥ ", alpha, ").")
        }
      )
    }
  })
  
  # QQ Plot
  output$qq_plot <- renderPlot({
    if (is.null(asumsi_results$data_filtered) || is.null(input$asumsi_variabel)) {
      # Plot kosong dengan pesan
      ggplot() + 
        annotate("text", x = 0, y = 0, label = "Klik 'Jalankan Uji Asumsi' untuk melihat plot", size = 6) +
        theme_void()
    } else {
      data <- asumsi_results$data_filtered
      var_name <- input$asumsi_variabel
      
      ggplot(data, aes(sample = .data[[var_name]])) +
        stat_qq() +
        stat_qq_line(color = "red", size = 1) +
        labs(
          title = paste("QQ Plot -", nama_indikator[[var_name]]),
          subtitle = paste("Filter:", input$asumsi_filter_region),
          x = "Theoretical Quantiles",
          y = "Sample Quantiles"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 12)
        )
    }
  })
  
  # Interpretasi QQ Plot
  output$interpretasi_qq_plot <- renderText({
    if (is.null(asumsi_results$data_filtered) || is.null(input$asumsi_variabel)) {
      "Interpretasi akan muncul setelah uji dijalankan."
    } else {
      paste(
        "INTERPRETASI QQ PLOT:",
        "",
        "• Jika titik-titik mengikuti garis merah (theoretical line) dengan baik,",
        "  maka data cenderung berdistribusi normal",
        "",
        "• Jika titik-titik menyimpang jauh dari garis merah,",
        "  maka data tidak berdistribusi normal",
        "",
        "• Perhatikan pola penyimpangan:",
        "  - Lengkungan S: distribusi memiliki skewness",
        "  - Ekor yang menyimpang: distribusi memiliki outlier",
        "  - Pola sistematis lainnya: distribusi tidak normal",
        sep = "\n"
      )
    }
  })
  
  # Histogram dengan kurva normal
  output$histogram_normal <- renderPlot({
    if (is.null(asumsi_results$data_filtered) || is.null(input$asumsi_variabel)) {
      # Plot kosong dengan pesan
      ggplot() + 
        annotate("text", x = 0, y = 0, label = "Klik 'Jalankan Uji Asumsi' untuk melihat plot", size = 6) +
        theme_void()
    } else {
      data <- asumsi_results$data_filtered
      var_name <- input$asumsi_variabel
      values <- data[[var_name]]
      values <- values[!is.na(values)]  # Hapus NA
      
      ggplot(data.frame(values = values), aes(x = values)) +
        geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", alpha = 0.7, color = "black") +
        stat_function(
          fun = dnorm,
          args = list(mean = mean(values), sd = sd(values)),
          color = "red",
          size = 1.2
        ) +
        labs(
          title = paste("Histogram dengan Kurva Normal -", nama_indikator[[var_name]]),
          subtitle = paste("Filter:", input$asumsi_filter_region),
          x = nama_indikator[[var_name]],
          y = "Density"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 12)
        )
    }
  })
  
  # Interpretasi Histogram
  output$interpretasi_histogram <- renderText({
    if (is.null(asumsi_results$data_filtered) || is.null(input$asumsi_variabel)) {
      "Interpretasi akan muncul setelah uji dijalankan."
    } else {
      paste(
        "INTERPRETASI HISTOGRAM:",
        "",
        "• Kurva merah menunjukkan distribusi normal teoritis",
        "  berdasarkan mean dan standar deviasi data",
        "",
        "• Jika histogram (bar biru) mengikuti pola kurva merah,",
        "  maka data cenderung berdistribusi normal",
        "",
        "• Perhatikan karakteristik distribusi:",
        "  - Simetris vs. miring (skewed)",
        "  - Bentuk lonceng vs. bentuk lain",
        "  - Keberadaan multiple peaks (bimodal/multimodal)",
        "",
        "• Semakin dekat histogram dengan kurva normal,",
        "  semakin besar kemungkinan data berdistribusi normal",
        sep = "\n"
      )
    }
  })
  
  # Download handlers untuk uji asumsi
  output$download_normalitas <- downloadHandler(
    filename = function() {
      paste0("Uji_Normalitas_", input$asumsi_variabel, "_", input$asumsi_filter_region, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(asumsi_results$normalitas)
      
      # Buat dokumen Word
      doc <- officer::read_docx()
      
      # Tambahkan header
      doc <- doc %>%
        officer::body_add_par("UJI NORMALITAS - SHAPIRO-WILK TEST", 
                              style = "heading 1") %>%
        officer::body_add_par("") %>%
        officer::body_add_par(paste("Variabel:", nama_indikator[[input$asumsi_variabel]])) %>%
        officer::body_add_par(paste("Filter Wilayah:", input$asumsi_filter_region)) %>%
        officer::body_add_par(paste("Tanggal Analisis:", Sys.Date())) %>%
        officer::body_add_par("") %>%
        
        # Langkah-langkah pengujian
        officer::body_add_par("LANGKAH-LANGKAH PENGUJIAN", style = "heading 2") %>%
        officer::body_add_par(isolate(output$langkah_normalitas())) %>%
        officer::body_add_par("") %>%
        
        # Hasil uji
        officer::body_add_par("HASIL UJI", style = "heading 2") %>%
        officer::body_add_par(isolate(output$hasil_normalitas())) %>%
        officer::body_add_par("") %>%
        
        # Interpretasi
        officer::body_add_par("INTERPRETASI", style = "heading 2") %>%
        officer::body_add_par(isolate(output$interpretasi_normalitas())) %>%
        officer::body_add_par("") %>%
        officer::body_add_par("Dashboard SOVIA-3MED - 2025")
      
      # Simpan dokumen
      print(doc, target = file)
    }
  )
  
  output$download_homogenitas <- downloadHandler(
    filename = function() {
      paste0("Uji_Homogenitas_", input$asumsi_variabel, "_", input$asumsi_filter_region, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      # Buat dokumen Word
      doc <- officer::read_docx()
      
      # Tambahkan header
      doc <- doc %>%
        officer::body_add_par("UJI HOMOGENITAS - LEVENE'S TEST", 
                              style = "heading 1") %>%
        officer::body_add_par("") %>%
        officer::body_add_par(paste("Variabel:", nama_indikator[[input$asumsi_variabel]])) %>%
        officer::body_add_par(paste("Filter Wilayah:", input$asumsi_filter_region)) %>%
        officer::body_add_par(paste("Tanggal Analisis:", Sys.Date())) %>%
        officer::body_add_par("") %>%
        
        # Langkah-langkah pengujian
        officer::body_add_par("LANGKAH-LANGKAH PENGUJIAN", style = "heading 2") %>%
        officer::body_add_par(isolate(output$langkah_homogenitas())) %>%
        officer::body_add_par("") %>%
        
        # Hasil uji
        officer::body_add_par("HASIL UJI", style = "heading 2") %>%
        officer::body_add_par(isolate(output$hasil_homogenitas())) %>%
        officer::body_add_par("") %>%
        
        # Interpretasi
        officer::body_add_par("INTERPRETASI", style = "heading 2") %>%
        officer::body_add_par(isolate(output$interpretasi_homogenitas())) %>%
        officer::body_add_par("") %>%
        officer::body_add_par("Dashboard SOVIA-3MED - 2025")
      
      # Simpan dokumen
      print(doc, target = file)
    }
  )
  
  output$download_qq_plot <- downloadHandler(
    filename = function() {
      paste0("QQ_Plot_", input$asumsi_variabel, "_", input$asumsi_filter_region, "_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      req(asumsi_results$data_filtered, input$asumsi_variabel)
      
      data <- asumsi_results$data_filtered
      var_name <- input$asumsi_variabel
      
      p <- ggplot(data, aes(sample = .data[[var_name]])) +
        stat_qq() +
        stat_qq_line(color = "red", size = 1) +
        labs(
          title = paste("QQ Plot -", nama_indikator[[var_name]]),
          subtitle = paste("Filter:", input$asumsi_filter_region),
          x = "Theoretical Quantiles",
          y = "Sample Quantiles"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 14),
          text = element_text(size = 12)
        )
      
      ggsave(file, p, width = 10, height = 6, dpi = 300, device = "jpeg")
    }
  )
  
  output$download_histogram_normal <- downloadHandler(
    filename = function() {
      paste0("Histogram_Normal_", input$asumsi_variabel, "_", input$asumsi_filter_region, "_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      req(asumsi_results$data_filtered, input$asumsi_variabel)
      
      data <- asumsi_results$data_filtered
      var_name <- input$asumsi_variabel
      values <- data[[var_name]]
      values <- values[!is.na(values)]  # Hapus NA
      
      p <- ggplot(data.frame(values = values), aes(x = values)) +
        geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", alpha = 0.7, color = "black") +
        stat_function(
          fun = dnorm,
          args = list(mean = mean(values), sd = sd(values)),
          color = "red",
          size = 1.2
        ) +
        labs(
          title = paste("Histogram dengan Kurva Normal -", nama_indikator[[var_name]]),
          subtitle = paste("Filter:", input$asumsi_filter_region),
          x = nama_indikator[[var_name]],
          y = "Density"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 14),
          text = element_text(size = 12)
        )
      
      ggsave(file, p, width = 10, height = 6, dpi = 300, device = "jpeg")
    }
  )

  observeEvent(input$download_asumsi_complete, {
    showModal(modalDialog(
      title = "Petunjuk Unduh Halaman",
      HTML("<p>Untuk mengunduh halaman ini sebagai PDF:</p>
         <ol>
           <li>Tekan <strong>Ctrl + P</strong> (atau <strong>Cmd + P</strong> di Mac)</li>
           <li>Pilih <strong>Save as PDF</strong> pada pilihan printer</li>
           <li>Klik <strong>Save</strong></li>
         </ol>"),
      easyClose = TRUE,
      footer = modalButton("Tutup")
    ))
  })
  
  # Download QQ Plot dengan interpretasi (format .docx)
  output$download_qq_plot_docx <- downloadHandler(
    filename = function() {
      paste0("QQ_Plot_Analisis_", input$asumsi_variabel, "_", input$asumsi_filter_region, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(asumsi_results$data_filtered, input$asumsi_variabel)
      
      # Buat dokumen Word
      doc <- officer::read_docx()
      
      # Simpan plot sementara
      temp_plot <- tempfile(fileext = ".png")
      data <- asumsi_results$data_filtered
      var_name <- input$asumsi_variabel
      
      p <- ggplot(data, aes(sample = .data[[var_name]])) +
        stat_qq() +
        stat_qq_line(color = "red", size = 1) +
        labs(
          title = paste("QQ Plot -", nama_indikator[[var_name]]),
          subtitle = paste("Filter:", input$asumsi_filter_region),
          x = "Theoretical Quantiles",
          y = "Sample Quantiles"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 14),
          text = element_text(size = 12)
        )
      
      ggsave(temp_plot, p, width = 10, height = 6, dpi = 300, device = "png")
      
      # Tambahkan konten ke dokumen
      doc <- doc %>%
        officer::body_add_par("ANALISIS QQ PLOT", style = "heading 1") %>%
        officer::body_add_par("") %>%
        officer::body_add_par(paste("Variabel:", nama_indikator[[input$asumsi_variabel]])) %>%
        officer::body_add_par(paste("Filter Wilayah:", input$asumsi_filter_region)) %>%
        officer::body_add_par(paste("Tanggal Analisis:", Sys.Date())) %>%
        officer::body_add_par("") %>%
        
        # Tambahkan grafik
        officer::body_add_par("GRAFIK QQ PLOT", style = "heading 2") %>%
        officer::body_add_img(temp_plot, width = 6, height = 4) %>%
        officer::body_add_par("") %>%
        
        # Tambahkan interpretasi
        officer::body_add_par("INTERPRETASI QQ PLOT", style = "heading 2") %>%
        officer::body_add_par(isolate(output$interpretasi_qq_plot())) %>%
        officer::body_add_par("") %>%
        
        # Footer
        officer::body_add_par("Dashboard SOVIA-3MED - 2025") %>%
        officer::body_add_par("Politeknik Statistika STIS")
      
      # Simpan dokumen
      print(doc, target = file)
      
      # Hapus file temporary
      unlink(temp_plot)
    }
  )
  
  # Download Histogram dengan interpretasi (format .docx)
  output$download_histogram_docx <- downloadHandler(
    filename = function() {
      paste0("Histogram_Analisis_", input$asumsi_variabel, "_", input$asumsi_filter_region, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(asumsi_results$data_filtered, input$asumsi_variabel)
      
      # Buat dokumen Word
      doc <- officer::read_docx()
      
      # Simpan plot sementara
      temp_plot <- tempfile(fileext = ".png")
      data <- asumsi_results$data_filtered
      var_name <- input$asumsi_variabel
      values <- data[[var_name]]
      values <- values[!is.na(values)]
      
      p <- ggplot(data.frame(values = values), aes(x = values)) +
        geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", alpha = 0.7, color = "black") +
        stat_function(
          fun = dnorm,
          args = list(mean = mean(values), sd = sd(values)),
          color = "red",
          size = 1.2
        ) +
        labs(
          title = paste("Histogram dengan Kurva Normal -", nama_indikator[[var_name]]),
          subtitle = paste("Filter:", input$asumsi_filter_region),
          x = nama_indikator[[var_name]],
          y = "Density"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 14),
          text = element_text(size = 12)
        )
      
      ggsave(temp_plot, p, width = 10, height = 6, dpi = 300, device = "png")
      
      # Tambahkan konten ke dokumen
      doc <- doc %>%
        officer::body_add_par("ANALISIS HISTOGRAM", style = "heading 1") %>%
        officer::body_add_par("") %>%
        officer::body_add_par(paste("Variabel:", nama_indikator[[input$asumsi_variabel]])) %>%
        officer::body_add_par(paste("Filter Wilayah:", input$asumsi_filter_region)) %>%
        officer::body_add_par(paste("Tanggal Analisis:", Sys.Date())) %>%
        officer::body_add_par("") %>%
        
        # Tambahkan grafik
        officer::body_add_par("GRAFIK HISTOGRAM", style = "heading 2") %>%
        officer::body_add_img(temp_plot, width = 6, height = 4) %>%
        officer::body_add_par("") %>%
        
        # Tambahkan interpretasi
        officer::body_add_par("INTERPRETASI HISTOGRAM", style = "heading 2") %>%
        officer::body_add_par(isolate(output$interpretasi_histogram())) %>%
        officer::body_add_par("") %>%
        
        # Footer
        officer::body_add_par("Dashboard SOVIA-3MED - 2025") %>%
        officer::body_add_par("Politeknik Statistika STIS")
      
      # Simpan dokumen
      print(doc, target = file)
      
      # Hapus file temporary
      unlink(temp_plot)
    }
  )
  
  #============================ INFERENSIA ============================
  
  observe({
    updateSelectInput(session, "wilayah1", choices = unique(geo_data$region))
    updateSelectInput(session, "wilayah2", choices = unique(geo_data$region))
  })
  
  sub_varinf <- reactive({
    req(input$sub_analysis)
    switch(input$sub_analysis,
           "Penyebaran Informasi Mitigasi" = c("LOWEDU", "POVERTY", "ILLITERATE", "DPRONE", "NOTRAINING", "NOELECTRIC"),
           "Prioritas Pertolongan Saat Bencana" = c("CHILDREN", "FEMALE", "ELDERLY", "NOTRAINING", "FHEAD"),
           "Padat Penduduk Tidak Siap Bencana" = c("POPULATION", "NOTRAINING", "DPRONE", "FAMILYSIZE", "NOSEWER")
    )
  })
  
  output$var_selector <- renderUI({
    selectInput("variabel", "Pilih Variabel:",
                choices = setNames(sub_varinf(), nama_indikator[sub_varinf()]))
  })
  
  # Reactive values
  state <- reactiveValues(hasil = NULL,
                          langkah = NULL,
                          interpretasi = NULL)
  
  observeEvent(input$run_test, {
    req(input$variabel, input$jenis_uji)
    
    # Ambil data
    data <- geo_data %>% st_drop_geometry()
    var_name <- input$variabel
    
    # Langkah-langkah pengujian
    if (input$jenis_uji == "Uji t Satu Sampel") {
      state$langkah <- paste(
        "1. Hipotesis:",
        paste0("   H₀: μ = ", input$nilai_hipotesis),
        paste0("   H₁: μ ≠ ", input$nilai_hipotesis),
        "",
        "2. Taraf Signifikansi: α = 0.05",
        "",
        "3. Statistik Uji: t = (x̄ - μ₀) / (s/√n)",
        "",
        "4. Kriteria Keputusan:",
        "   Tolak H₀ jika |t| > t(α/2, n-1) atau p-value < α",
        "",
        "5. Kesimpulan berdasarkan p-value",
        sep = "\n"
      )
      
      # Jalankan uji
      test_result <- t.test(data[[var_name]], mu = input$nilai_hipotesis)
      state$hasil <- test_result
      
      # Interpretasi
      keputusan <- ifelse(test_result$p.value < 0.05, "Tolak H₀", "Gagal tolak H₀")
      state$interpretasi <- paste0(
        "Keputusan: ", keputusan, "\n\n",
        "Interpretasi: ",
        if (test_result$p.value < 0.05) {
          paste0("Pada taraf signifikansi 0.05, terdapat cukup bukti untuk menyatakan bahwa rata-rata ", nama_indikator[[var_name]], " berbeda secara signifikan dari ", input$nilai_hipotesis, " (p-value = ", format(test_result$p.value, scientific = TRUE), ").")
        } else {
          paste0("Pada taraf signifikansi 0.05, tidak terdapat cukup bukti untuk menyatakan bahwa rata-rata ", nama_indikator[[var_name]], " berbeda dari ", input$nilai_hipotesis, " (p-value = ", format(test_result$p.value, scientific = TRUE), ").")
        }
      )
      
    } else if (input$jenis_uji == "Uji t Dua Sampel") {
      req(input$wilayah1, input$wilayah2)
      
      state$langkah <- paste(
        "1. Hipotesis:",
        "   H₀: μ₁ = μ₂ (rata-rata kedua kelompok sama)",
        "   H₁: μ₁ ≠ μ₂ (rata-rata kedua kelompok berbeda)",
        "",
        "2. Taraf Signifikansi: α = 0.05",
        "",
        "3. Statistik Uji: t = (x̄₁ - x̄₂) / sp√(1/n₁ + 1/n₂)",
        "",
        "4. Kriteria Keputusan:",
        "   Tolak H₀ jika |t| > t(α/2, df) atau p-value < α",
        "",
        "5. Kesimpulan berdasarkan p-value",
        sep = "\n"
      )
      
      # Ambil data untuk kedua wilayah
      data1 <- data[data$region == input$wilayah1, var_name]
      data2 <- data[data$region == input$wilayah2, var_name]
      
      # Jalankan uji
      test_result <- t.test(data1, data2)
      state$hasil <- test_result
      
      # Interpretasi
      keputusan <- ifelse(test_result$p.value < 0.05, "Tolak H₀", "Gagal tolak H₀")
      state$interpretasi <- paste0(
        "Keputusan: ", keputusan, "\n\n",
        "Interpretasi: ",
        if (test_result$p.value < 0.05) {
          paste0("Pada taraf signifikansi 0.05, terdapat perbedaan yang signifikan antara rata-rata ", nama_indikator[[var_name]], " di ", input$wilayah1, " dan ", input$wilayah2, " (p-value = ", format(test_result$p.value, scientific = TRUE), ").")
        } else {
          paste0("Pada taraf signifikansi 0.05, tidak terdapat perbedaan yang signifikan antara rata-rata ", nama_indikator[[var_name]], " di ", input$wilayah1, " dan ", input$wilayah2, " (p-value = ", format(test_result$p.value, scientific = TRUE), ").")
        }
      )
      
    } else if (input$jenis_uji == "Uji Proporsi Satu Kelompok") {
      state$langkah <- paste(
        "1. Hipotesis:",
        paste0("   H₀: p = ", input$nilai_proporsi),
        paste0("   H₁: p ≠ ", input$nilai_proporsi),
        "",
        "2. Taraf Signifikansi: α = 0.05",
        "",
        "3. Statistik Uji: z = (p̂ - p₀) / √(p₀(1-p₀)/n)",
        "",
        "4. Kriteria Keputusan:",
        "   Tolak H₀ jika |z| > z(α/2) atau p-value < α",
        "",
        "5. Kesimpulan berdasarkan p-value",
        sep = "\n"
      )
      
      # Konversi ke proporsi (asumsi data dalam persen)
      prop_data <- data[[var_name]] / 100
      n <- length(prop_data[!is.na(prop_data)])
      x <- sum(prop_data >= input$nilai_proporsi, na.rm = TRUE)
      
      # Jalankan uji proporsi
      test_result <- prop.test(x, n, p = input$nilai_proporsi)
      state$hasil <- test_result
      
      # Interpretasi
      keputusan <- ifelse(test_result$p.value < 0.05, "Tolak H₀", "Gagal tolak H₀")
      state$interpretasi <- paste0(
        "Keputusan: ", keputusan, "\n\n",
        "Interpretasi: ",
        if (test_result$p.value < 0.05) {
          paste0("Pada taraf signifikansi 0.05, proporsi ", nama_indikator[[var_name]], " berbeda secara signifikan dari ", input$nilai_proporsi, " (p-value = ", format(test_result$p.value, scientific = TRUE), ").")
        } else {
          paste0("Pada taraf signifikansi 0.05, proporsi ", nama_indikator[[var_name]], " tidak berbeda secara signifikan dari ", input$nilai_proporsi, " (p-value = ", format(test_result$p.value, scientific = TRUE), ").")
        }
      )
      
    } else if (input$jenis_uji == "Uji Proporsi Dua Kelompok") {
      req(input$wilayah1, input$wilayah2)
      
      state$langkah <- paste(
        "1. Hipotesis:",
        "   H₀: p₁ = p₂ (proporsi kedua kelompok sama)",
        "   H₁: p₁ ≠ p₂ (proporsi kedua kelompok berbeda)",
        "",
        "2. Taraf Signifikansi: α = 0.05",
        "",
        "3. Statistik Uji: z = (p̂₁ - p̂₂) / √(p̂(1-p̂)(1/n₁ + 1/n₂))",
        "",
        "4. Kriteria Keputusan:",
        "   Tolak H₀ jika |z| > z(α/2) atau p-value < α",
        "",
        "5. Kesimpulan berdasarkan p-value",
        sep = "\n"
      )
      
      # Ambil data untuk kedua wilayah
      data1 <- data[data$region == input$wilayah1, var_name]
      data2 <- data[data$region == input$wilayah2, var_name]
      
      # Konversi ke proporsi dan hitung sukses
      prop1 <- data1 / 100
      prop2 <- data2 / 100
      n1 <- length(prop1[!is.na(prop1)])
      n2 <- length(prop2[!is.na(prop2)])
      x1 <- sum(prop1 >= 0.5, na.rm = TRUE)  # Asumsi threshold 50%
      x2 <- sum(prop2 >= 0.5, na.rm = TRUE)
      
      # Jalankan uji proporsi dua sampel
      test_result <- prop.test(c(x1, x2), c(n1, n2))
      state$hasil <- test_result
      
      # Interpretasi
      keputusan <- ifelse(test_result$p.value < 0.05, "Tolak H₀", "Gagal tolak H₀")
      state$interpretasi <- paste0(
        "Keputusan: ", keputusan, "\n\n",
        "Interpretasi: ",
        if (test_result$p.value < 0.05) {
          paste0("Pada taraf signifikansi 0.05, terdapat perbedaan yang signifikan antara proporsi ", nama_indikator[[var_name]], " di ", input$wilayah1, " dan ", input$wilayah2, " (p-value = ", format(test_result$p.value, scientific = TRUE), ").")
        } else {
          paste0("Pada taraf signifikansi 0.05, tidak terdapat perbedaan yang signifikan antara proporsi ", nama_indikator[[var_name]], " di ", input$wilayah1, " dan ", input$wilayah2, " (p-value = ", format(test_result$p.value, scientific = TRUE), ").")
        }
      )
      
    } else if (input$jenis_uji == "Uji Variansi Satu Kelompok") {
      state$langkah <- paste(
        "1. Hipotesis:",
        paste0("   H₀: σ² = ", input$varian_hipotesis),
        paste0("   H₁: σ² ≠ ", input$varian_hipotesis),
        "",
        "2. Taraf Signifikansi: α = 0.05",
        "",
        "3. Statistik Uji: χ² = (n-1)s² / σ₀²",
        "",
        "4. Kriteria Keputusan:",
        "   Tolak H₀ jika χ² < χ²(α/2, n-1) atau χ² > χ²(1-α/2, n-1)",
        "",
        "5. Kesimpulan berdasarkan p-value",
        sep = "\n"
      )
      
      # Hitung statistik chi-square untuk variansi
      values <- data[[var_name]][!is.na(data[[var_name]])]
      n <- length(values)
      s2 <- var(values)
      chi_stat <- (n - 1) * s2 / input$varian_hipotesis
      p_value <- 2 * min(pchisq(chi_stat, n-1), 1 - pchisq(chi_stat, n-1))
      
      test_result <- list(
        statistic = chi_stat,
        parameter = n - 1,
        p.value = p_value,
        estimate = s2,
        null.value = input$varian_hipotesis
      )
      
      state$hasil <- test_result
      
      # Interpretasi
      keputusan <- ifelse(p_value < 0.05, "Tolak H₀", "Gagal tolak H₀")
      state$interpretasi <- paste0(
        "Keputusan: ", keputusan, "\n\n",
        "Interpretasi: ",
        if (p_value < 0.05) {
          paste0("Pada taraf signifikansi 0.05, variansi ", nama_indikator[[var_name]], " berbeda secara signifikan dari ", input$varian_hipotesis, " (p-value = ", format(p_value, scientific = TRUE), ").")
        } else {
          paste0("Pada taraf signifikansi 0.05, variansi ", nama_indikator[[var_name]], " tidak berbeda secara signifikan dari ", input$varian_hipotesis, " (p-value = ", format(p_value, scientific = TRUE), ").")
        }
      )
      
    } else if (input$jenis_uji == "Uji Variansi Dua Kelompok") {
      req(input$wilayah1, input$wilayah2)
      
      state$langkah <- paste(
        "1. Hipotesis:",
        "   H₀: σ₁² = σ₂² (variansi kedua kelompok sama)",
        "   H₁: σ₁² ≠ σ₂² (variansi kedua kelompok berbeda)",
        "",
        "2. Taraf Signifikansi: α = 0.05",
        "",
        "3. Statistik Uji: F = s₁² / s₂²",
        "",
        "4. Kriteria Keputusan:",
        "   Tolak H₀ jika F < F(α/2, n₁-1, n₂-1) atau F > F(1-α/2, n₁-1, n₂-1)",
        "",
        "5. Kesimpulan berdasarkan p-value",
        sep = "\n"
      )
      
      # Ambil data untuk kedua wilayah
      data1 <- data[data$region == input$wilayah1, var_name]
      data2 <- data[data$region == input$wilayah2, var_name]
      
      # Jalankan uji F untuk variansi
      test_result <- var.test(data1, data2)
      state$hasil <- test_result
      
      # Interpretasi
      keputusan <- ifelse(test_result$p.value < 0.05, "Tolak H₀", "Gagal tolak H₀")
      state$interpretasi <- paste0(
        "Keputusan: ", keputusan, "\n\n",
        "Interpretasi: ",
        if (test_result$p.value < 0.05) {
          paste0("Pada taraf signifikansi 0.05, terdapat perbedaan yang signifikan antara variansi ", nama_indikator[[var_name]], " di ", input$wilayah1, " dan ", input$wilayah2, " (p-value = ", format(test_result$p.value, scientific = TRUE), ").")
        } else {
          paste0("Pada taraf signifikansi 0.05, tidak terdapat perbedaan yang signifikan antara variansi ", nama_indikator[[var_name]], " di ", input$wilayah1, " dan ", input$wilayah2, " (p-value = ", format(test_result$p.value, scientific = TRUE), ").")
        }
      )
      
    } else if (input$jenis_uji == "ANOVA (Satu Arah)") {
      state$langkah <- paste(
        "1. Hipotesis:",
        "   H₀: μ₁ = μ₂ = ... = μₖ (semua rata-rata kelompok sama)",
        "   H₁: Minimal ada satu rata-rata yang berbeda",
        "",
        "2. Taraf Signifikansi: α = 0.05",
        "",
        "3. Statistik Uji: F = MSB / MSW",
        "",
        "4. Kriteria Keputusan:",
        "   Tolak H₀ jika F > F(α, k-1, N-k) atau p-value < α",
        "",
        "5. Kesimpulan berdasarkan p-value",
        sep = "\n"
      )
      
      # Jalankan ANOVA
      formula_str <- paste(var_name, "~ region")
      test_result <- aov(as.formula(formula_str), data = data)
      summary_result <- summary(test_result)
      
      state$hasil <- summary_result
      
      # Interpretasi
      p_value <- summary_result[[1]]$`Pr(>F)`[1]
      keputusan <- ifelse(p_value < 0.05, "Tolak H₀", "Gagal tolak H₀")
      state$interpretasi <- paste0(
        "Keputusan: ", keputusan, "\n\n",
        "Interpretasi: ",
        if (p_value < 0.05) {
          paste0("Pada taraf signifikansi 0.05, terdapat perbedaan yang signifikan antara rata-rata ", nama_indikator[[var_name]], " di berbagai wilayah (p-value = ", format(p_value, scientific = TRUE), ").")
        } else {
          paste0("Pada taraf signifikansi 0.05, tidak terdapat perbedaan yang signifikan antara rata-rata ", nama_indikator[[var_name]], " di berbagai wilayah (p-value = ", format(p_value, scientific = TRUE), ").")
        }
      )
    }
  })
  
  # Output
  output$langkah_pengujian <- renderUI({
    if (!is.null(state$langkah)) {
      pre(state$langkah)
    }
  })
  
  output$hasil_uji <- renderPrint({
    if (!is.null(state$hasil)) {
      if (input$jenis_uji == "Uji Variansi Satu Kelompok") {
        cat("Chi-Square Test for Variance\n")
        cat("============================\n")
        cat("Chi-square =", round(state$hasil$statistic, 6), "\n")
        cat("df =", state$hasil$parameter, "\n")
        cat("p-value =", format(state$hasil$p.value, scientific = TRUE), "\n")
        cat("Sample variance =", round(state$hasil$estimate, 6), "\n")
        cat("Hypothesized variance =", state$hasil$null.value, "\n")
      } else {
        print(state$hasil)
      }
    }
  })
  
  output$interpretasi <- renderUI({
    if (!is.null(state$interpretasi)) {
      p(state$interpretasi)
    }
  })
  
  # Download handler untuk inferensia
  output$download_inferensia_hasil <- downloadHandler(
    filename = function() {
      paste0("Hasil_Uji_", gsub(" ", "_", input$jenis_uji), "_", input$variabel, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      content <- paste(
        paste("UJI STATISTIK:", toupper(input$jenis_uji)),
        paste(rep("=", nchar(paste("UJI STATISTIK:", toupper(input$jenis_uji)))), collapse = ""),
        "",
        paste("Sub Analisis:", input$sub_analysis),
        paste("Variabel:", ifelse(is.null(input$variabel), "Belum dipilih", nama_indikator[[input$variabel]])),
        paste("Tanggal Analisis:", Sys.Date()),
        "",
        "LANGKAH-LANGKAH PENGUJIAN",
        "=========================",
        ifelse(is.null(state$langkah), "Belum dijalankan", state$langkah),
        "",
        "HASIL UJI STATISTIK",
        "===================",
        ifelse(is.null(state$hasil), "Belum dijalankan", paste(capture.output(print(state$hasil)), collapse = "\n")),
        "",
        "INTERPRETASI HASIL",
        "==================",
        ifelse(is.null(state$interpretasi), "Belum dijalankan", state$interpretasi),
        "",
        "Dashboard SOVIA-3MED - 2025",
        "Politeknik Statistika STIS"
      )
      
      writeLines(content, file)
    }
  )
  
  output$download_inferensia_complete <- downloadHandler(
    filename = function() {
      paste0("SOVIA3MED_Inferensia_", gsub(" ", "_", input$jenis_uji), "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      content <- paste(
        "SOVIA-3MED DASHBOARD - ANALISIS INFERENSIA",
        "==========================================",
        "",
        paste("Sub Analisis:", input$sub_analysis),
        paste("Jenis Uji:", input$jenis_uji),
        paste("Variabel:", ifelse(is.null(input$variabel), "Belum dipilih", nama_indikator[[input$variabel]])),
        paste("Tanggal Analisis:", Sys.Date()),
        "",
        "LANGKAH-LANGKAH PENGUJIAN",
        "=========================",
        ifelse(is.null(state$langkah), "Belum dijalankan", state$langkah),
        "",
        "HASIL UJI STATISTIK",
        "===================",
        ifelse(is.null(state$hasil), "Belum dijalankan", paste(capture.output(print(state$hasil)), collapse = "\n")),
        "",
        "INTERPRETASI HASIL",
        "==================",
        ifelse(is.null(state$interpretasi), "Belum dijalankan", state$interpretasi),
        "",
        "KESIMPULAN",
        "==========",
        "Berdasarkan hasil uji statistik di atas, dapat disimpulkan apakah terdapat perbedaan yang signifikan atau tidak sesuai dengan hipotesis yang diajukan.",
        "",
        "Dashboard SOVIA-3MED - 2025",
        "Politeknik Statistika STIS"
      )
      
      writeLines(content, file)
    }
  )
  
  #============================ REGRESI ============================
  
  # Reactive values untuk regresi
  regresi_results <- reactiveValues(
    model = NULL,
    data = NULL,
    variables = NULL,
    indeks_data = NULL
  )
  
  # Jalankan analisis regresi
  observeEvent(input$run_regresi, {
    req(input$sub_regresi)
    
    # Ambil variabel berdasarkan sub analisis
    vars <- sub_analisis_choices[[input$sub_regresi]]
    
    # Hitung indeks kerentanan sebagai variabel Y
    indeks_data <- hitung_indeks(geo_data, input$sub_regresi)
    
    # Siapkan data dengan variabel X dari sub analisis
    data_x <- geo_data %>%
      st_drop_geometry() %>%
      dplyr::select(nmkab, all_of(vars)) %>%
      na.omit()
    
    # Gabungkan dengan indeks sebagai Y
    if (input$sub_regresi == "Dampak") {
      y_var <- "indeks_dampak"
    } else if (input$sub_regresi == "Mitigasi") {
      y_var <- "indeks_mitigasi"
    } else if (input$sub_regresi == "Evakuasi") {
      y_var <- "indeks_evakuasi"
    }
    
    # Gabungkan data
    data_final <- data_x %>%
      left_join(indeks_data %>% dplyr::select(nmkab, all_of(y_var)), by = "nmkab") %>%
      na.omit()
    
    regresi_results$data <- data_final
    regresi_results$variables <- c(y_var, vars)
    regresi_results$indeks_data <- indeks_data
    
    # Buat formula dengan indeks sebagai Y
    formula_str <- paste(y_var, "~", paste(vars, collapse = " + "))
    
    # Validasi data
    if (nrow(data_final) < 10) {
      showNotification("Data tidak cukup untuk analisis regresi!", type = "error")
      return()
    }
    
    # Jalankan regresi
    tryCatch({
      if (input$metode_regresi == "lm") {
        model <- lm(as.formula(formula_str), data = data_final)
      } else if (input$metode_regresi == "step") {
        full_model <- lm(as.formula(formula_str), data = data_final)
        model <- step(full_model, trace = FALSE)
      }
      
      regresi_results$model <- model
      showNotification("Analisis regresi berhasil dijalankan!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error dalam analisis:", e$message), type = "error")
    })
  })
  
  # Informasi Model
  output$info_model <- renderText({
    req(input$sub_regresi)
    
    if (input$sub_regresi == "Dampak") {
      y_desc <- "Indeks Dampak"
    } else if (input$sub_regresi == "Mitigasi") {
      y_desc <- "Indeks Mitigasi"
    } else if (input$sub_regresi == "Evakuasi") {
      y_desc <- "Indeks Evakuasi"
    }
    
    vars <- sub_analisis_choices[[input$sub_regresi]]
    
    # Gabungkan semua variabel X dalam satu string
    x_vars_list <- paste("- ", nama_indikator[vars], collapse = "\n")
    
    paste(
      "INFORMASI MODEL REGRESI",
      "=======================",
      "",
      paste("Variabel Dependen (Y):", y_desc),
      "",
      "Variabel Independen (X):",
      x_vars_list,
      "",
      paste("Sub Analisis:", input$sub_regresi),
      "",
      "Tujuan:",
      "Menganalisis faktor-faktor yang mempengaruhi",
      "tingkat kerentanan sosial dalam konteks bencana.",
      sep = "\n"
    )
  })
  
  # Penjelasan Metode
  output$penjelasan_metode <- renderText({
    req(input$metode_regresi)
    
    if (input$metode_regresi == "lm") {
      paste(
        "MULTIPLE LINEAR REGRESSION",
        "==========================",
        "",
        "Deskripsi:",
        "Metode regresi yang menggunakan semua variabel",
        "independen yang tersedia dalam model.",
        "",
        "Karakteristik:",
        "• Menggunakan semua prediktor",
        "• Tidak ada seleksi variabel",
        "• Model lengkap (full model)",
        "• Cocok jika semua variabel teoritis penting",
        "",
        "Formula:",
        "Y = β₀ + β₁X₁ + β₂X₂ + ... + βₖXₖ + ε",
        "",
        "Kelebihan:",
        "• Mudah diinterpretasi",
        "• Mencakup semua variabel",
        "• Tidak ada bias seleksi",
        "",
        "Kekurangan:",
        "• Mungkin ada variabel tidak signifikan",
        "• Risiko overfitting pada sampel kecil",
        sep = "\n"
      )
    } else {
      paste(
        "STEPWISE REGRESSION",
        "===================",
        "",
        "Deskripsi:",
        "Metode regresi yang melakukan seleksi variabel",
        "secara otomatis berdasarkan kriteria statistik.",
        "",
        "Karakteristik:",
        "• Seleksi variabel otomatis",
        "• Menggunakan kriteria AIC",
        "• Menambah/mengurangi variabel secara iteratif",
        "• Menghasilkan model parsimonious",
        "",
        "Proses:",
        "1. Mulai dengan model lengkap",
        "2. Evaluasi setiap variabel",
        "3. Hapus variabel tidak signifikan",
        "4. Tambah kembali jika diperlukan",
        "5. Ulangi hingga optimal",
        "",
        "Kelebihan:",
        "• Model lebih sederhana",
        "• Hanya variabel penting yang tersisa",
        "• Mengurangi multikolinearitas",
        "",
        "Kekurangan:",
        "• Mungkin menghilangkan variabel penting",
        "• Hasil dapat bervariasi antar sampel",
        sep = "\n"
      )
    }
  })
  
  # Summary model regresi
  output$summary_regresi <- renderPrint({
    if (is.null(regresi_results$model)) {
      cat("Model belum dijalankan.\n")
      cat("Klik 'Jalankan Analisis Regresi' untuk memulai.\n")
      return()
    }
    
    model <- regresi_results$model
    cat("RINGKASAN MODEL REGRESI\n")
    cat("=======================\n\n")
    
    # Informasi dasar
    cat("Formula:", deparse(formula(model)), "\n")
    cat("Metode:", ifelse(input$metode_regresi == "lm", "Multiple Linear Regression", "Stepwise Regression"), "\n\n")
    
    # Summary statistik
    summary(model)
  })
  
  # Goodness of Fit
  output$goodness_of_fit <- renderText({
    if (is.null(regresi_results$model)) {
      return("Model belum dijalankan.")
    }
    
    model <- regresi_results$model
    summary_model <- summary(model)
    
    # Statistik model
    r_squared <- round(summary_model$r.squared, 4)
    adj_r_squared <- round(summary_model$adj.r.squared, 4)
    rmse <- round(sqrt(mean(residuals(model)^2)), 4)
    
    # F-test
    f_stat <- summary_model$fstatistic
    f_value <- round(f_stat[1], 4)
    f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
    
    paste(
      "GOODNESS OF FIT",
      "===============",
      "",
      "Statistik Model:",
      paste("• R-squared:", r_squared, paste0("(", round(r_squared * 100, 2), "%)")),
      paste("• Adjusted R-squared:", adj_r_squared),
      paste("• RMSE:", rmse),
      paste("• Residual Standard Error:", round(summary_model$sigma, 4)),
      "",
      "Uji Signifikansi Model (F-test):",
      paste("• F-statistic:", f_value),
      paste("• p-value:", format(f_p_value, scientific = TRUE)),
      "",
      "Interpretasi Model:",
      if (f_p_value < 0.05) {
        "✅ Model secara keseluruhan SIGNIFIKAN (p < 0.05)"
      } else {
        "❌ Model secara keseluruhan TIDAK SIGNIFIKAN (p ≥ 0.05)"
      },
      "",
      "Kualitas Prediksi (R-squared):",
      if (r_squared > 0.7) {
        "✅ Model sangat baik (R² > 0.7)"
      } else if (r_squared > 0.5) {
        "⚠️ Model cukup baik (0.5 < R² ≤ 0.7)"
      } else if (r_squared > 0.3) {
        "⚠️ Model sedang (0.3 < R² ≤ 0.5)"
      } else {
        "❌ Model kurang baik (R² ≤ 0.3)"
      },
      sep = "\n"
    )
  })
  
  # Uji Normalitas Residual
  output$uji_normalitas_residual <- renderPrint({
    if (is.null(regresi_results$model)) {
      cat("Model belum dijalankan.\n")
      return()
    }
    
    residuals_model <- residuals(regresi_results$model)
    
    cat("UJI NORMALITAS RESIDUAL\n")
    cat("=======================\n\n")
    
    cat("Hipotesis:\n")
    cat("H₀: Residual berdistribusi normal\n")
    cat("H₁: Residual tidak berdistribusi normal\n")
    cat("α = 0.05\n\n")
    
    # Shapiro-Wilk Test
    if (length(residuals_model) <= 5000) {
      shapiro_result <- shapiro.test(residuals_model)
      cat("Shapiro-Wilk Test:\n")
      cat("W =", round(shapiro_result$statistic, 6), "\n")
      cat("p-value =", format(shapiro_result$p.value, scientific = TRUE), "\n\n")
      
      cat("Keputusan:\n")
      if (shapiro_result$p.value < 0.05) {
        cat("❌ Tolak H₀ (p < 0.05)\n")
        cat("❌ KESIMPULAN: Residual TIDAK berdistribusi normal\n")
        cat("⚠️ Asumsi normalitas DILANGGAR\n")
      } else {
        cat("✅ Gagal tolak H₀ (p ≥ 0.05)\n")
        cat("✅ KESIMPULAN: Residual berdistribusi normal\n")
        cat("✅ Asumsi normalitas TERPENUHI\n")
      }
    } else {
      cat("Sampel terlalu besar untuk Shapiro-Wilk.\n")
      cat("Gunakan Q-Q plot untuk evaluasi visual.\n")
    }
  })
  
  # Uji Multikolinearitas
  output$uji_multikolinearitas <- renderPrint({
    if (is.null(regresi_results$model)) {
      cat("Model belum dijalankan.\n")
      return()
    }
    
    cat("UJI MULTIKOLINEARITAS (VIF)\n")
    cat("===========================\n\n")
    
    cat("Tujuan: Mendeteksi korelasi tinggi antar variabel independen\n\n")
    
    tryCatch({
      vif_values <- car::vif(regresi_results$model)
      
      cat("Variance Inflation Factor (VIF):\n")
      cat("--------------------------------\n")
      for (i in 1:length(vif_values)) {
        var_name <- names(vif_values)[i]
        vif_val <- round(as.numeric(vif_values[i]), 3)
        status <- ifelse(vif_val > 10, "SERIUS", 
                         ifelse(vif_val > 5, "MODERAT", "AMAN"))
        cat(sprintf("%-15s: %6.3f (%s)\n", nama_indikator[[var_name]], vif_val, status))
      }
      cat("\n")
      
      # Interpretasi keseluruhan
      max_vif <- max(vif_values)
      cat("Interpretasi Keseluruhan:\n")
      if (max_vif > 10) {
        cat("❌ Terdapat multikolinearitas SERIUS (VIF > 10)\n")
        cat("⚠️ Asumsi tidak terpenuhi\n")
        cat("💡 Saran: Hapus variabel dengan VIF tertinggi\n")
      } else if (max_vif > 5) {
        cat("⚠️ Terdapat multikolinearitas MODERAT (VIF > 5)\n")
        cat("⚠️ Perlu perhatian dalam interpretasi\n")
      } else {
        cat("✅ TIDAK terdapat multikolinearitas (VIF ≤ 5)\n")
        cat("✅ Asumsi terpenuhi\n")
      }
      
      cat("\nKriteria:\n")
      cat("• VIF < 5: Tidak ada masalah\n")
      cat("• VIF 5-10: Multikolinearitas moderat\n")
      cat("• VIF > 10: Multikolinearitas serius\n")
      
    }, error = function(e) {
      cat("Error dalam menghitung VIF:", e$message, "\n")
      cat("Kemungkinan hanya ada satu prediktor.\n")
    })
  })
  
  # Uji Heteroskedastisitas
  output$uji_heteroskedastisitas <- renderPrint({
    if (is.null(regresi_results$model)) {
      cat("Model belum dijalankan.\n")
      return()
    }
    
    cat("UJI HETEROSKEDASTISITAS\n")
    cat("=======================\n\n")
    
    cat("Tujuan: Menguji apakah variansi residual konstan\n")
    cat("Hipotesis:\n")
    cat("H₀: Variansi residual konstan (homoskedastisitas)\n")
    cat("H₁: Variansi residual tidak konstan (heteroskedastisitas)\n")
    cat("α = 0.05\n\n")
    
    # Breusch-Pagan Test
    tryCatch({
      bp_test <- lmtest::bptest(regresi_results$model)
      
      cat("Breusch-Pagan Test:\n")
      cat("BP =", round(bp_test$statistic, 6), "\n")
      cat("df =", bp_test$parameter, "\n")
      cat("p-value =", format(bp_test$p.value, scientific = TRUE), "\n\n")
      
      cat("Keputusan:\n")
      if (bp_test$p.value < 0.05) {
        cat("❌ Tolak H₀ (p < 0.05)\n")
        cat("❌ KESIMPULAN: Terdapat HETEROSKEDASTISITAS\n")
        cat("⚠️ Asumsi homoskedastisitas DILANGGAR\n")
        cat("💡 Saran: Gunakan robust standard errors\n")
      } else {
        cat("✅ Gagal tolak H₀ (p ≥ 0.05)\n")
        cat("✅ KESIMPULAN: TIDAK terdapat heteroskedastisitas\n")
        cat("✅ Asumsi homoskedastisitas TERPENUHI\n")
      }
      
    }, error = function(e) {
      cat("Error dalam uji Breusch-Pagan:", e$message, "\n")
    })
  })
  
  # Uji Autokorelasi
  output$uji_autokorelasi <- renderPrint({
    if (is.null(regresi_results$model)) {
      cat("Model belum dijalankan.\n")
      return()
    }
    
    cat("UJI AUTOKORELASI\n")
    cat("================\n\n")
    
    cat("Tujuan: Mendeteksi korelasi antar residual\n")
    cat("Hipotesis:\n")
    cat("H₀: Tidak ada autokorelasi\n")
    cat("H₁: Ada autokorelasi\n")
    cat("α = 0.05\n\n")
    
    # Durbin-Watson Test
    tryCatch({
      dw_test <- lmtest::dwtest(regresi_results$model)
      
      cat("Durbin-Watson Test:\n")
      cat("DW =", round(dw_test$statistic, 6), "\n")
      cat("p-value =", format(dw_test$p.value, scientific = TRUE), "\n\n")
      
      # Interpretasi berdasarkan nilai DW
      dw_value <- as.numeric(dw_test$statistic)
      cat("Keputusan:\n")
      if (dw_value < 1.5) {
        cat("❌ DW < 1.5: Autokorelasi POSITIF\n")
        cat("⚠️ Asumsi independensi residual DILANGGAR\n")
      } else if (dw_value > 2.5) {
        cat("❌ DW > 2.5: Autokorelasi NEGATIF\n")
        cat("⚠️ Asumsi independensi residual DILANGGAR\n")
      } else {
        cat("✅ 1.5 ≤ DW ≤ 2.5: TIDAK ada autokorelasi\n")
        cat("✅ Asumsi independensi residual TERPENUHI\n")
      }
      
      cat("\nPanduan Interpretasi:\n")
      cat("• DW ≈ 2: Tidak ada autokorelasi (ideal)\n")
      cat("• DW < 1.5: Autokorelasi positif serius\n")
      cat("• DW > 2.5: Autokorelasi negatif serius\n")
      cat("• 1.5 ≤ DW ≤ 2.5: Dapat diterima\n")
      
    }, error = function(e) {
      cat("Error dalam uji Durbin-Watson:", e$message, "\n")
    })
  })
  
  # Interpretasi Lengkap Model Regresi
  # Interpretasi Lengkap Model Regresi
  output$interpretasi_regresi_lengkap <- renderUI({
    if (is.null(regresi_results$model)) {
      return(div(
        h5("Model belum dijalankan."),
        p("Klik 'Jalankan Analisis Regresi' untuk melihat interpretasi.")
      ))
    }
    
    model <- regresi_results$model
    summary_model <- summary(model)
    
    # Informasi dasar model
    r_squared <- round(summary_model$r.squared, 4)
    adj_r_squared <- round(summary_model$adj.r.squared, 4)
    f_statistic <- round(summary_model$fstatistic[1], 4)
    f_p_value <- pf(summary_model$fstatistic[1], 
                    summary_model$fstatistic[2], 
                    summary_model$fstatistic[3], 
                    lower.tail = FALSE)
    
    # Koefisien signifikan
    coef_table <- summary_model$coefficients
    sig_vars <- rownames(coef_table)[coef_table[, 4] < 0.05]
    sig_vars <- sig_vars[sig_vars != "(Intercept)"]
    
    # Status asumsi
    residuals_model <- residuals(model)
    shapiro_ok <- if(length(residuals_model) <= 5000) {
      shapiro.test(residuals_model)$p.value >= 0.05
    } else { TRUE }
    
    # VIF check
    vif_ok <- tryCatch({
      vif_vals <- car::vif(model)
      max(vif_vals) <= 5
    }, error = function(e) TRUE)
    
    # Heteroskedastisitas check
    hetero_ok <- tryCatch({
      bp_test <- lmtest::bptest(model)
      bp_test$p.value >= 0.05
    }, error = function(e) TRUE)
    
    # Autokorelasi check
    auto_ok <- tryCatch({
      dw_test <- lmtest::dwtest(model)
      dw_val <- as.numeric(dw_test$statistic)
      dw_val >= 1.5 && dw_val <= 2.5
    }, error = function(e) TRUE)
    
    div(
      # Header
      h4("INTERPRETASI LENGKAP MODEL REGRESI"),
      p(paste("Sub Analisis:", input$sub_regresi, "| Metode:", ifelse(input$metode_regresi == "lm", "Multiple Linear", "Stepwise"))),
      
      # Informasi Dasar
      wellPanel(
        h5("INFORMASI DASAR MODEL"),
        p(strong("Variabel Dependen: "), 
          if(input$sub_regresi == "Dampak") "Indeks Dampak" else 
            if(input$sub_regresi == "Mitigasi") "Indeks Mitigasi" else "Indeks Evakuasi"),
        p(strong("Metode: "), ifelse(input$metode_regresi == "lm", "Multiple Linear Regression", "Stepwise Regression")),
        p(strong("Jumlah Prediktor: "), length(coef_table[,1]) - 1)
      ),
      
      # Kualitas Model
      wellPanel(
        h5("KUALITAS DAN SIGNIFIKANSI MODEL"),
        p(strong("R-squared: "), paste0(r_squared, " (", round(r_squared * 100, 2), "%)")),
        p(strong("Adjusted R-squared: "), adj_r_squared),
        p(strong("F-statistic: "), f_statistic, " (p-value: ", format(f_p_value, scientific = TRUE), ")"),
        br(),
        p(if (f_p_value < 0.05) {
          strong("✅ Model signifikan secara keseluruhan")
        } else {
          strong("❌ Model tidak signifikan")
        }),
        p(if (r_squared > 0.7) {
          strong("✅ Daya prediksi sangat baik")
        } else if (r_squared > 0.5) {
          strong("⚠️ Daya prediksi cukup baik")
        } else {
          strong("❌ Daya prediksi kurang baik")
        })
      ),
      
      # Variabel Signifikan
      if (length(sig_vars) > 0) {
        wellPanel(
          h5("VARIABEL YANG BERPENGARUH SIGNIFIKAN"),
          lapply(sig_vars, function(var) {
            coef_val <- round(coef_table[var, 1], 4)
            p_val <- format(coef_table[var, 4], scientific = TRUE)
            interpretation <- if(coef_val > 0) "meningkatkan" else "menurunkan"
            
            div(
              p(strong(nama_indikator[[var]])),
              p("Koefisien: ", strong(coef_val), " | p-value: ", strong(p_val)),
              p(em(paste("Setiap peningkatan 1 unit akan", interpretation, "indeks kerentanan sebesar", abs(coef_val), "unit"))),
              hr()
            )
          })
        )
      } else {
        wellPanel(
          h5("TIDAK ADA VARIABEL SIGNIFIKAN"),
          p("Tidak ada variabel yang berpengaruh signifikan pada α = 0.05")
        )
      },
      
      # Status Asumsi
      wellPanel(
        h5("STATUS ASUMSI KLASIK"),
        fluidRow(
          column(6,
                 p(if(shapiro_ok) "✅ Normalitas residual" else "❌ Normalitas residual"),
                 p(if(vif_ok) "✅ Tidak ada multikolinearitas" else "❌ Ada multikolinearitas")
          ),
          column(6,
                 p(if(hetero_ok) "✅ Homoskedastisitas" else "❌ Heteroskedastisitas"),
                 p(if(auto_ok) "✅ Tidak ada autokorelasi" else "❌ Ada autokorelasi")
          )
        ),
        br(),
        p(if(all(c(shapiro_ok, vif_ok, hetero_ok, auto_ok))) {
          strong("✅ Semua asumsi klasik terpenuhi - Model valid untuk digunakan")
        } else {
          strong("⚠️ Beberapa asumsi tidak terpenuhi - Interpretasi dengan hati-hati")
        })
      ),
      
      # Rekomendasi
      wellPanel(
        h5("REKOMENDASI DAN KESIMPULAN"),
        p("Berdasarkan analisis regresi untuk kerentanan sosial dalam konteks", strong(tolower(input$sub_regresi)), "bencana:"),
        tags$ul(
          if(length(sig_vars) > 0) {
            tags$li("Fokus pada variabel yang signifikan untuk intervensi kebijakan")
          },
          tags$li("Gunakan hasil model untuk identifikasi daerah prioritas"),
          tags$li("Validasi model dengan data tambahan jika memungkinkan"),
          if(!all(c(shapiro_ok, vif_ok, hetero_ok, auto_ok))) {
            tags$li("Pertimbangkan transformasi data atau metode alternatif untuk mengatasi pelanggaran asumsi")
          }
        ),
        p(strong("Catatan: "), "Model ini dapat digunakan untuk perencanaan mitigasi bencana dan alokasi sumber daya berdasarkan tingkat kerentanan sosial.")
      )
    )
  })
  
  # Download handler untuk halaman regresi lengkap
  output$download_regresi_complete <- downloadHandler(
    filename = function() {
      paste0("SOVIA3MED_Analisis_Regresi_", input$sub_regresi, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      # Buat dokumen Word lengkap
      doc <- officer::read_docx()
      
      doc <- doc %>%
        officer::body_add_par("SOVIA-3MED DASHBOARD - ANALISIS REGRESI", style = "heading 1") %>%
        officer::body_add_par("") %>%
        officer::body_add_par(paste("Sub Analisis:", input$sub_regresi)) %>%
        officer::body_add_par(paste("Metode Regresi:", ifelse(input$metode_regresi == "lm", "Multiple Linear Regression", "Stepwise Regression"))) %>%
        officer::body_add_par(paste("Tanggal Analisis:", Sys.Date())) %>%
        officer::body_add_par("") %>%
        
        # Informasi Model
        officer::body_add_par("INFORMASI MODEL", style = "heading 2") %>%
        officer::body_add_par(isolate(output$info_model())) %>%
        officer::body_add_par("") %>%
        
        # Penjelasan Metode
        officer::body_add_par("PENJELASAN METODE", style = "heading 2") %>%
        officer::body_add_par(isolate(output$penjelasan_metode())) %>%
        officer::body_add_par("") %>%
        
        # UJI ASUMSI KLASIK
        officer::body_add_par("UJI ASUMSI KLASIK", style = "heading 2") %>%
        officer::body_add_par("1. Normalitas Residual", style = "heading 3") %>%
        officer::body_add_par(ifelse(is.null(regresi_results$model), "Model belum dijalankan", isolate(output$uji_normalitas_residual()))) %>%
        officer::body_add_par("") %>%
        officer::body_add_par("2. Multikolinearitas", style = "heading 3") %>%
        officer::body_add_par(ifelse(is.null(regresi_results$model), "Model belum dijalankan", isolate(output$uji_multikolinearitas()))) %>%
        officer::body_add_par("") %>%
        officer::body_add_par("3. Heteroskedastisitas", style = "heading 3") %>%
        officer::body_add_par(ifelse(is.null(regresi_results$model), "Model belum dijalankan", isolate(output$uji_heteroskedastisitas()))) %>%
        officer::body_add_par("") %>%
        officer::body_add_par("4. Autokorelasi", style = "heading 3") %>%
        officer::body_add_par(ifelse(is.null(regresi_results$model), "Model belum dijalankan", isolate(output$uji_autokorelasi()))) %>%
        officer::body_add_par("") %>%
        
        # Ringkasan Model
        officer::body_add_par("RINGKASAN MODEL REGRESI", style = "heading 2") %>%
        officer::body_add_par(ifelse(is.null(regresi_results$model), "Model belum dijalankan", 
                                     paste(capture.output(summary(regresi_results$model)), collapse = "\n"))) %>%
        officer::body_add_par("") %>%
        
        # Goodness of Fit
        officer::body_add_par("GOODNESS OF FIT", style = "heading 2") %>%
        officer::body_add_par(isolate(output$goodness_of_fit())) %>%
        officer::body_add_par("") %>%
        
        # Interpretasi dan Kesimpulan
        officer::body_add_par("INTERPRETASI DAN KESIMPULAN", style = "heading 2") %>%
        officer::body_add_par("Model regresi ini digunakan untuk menganalisis faktor-faktor yang mempengaruhi kerentanan sosial dalam konteks mitigasi, evakuasi, atau dampak bencana. Hasil analisis dapat digunakan untuk perencanaan kebijakan yang lebih tepat sasaran dan alokasi sumber daya yang efektif.") %>%
        officer::body_add_par("") %>%
        officer::body_add_par("Pastikan semua asumsi klasik terpenuhi sebelum menggunakan model untuk prediksi atau pengambilan keputusan kebijakan.") %>%
        officer::body_add_par("") %>%
        officer::body_add_par("Dashboard SOVIA-3MED - 2025") %>%
        officer::body_add_par("Politeknik Statistika STIS")
      
      print(doc, target = file)
    }
  )
  
  #============================ DOWNLOAD
  
  # SEBELUM
  output$my_plot <- renderPlot({
    ggplot(data(), aes(x, y)) + geom_point()
  })
  
  # SESUDAH  
  output$my_plot <- renderPlot({
    p <- ggplot(data(), aes(x, y)) + geom_point()
    values$nama_tab_plots[["my_plot"]] <- p  # TAMBAHKAN INI
    return(p)
  })
  
    # === REACTIVE VALUES UNTUK MENYIMPAN OUTPUT ===
    values <- reactiveValues(
      # Beranda
      beranda_plots = list(),
      beranda_tables = list(),
      beranda_summary = NULL,
      
      # Eksplorasi Data  
      eksplorasi_plots = list(),
      eksplorasi_tables = list(),
      eksplorasi_summary = NULL,
      
      # Manajemen Data
      manajemen_data = NULL,
      manajemen_summary = NULL,
      
      # Uji Asumsi
      asumsi_plots = list(),
      asumsi_results = list(),
      asumsi_interpretations = list(),
      
      # Inferensia
      inferensia_plots = list(),
      inferensia_results = list(),
      inferensia_interpretations = list(),
      
      # Regresi
      regresi_plots = list(),
      regresi_results = list(),
      regresi_interpretations = list()
    )
    
    # === EXISTING TAB LOGIC (contoh modifikasi) ===
    
    # BERANDA - modifikasi output existing dengan menyimpan ke values
    output$overview_plot <- renderPlot({
      p <- ggplot(data(), aes(x = variable)) + geom_bar()
      values$beranda_plots[["overview"]] <- p
      return(p)
    })
    
    # EKSPLORASI - modifikasi output existing
    output$histogram_plot <- renderPlot({
      p <- ggplot(data(), aes(x = selected_var)) + geom_histogram()
      values$eksplorasi_plots[["histogram"]] <- p
      return(p)
    })
    
    # ... (lanjutkan untuk semua output existing di tab lain)
    
    #============================ DOWNLOAD HANDLERS ============================
    
    # === HELPER FUNCTIONS ===
    show_progress <- function() {
      if(require(shinyjs)) {
        shinyjs::show("download_progress")
        shinyjs::html("download_status", "Memproses file download...")
      }
    }
    
    hide_progress <- function() {
      if(require(shinyjs)) {
        shinyjs::hide("download_progress")
        shinyjs::html("download_status", "Download selesai!")
      }
    }
    
    # Function untuk save plots
    save_plots <- function(plots_list, temp_dir, prefix = "") {
      if(length(plots_list) > 0) {
        for(i in 1:length(plots_list)) {
          plot_name <- names(plots_list)[i]
          if(is.null(plot_name) || plot_name == "") plot_name <- paste0("plot_", i)
          
          tryCatch({
            ggsave(
              filename = file.path(temp_dir, paste0(prefix, plot_name, ".jpg")),
              plot = plots_list[[i]],
              width = 12, height = 8, dpi = 300
            )
          }, error = function(e) {
            # Jika ggplot gagal, coba save sebagai base plot
            jpeg(file.path(temp_dir, paste0(prefix, plot_name, ".jpg")), 
                 width = 1200, height = 800, res = 300)
            print(plots_list[[i]])
            dev.off()
          })
        }
      }
    }
    
    # Function untuk save tables
    save_tables <- function(tables_list, temp_dir, prefix = "") {
      if(length(tables_list) > 0) {
        for(i in 1:length(tables_list)) {
          table_name <- names(tables_list)[i]
          if(is.null(table_name) || table_name == "") table_name <- paste0("table_", i)
          
          write.csv(tables_list[[i]], 
                    file.path(temp_dir, paste0(prefix, table_name, ".csv")),
                    row.names = FALSE)
        }
      }
    }
    
    # Function untuk save interpretations
    save_interpretations <- function(interp_list, temp_dir, prefix = "") {
      if(length(interp_list) > 0) {
        for(i in 1:length(interp_list)) {
          interp_name <- names(interp_list)[i]
          if(is.null(interp_name) || interp_name == "") interp_name <- paste0("interpretation_", i)
          
          writeLines(interp_list[[i]], 
                     file.path(temp_dir, paste0(prefix, interp_name, ".txt")))
        }
      }
    }
    
    # === DOWNLOAD BERANDA ===
    output$download_beranda <- downloadHandler(
      filename = function() {
        paste0("SOVIA_Beranda_", Sys.Date(), ".zip")
      },
      content = function(file) {
        show_progress()
        
        temp_dir <- tempdir()
        
        # Save plots
        save_plots(values$beranda_plots, temp_dir, "beranda_")
        
        # Save tables
        save_tables(values$beranda_tables, temp_dir, "beranda_")
        
        # Save summary
        if(!is.null(values$beranda_summary)) {
          write.csv(values$beranda_summary, 
                    file.path(temp_dir, "beranda_summary.csv"),
                    row.names = FALSE)
        }
        
        # Create info file
        info_text <- paste(
          "SOVIA-3MED Dashboard - Beranda",
          paste("Tanggal Download:", Sys.time()),
          "Berisi: Overview data dan ringkasan dashboard",
          sep = "\n"
        )
        writeLines(info_text, file.path(temp_dir, "README.txt"))
        
        # Zip files
        files_to_zip <- list.files(temp_dir, full.names = TRUE)
        zip(file, files_to_zip)
        
        hide_progress()
      }
    )
    
    # === DOWNLOAD EKSPLORASI ===
    output$download_eksplorasi <- downloadHandler(
      filename = function() {
        paste0("SOVIA_Eksplorasi_", Sys.Date(), ".zip")
      },
      content = function(file) {
        show_progress()
        
        temp_dir <- tempdir()
        
        # Save plots
        save_plots(values$eksplorasi_plots, temp_dir, "eksplorasi_")
        
        # Save tables
        save_tables(values$eksplorasi_tables, temp_dir, "eksplorasi_")
        
        # Save summary
        if(!is.null(values$eksplorasi_summary)) {
          write.csv(values$eksplorasi_summary, 
                    file.path(temp_dir, "eksplorasi_summary.csv"),
                    row.names = FALSE)
        }
        
        # Create info file
        info_text <- paste(
          "SOVIA-3MED Dashboard - Eksplorasi Data",
          paste("Tanggal Download:", Sys.time()),
          "Berisi: Grafik eksplorasi dan statistik deskriptif",
          sep = "\n"
        )
        writeLines(info_text, file.path(temp_dir, "README.txt"))
        
        # Zip files
        files_to_zip <- list.files(temp_dir, full.names = TRUE)
        zip(file, files_to_zip)
        
        hide_progress()
      }
    )
    
    # === DOWNLOAD MANAJEMEN ===
    output$download_manajemen <- downloadHandler(
      filename = function() {
        paste0("SOVIA_Manajemen_", Sys.Date(), ".zip")
      },
      content = function(file) {
        show_progress()
        
        temp_dir <- tempdir()
        
        # Save processed data
        if(!is.null(values$manajemen_data)) {
          write.csv(values$manajemen_data, 
                    file.path(temp_dir, "data_processed.csv"),
                    row.names = FALSE)
        }
        
        # Save summary
        if(!is.null(values$manajemen_summary)) {
          write.csv(values$manajemen_summary, 
                    file.path(temp_dir, "manajemen_summary.csv"),
                    row.names = FALSE)
        }
        
        # Create info file
        info_text <- paste(
          "SOVIA-3MED Dashboard - Manajemen Data",
          paste("Tanggal Download:", Sys.time()),
          "Berisi: Data yang telah diproses dan dibersihkan",
          sep = "\n"
        )
        writeLines(info_text, file.path(temp_dir, "README.txt"))
        
        # Zip files
        files_to_zip <- list.files(temp_dir, full.names = TRUE)
        zip(file, files_to_zip)
        
        hide_progress()
      }
    )
    
    # === DOWNLOAD UJI ASUMSI ===
    output$download_asumsi <- downloadHandler(
      filename = function() {
        paste0("SOVIA_UjiAsumsi_", Sys.Date(), ".zip")
      },
      content = function(file) {
        show_progress()
        
        temp_dir <- tempdir()
        
        # Save plots
        save_plots(values$asumsi_plots, temp_dir, "asumsi_")
        
        # Save results
        if(length(values$asumsi_results) > 0) {
          for(i in 1:length(values$asumsi_results)) {
            result_name <- names(values$asumsi_results)[i]
            if(is.null(result_name)) result_name <- paste0("result_", i)
            
            # Save as text file
            capture.output(values$asumsi_results[[i]], 
                           file = file.path(temp_dir, paste0("asumsi_", result_name, ".txt")))
          }
        }
        
        # Save interpretations
        save_interpretations(values$asumsi_interpretations, temp_dir, "asumsi_")
        
        # Create info file
        info_text <- paste(
          "SOVIA-3MED Dashboard - Uji Asumsi",
          paste("Tanggal Download:", Sys.time()),
          "Berisi: Hasil uji normalitas, homogenitas, dan asumsi lainnya",
          sep = "\n"
        )
        writeLines(info_text, file.path(temp_dir, "README.txt"))
        
        # Zip files
        files_to_zip <- list.files(temp_dir, full.names = TRUE)
        zip(file, files_to_zip)
        
        hide_progress()
      }
    )
    
    # === DOWNLOAD INFERENSIA ===
    output$download_inferensia <- downloadHandler(
      filename = function() {
        paste0("SOVIA_Inferensia_", Sys.Date(), ".zip")
      },
      content = function(file) {
        show_progress()
        
        temp_dir <- tempdir()
        
        # Save plots
        save_plots(values$inferensia_plots, temp_dir, "inferensia_")
        
        # Save results
        if(length(values$inferensia_results) > 0) {
          for(i in 1:length(values$inferensia_results)) {
            result_name <- names(values$inferensia_results)[i]
            if(is.null(result_name)) result_name <- paste0("result_", i)
            
            capture.output(values$inferensia_results[[i]], 
                           file = file.path(temp_dir, paste0("inferensia_", result_name, ".txt")))
          }
        }
        
        # Save interpretations
        save_interpretations(values$inferensia_interpretations, temp_dir, "inferensia_")
        
        # Create info file
        info_text <- paste(
          "SOVIA-3MED Dashboard - Inferensia",
          paste("Tanggal Download:", Sys.time()),
          "Berisi: Hasil uji hipotesis dan analisis inferensial",
          sep = "\n"
        )
        writeLines(info_text, file.path(temp_dir, "README.txt"))
        
        # Zip files
        files_to_zip <- list.files(temp_dir, full.names = TRUE)
        zip(file, files_to_zip)
        
        hide_progress()
      }
    )
    
    # === DOWNLOAD REGRESI ===
    output$download_regresi <- downloadHandler(
      filename = function() {
        paste0("SOVIA_Regresi_", Sys.Date(), ".zip")
      },
      content = function(file) {
        show_progress()
        
        temp_dir <- tempdir()
        
        # Save plots
        save_plots(values$regresi_plots, temp_dir, "regresi_")
        
        # Save results
        if(length(values$regresi_results) > 0) {
          for(i in 1:length(values$regresi_results)) {
            result_name <- names(values$regresi_results)[i]
            if(is.null(result_name)) result_name <- paste0("result_", i)
            
            capture.output(values$regresi_results[[i]], 
                           file = file.path(temp_dir, paste0("regresi_", result_name, ".txt")))
          }
        }
        
        # Save interpretations
        save_interpretations(values$regresi_interpretations, temp_dir, "regresi_")
        
        # Create info file
        info_text <- paste(
          "SOVIA-3MED Dashboard - Regresi",
          paste("Tanggal Download:", Sys.time()),
          "Berisi: Model regresi dan hasil analisis prediktif",
          sep = "\n"
        )
        writeLines(info_text, file.path(temp_dir, "README.txt"))
        
        # Zip files
        files_to_zip <- list.files(temp_dir, full.names = TRUE)
        zip(file, files_to_zip)
        
        hide_progress()
      }
    )
    
    # === DOWNLOAD SEMUA HALAMAN ===
    output$download_all <- downloadHandler(
      filename = function() {
        paste0("SOVIA_Dashboard_Lengkap_", Sys.Date(), ".zip")
      },
      content = function(file) {
        show_progress()
        
        temp_dir <- tempdir()
        
        # Create subdirectories
        dir.create(file.path(temp_dir, "01_Beranda"), showWarnings = FALSE)
        dir.create(file.path(temp_dir, "02_Eksplorasi"), showWarnings = FALSE)
        dir.create(file.path(temp_dir, "03_Manajemen"), showWarnings = FALSE)
        dir.create(file.path(temp_dir, "04_UjiAsumsi"), showWarnings = FALSE)
        dir.create(file.path(temp_dir, "05_Inferensia"), showWarnings = FALSE)
        dir.create(file.path(temp_dir, "06_Regresi"), showWarnings = FALSE)
        
        # Save all content to respective folders
        save_plots(values$beranda_plots, file.path(temp_dir, "01_Beranda"), "")
        save_tables(values$beranda_tables, file.path(temp_dir, "01_Beranda"), "")
        
        save_plots(values$eksplorasi_plots, file.path(temp_dir, "02_Eksplorasi"), "")
        save_tables(values$eksplorasi_tables, file.path(temp_dir, "02_Eksplorasi"), "")
        
        if(!is.null(values$manajemen_data)) {
          write.csv(values$manajemen_data, 
                    file.path(temp_dir, "03_Manajemen", "data_processed.csv"),
                    row.names = FALSE)
        }
        
        save_plots(values$asumsi_plots, file.path(temp_dir, "04_UjiAsumsi"), "")
        save_interpretations(values$asumsi_interpretations, file.path(temp_dir, "04_UjiAsumsi"), "")
        
        save_plots(values$inferensia_plots, file.path(temp_dir, "05_Inferensia"), "")
        save_interpretations(values$inferensia_interpretations, file.path(temp_dir, "05_Inferensia"), "")
        
        save_plots(values$regresi_plots, file.path(temp_dir, "06_Regresi"), "")
        save_interpretations(values$regresi_interpretations, file.path(temp_dir, "06_Regresi"), "")
        
        # Create main README
        readme_text <- paste(
          "SOVIA-3MED Dashboard - Download Lengkap",
          paste("Tanggal Download:", Sys.time()),
          "",
          "Struktur Folder:",
          "01_Beranda/ - Overview dan ringkasan dashboard",
          "02_Eksplorasi/ - Grafik eksplorasi dan statistik deskriptif", 
          "03_Manajemen/ - Data yang telah diproses",
          "04_UjiAsumsi/ - Hasil uji normalitas dan homogenitas",
          "05_Inferensia/ - Hasil uji hipotesis",
          "06_Regresi/ - Model regresi dan prediksi",
          "",
          "Format File:",
          "- Grafik: JPG (resolusi tinggi)",
          "- Tabel: CSV",
          "- Interpretasi: TXT",
          sep = "\n"
        )
        writeLines(readme_text, file.path(temp_dir, "README.txt"))
        
        # Zip all files
        files_to_zip <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)
        zip(file, files_to_zip)
        
        hide_progress()
      }
    )
    
    # === DOWNLOAD RINGKASAN EKSEKUTIF ===
    output$download_executive <- downloadHandler(
      filename = function() {
        paste0("SOVIA_Ringkasan_Eksekutif_", Sys.Date(), ".zip")
      },
      content = function(file) {
        show_progress()
        
        temp_dir <- tempdir()
        
        # Create executive summary
        executive_summary <- paste(
          "RINGKASAN EKSEKUTIF",
          "DASHBOARD SOVIA-3MED",
          paste("Tanggal:", Sys.Date()),
          "",
          "=== TEMUAN UTAMA ===",
          "1. Analisis Deskriptif:",
          "   - [Ringkasan dari eksplorasi data]",
          "",
          "2. Uji Asumsi:",
          "   - [Ringkasan hasil uji normalitas dan homogenitas]",
          "",
          "3. Analisis Inferensial:",
          "   - [Ringkasan uji hipotesis]",
          "",
          "4. Analisis Regresi:",
          "   - [Ringkasan model prediktif]",
          "",
          "=== REKOMENDASI ===",
          "1. [Rekomendasi berdasarkan temuan]",
          "2. [Saran tindak lanjut]",
          "",
          "=== CATATAN METODOLOGI ===",
          "- Data dianalisis menggunakan R Shiny Dashboard",
          "- Semua uji statistik menggunakan tingkat signifikansi α = 0.05",
          "- Visualisasi dibuat dengan ggplot2",
          sep = "\n"
        )
        
        writeLines(executive_summary, file.path(temp_dir, "Ringkasan_Eksekutif.txt"))
        
        # Save key plots only (1-2 per section)
        if(length(values$eksplorasi_plots) > 0) {
          key_plot <- values$eksplorasi_plots[[1]]
          ggsave(file.path(temp_dir, "Key_Eksplorasi_Plot.jpg"), 
                 plot = key_plot, width = 10, height = 6, dpi = 300)
        }
        
        if(length(values$regresi_plots) > 0) {
          key_plot <- values$regresi_plots[[1]]
          ggsave(file.path(temp_dir, "Key_Regresi_Plot.jpg"), 
                 plot = key_plot, width = 10, height = 6, dpi = 300)
        }
        
        # Zip files
        files_to_zip <- list.files(temp_dir, full.names = TRUE)
        zip(file, files_to_zip)
        
        hide_progress()
      }
    )
    
   # End of server function
}


# Jalankan aplikasi
shinyApp(ui = ui, server = server)
