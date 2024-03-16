# Paket ----
library(shiny)
library(bslib)
library(tidyverse)
options(shiny.mathjax.url = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js")

# Tautan ----
tautan_apl_lain <- tags$a(
  shiny::icon("shapes"),
  "Lainnya",
  href = "https://people.usd.ac.id/~ydkristanto/index.php/media-pengajaran/",
  target = "_blank"
)
tautan_github <- tags$a(
  shiny::icon("github"),
  "Github",
  href = "https://github.com/ydkristanto/apl-simulasi-tabungan",
  target = "_blank"
)

# Antarmuka ----
ui <- page_navbar(
  title = "Simulasi Tabungan",
  id = "simulasi_tabungan",
  ## Sidebar ----
  sidebar = sidebar(
    ### Sidebar deposito ----
    conditionalPanel(
      "input.simulasi_tabungan === 'Deposito'",
      selectInput(
        "jenis_simulasi_dep",
        div("Simulasi:", style = "font-weight: bold;"),
        choices = c(
          "Setoran Awal" = "setor_awal_dep",
          "Periode" = "periode_dep",
          "Bunga Per Tahun" = "bunga_dep",
          "Total Deposito" = "total_dep"
        ),
        selected = "total_dep"
      ),
      selectInput(
        "hitung_bunga_dep",
        div("Penghitungan Bunga", style = "font-weight: bold;"),
        choices = c(
          "Tahunan" = "1",
          "Semesteran" = "2",
          "Caturwulanan" = "3",
          "Triwulanan" = "4",
          "Dua bulanan" = "6",
          "Bulanan" = "12",
          "Kontinu" = "kontinu"
        ),
        selected = "12"
      ),
      conditionalPanel(
        "input.jenis_simulasi_dep != 'setor_awal_dep'",
        sliderInput(
          "setor_awal_dep",
          div("Setoran Awal (Rp)", style = "font-weight: bold;"),
          min = 1000000,
          max = 100000000,
          value = 10000000,
          step = 100000,
          ticks = FALSE
        )
      ),
      conditionalPanel(
        "input.jenis_simulasi_dep != 'periode_dep'",
        sliderInput(
          "periode_dep",
          div("Periode (Tahun)", style = "font-weight: bold;"),
          min = 0,
          max = 50,
          value = 10,
          step = 1,
          ticks = FALSE
        )
      ),
      conditionalPanel(
        "input.jenis_simulasi_dep != 'bunga_dep'",
        sliderInput(
          "bunga_dep",
          div("Bunga Per Tahun (%)", style = "font-weight: bold;"),
          min = 0,
          max = 12,
          value = 4,
          step = .1,
          ticks = FALSE,
          sep = "."
        )
      ),
      conditionalPanel(
        "input.jenis_simulasi_dep != 'total_dep'",
        sliderInput(
          "total_dep",
          div("Deposito Akhir", style = "font-weight: bold;"),
          min = 10000000,
          max = 100000000,
          value = 50000000,
          step = 1000000,
          ticks = FALSE,
          sep = "."
        )
      ),
      p(
        actionButton(
          "simpan_dep", "Simpan",
          width = "100%"
        )
      ),
      p(
        actionButton(
          "hapus_dep", "Hapus",
          width = "100%"
        )
      )
    ),
    ### Sidebar tabungan berjangka ----
    conditionalPanel(
      "input.simulasi_tabungan === 'Tabungan Berjangka'",
      selectInput(
        "jenis_simulasi",
        div("Simulasi:", style = "font-weight: bold;"),
        choices = c(
          "Setoran Bulanan" = "setor_bulanan",
          "Periode" = "periode",
          "Total Tabungan" = "total"
        ),
        selected = "total"
      ),
      conditionalPanel(
        "input.jenis_simulasi != 'setor_bulanan'",
        sliderInput(
          "setor_bulanan",
          div("Setoran Bulanan (Rp)", style = "font-weight: bold;"),
          min = 100000,
          max = 10000000,
          value = 500000,
          step = 10000,
          ticks = FALSE,
          sep = "."
        )
      ),
      conditionalPanel(
        "input.jenis_simulasi != 'periode'",
        sliderInput(
          "periode",
          div("Periode (Bulan)", style = "font-weight: bold;"),
          min = 0,
          max = 60,
          value = 24,
          step = 1,
          ticks = FALSE
        )
      ),
      sliderInput(
        "bunga",
        div("Bunga Per Tahun (%)", style = "font-weight: bold;"),
        min = 0,
        max = 12,
        value = 2.5,
        step = .1,
        ticks = FALSE,
        sep = "."
      ),
      conditionalPanel(
        "input.jenis_simulasi != 'total'",
        sliderInput(
          "total",
          div("Total Tabungan", style = "font-weight: bold;"),
          min = 10000000,
          max = 100000000,
          value = 50000000,
          step = 1000000,
          ticks = FALSE,
          sep = "."
        )
      ),
      p(
        actionButton(
          "simpan", "Simpan",
          width = "100%"
        )
      ),
      p(
        actionButton(
          "hapus", "Hapus",
          width = "100%"
        )
      )
    ),
    ### Sidebar informasi ----
    conditionalPanel(
      "input.simulasi_tabungan === 'Informasi'",
      h4(
        "Deskripsi",
        style = "font-weight: bold; font-size: inherit;"
      ),
      p(
        "Aplikasi Shiny ini mensimulasikan penghitungan deposito dan tabungan berjangka."
      ),
      hr(),
      h4(
        "Lisensi MIT",
        style = "font-weight: bold; font-size: inherit;"
      ),
      p(
        "Copyright © 2024 Yosep Dwi Kristanto"
      )
    )
  ),
  ## Konten utama ----
  ### Deposito ----
  nav_panel(
    "Deposito",
    layout_columns(
      uiOutput(
        "kotak_setoran_awal_dep",
        fill = TRUE
      ),
      uiOutput(
        "kotak_periode_dep",
        fill = TRUE
      ),
      uiOutput(
        "kotak_bunga_dep",
        fill = TRUE
      ),
      uiOutput(
        "kotak_total_dep",
        fill = TRUE
      ),
      navset_card_underline(
        nav_panel(
          "Tabel",
          tableOutput("tabel_data_dep")
        ),
        nav_panel(
          title = div(
            "Grafik  ",
            popover(
              trigger = icon("gear"),
              title = "Pengaturan Tampilan",
              selectInput(
                "sumbu_x_dep",
                "Sumbu x:",
                choices = c(
                  "Penghitungan Bunga",
                  "Setoran Awal (Rp)",
                  "Periode (Tahun)",
                  "Bunga Per Tahun (%)",
                  "Total (Rp)"
                ),
                selected = "Periode (Tahun)"
              ),
              selectInput(
                "sumbu_y_dep",
                "Sumbu y:",
                choices = c(
                  "Penghitungan Bunga",
                  "Setoran Awal (Rp)",
                  "Periode (Tahun)",
                  "Bunga Per Tahun (%)",
                  "Total (Rp)"
                ),
                selected = "Total (Rp)"
              ),
              hr(),
              selectInput(
                "warna_dep",
                "Warna:",
                choices = c(
                  "-",
                  "Penghitungan Bunga",
                  "Setoran Awal (Rp)",
                  "Periode (Tahun)",
                  "Bunga Per Tahun (%)",
                  "Total (Rp)"
                ),
                selected = "-"
              ),
              selectInput(
                "facet_dep",
                "Sisi:",
                choices = c(
                  "-",
                  "Penghitungan Bunga",
                  "Setoran Awal (Rp)",
                  "Periode (Tahun)",
                  "Bunga Per Tahun (%)",
                  "Total (Rp)"
                ),
                selected = "-"
              ),
              placement = c("auto")
            )
          ),
          plotOutput(
            "grafik_data_dep",
            fill = TRUE
          )
        ),
        full_screen = TRUE
      ),
      col_widths = c(3, 3, 3, 3, 12),
      row_heights = c(1, 3)
    )
  ),
  ### Tabungan berjangka ----
  nav_panel(
    "Tabungan Berjangka",
    layout_columns(
      uiOutput(
        "kotak_setoran",
        fill = TRUE
      ),
      uiOutput(
        "kotak_periode",
        fill = TRUE
      ),
      uiOutput(
        "kotak_bunga",
        fill = TRUE
      ),
      uiOutput(
        "kotak_total",
        fill = TRUE
      ),
      navset_card_underline(
        nav_panel(
          "Tabel",
          tableOutput("tabel_data")
        ),
        nav_panel(
          title = div(
            "Grafik  ",
            popover(
              trigger = icon("gear"),
              title = "Pengaturan Tampilan",
              selectInput(
                "sumbu_x",
                "Sumbu x:",
                choices = c(
                  "Setoran Bulanan (Rp)",
                  "Periode (Bulan)",
                  "Bunga Per Tahun (%)",
                  "Total (Rp)"
                ),
                selected = "Periode (Bulan)"
              ),
              selectInput(
                "sumbu_y",
                "Sumbu y:",
                choices = c(
                  "Setoran Bulanan (Rp)",
                  "Periode (Bulan)",
                  "Bunga Per Tahun (%)",
                  "Total (Rp)"
                ),
                selected = "Total (Rp)"
              ),
              hr(),
              selectInput(
                "warna",
                "Warna:",
                choices = c(
                  "-",
                  "Setoran Bulanan (Rp)",
                  "Periode (Bulan)",
                  "Bunga Per Tahun (%)",
                  "Total (Rp)"
                ),
                selected = "-"
              ),
              selectInput(
                "facet",
                "Sisi:",
                choices = c(
                  "-",
                  "Setoran Bulanan (Rp)",
                  "Periode (Bulan)",
                  "Bunga Per Tahun (%)",
                  "Total (Rp)"
                ),
                selected = "-"
              ),
              placement = c("auto")
            )
          ),
          plotOutput(
            "grafik_data",
            fill = TRUE
          )
        ),
        full_screen = TRUE
      ),
      col_widths = c(3, 3, 3, 3, 12),
      row_heights = c(1, 3)
    )
  ),
  ### Informasi ----
  nav_panel(
    "Informasi",
    layout_column_wrap(
      width = 1 / 2,
      navset_card_underline(
        #### Tentang ----
        nav_panel(
          title = "Tentang",
          p("Matematika yang terlibat dalam permasalahan deposito dan tabungan berjangka merupakan matematika yang cukup kompleks bagi sebagian besar peserta didik. Aplikasi berbasis web ini menyediakan sarana bagi peserta didik untuk melakukan simulasi terhadap penghitungan deposito dan tabungan berjangka. Dengan demikian, peserta didik diharapkan dapat menemukan ide elementer dari permasalahan deposito dan tabungan berjangka."),
          p("Bagi pendidik, aplikasi ini dapat digunakan untuk memfasilitasi peserta didiknya bermatematika. Aplikasi ini cocok untuk pembelajaran klasikal, penyelidikan dalam kelompok kecil maupun individu. Sebagai saran, pendidik dapat menyiapkan beberapa masalah tentang deposito atau tabungan berjangka sebagai panduan peserta didik untuk melakukan penyelidikan matematis.")
        ),
        nav_panel(
          #### Alat ----
          title = "Alat",
          p("Aplikasi ini dikembangkan dengan menggunakan bahasa pemrograman", a("R", href = "https://www.R-project.org/", target = "_blank"), "dan paket", a("Shiny.", href = "https://CRAN.R-project.org/package=shiny", target = "_blank"), "Paket", a("shinylive", href = "https://posit-dev.github.io/r-shinylive/", target = "_blank"), "digunakan untuk mengekspor aplikasi ini agar dapat dijalankan di peramban web tanpa peladen R yang terpisah. Tata letak dasbor ini diatur dengan menggunakan ", a("bslib.", href = "https://CRAN.R-project.org/package=bslib", target = "_blank"))
        ),
        nav_panel(
          #### Pengembang ----
          title = "Pengembang",
          p("Pengembang dan pemelihara aplikasi ini adalah", a("Yosep Dwi Kristanto,", href = "https://people.usd.ac.id/~ydkristanto/", target = "_blank"), "seorang dosen dan peneliti di program studi", a("Pendidikan Matematika,", href = "https://usd.ac.id/s1pmat", target = "_blank"), a("Universitas Sanata Dharma,", href = "https://www.usd.ac.id/", target = "_blank"), "Yogyakarta.")
        ),
        nav_panel(
          #### Kode Sumber ----
          title = "Kode Sumber",
          p("Kode sumber aplikasi ini tersedia di", a("repositori Github.", href = "https://github.com/ydkristanto/apl-simulasi-tabungan", target = "_blank"), "Jika Anda ingin melaporkan masalah atau meminta fitur tambahan terhadap aplikasi ini, silakan", a("buat sebuah isu", href = "https://github.com/ydkristanto/apl-simulasi-tabungan/issues", target = "_blank"), "atau lebih baik lagi", a("minta penarikan", href = "https://github.com/ydkristanto/apl-simulasi-tabungan/pulls", target = "_blank"), "di repositori tersebut.")
        )
      ),
      #### Matematika ----
      navset_card_underline(
        nav_panel(
          title = "Deposito",
          withMathJax(),
          p("Deposito merupakan tabungan/simpanan yang pencairannya hanya dapat dilakukan pada jangka waktu tertentu. Bunga yang diterima dalam deposito tersebut merupakan bunga majemuk dan penghitungannya dilakukan setiap periode tertentu (umumnya tiap akhir bulan atau 12 kali dalam setahun)."),
          p("Misalkan seorang nasabah memberikan setoran awal sejumlah \\(P\\) kepada bank yang memberikan bunga \\(r\\) per tahunnya dan dihitung \\(n\\) kali setiap tahunnya. Dalam jangka waktu \\(t\\) tahun, jumlah tabungannya dapat dihitung dengan menggunakan rumus berikut."),
          p("$$A(t)=P\\left(1+\\frac{r}{n}\\right)^{nt}$$"),
          p("Ketika penghitungan bunga dilakukan secara kontinu (\\(n\\) mendekati tak hingga), rumusnya menjadi seperti berikut."),
          p("$$A(t)=Pe^{rt}$$dengan \\(e\\) adalah sebuah konstanta matematis yang nilainya kurang lebih sama dengan 2,718282.")
        ),
        nav_panel(
          title = "Tabungan Berjangka",
          p("Tabungan berjangka merupakan tabungan/simpanan yang memungkinkan nasabah menabung secara rutin dalam jangka waktu tertentu. Tabungan berjangka ini merupakan bagian dari ", a("anuitas.", href = "https://kbbi.kemdikbud.go.id/entri/anuitas", target = "_blank")),
          p("Misalnya seorang nasabah melakukan setoran sejumlah \\(R\\) secara rutin sebanyak \\(n\\) kali kepada bank yang memberikan bunga \\(i\\) per periode waktu tertentu. Jumlah tabungan akhirnya dapat ditentukan dengan rumus berikut."),
          p("$$A_f=R\\frac{(1+i)^n-1}{i}$$"),
          p("Dalam aplikasi ini, tabungan berjangka tersebut menganggap bahwa nasabah melakukan setoran dengan besaran yang sama di setiap akhir bulannya. Setoran pertama dilakukan pada bulan pertama. Setoran akhir dilakukan pada tanggal yang sama dengan tanggal berakhirnya tabungan berjangka tersebut.")
        )
      )
    )
  ),
  nav_spacer(),
  nav_menu(
    title = "Tautan",
    nav_item(tautan_apl_lain),
    nav_item(tautan_github),
    icon = shiny::icon("link"),
    align = "right"
  ),
  tags$head(
    HTML('
      <!-- Google tag (gtag.js) -->
      <script async src="https://www.googletagmanager.com/gtag/js?id=G-XWLYNEVE4J"></script>
    '),
    HTML("
      <script>
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag('js', new Date());
      
        gtag('config', 'G-XWLYNEVE4J');
      </script>
    ")
  )
)

# Peladen ----
server <- function(input, output, session) {
  ## Deposito ----
  ### kotak_setoran_awal_dep ----
  output$kotak_setoran_awal_dep <- renderUI({
    jenis_simulasi <- input$jenis_simulasi_dep
    n <- ifelse(
      input$hitung_bunga_dep != "kontinu",
      as.numeric(input$hitung_bunga_dep),
      input$hitung_bunga_dep
    )
    P <- input$setor_awal_dep
    t <- input$periode_dep
    r <- input$bunga_dep / 100
    A <- input$total_dep

    warna_latar <- if (jenis_simulasi == "setor_awal_dep") {
      "primary"
    } else {
      "secondary"
    }

    if (jenis_simulasi == "setor_awal_dep") {
      nilai_setor_awal <- ifelse(
        n != "kontinu",
        A * (1 + r / n)^(-n * t),
        A * exp(1)^(-r * t)
      )
    } else {
      nilai_setor_awal <- P
    }

    if (nilai_setor_awal < 1e+3) {
      setor_awal_simpel <- round(nilai_setor_awal, 2)
    } else if (nilai_setor_awal >= 1e+3 & nilai_setor_awal < 1e+6) {
      setor_awal_simpel <- round(nilai_setor_awal / 1e+3, 2)
    } else {
      setor_awal_simpel <- round(nilai_setor_awal / 1e+6, 2)
    }

    if (nilai_setor_awal < 1e+3) {
      satuan <- "rupiah"
    } else if (nilai_setor_awal >= 1e+3 & nilai_setor_awal < 1e+6) {
      satuan <- "ribu rupiah"
    } else {
      satuan <- "juta rupiah"
    }

    value_box(
      title = "Setoran Awal",
      value = format(
        setor_awal_simpel,
        big.mark = ".",
        decimal.mark = ","
      ),
      satuan,
      theme = warna_latar
    )
  })
  ### kotak_periode_dep ----
  output$kotak_periode_dep <- renderUI({
    jenis_simulasi <- input$jenis_simulasi_dep
    n <- ifelse(
      input$hitung_bunga_dep != "kontinu",
      as.numeric(input$hitung_bunga_dep),
      input$hitung_bunga_dep
    )
    P <- input$setor_awal_dep
    t <- input$periode_dep
    r <- input$bunga_dep / 100
    A <- input$total_dep

    if (jenis_simulasi == "periode_dep") {
      warna_latar <- "primary"
    } else {
      warna_latar <- "secondary"
    }

    if (jenis_simulasi == "periode_dep") {
      nilai_periode <- ifelse(
        n != "kontinu",
        round(log(A / P) / (n * log(1 + r / n)), 1),
        log(A / P) / r
      )
    } else {
      nilai_periode <- t
    }

    value_box(
      title = "Periode",
      value = formatC(
        nilai_periode,
        decimal.mark = ",",
        big.mark = "."
      ),
      "tahun",
      theme = warna_latar
    )
  })
  ### kotak_bunga_dep ----
  output$kotak_bunga_dep <- renderUI({
    jenis_simulasi <- input$jenis_simulasi_dep
    n <- ifelse(
      input$hitung_bunga_dep != "kontinu",
      as.numeric(input$hitung_bunga_dep),
      input$hitung_bunga_dep
    )
    P <- input$setor_awal_dep
    t <- input$periode_dep
    r <- input$bunga_dep / 100
    A <- input$total_dep

    if (jenis_simulasi == "bunga_dep") {
      warna_latar <- "primary"
    } else {
      warna_latar <- "secondary"
    }

    if (jenis_simulasi == "bunga_dep") {
      nilai_bunga <- ifelse(
        n != "kontinu",
        round(100 * n * ((A / P)^(1 / (n * t)) - 1), 2),
        round(100 * log(A / P) / t, 2)
      )
    } else {
      nilai_bunga <- 100 * r
    }

    value_box(
      title = "Bunga Per Tahun",
      value = formatC(
        nilai_bunga,
        decimal.mark = ",",
        big.mark = "."
      ),
      "persen (%)",
      theme = warna_latar
    )
  })
  ### kotak_total_dep ----
  output$kotak_total_dep <- renderUI({
    jenis_simulasi <- input$jenis_simulasi_dep
    n <- ifelse(
      input$hitung_bunga_dep != "kontinu",
      as.numeric(input$hitung_bunga_dep),
      input$hitung_bunga_dep
    )
    P <- input$setor_awal_dep
    t <- input$periode_dep
    r <- input$bunga_dep / 100
    A <- input$total_dep

    if (jenis_simulasi == "total_dep") {
      warna_latar <- "primary"
    } else {
      warna_latar <- "secondary"
    }
    if (jenis_simulasi == "total_dep") {
      nilai_total <- ifelse(
        n != "kontinu",
        P * (1 + r / n)^(n * t),
        P * exp(1)^(r * t)
      )
    } else {
      nilai_total <- A
    }
    total_simpel <- if (nilai_total < 1e+3) {
      round(nilai_total, 2)
    } else if (nilai_total >= 1e+3 & nilai_total < 1e+6) {
      round(nilai_total / 1e+3, 4)
    } else if (nilai_total >= 1e+6 & nilai_total < 1e+9) {
      round(nilai_total / 1e+6, 4)
    } else if (nilai_total >= 1e+9 & nilai_total < 1e+12) {
      round(nilai_total / 1e+9, 4)
    } else if (nilai_total >= 1e+12 & nilai_total < 1e+15) {
      round(nilai_total / 1e+12, 4)
    } else {
      formatC(
        nilai_total,
        format = "e",
        digits = 2
      ) %>% as.numeric()
    }
    satuan <- if (nilai_total < 1e+3) {
      "rupiah"
    } else if (nilai_total >= 1e+3 & nilai_total < 1e+6) {
      "ribu rupiah"
    } else if (nilai_total >= 1e+6 & nilai_total < 1e+9) {
      "juta rupiah"
    } else if (nilai_total >= 1e+9 & nilai_total < 1e+12) {
      "miliar rupiah"
    } else if (nilai_total >= 1e+12 & nilai_total < 1e+15) {
      "triliun rupiah"
    } else {
      "rupiah"
    }

    value_box(
      title = "Total Deposito",
      value = formatC(
        total_simpel,
        decimal.mark = ",",
        big.mark = "."
      ),
      satuan,
      theme = warna_latar
    )
  })
  ### Awal tabel_data_dep ----
  tabel_data_dep <- reactiveVal(
    tibble(
      `Penghitungan Bunga` = character(),
      `Setoran Awal (Rp)` = numeric(),
      `Periode (Tahun)` = numeric(),
      `Bunga Per Tahun (%)` = numeric(),
      `Total (Rp)` = numeric()
    )
  )
  ### Membuat baris tabel ----
  observeEvent(input$simpan_dep, {
    jenis_simulasi <- input$jenis_simulasi_dep
    hitung_bunga <- input$hitung_bunga_dep
    hitungan_bunga <- ifelse(
      hitung_bunga == "1", "Tahunan",
      ifelse(
        hitung_bunga == "2", "Semesteran",
        ifelse(
          hitung_bunga == "3", "Caturwulanan",
          ifelse(
            hitung_bunga == "4", "Triwulanan",
            ifelse(
              hitung_bunga == "6", "Dua bulanan",
              ifelse(
                hitung_bunga == "12", "Bulanan",
                "Kontinu"
              )
            )
          )
        )
      )
    )
    n <- ifelse(
      hitung_bunga != "kontinu",
      as.numeric(hitung_bunga),
      hitung_bunga
    )
    P <- input$setor_awal_dep
    t <- input$periode_dep
    r <- input$bunga_dep / 100
    A <- input$total_dep

    if (jenis_simulasi == "periode_dep") {
      nilai_periode <- ifelse(
        n != "kontinu",
        round(log(A / P) / (n * log(1 + r / n)), 1),
        log(A / P) / r
      )
      nilai_setor_awal <- P
      nilai_bunga <- 100 * r
      nilai_total <- A
    } else if (jenis_simulasi == "setor_awal_dep") {
      nilai_setor_awal <- nilai_setor_awal <- ifelse(
        n != "kontinu",
        round(A * (1 + r / n)^(-n * t), 2),
        round(A * exp(1)^(-r * t), 2)
      )
      nilai_periode <- t
      nilai_bunga <- 100 * r
      nilai_total <- A
    } else if (jenis_simulasi == "bunga_dep") {
      nilai_bunga <- ifelse(
        n != "kontinu",
        round(100 * n * ((A / P)^(1 / (n * t)) - 1), 2),
        round(100 * log(A / P) / t, 2)
      )
      nilai_periode <- t
      nilai_setor_awal <- R
      nilai_total <- A
    } else {
      nilai_total <- ifelse(
        n != "kontinu",
        round(P * (1 + r / n)^(n * t), 2),
        round(P * exp(1)^(r * t), 2)
      )
      nilai_bunga <- 100 * r
      nilai_periode <- t
      nilai_setor_awal <- P
    }

    tabel_data_dep() %>%
      add_row(
        `Penghitungan Bunga` = hitungan_bunga,
        `Setoran Awal (Rp)` = nilai_setor_awal,
        `Periode (Tahun)` = nilai_periode,
        `Bunga Per Tahun (%)` = nilai_bunga,
        `Total (Rp)` = nilai_total
      ) %>%
      tabel_data_dep()
  })

  ### Menghapus semua baris tabel ----
  observeEvent(input$hapus_dep, {
    tabel_data_dep(
      tibble(
        `Penghitungan Bunga` = character(),
        `Setoran Awal (Rp)` = numeric(),
        `Periode (Tahun)` = numeric(),
        `Bunga Per Tahun (%)` = numeric(),
        `Total (Rp)` = numeric()
      )
    )
  })

  ### Luaran tabel_data_dep ----
  output$tabel_data_dep <- renderTable(
    tabel_data_dep() %>%
      mutate(
        `Penghitungan Bunga` = as.factor(`Penghitungan Bunga`),
        `Setoran Awal (Rp)` = formatC(
          `Setoran Awal (Rp)`,
          format = "f",
          digits = 2,
          big.mark = ".",
          decimal.mark = ","
        ),
        `Periode (Tahun)` = formatC(
          `Periode (Tahun)`,
          format = "f",
          digits = 0,
          big.mark = ".",
          decimal.mark = ","
        ),
        `Bunga Per Tahun (%)` = formatC(
          `Bunga Per Tahun (%)`,
          format = "f",
          digits = 1,
          big.mark = ".",
          decimal.mark = ","
        ),
        `Total (Rp)` = formatC(
          `Total (Rp)`,
          format = "f",
          digits = 2,
          big.mark = ".",
          decimal.mark = ","
        )
      ),
    striped = TRUE,
    hover = TRUE,
    rownames = TRUE,
    align = "r",
    width = "100%"
  )
  ### grafik_data_dep ----
  output$grafik_data_dep <- renderPlot({
    x <- as.character(input$sumbu_x_dep)
    y <- as.character(input$sumbu_y_dep)
    warna <- as.character(input$warna_dep)
    facet <- as.character(input$facet_dep)
    
    plot_awal <- tabel_data_dep() %>%
      ggplot(aes(.data[[x]], .data[[y]])) +
      theme_bw(base_size = 16) +
      labs(
        title = paste0("Hubungan Antara ", x, " dan ", y)
      ) +
      theme(
        plot.title = element_text(
          face = "bold",
          hjust = .5
        )
      )
    plot_baru <- if (warna == "-" & facet == "-") {
      plot_awal +
        geom_point(
          color = "#386cb0",
          size = 5
        )
    } else if (warna != "-" & warna != "Penghitungan Bunga" & facet == "-") {
      plot_awal +
        geom_point(
          aes(color = .data[[warna]]),
          size = 5
        ) +
        scale_color_viridis_b() +
        theme(
          legend.position = "right"
        )
    } else if (warna == "Penghitungan Bunga" & facet == "-") {
      plot_awal +
        geom_point(
          aes(color = .data[[warna]]),
          size = 5
        ) +
        scale_color_viridis_d() +
        theme(
          legend.position = "right"
        )
    } else if (warna == "-" & facet != "-") {
      plot_awal +
        geom_point(
          color = "#386cb0",
          size = 5
        ) +
        facet_grid(
          ~factor(.data[[facet]])
        )
    } else if (warna == "Penghitungan Bunga" & facet != "-") {
      plot_awal +
        geom_point(
          aes(
            color = .data[[warna]]
          ),
          size = 5
        ) +
        scale_color_viridis_d() +
        facet_grid(
          ~factor(.data[[facet]])
        ) +
        theme(
          legend.position = "bottom"
        )
    } else {
      plot_awal +
        geom_point(
          aes(
            color = .data[[warna]]
          ),
          size = 5
        ) +
        scale_color_viridis_b() +
        facet_grid(
          ~factor(.data[[facet]])
        ) +
        theme(
          legend.position = "bottom"
        )
    }
    
    plot_baru
  })

  ## Tabungan berjangka ----
  ### kotak_periode ----
  output$kotak_periode <- renderUI({
    jenis_simulasi <- input$jenis_simulasi
    R <- input$setor_bulanan
    n <- input$periode
    i <- input$bunga / (12 * 100)
    A <- input$total
    if (jenis_simulasi == "periode") {
      warna_latar <- "primary"
    } else {
      warna_latar <- "secondary"
    }
    if (jenis_simulasi == "periode") {
      nilai_periode <- ifelse(
        i != 0,
        round(log((A * i + R) / R, base = 1 + i), 1),
        round(A / R, 1)
      )
    } else {
      nilai_periode <- n
    }

    value_box(
      title = "Periode",
      value = formatC(
        nilai_periode,
        decimal.mark = ",",
        big.mark = "."
      ),
      "bulan",
      theme = warna_latar
    )
  })
  ### kotak_setoran ----
  output$kotak_setoran <- renderUI({
    jenis_simulasi <- input$jenis_simulasi
    R <- input$setor_bulanan
    n <- input$periode
    i <- input$bunga / 12
    A <- input$total
    warna_latar <- if (jenis_simulasi == "setor_bulanan") {
      "primary"
    } else {
      "secondary"
    }
    if (jenis_simulasi == "setor_bulanan") {
      nilai_setoran <- ifelse(
        i != 0,
        A * i / ((1 + i)^n - 1),
        A / n
      )
    } else {
      nilai_setoran <- R
    }
    setoran_simpel <- if (nilai_setoran < 1e+3) {
      round(nilai_setoran, 2)
    } else if (nilai_setoran >= 1e+3 & nilai_setoran < 1e+6) {
      round(nilai_setoran / 1e+3, 2)
    } else {
      round(nilai_setoran / 1e+6, 2)
    }
    satuan <- if (nilai_setoran < 1e+3) {
      "rupiah"
    } else if (nilai_setoran >= 1e+3 & nilai_setoran < 1e+6) {
      "ribu rupiah"
    } else {
      "juta rupiah"
    }

    value_box(
      title = "Setoran Bulanan",
      value = format(
        setoran_simpel,
        big.mark = ".",
        decimal.mark = ","
      ),
      satuan,
      theme = warna_latar
    )
  })
  ### kotak_bunga ----
  output$kotak_bunga <- renderUI({
    value_box(
      title = "Bunga Per Tahun",
      value = formatC(
        input$bunga,
        decimal.mark = ",",
        big.mark = "."
      ),
      "persen (%)",
      theme = "secondary"
    )
  })
  ### kotak_total ----
  output$kotak_total <- renderUI({
    jenis_simulasi <- input$jenis_simulasi
    R <- input$setor_bulanan
    n <- input$periode
    i <- input$bunga / (100 * 12)
    A <- input$total
    warna_latar <- if (jenis_simulasi == "total") {
      "primary"
    } else {
      "secondary"
    }
    if (jenis_simulasi == "total") {
      nilai_total <- ifelse(
        i != 0,
        R * ((1 + i)^n - 1) / i,
        n * R
      )
    } else {
      nilai_total <- A
    }
    total_simpel <- if (nilai_total < 1e+3) {
      round(nilai_total, 2)
    } else if (nilai_total >= 1e+3 & nilai_total < 1e+6) {
      round(nilai_total / 1e+3, 4)
    } else if (nilai_total >= 1e+6 & nilai_total < 1e+9) {
      round(nilai_total / 1e+6, 4)
    } else if (nilai_total >= 1e+9 & nilai_total < 1e+12) {
      round(nilai_total / 1e+9, 4)
    } else if (nilai_total >= 1e+12 & nilai_total < 1e+15) {
      round(nilai_total / 1e+12, 4)
    } else {
      formatC(
        nilai_total,
        format = "e",
        digits = 2
      ) %>% as.numeric()
    }
    satuan <- if (nilai_total < 1e+3) {
      "rupiah"
    } else if (nilai_total >= 1e+3 & nilai_total < 1e+6) {
      "ribu rupiah"
    } else if (nilai_total >= 1e+6 & nilai_total < 1e+9) {
      "juta rupiah"
    } else if (nilai_total >= 1e+9 & nilai_total < 1e+12) {
      "miliar rupiah"
    } else if (nilai_total >= 1e+12 & nilai_total < 1e+15) {
      "triliun rupiah"
    } else {
      "rupiah"
    }

    value_box(
      title = "Total Tabungan",
      value = formatC(
        total_simpel,
        decimal.mark = ",",
        big.mark = "."
      ),
      satuan,
      theme = warna_latar
    )
  })
  ### Awal tabel_data ----
  tabel_data <- reactiveVal(
    tibble(
      `Setoran Bulanan (Rp)` = numeric(),
      `Periode (Bulan)` = numeric(),
      `Bunga Per Tahun (%)` = numeric(),
      `Total (Rp)` = numeric()
    )
  )

  ### Membuat baris tabel ----
  observeEvent(input$simpan, {
    jenis_simulasi <- input$jenis_simulasi
    R <- input$setor_bulanan
    n <- input$periode
    i <- input$bunga / (12 * 100)
    A <- input$total
    if (jenis_simulasi == "periode") {
      nilai_periode <- ifelse(
        i != 0,
        ceiling(log((A * i + R) / R, base = 1 + i)),
        ceiling(A / R)
      )
      nilai_setoran <- R
      nilai_bunga <- 1200 * i
      nilai_total <- A
    } else if (jenis_simulasi == "setor_bulanan") {
      nilai_setoran <- ifelse(
        i != 0,
        round(A * i / ((1 + i)^n - 1), 2),
        round(A / n, 2)
      )
      nilai_periode <- n
      nilai_bunga <- 1200 * i
      nilai_total <- A
    } else {
      nilai_total <- ifelse(
        i != 0,
        round(R * ((1 + i)^n - 1) / i, 2),
        n * R
      )
      nilai_periode <- n
      nilai_setoran <- R
      nilai_bunga <- 1200 * i
    }
    tabel_data() %>%
      add_row(
        `Setoran Bulanan (Rp)` = nilai_setoran,
        `Periode (Bulan)` = nilai_periode,
        `Bunga Per Tahun (%)` = nilai_bunga,
        `Total (Rp)` = nilai_total
      ) %>%
      tabel_data()
  })

  ### Menghapus semua baris tabel ----
  observeEvent(input$hapus, {
    tabel_data(
      tibble(
        `Setoran Bulanan (Rp)` = numeric(),
        `Periode (Bulan)` = numeric(),
        `Bunga Per Tahun (%)` = numeric(),
        `Total (Rp)` = numeric()
      )
    )
  })

  ### Luaran tabel_data ----
  output$tabel_data <- renderTable(
    tabel_data() %>%
      mutate(
        `Setoran Bulanan (Rp)` = formatC(
          `Setoran Bulanan (Rp)`,
          format = "f",
          digits = 2,
          big.mark = ".",
          decimal.mark = ","
        ),
        `Periode (Bulan)` = formatC(
          `Periode (Bulan)`,
          format = "f",
          digits = 0,
          big.mark = ".",
          decimal.mark = ","
        ),
        `Bunga Per Tahun (%)` = formatC(
          `Bunga Per Tahun (%)`,
          format = "f",
          digits = 1,
          big.mark = ".",
          decimal.mark = ","
        ),
        `Total (Rp)` = formatC(
          `Total (Rp)`,
          format = "f",
          digits = 2,
          big.mark = ".",
          decimal.mark = ","
        )
      ),
    striped = TRUE,
    hover = TRUE,
    rownames = TRUE,
    align = "r",
    width = "100%"
  )
  ### grafik_data ----
  output$grafik_data <- renderPlot({
    x <- as.character(input$sumbu_x)
    y <- as.character(input$sumbu_y)
    warna <- as.character(input$warna)
    facet <- as.character(input$facet)
    
    plot_awal <- tabel_data() %>%
      ggplot(aes(.data[[x]], .data[[y]])) +
      theme_bw(base_size = 16) +
      labs(
        title = paste0("Hubungan Antara ", x, " dan ", y)
      ) +
      theme(
        plot.title = element_text(
          face = "bold",
          hjust = .5
        )
      )
    plot_baru <- if (warna == "-" & facet == "-") {
      plot_awal +
        geom_point(
          size = 5
        )
    } else if (warna != "-" & facet == "-") {
      plot_awal +
        geom_point(
          aes(color = .data[[warna]]),
          size = 5
        ) +
        scale_color_viridis_b() +
        theme(
          legend.position = "right"
        )
    } else if (warna == "-" & facet != "-") {
      plot_awal +
        geom_point(
          size = 5
        ) +
        facet_grid(
          ~factor(.data[[facet]])
        )
    } else {
      plot_awal +
        geom_point(
          aes(
            color = .data[[warna]]
          ),
          size = 5
        ) +
        scale_color_viridis_b() +
        facet_grid(
          ~factor(.data[[facet]])
        ) +
        theme(
          legend.position = "bottom"
        )
    }
    
    plot_baru
  })
}

# Aplikasi Shiny ----
shinyApp(ui, server)
