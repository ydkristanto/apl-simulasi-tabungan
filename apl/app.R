# Paket ----
library(shiny)
library(bslib)
library(tidyverse)
library(knitr)
library(kableExtra)

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
    selectInput(
      "jenis_simulasi",
      div("Simulasi:", style = "font-weight: bold;"),
      choices = c(
        "Setoran Bulanan" = "setor_bulanan",
        "Periode (Bulan)" = "periode",
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
        max = 1000000,
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
        min = 1,
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
      max = 5,
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
    actionButton(
      "simpan", "Simpan"
    ),
    actionButton(
      "hapus", "Hapus"
    )
  ),
  ## Konten utama ----
  nav_panel(
    ### Deposito ----
    "Deposito",
    layout_columns(
      uiOutput("kotak_hitungan_dep"),
      uiOutput("kotak_periode_dep"),
      uiOutput("kotak_bunga_dep"),
      uiOutput("kotak_total_dep"),
      card(
        tableOutput("tabel_data_dep")
      ),
      col_widths = c(3, 3, 3, 3, 12),
      row_heights = c(1.75, 3)
    )
  ),
  nav_panel(
    ### Tabungan berjangka ----
    "Tabungan Berjangka",
    layout_columns(
      uiOutput("kotak_setoran"),
      uiOutput("kotak_periode"),
      uiOutput("kotak_bunga"),
      uiOutput("kotak_total"),
      card(
        tableOutput("tabel_data")
      ),
      col_widths = c(3, 3, 3, 3, 12),
      row_heights = c(1.75, 3)
    )
  ),
  nav_panel(
    "Informasi"
  ),
  nav_spacer(),
  nav_menu(
    title = "Tautan",
    nav_item(tautan_apl_lain),
    nav_item(tautan_github),
    icon = shiny::icon("link"),
    align = "right"
  )
)

# Peladen ----
server <- function(input, output, session) {
  ## kotak_periode ----
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
        A / R
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
  ## kotak_setoran ----
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
    nilai_setoran <- if (jenis_simulasi == "setor_bulanan") {
      A * i / ((1 + i)^n - 1)
    } else {
      R
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
  ## kotak_bunga ----
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
  ## kotak_total ----
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
    nilai_total <- if (jenis_simulasi == "total") {
      R * ((1 + i)^n - 1) / i
    } else {
      A
    }
    total_simpel <- if (nilai_total < 1e+3) {
      round(nilai_setoran, 2)
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
  ## Awal tabel_data ----
  tabel_data <- reactiveVal(
    tibble(
      `Periode (Bulan)` = character(),
      `Setoran Bulanan (Rp)` = character(),
      `Bunga Per Tahun (%)` = character(),
      `Total (Rp)` = character()
    )
  )
  
  ## Membuat baris tabel ----
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
        A / R
      )
      nilai_setoran <- R
      nilai_bunga <- 1200 * i
      nilai_total <- A
    } else if (jenis_simulasi == "setor_bulanan") {
      nilai_setoran <- round(A * i / ((1 + i)^n - 1), 2)
      nilai_periode <- n
      nilai_bunga <- 1200 * i
      nilai_total <- A
    } else {
      nilai_total <- round(R * ((1 + i)^n - 1) / i, 2)
      nilai_periode <- n
      nilai_setoran <- R
      nilai_bunga <- 1200 * i
    }
    tabel_data() %>%
      add_row(
        `Periode (Bulan)` = formatC(
          nilai_periode,
          big.mark = ".",
          decimal.mark = ","
        ),
        `Setoran Bulanan (Rp)` = formatC(
          nilai_setoran,
          big.mark = ".",
          decimal.mark = ",",
          format = "f",
          digits = 2
        ),
        `Bunga Per Tahun (%)` = formatC(
          nilai_bunga,
          big.mark = ".",
          decimal.mark = ",",
          format = "f",
          digits = 1
        ),
        `Total (Rp)` = formatC(
          nilai_total,
          big.mark = ".",
          decimal.mark = ",",
          format = "f",
          digits = 2
        )
      ) %>%
      tabel_data()
  })
  
  ## Menghapus semua baris tabel ----
  observeEvent(input$hapus, {
    tabel_data(
      tibble(
        `Periode (Bulan)` = character(),
        `Setoran Bulanan (Rp)` = character(),
        `Bunga Per Tahun (%)` = character(),
        `Total (Rp)` = character()
      )
    )
  })
  
  ## Luaran tabel_data ----
  output$tabel_data <- renderTable(
    tabel_data(),
    striped = TRUE,
    hover = TRUE,
    rownames = TRUE,
    align = "r",
    width = "100%"
  )
}

# Aplikasi Shiny ----
shinyApp(ui, server)
