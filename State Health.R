#  India State Debt Dashboard 

library(shiny)
library(leaflet)
library(sf)
library(rnaturalearth)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rnaturalearthdata)
library(plotly)
library(DT)
library(bslib)
library(htmltools)

# ─────────────────────────────────────────────────────────────
# 1. DATA PREPARATION
# ─────────────────────────────────────────────────────────────

df <- state_debt   # columns: State, Year, Debt_to_GSDP

# ── Comprehensive name harmonisation (RBI → Natural Earth) ──
name_map <- c(
  "NCT Delhi"           = "Delhi",
  "Odisha"              = "Orissa",
  "Uttarakhand"         = "Uttaranchal",
  "Jammu & Kashmir"     = "Jammu and Kashmir",
  "Andaman & Nicobar"   = "Andaman and Nicobar",
  "Dadra & Nagar Haveli"= "Dadra and Nagar Haveli",
  "Daman & Diu"         = "Daman and Diu"
)
df$State <- dplyr::recode(df$State, !!!name_map)

# ── YoY change ──
df <- df %>%
  arrange(State, Year) %>%
  group_by(State) %>%
  mutate(YoY_Change = round(Debt_to_GSDP - lag(Debt_to_GSDP), 2)) %>%
  ungroup()

# ── Quartile helper ──
add_quartile <- function(data) {
  data %>% mutate(
    Quartile       = ntile(Debt_to_GSDP, 4),
    Quartile_Label = case_when(
      Quartile == 1 ~ "Q1 — Lowest",
      Quartile == 2 ~ "Q2 — Below Median",
      Quartile == 3 ~ "Q3 — Above Median",
      Quartile == 4 ~ "Q4 — Highest",
      TRUE          ~ NA_character_
    )
  )
}

# ── India map — FIX: use returnclass="sf" and repair geometry ──
india_map <- ne_states(country = "india", returnclass = "sf") %>%
  st_make_valid() %>%                    # repair any invalid geometries
  st_transform(4326)                     # ensure WGS84 CRS for leaflet

# Inspect available name column (run once to verify):
# unique(india_map$name)

years_available <- sort(unique(df$Year), decreasing = TRUE)

# ─────────────────────────────────────────────────────────────
# 2. UI  — Dark editorial aesthetic
# ─────────────────────────────────────────────────────────────

DARK  <- "#0d1117"
PANEL <- "#161b22"
CARD  <- "#1e2530"
BORDER<- "#30363d"
GOLD  <- "#d4a042"
TEXT1 <- "#e6edf3"
TEXT2 <- "#8b949e"
RED   <- "#f85149"
GREEN <- "#3fb950"

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "darkly"),
  
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Bebas+Neue&family=IBM+Plex+Sans:wght@300;400;500;600&family=IBM+Plex+Mono:wght@400&display=swap"
    ),
    tags$style(HTML(sprintf("
      html, body { background:%s !important; color:%s; font-family:'IBM Plex Sans',sans-serif; }

      /* ── Typography ── */
      h1,h2,h3,h4 { font-family:'Bebas Neue',sans-serif; letter-spacing:.06em; color:%s; }

      /* ── Dashboard header ── */
      .dash-header {
        background:%s;
        border-bottom:2px solid %s;
        padding:22px 32px 16px;
        margin-bottom:0;
      }
      .dash-header h1 { font-size:2.4rem; margin:0; line-height:1; color:%s; }
      .dash-header .sub { font-size:.8rem; color:%s; letter-spacing:.12em;
                          text-transform:uppercase; margin-top:4px; font-weight:300; }

      /* ── Sidebar ── */
      .sidebar-wrap {
        background:%s;
        border-right:1px solid %s;
        padding:24px 20px;
        min-height:100vh;
      }
      .sidebar-wrap label { font-size:.72rem; text-transform:uppercase;
                            letter-spacing:.1em; color:%s; font-weight:500; }
      .form-control, .form-select {
        background:%s !important; color:%s !important;
        border:1px solid %s !important; font-size:.88rem;
      }
      .sidebar-divider { border-color:%s; margin:16px 0; }
      .sidebar-section-title {
        font-family:'Bebas Neue',sans-serif; font-size:1rem;
        color:%s; letter-spacing:.08em; margin:14px 0 6px;
      }

      /* ── KPI Cards ── */
      .kpi-grid { display:grid; grid-template-columns:repeat(4,1fr); gap:12px;
                  padding:16px 24px; background:%s; border-bottom:1px solid %s; }
      .kpi-card { background:%s; border:1px solid %s; border-radius:8px;
                  padding:14px 16px; }
      .kpi-val  { font-family:'Bebas Neue',sans-serif; font-size:2rem;
                  line-height:1; margin:0; }
      .kpi-lbl  { font-size:.68rem; text-transform:uppercase; letter-spacing:.1em;
                  color:%s; margin-top:3px; }
      .kpi-sub  { font-size:.78rem; color:%s; margin-top:2px; font-weight:500; }

      /* ── Tab bar ── */
      .nav-tabs { background:%s; border-bottom:1px solid %s !important;
                  padding:0 20px; }
      .nav-tabs .nav-link {
        color:%s !important; font-size:.78rem; text-transform:uppercase;
        letter-spacing:.08em; border:none !important; padding:12px 16px;
        border-bottom:2px solid transparent !important;
      }
      .nav-tabs .nav-link.active {
        background:transparent !important; color:%s !important;
        border-bottom:2px solid %s !important;
      }
      .tab-content { background:%s; padding:20px 24px; }

      /* ── Legend note ── */
      .map-note { font-size:.72rem; color:%s; margin-top:8px;
                  font-style:italic; }

      /* ── DataTable ── */
      table.dataTable { background:%s !important; color:%s !important; }
      table.dataTable thead th {
        background:%s !important; color:%s !important;
        font-family:'Bebas Neue',sans-serif; letter-spacing:.06em;
        border-bottom:1px solid %s !important; font-size:.9rem;
      }
      table.dataTable tbody tr { background:%s !important; }
      table.dataTable tbody tr:hover { background:%s !important; }
      .dataTables_wrapper { color:%s !important; }
      .dataTables_wrapper .dataTables_filter input {
        background:%s !important; color:%s !important;
        border:1px solid %s !important; }
      .dataTables_wrapper select {
        background:%s !important; color:%s !important; }

      /* ── Section headings inside tabs ── */
      .section-hd { font-family:'Bebas Neue',sans-serif; font-size:1.1rem;
                    color:%s; letter-spacing:.06em; margin:16px 0 10px;
                    border-bottom:1px solid %s; padding-bottom:4px; }

      /* ── Scrollbar ── */
      ::-webkit-scrollbar { width:6px; height:6px; }
      ::-webkit-scrollbar-track { background:%s; }
      ::-webkit-scrollbar-thumb { background:%s; border-radius:3px; }
    ",
                            DARK, TEXT1, TEXT1,      # html/body + h tags
                            PANEL, GOLD, TEXT1, TEXT2, # header
                            PANEL, BORDER, TEXT2, CARD, TEXT1, BORDER, BORDER, GOLD, # sidebar
                            DARK, BORDER, CARD, BORDER, TEXT2, TEXT2, # kpi
                            PANEL, BORDER, TEXT2, TEXT1, GOLD, DARK,  # tabs
                            TEXT2,                   # map note
                            CARD, TEXT1, PANEL, GOLD, BORDER, CARD, PANEL, TEXT1, # DT
                            CARD, TEXT1, BORDER, CARD, TEXT1, # DT inputs
                            GOLD, BORDER,            # section-hd
                            DARK, BORDER             # scrollbar
    )))
  ),
  
  # ── Header ──────────────────────────────────────────────────
  div(class = "dash-header",
      fluidRow(
        column(9,
               tags$h1("India State Debt Monitor"),
               div(class = "sub", "Debt-to-GSDP ratio · State finance analysis · RBI data")
        ),
        column(3, align = "right",
               div(style = sprintf("font-family:'IBM Plex Mono',monospace;font-size:.75rem;color:%s;padding-top:12px;", TEXT2),
                   textOutput("last_year_label", inline = TRUE))
        )
      )
  ),
  
  # ── KPI strip ───────────────────────────────────────────────
  uiOutput("kpi_row"),
  
  # ── Body ────────────────────────────────────────────────────
  fluidRow(style = "margin:0;",
           
           # Sidebar
           column(2, style = "padding:0;",
                  div(class = "sidebar-wrap",
                      div(class = "sidebar-section-title", "Reference Year"),
                      selectInput("year_choice", NULL,
                                  choices = years_available, selected = years_available[1]),
                      div(class = "sidebar-section-title", "Map Metric"),
                      selectInput("map_metric", NULL,
                                  choices = c("Debt-to-GSDP (%)" = "Debt_to_GSDP",
                                              "YoY Change (pp)"   = "YoY_Change")),
                      tags$hr(class = "sidebar-divider"),
                      div(class = "sidebar-section-title", "Trend States"),
                      selectInput("trend_states", NULL,
                                  choices  = sort(unique(df$State)),
                                  selected = head(sort(unique(df$State)), 4),
                                  multiple = TRUE),
                      tags$hr(class = "sidebar-divider"),
                      div(class = "sidebar-section-title", "Scatter Year"),
                      sliderInput("scatter_year", NULL,
                                  min = min(df$Year), max = max(df$Year),
                                  value = max(df$Year), step = 1, sep = "",
                                  ticks = FALSE)
                  )
           ),
           
           # Main
           column(10, style = "padding:0;",
                  tabsetPanel(id = "main_tabs",
                              
                              # ── Tab 1: Map ──────────────────────────────────────
                              tabPanel("Choropleth Map",
                                       div(style = "padding:20px;",
                                           leafletOutput("debtMap", height = "560px"),
                                           div(class = "map-note",
                                               "Grey areas indicate states absent from the dataset or name mismatches between RBI and Natural Earth boundaries.")
                                       )
                              ),
                              
                              # ── Tab 2: Rankings ─────────────────────────────────
                              tabPanel("State Rankings",
                                       fluidRow(
                                         column(6,
                                                div(class = "section-hd", "Top 10 Highest Debt States"),
                                                plotlyOutput("bar_top10", height = "320px")
                                         ),
                                         column(6,
                                                div(class = "section-hd", "Fiscal Quartile Distribution"),
                                                plotlyOutput("quartile_pie", height = "320px")
                                         )
                                       ),
                                       div(class = "section-hd", "Full Rankings"),
                                       DTOutput("rankings_table")
                              ),
                              
                              # ── Tab 3: Trends ────────────────────────────────────
                              tabPanel("Trend Analysis",
                                       div(class = "section-hd", "State Trajectories"),
                                       plotlyOutput("trend_plot", height = "360px"),
                                       div(class = "section-hd", "National Average ± 1 SD"),
                                       plotlyOutput("nat_avg_trend", height = "200px")
                              ),
                              
                              # ── Tab 4: YoY Heatmap ───────────────────────────────
                              tabPanel("YoY Heatmap",
                                       div(class = "section-hd", "Year-on-Year Change (pp)"),
                                       plotlyOutput("yoy_heatmap", height = "580px")
                              ),
                              
                              # ── Tab 5: Scatter ───────────────────────────────────
                              tabPanel("Cross-Sectional",
                                       div(class = "section-hd", "Debt Level vs Selected Metric"),
                                       plotlyOutput("scatter_plot", height = "460px")
                              )
                  )
           )
  )
)

# ─────────────────────────────────────────────────────────────
# 3. SERVER
# ─────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  yr_data <- reactive({
    df %>% filter(Year == input$year_choice) %>% add_quartile()
  })
  
  output$last_year_label <- renderText({
    paste("Latest:", max(df$Year))
  })
  
  # ── KPI strip ──────────────────────────────────────────────
  output$kpi_row <- renderUI({
    d       <- yr_data()
    avg     <- round(mean(d$Debt_to_GSDP, na.rm = TRUE), 1)
    max_row <- d %>% slice_max(Debt_to_GSDP, n = 1, with_ties = FALSE)
    min_row <- d %>% slice_min(Debt_to_GSDP, n = 1, with_ties = FALSE)
    over30  <- sum(d$Debt_to_GSDP > 30, na.rm = TRUE)
    pct30   <- round(over30 / sum(!is.na(d$Debt_to_GSDP)) * 100)
    
    kpi <- function(val, col, lbl, sub) {
      div(class = "kpi-card",
          div(class = "kpi-val", style = sprintf("color:%s;", col), val),
          div(class = "kpi-lbl", lbl),
          div(class = "kpi-sub", sub)
      )
    }
    
    div(class = "kpi-grid",
        kpi(paste0(avg, "%"), GOLD,  "National avg debt/GSDP", input$year_choice),
        kpi(paste0(max_row$Debt_to_GSDP, "%"), RED,  "Highest", max_row$State),
        kpi(paste0(min_row$Debt_to_GSDP, "%"), GREEN, "Lowest", min_row$State),
        kpi(over30, "#c9a227", "States > 30% debt", paste0(pct30, "% of total"))
    )
  })
  
  # ── Choropleth map ─────────────────────────────────────────
  # KEY FIX: build popup/label AFTER join so NAs are handled safely
  output$debtMap <- renderLeaflet({
    d      <- yr_data()
    metric <- input$map_metric
    
    map_j <- left_join(india_map, d, by = c("name" = "State"))
    
    # Safe label construction
    map_j$popup_txt <- ifelse(
      is.na(map_j$Debt_to_GSDP),
      paste0("<b>", map_j$name, "</b><br>No data"),
      paste0(
        "<b>", map_j$name, "</b><br>",
        "Debt/GSDP: <b>", round(map_j$Debt_to_GSDP, 1), "%</b><br>",
        "YoY: ",
        ifelse(is.na(map_j$YoY_Change), "N/A",
               paste0(ifelse(map_j$YoY_Change > 0, "+", ""),
                      round(map_j$YoY_Change, 1), " pp")),
        "<br>Year: ", input$year_choice
      )
    )
    
    if (metric == "YoY_Change") {
      vals  <- map_j$YoY_Change
      pal   <- colorNumeric(
        palette  = c("#1a7abf", "#1e2530", "#f85149"),
        domain   = c(-max(abs(vals), na.rm = TRUE),
                     max(abs(vals), na.rm = TRUE)),
        na.color = "#2d333b"
      )
      fill  <- ~pal(YoY_Change)
      leg_v <- ~YoY_Change
      leg_t <- "YoY \u0394 (pp)"
    } else {
      vals  <- df$Debt_to_GSDP
      pal   <- colorNumeric(
        palette  = c("#1e2d1e", "#d4a042", "#f85149"),
        domain   = vals,
        na.color = "#2d333b"
      )
      fill  <- ~pal(Debt_to_GSDP)
      leg_v <- ~Debt_to_GSDP
      leg_t <- "Debt % GSDP"
    }
    
    leaflet(map_j,
            options = leafletOptions(zoomControl = TRUE)) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = 78.96, lat = 20.59, zoom = 4) %>%
      addPolygons(
        fillColor        = fill,
        weight           = 0.6,
        color            = "#30363d",
        fillOpacity      = 0.85,
        popup            = ~lapply(popup_txt, HTML),
        label            = ~name,
        labelOptions     = labelOptions(
          style = list(
            "background-color" = CARD,
            "color"            = TEXT1,
            "border"           = paste0("1px solid ", BORDER),
            "font-family"      = "'IBM Plex Sans', sans-serif",
            "font-size"        = "12px",
            "padding"          = "4px 8px",
            "border-radius"    = "4px"
          )
        ),
        highlightOptions = highlightOptions(
          weight    = 2,
          color     = GOLD,
          fillOpacity = 0.95,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal       = pal,
        values    = leg_v,
        title     = leg_t,
        position  = "bottomright",
        opacity   = 0.9,
        labFormat = labelFormat(suffix = ifelse(metric=="Debt_to_GSDP","%"," pp"))
      )
  })
  
  # ── Plotly theme helper ─────────────────────────────────────
  dark_layout <- function(p, ...) {
    p %>% layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font          = list(family = "IBM Plex Sans", color = TEXT1),
      xaxis = list(gridcolor = BORDER, zerolinecolor = BORDER, ...),
      yaxis = list(gridcolor = BORDER, zerolinecolor = BORDER),
      ...
    )
  }
  
  # ── Bar: top 10 ─────────────────────────────────────────────
  output$bar_top10 <- renderPlotly({
    d <- yr_data() %>% slice_max(Debt_to_GSDP, n = 10, with_ties = FALSE)
    plot_ly(d,
            x = ~reorder(State, Debt_to_GSDP), y = ~Debt_to_GSDP,
            type = "bar",
            marker = list(
              color = ~Debt_to_GSDP,
              colorscale = list(c(0, "#2a5c38"), c(0.5, GOLD), c(1, RED)),
              showscale  = FALSE,
              line       = list(width = 0)
            ),
            hovertemplate = "<b>%{x}</b><br>%{y:.1f}%<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "", tickangle = -35, tickfont = list(size = 9),
                     gridcolor = BORDER),
        yaxis = list(title = "Debt/GSDP (%)", gridcolor = BORDER),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor  = "rgba(0,0,0,0)",
        font = list(family = "IBM Plex Sans", color = TEXT1),
        margin = list(b = 90)
      )
  })
  
  # ── Pie: quartile ───────────────────────────────────────────
  output$quartile_pie <- renderPlotly({
    d <- yr_data() %>% filter(!is.na(Quartile_Label)) %>% count(Quartile_Label)
    plot_ly(d,
            labels = ~Quartile_Label, values = ~n, type = "pie",
            marker = list(colors = c(GREEN, "#d4a042", "#e07b22", RED),
                          line    = list(color = DARK, width = 1.5)),
            textinfo = "label+percent",
            hovertemplate = "%{label}: %{value} states<extra></extra>"
    ) %>%
      layout(
        showlegend    = FALSE,
        paper_bgcolor = "rgba(0,0,0,0)",
        font          = list(family = "IBM Plex Sans", color = TEXT1)
      )
  })
  
  # ── Rankings table ──────────────────────────────────────────
  output$rankings_table <- renderDT({
    d <- yr_data() %>%
      arrange(desc(Debt_to_GSDP)) %>%
      mutate(Rank = row_number()) %>%
      select(Rank, State, Debt_to_GSDP, YoY_Change, Quartile_Label)
    
    datatable(d,
              options  = list(pageLength = 15, dom = "ftp"),
              colnames = c("#", "State", "Debt/GSDP (%)", "YoY Δ (pp)", "Quartile"),
              rownames = FALSE
    ) %>%
      formatStyle("YoY_Change",
                  color = styleInterval(0, c(GREEN, RED))
      ) %>%
      formatStyle("Debt_to_GSDP",
                  background         = styleColorBar(d$Debt_to_GSDP, GOLD),
                  backgroundSize     = "100% 70%",
                  backgroundRepeat   = "no-repeat",
                  backgroundPosition = "center"
      )
  })
  
  # ── Trend lines ─────────────────────────────────────────────
  output$trend_plot <- renderPlotly({
    req(input$trend_states)
    d <- df %>% filter(State %in% input$trend_states)
    plot_ly(d, x = ~Year, y = ~Debt_to_GSDP, color = ~State,
            type = "scatter", mode = "lines+markers",
            colors = c(GOLD, RED, GREEN, "#5bcefa", "#c490e4", "#ff8c42"),
            hovertemplate = "<b>%{fullData.name}</b> %{x}: %{y:.1f}%<extra></extra>"
    ) %>%
      layout(
        xaxis  = list(title = "Year", dtick = 1, gridcolor = BORDER),
        yaxis  = list(title = "Debt/GSDP (%)", gridcolor = BORDER),
        legend = list(orientation = "h", y = -0.25, font = list(size = 11)),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor  = "rgba(0,0,0,0)",
        font  = list(family = "IBM Plex Sans", color = TEXT1)
      )
  })
  
  output$nat_avg_trend <- renderPlotly({
    d <- df %>%
      group_by(Year) %>%
      summarise(Avg = mean(Debt_to_GSDP, na.rm = TRUE),
                SD  = sd(Debt_to_GSDP,   na.rm = TRUE), .groups = "drop")
    plot_ly(d) %>%
      add_ribbons(x = ~Year, ymin = ~Avg - SD, ymax = ~Avg + SD,
                  fillcolor = "rgba(212,160,66,.18)", line = list(color = "transparent"),
                  name = "\u00b11 SD") %>%
      add_lines(x = ~Year, y = ~Avg,
                line = list(color = GOLD, width = 2.5), name = "National Avg") %>%
      layout(
        xaxis  = list(title = "Year", dtick = 1, gridcolor = BORDER),
        yaxis  = list(title = "Avg Debt/GSDP (%)", gridcolor = BORDER),
        legend = list(orientation = "h", y = -0.45),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor  = "rgba(0,0,0,0)",
        font  = list(family = "IBM Plex Sans", color = TEXT1)
      )
  })
  
  # ── YoY heatmap ─────────────────────────────────────────────
  output$yoy_heatmap <- renderPlotly({
    d <- df %>% filter(!is.na(YoY_Change))
    plot_ly(d, x = ~Year, y = ~State, z = ~YoY_Change,
            type = "heatmap",
            colorscale = list(c(0, "#1a7abf"), c(0.5, PANEL), c(1, RED)),
            zmid = 0,
            hovertemplate = "<b>%{y}</b> %{x}: %{z:+.1f} pp<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Year", dtick = 1, gridcolor = BORDER),
        yaxis = list(title = "", tickfont = list(size = 9), gridcolor = BORDER),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor  = "rgba(0,0,0,0)",
        font  = list(family = "IBM Plex Sans", color = TEXT1)
      )
  })
  
  # ── Scatter ─────────────────────────────────────────────────
  output$scatter_plot <- renderPlotly({
    d    <- df %>% filter(Year == input$scatter_year) %>% add_quartile()
    med_x <- median(d$Debt_to_GSDP, na.rm = TRUE)
    med_y <- median(d$YoY_Change,    na.rm = TRUE)
    plot_ly(d, x = ~Debt_to_GSDP, y = ~YoY_Change,
            type = "scatter", mode = "markers+text",
            text = ~State, textposition = "top center",
            textfont = list(size = 8, color = TEXT2),
            marker = list(
              size   = ~pmax(Debt_to_GSDP * 0.5, 8),
              color  = ~Debt_to_GSDP,
              colorscale = list(c(0, GREEN), c(0.5, GOLD), c(1, RED)),
              opacity = 0.82,
              line    = list(width = 0.5, color = DARK)
            ),
            hovertemplate = "<b>%{text}</b><br>Debt: %{x:.1f}%<br>YoY: %{y:+.1f}pp<extra></extra>"
    ) %>%
      layout(
        shapes = list(
          list(type="line", x0=med_x, x1=med_x,
               y0=min(d$YoY_Change,na.rm=TRUE), y1=max(d$YoY_Change,na.rm=TRUE),
               line=list(dash="dot", color=BORDER, width=1)),
          list(type="line", y0=med_y, y1=med_y,
               x0=min(d$Debt_to_GSDP,na.rm=TRUE), x1=max(d$Debt_to_GSDP,na.rm=TRUE),
               line=list(dash="dot", color=BORDER, width=1))
        ),
        xaxis = list(title = "Debt/GSDP (%)", gridcolor = BORDER),
        yaxis = list(title = "YoY \u0394 (pp)",  gridcolor = BORDER),
        paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor  = "rgba(0,0,0,0)",
        font  = list(family = "IBM Plex Sans", color = TEXT1)
      )
  })
}

# ─────────────────────────────────────────────────────────────
shinyApp(ui, server)