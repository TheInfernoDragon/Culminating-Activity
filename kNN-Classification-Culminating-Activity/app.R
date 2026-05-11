library(shiny)
library(class)
library(caret)

# ── Data prep ─────────────────────────────────────────────────────────────────
dev_data <- read.csv("ai_dev_productivity.csv")

dev.set <- reactive({
      dev_data[, c("task_success", "hours_coding", "coffee_intake_mg", 
                   "distractions", "sleep_hours", "commits", 
                   "bugs_reported", "ai_usage_hours", "cognitive_load")]
})

dev_data$task_success <- factor(dev_data$task_success, levels = c(0, 1),
                                labels = c("Fail", "Success"))

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Fira+Code:wght@400;500;700&family=DM+Sans:wght@400;600;800&display=swap"
    ),
    tags$style(HTML("
      *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }

      body {
        background-color: #f5f7fa;
        color: #24292f;
        font-family: 'Fira Code', monospace;
        font-size: 13px;
        min-height: 100vh;
      }

      .dash-header {
        background: linear-gradient(135deg, #ffffff 60%, #eef2f7);
        border-bottom: 1px solid #d0d7de;
        padding: 26px 40px 20px;
        position: relative;
        overflow: hidden;
      }
      .dash-header::before {
        content: '';
        position: absolute;
        top: -60px; right: -60px;
        width: 260px; height: 260px;
        border-radius: 50%;
        background: radial-gradient(circle, rgba(9,105,218,0.06) 0%, transparent 70%);
        pointer-events: none;
      }
      .dash-title {
        font-family: 'DM Sans', sans-serif;
        font-weight: 800;
        font-size: 24px;
        color: #24292f;
        letter-spacing: -0.5px;
      }
      .dash-title span { color: #0969da; }
      .dash-sub {
        font-size: 11px;
        color: #57606a;
        margin-top: 4px;
        letter-spacing: 0.6px;
        text-transform: uppercase;
      }

      .nav-tabs {
        border-bottom: 1px solid #d0d7de !important;
        background: #ffffff;
        padding: 0 40px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.04);
      }
      .nav-tabs > li > a {
        font-family: 'DM Sans', sans-serif;
        font-size: 13px;
        font-weight: 600;
        color: #57606a !important;
        border: none !important;
        border-bottom: 2px solid transparent !important;
        border-radius: 0 !important;
        padding: 13px 20px !important;
        background: transparent !important;
        letter-spacing: 0.2px;
        transition: color .2s, border-color .2s;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li > a:hover {
        color: #0969da !important;
        border-bottom-color: #0969da !important;
        background: transparent !important;
      }
      .tab-content { background: #f5f7fa; padding: 0; }

      .tab-body {
        display: grid;
        grid-template-columns: 270px 1fr;
        gap: 0;
        min-height: calc(100vh - 125px);
      }
      .sidebar {
        background: #ffffff;
        border-right: 1px solid #d0d7de;
        padding: 24px 18px;
        position: sticky;
        top: 0;
        height: calc(100vh - 125px);
        overflow-y: auto;
      }
      .main-area { padding: 24px 28px; overflow-y: auto; }

      .sb-section { margin-bottom: 24px; }
      .sb-label {
        font-family: 'DM Sans', sans-serif;
        font-size: 10px;
        font-weight: 700;
        color: #57606a;
        letter-spacing: 1.2px;
        text-transform: uppercase;
        margin-bottom: 10px;
        padding-bottom: 6px;
        border-bottom: 1px solid #d0d7de;
      }

      .form-control, select.form-control {
        background: #f6f8fa !important;
        border: 1px solid #d0d7de !important;
        color: #24292f !important;
        border-radius: 6px !important;
        font-family: 'Fira Code', monospace !important;
        font-size: 12px !important;
        padding: 7px 10px !important;
        transition: border-color .2s;
      }
      .form-control:focus {
        border-color: #0969da !important;
        outline: none !important;
        box-shadow: 0 0 0 3px rgba(9,105,218,0.1) !important;
      }
      .control-label {
        color: #57606a !important;
        font-size: 11px !important;
        font-family: 'Fira Code', monospace !important;
        margin-bottom: 5px !important;
      }

      .irs--shiny .irs-bar { background: #0969da !important; border-top-color: #0969da !important; border-bottom-color: #0969da !important; }
      .irs--shiny .irs-handle { background: #0969da !important; border-color: #0969da !important; }
      .irs--shiny .irs-single { background: #0969da !important; font-family: 'Fira Code', monospace !important; font-size: 11px !important; }
      .irs--shiny .irs-line { background: #d0d7de !important; border-color: #d0d7de !important; }
      .irs--shiny .irs-min, .irs--shiny .irs-max { color: #57606a !important; font-size: 10px !important; font-family: 'Fira Code', monospace !important; background: transparent !important; }

      .btn-run {
        width: 100%;
        background: #0969da !important;
        color: #ffffff !important;
        border: none !important;
        border-radius: 6px !important;
        font-family: 'DM Sans', sans-serif !important;
        font-weight: 700 !important;
        font-size: 12px !important;
        letter-spacing: 0.4px !important;
        padding: 10px !important;
        cursor: pointer !important;
        transition: background .2s, transform .1s !important;
        margin-top: 8px;
      }
      .btn-run:hover { background: #218bff !important; }
      .btn-run:active { transform: scale(0.98) !important; }

      .metrics-row {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 12px;
        margin-bottom: 20px;
      }
      .metric-card {
        background: #ffffff;
        border: 1px solid #d0d7de;
        border-radius: 8px;
        padding: 16px;
        position: relative;
        overflow: hidden;
        box-shadow: 0 1px 3px rgba(0,0,0,0.04);
      }
      .metric-card::before {
        content: '';
        position: absolute;
        top: 0; left: 0; right: 0;
        height: 3px;
      }
      .metric-card.blue::before  { background: #0969da; }
      .metric-card.green::before { background: #1a7f37; }
      .metric-card.amber::before { background: #9a6700; }
      .metric-card.coral::before { background: #cf222e; }
      .metric-label {
        font-size: 10px;
        color: #57606a;
        letter-spacing: 0.8px;
        text-transform: uppercase;
        margin-bottom: 8px;
        font-family: 'DM Sans', sans-serif;
      }
      .metric-value {
        font-family: 'DM Sans', sans-serif;
        font-size: 22px;
        font-weight: 800;
        color: #24292f;
      }
      .metric-sub { font-size: 10px; color: #57606a; margin-top: 2px; }

      .plot-panel {
        background: #ffffff;
        border: 1px solid #d0d7de;
        border-radius: 8px;
        padding: 20px;
        margin-bottom: 18px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.04);
      }
      .panel-title {
        font-family: 'DM Sans', sans-serif;
        font-weight: 700;
        font-size: 12px;
        color: #57606a;
        letter-spacing: 0.6px;
        text-transform: uppercase;
        margin-bottom: 14px;
        display: flex;
        align-items: center;
        gap: 8px;
      }
      .panel-title::before {
        content: '';
        display: inline-block;
        width: 3px; height: 13px;
        background: #0969da;
        border-radius: 2px;
        flex-shrink: 0;
      }
      .plot-grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 18px;
      }

      .cm-wrap { overflow-x: auto; }
      .cm-table {
        width: 100%;
        border-collapse: collapse;
        font-size: 14px;
      }
      .cm-table th {
        background: #f6f8fa;
        color: #57606a;
        padding: 14px 18px;
        text-align: center;
        font-weight: 600;
        border: 1px solid #d0d7de;
        font-family: 'DM Sans', sans-serif;
        font-size: 12px;
        letter-spacing: 0.4px;
      }
      .cm-table td {
        padding: 24px 32px;
        text-align: center;
        border: 1px solid #d0d7de;
        font-family: 'DM Sans', sans-serif;
        font-size: 32px;
        font-weight: 800;
      }
      .cm-tp { background: rgba(26,127,55,0.1);  color: #1a7f37; }
      .cm-tn { background: rgba(9,105,218,0.08); color: #0969da; }
      .cm-fp { background: rgba(207,34,46,0.08); color: #cf222e; }
      .cm-fn { background: rgba(154,103,0,0.1);  color: #9a6700; }
      .cm-sub {
        display: block;
        font-size: 11px;
        font-weight: 400;
        opacity: 0.7;
        margin-top: 5px;
        font-family: 'Fira Code', monospace;
      }

      .info-box {
        background: rgba(9,105,218,0.05);
        border: 1px solid rgba(9,105,218,0.18);
        border-radius: 6px;
        padding: 12px 14px;
        font-size: 11px;
        color: #57606a;
        line-height: 1.75;
        margin-bottom: 14px;
        font-family: 'Fira Code', monospace;
      }
      .info-box strong { color: #0969da; font-weight: 600; }

      .acc-bar-wrap { margin-top: 6px; }
      .acc-bar-row { display: flex; align-items: center; gap: 10px; margin-bottom: 7px; }
      .acc-bar-k { width: 28px; text-align: right; font-size: 11px; color: #57606a; flex-shrink: 0; font-family: 'Fira Code', monospace; }
      .acc-bar-track { flex: 1; background: #f6f8fa; border-radius: 3px; height: 18px; overflow: hidden; border: 1px solid #d0d7de; }
      .acc-bar-fill { height: 100%; border-radius: 2px; transition: width 0.5s cubic-bezier(.4,0,.2,1); }
      .acc-bar-pct { width: 46px; font-size: 11px; color: #24292f; flex-shrink: 0; text-align: right; font-family: 'Fira Code', monospace; }
      .acc-bar-best .acc-bar-fill  { background: #0969da; }
      .acc-bar-other .acc-bar-fill { background: #d0d7de; }
      .acc-bar-best .acc-bar-pct   { color: #0969da; font-weight: 600; }

      .ds-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 14px; margin-bottom: 24px; }
      .ds-card { background: #ffffff; border: 1px solid #d0d7de; border-radius: 8px; padding: 16px; box-shadow: 0 1px 2px rgba(0,0,0,0.04); }
      .ds-card-name { font-family: 'DM Sans', sans-serif; font-size: 13px; font-weight: 700; color: #0969da; margin-bottom: 4px; }
      .ds-card-type { font-size: 10px; color: #57606a; text-transform: uppercase; letter-spacing: 0.8px; margin-bottom: 8px; font-family: 'Fira Code', monospace; }
      .ds-card-desc { font-size: 11px; color: #57606a; line-height: 1.7; }
      .ds-badge { display: inline-block; font-size: 10px; padding: 2px 8px; border-radius: 20px; margin-top: 8px; font-weight: 600; font-family: 'DM Sans', sans-serif; }
      .badge-target  { background: rgba(207,34,46,0.1);  color: #cf222e; }
      .badge-feature { background: rgba(9,105,218,0.1);  color: #0969da; }

      .q-card { border: 1px solid #d0d7de; border-radius: 8px; padding: 16px 18px; margin-bottom: 12px; background: #ffffff; box-shadow: 0 1px 2px rgba(0,0,0,0.04); }
      .q-label { font-size: 11px; font-weight: 700; color: #57606a; margin-bottom: 6px; font-family: 'DM Sans', sans-serif; text-transform: uppercase; letter-spacing: 0.5px; }
      .q-ans { font-size: 12px; color: #57606a; font-family: 'Fira Code', monospace; line-height: 1.75; border-left: 3px solid #1a7f37; padding-left: 12px; margin-top: 6px; }
      .q-ans strong { color: #0969da; font-weight: 600; }

      .bal-label { font-size: 10px; color: #57606a; margin-bottom: 4px; font-family: 'Fira Code', monospace; }
      .bal-track { background: #f6f8fa; border-radius: 3px; height: 8px; overflow: hidden; border: 1px solid #d0d7de; margin-bottom: 10px; }

      ::-webkit-scrollbar { width: 5px; height: 5px; }
      ::-webkit-scrollbar-track { background: #f6f8fa; }
      ::-webkit-scrollbar-thumb { background: #d0d7de; border-radius: 3px; }
      .shiny-spinner-output-container { min-height: 0; }
    "))
  ),
  
  div(class = "dash-header",
      div(class = "dash-title", "AI", "Developer", "Productivity", tags$span(" kNN"), " Dashboard"),
      div(class = "dash-sub", "kNN · Confusion Matrix · Scatter Plot")
  ),
  
  tabsetPanel(id = "main_tabs",
              
              # ── Tab 1: Dataset ─────────────────────────────────────────────────────
              tabPanel("Dataset",
                       div(class = "tab-body",
                           div(class = "sidebar",
                               div(class = "sb-section",
                                   div(class = "sb-label", "Overview"),
                                   div(class = "info-box",
                                       tags$strong("500 observations"), tags$br(),
                                       "Features: Coding Hours, Coffee Intake (mg), Distractions, Sleep Hours, AI Usage Hours, Cognitive Load, Bugs Reported.", tags$br(), tags$br(),
                                       "Target: Task Success (0 = Failed, 1 = Success)"
                                   )
                               ),
                               div(class = "sb-section",
                                   div(class = "sb-label", "Class balance"),
                                   uiOutput("task_balance_ui")
                               )
                           ),
                           div(class = "main-area",
                               div(class = "ds-grid",
                                   div(class = "ds-card",
                                       div(class = "ds-card-name", "Task Success"),
                                       div(class = "ds-card-desc", "Target column — whether the daily productivity goal was achieved (0/1)."),
                                       div(class = "ds-badge badge-target", "TARGET")
                                   ),
                                   div(class = "ds-card",
                                       div(class = "ds-card-name", "Coffee Intake (mg)"),
                                       div(class = "ds-card-desc", "Daily caffeine intake in milligrams (0–600 mg)."),
                                       div(class = "ds-badge badge-feature", "FEATURE")
                                   ),
                                   div(class = "ds-card",
                                       div(class = "ds-card-name", "Distractions"),
                                       div(class = "ds-card-desc", "Number of distractions (e.g., meetings, Slack notifications) (0–10)."),
                                       div(class = "ds-badge badge-feature", "FEATURE")
                                   ),
                                   div(class = "ds-card",
                                       div(class = "ds-card-name", "Sleep Hours"),
                                       div(class = "ds-card-desc", "Number of hours of sleep the previous night (3–10 hours)."),
                                       div(class = "ds-badge badge-feature", "FEATURE")
                                   ),
                                   div(class = "ds-card",
                                       div(class = "ds-card-name", "Commits"),
                                       div(class = "ds-card-desc", "Number of code commits pushed during the day."),
                                       div(class = "ds-badge badge-feature", "FEATURE")
                                   ),
                                   div(class = "ds-card",
                                       div(class = "ds-card-name", "Cognitive Load"),
                                       div(class = "ds-card-desc", "Self-reported mental strain on a scale of 1 to 10."),
                                       div(class = "ds-badge badge-feature", "FEATURE")
                                   ),
                                   div(class = "ds-card",
                                       div(class = "ds-card-name", "Commits"),
                                       div(class = "ds-card-desc", "Number of code commits pushed during the day."),
                                       div(class = "ds-badge badge-feature", "FEATURE")
                                   ),
                                   div(class = "ds-card",
                                       div(class = "ds-card-name", "Coding Hours"),
                                       div(class = "ds-card-desc", "Total focused hours spent on software development work (0–12 hours)."),
                                       div(class = "ds-badge badge-feature", "FEATURE")
                                   ),
                                   div(class = "ds-card",
                                       div(class = "ds-card-name", "Sleep Hours"),
                                       div(class = "ds-card-desc", "TNumber of hours of sleep the previous night (3–10 hours)."),
                                       div(class = "ds-badge badge-feature", "FEATURE")
                                   ),
                                   div(class = "ds-card",
                                       div(class = "ds-card-name", "Split"),
                                       div(class = "ds-card-type", "100 test / 400 train"),
                                       div(class = "ds-card-desc", "Fixed 200-row test set. set.seed(31) ensures reproducible results."),
                                       div(class = "ds-badge badge-feature", "SETUP")
                                   )
                               ),
                               div(class = "plot-panel",
                                   div(class = "panel-title", "Feature Distributions"),
                                   div(class = "plot-grid",
                                       plotOutput("dist_hours",        height = "200px"),
                                       plotOutput("dist_coffee",       height = "200px"),
                                       plotOutput("dist_sleep",        height = "200px"), # New addition
                                       
                                       plotOutput("dist_commits",      height = "200px"),
                                       plotOutput("dist_ai_usage",     height = "200px"),
                                       plotOutput("dist_bugs",         height = "200px"),
                                       
                                       plotOutput("dist_distractions", height = "200px"),
                                       plotOutput("dist_load",         height = "200px"),
                                       plotOutput("dist_success",      height = "200px")
                                   )
                               )
                           )
                       )
              ),
              
              # ── Tab 2: kNN ─────────────────────────────────────────────────────────
              tabPanel("kNN",
                       div(class = "tab-body",
                           div(class = "sidebar",
                               div(class = "sb-section",
                                   div(class = "sb-label", "Parameters"),
                                   sliderInput("knn_k", "Number of neighbours (k)",
                                               min = 1, max = 31, value = 5, step = 2),
                                   actionButton("run_knn", "Run kNN", class = "btn-run")
                               ),
                               div(class = "sb-section",
                                   div(class = "sb-label", "What is kNN?"),
                                   div(class = "info-box",
                                       "Classifies each developer's success by majority vote of their ", tags$strong("k nearest"),
                                       "peers in the training set.", tags$br(), tags$br(),
                                       "Distance-based — scaling is ", tags$strong("required"),
                                       " so metrics like ", tags$strong("Caffeine (mg)"), " don't dominate ", 
                                       tags$strong("Sleep Hours"), " or ", tags$strong("AI Usage.")
                                   )
                               ),
                               div(class = "sb-section",
                                   div(class = "sb-label", "Fixed settings"),
                                   div(class = "info-box",
                                       tags$strong("set.seed(31)"), tags$br(),
                                       "Test set: 100 rows", tags$br(),
                                       "Train set: 400 rows"
                                   )
                               )
                           ),
                           div(class = "main-area",
                               uiOutput("knn_metrics"),
                               div(class = "plot-panel",
                                   div(class = "panel-title", "Confusion matrix"),
                                   div(class = "cm-wrap", uiOutput("knn_cm"))
                               ),
                               div(class = "plot-panel",
                                   div(class = "panel-title", "Accuracy across k values"),
                                   uiOutput("knn_acc_bars")
                               )
                           )
                       )
              ),
              
              # ── Tab 3: Scatter Plot ────────────────────────────────────────────────
              tabPanel("Scatter Plot",
                       div(class = "tab-body",
                           div(class = "sidebar",
                               div(class = "sb-section",
                                   div(class = "sb-label", "About this plot"),
                                   div(class = "info-box",
                                       "Each point represents a ", tags$strong("test developer"), ".", tags$br(), tags$br(),
                                       tags$strong("Color"), " = predicted outcome.", tags$br(),
                                       tags$strong("Green"), " = Success.", tags$br(),
                                       tags$strong("Red"), " = Fail.", tags$br(), tags$br(),
                                       tags$strong("Shape"), ":", tags$br(),
                                       "Circle = correct prediction.", tags$br(),
                                       "X = misclassified.", tags$br(), tags$br(),
                                       "Run kNN first to populate."
                                   )
                               ),
                               div(class = "sb-section",
                                   div(class = "sb-label", "Why Hours & Commits?"),
                                   div(class = "info-box",
                                       "Both are ", tags$strong("continuous"), " variables that show a natural spread across the axis.",
                                       tags$br(), tags$br(),
                                       "Binary metrics like 'Success' would just stack points into two columns, making the visualization impossible to read."
                                   )
                               )
                           ),
                           div(class = "main-area",
                               div(class = "plot-panel",
                                   div(class = "panel-title", "Coding Hours vs Commits — kNN Prediction Results"),
                                   plotOutput("knn_scatter", height = "500px")
                               )
                           )
                       )
              )
  )
)

# --- Keep split_data OUTSIDE the server function ---
split_data <- function() {
  set.seed(31)
  n <- nrow(dev_data)
  train_size <- floor(0.8 * n)
  train_idx  <- sample(seq_len(n), size = train_size)
  list(train = dev_data[train_idx, ], test = dev_data[-train_idx, ])
}

# --- Server Logic ---
server <- function(input, output, session) {
  
  light_par <- function() {
    par(bg = "#ffffff", col.axis = "#57606a", col.lab = "#57606a",
        col.main = "#24292f", fg = "#d0d7de",
        family = "mono", cex.axis = 0.8, cex.lab = 0.85)
  }
  
  output$task_balance_ui <- renderUI({
    req(dev_data)
    
    n      <- nrow(dev_data)
    # Ensure these are single numeric values
    succ   <- as.integer(sum(dev_data$task_success == "Success", na.rm = TRUE))
    fail   <- as.integer(n - succ)
    
    # Calculate percentages as single values
    pct_s  <- if(n > 0) round((succ / n) * 100) else 0
    pct_f  <- 100 - pct_s
    
    tagList(
      div(class = "bal-label", paste0("Success: ", succ, " (", pct_s, "%)")),
      div(class = "bal-track",
          div(style = paste0("width:", pct_s, "%;background:#1a7f37;height:100%;border-radius:2px;"))
      ),
      div(class = "bal-label", paste0("Failed: ", fail, " (", pct_f, "%)")),
      div(class = "bal-track",
          div(style = paste0("width:", pct_f, "%;background:#cf222e;height:100%;border-radius:2px;"))
      )
    )
  })
  
  make_dist_plot <- function(col, main, fill_col, is_cat = FALSE) {
    req(dev.set())
    light_par()
    if (is_cat) {
      tb <- table(dev.set()[[col]])
      barplot(tb, col = fill_col, border = NA, main = main,
              col.main = "#24292f", ylab = "Count")
    } else {
      hist(dev.set()[[col]], col = fill_col, border = NA, main = main,
           xlab = "", col.main = "#24292f", breaks = 20)
    }
  }
  
  # Render all Distribution Plots
  output$dist_hours       <- renderPlot({ make_dist_plot("hours_coding",    "Coding Hours",    "#0969da") }, bg = "#ffffff")
  output$dist_coffee      <- renderPlot({ make_dist_plot("coffee_intake_mg", "Caffeine (mg)",   "#9a6700") }, bg = "#ffffff")
  output$dist_commits     <- renderPlot({ make_dist_plot("commits",          "Total Commits",   "#1a7f37") }, bg = "#ffffff")
  output$dist_success     <- renderPlot({ make_dist_plot("task_success",     "Success Rate",    "#8250df", TRUE) }, bg = "#ffffff")
  output$dist_bugs        <- renderPlot({ make_dist_plot("bugs_reported",    "Bugs Found",      "#cf222e") }, bg = "#ffffff")
  output$dist_load        <- renderPlot({ make_dist_plot("cognitive_load",   "Cognitive Load",  "#57606a") }, bg = "#ffffff")
  output$dist_ai_usage    <- renderPlot({ make_dist_plot("ai_usage_hours",   "AI Usage (hrs)",  "#0969da") }, bg = "#ffffff")
  output$dist_distractions <- renderPlot({ make_dist_plot("distractions",     "Distractions",    "#cf222e") }, bg = "#ffffff")
  output$dist_sleep       <- renderPlot({ make_dist_plot("sleep_hours",      "Sleep Hours",     "#08306b") }, bg = "#ffffff")
  
  knn_results <- reactiveVal(NULL)
  
  # Confusion Matrix HTML Helper
  cm_html <- function(cm) {
    tbl <- cm$table
    tn <- tbl[1, 1]; fp <- tbl[1, 2]
    fn <- tbl[2, 1]; tp <- tbl[2, 2]
    
    tags$table(class = "cm-table",
               tags$thead(
                 tags$tr(
                   tags$th(""), tags$th("Predicted: Fail"), tags$th("Predicted: Success")
                 )
               ),
               tags$tbody(
                 tags$tr(
                   tags$th("Actual: Fail"),
                   tags$td(class = "cm-tn", tn, tags$span(class = "cm-sub", "True Negative")),
                   tags$td(class = "cm-fp", fp, tags$span(class = "cm-sub", "False Positive"))
                 ),
                 tags$tr(
                   tags$th("Actual: Success"),
                   tags$td(class = "cm-fn", fn, tags$span(class = "cm-sub", "False Negative")),
                   tags$td(class = "cm-tp", tp, tags$span(class = "cm-sub", "True Positive"))
                 )
               )
    )
  }
  
  # Run kNN Event
  observeEvent(input$run_knn, {
    req(dev.set())
    
    model_features <- c("hours_coding", "coffee_intake_mg", "distractions", 
                        "sleep_hours", "commits", "bugs_reported", 
                        "ai_usage_hours", "cognitive_load")
    
    # Split the data
    d <- split_data()
    train_df <- d$train
    test_df  <- d$test
    
    # Scale Features
    tr_raw  <- train_df[, model_features]
    te_raw  <- test_df[, model_features]
    
    tr_sc   <- scale(tr_raw)
    te_sc   <- scale(te_raw,
                     center = attr(tr_sc, "scaled:center"),
                     scale  = attr(tr_sc, "scaled:scale"))
    
    k_vals  <- seq(1, 31, by = 2)
    acc_vec <- sapply(k_vals, function(k_val) {
      pred <- class::knn(tr_sc, te_sc, train_df$task_success, k = k_val)
      mean(pred == test_df$task_success)
    })
    
    # Run user-selected K
    pred_best <- class::knn(tr_sc, te_sc, train_df$task_success, k = input$knn_k)
    pred_best <- factor(pred_best, levels = c("Fail", "Success"))
    test_df$task_success <- factor(test_df$task_success, levels = c("Fail", "Success"))
    cm <- caret::confusionMatrix(pred_best, test_df$task_success, positive = "Success")
    
    knn_results(list(
      cm      = cm,
      k_vals  = k_vals,
      acc_vec = acc_vec,
      best_k  = input$knn_k,
      pred    = pred_best,
      test_df = test_df
    ))
  })
  
  output$knn_acc_bars <- renderUI({
    r <- knn_results(); req(r)
    
    bars <- lapply(seq_along(r$k_vals), function(i) {
      k       <- r$k_vals[i]
      acc     <- r$acc_vec[i]
      is_best <- k == r$best_k
      
      div(class = paste("acc-bar-row", ifelse(is_best, "acc-bar-best", "acc-bar-other")),
          div(class = "acc-bar-k", paste0("k=", k)),
          div(class = "acc-bar-track",
              div(class = "acc-bar-fill",
                  style = paste0("width:", round(acc * 100, 1), "%;",
                                 "background:", ifelse(is_best, "#0969da", "#d0d7de"), ";"))
          ),
          div(class = "acc-bar-pct", paste0(round(acc * 100, 1), "%"))
      )
    })
    
    div(class = "acc-bar-wrap", bars)
  })
  
  # Scatter Plot
  output$knn_scatter <- renderPlot({
    r <- knn_results()
    req(r)
    
    plot_df         <- r$test_df
    plot_df$pred    <- r$pred
    plot_df$correct <- r$pred == plot_df$task_success
    
    col_map <- ifelse(plot_df$pred == "Success", "#1a7f37", "#cf222e")
    pch_map <- ifelse(plot_df$correct, 16, 4)
    
    par(bg = "#ffffff", family = "mono", mar = c(5, 5, 2, 12), xpd = TRUE)
    
    plot(plot_df$hours_coding, plot_df$commits,
         col  = col_map, pch  = pch_map, cex  = 1.4,
         xlab = "Coding Hours", 
         ylab = "Commits",
         axes = FALSE, 
         # Ensure this is a single string
         main = paste("kNN Prediction: Hours vs. Commits"))
    
    axis(1, col = "#d0d7de"); axis(2, col = "#d0d7de")
    box(col = "#d0d7de"); grid(col = "#f0f0f0")
    
    legend("topright", inset = c(-0.17, 0),
           legend = c("Predicted Success", "Predicted Fail", "", "Correct", "Error"),
           col = c("#1a7f37", "#cf222e", NA, "#57606a", "#57606a"),
           pch = c(16, 16, NA, 16, 4), bty = "n")
  }, bg = "#ffffff")
  
  # Add Metric Card and CM Outputs (Missing from your snippet)
  output$knn_metrics <- renderUI({
    r <- knn_results(); req(r)
    
    tbl  <- r$cm$table
    tn   <- tbl[1, 1]; fp <- tbl[1, 2]
    fn   <- tbl[2, 1]; tp <- tbl[2, 2]
    
    acc        <- round(((tp + tn) / (tp + tn + fp + fn)) * 100, 1)
    sens       <- round((tp / (tp + fn)) * 100, 1)
    spec       <- round((tn / (tn + fp)) * 100, 1)
    f1   <- round((2 * tp / (2 * tp + fp + fn)) * 100, 1)
    
    div(class = "metrics-row",
        div(class = "metric-card blue",
            div(class = "metric-label", "Accuracy"),
            div(class = "metric-value", paste0(acc, "%")),
            div(class = "metric-sub",   paste0("k = ", r$best_k))
        ),
        div(class = "metric-card green",
            div(class = "metric-label", "Sensitivity"),
            div(class = "metric-value", paste0(sens, "%")),
            div(class = "metric-sub",   "True Positive Rate")
        ),
        div(class = "metric-card amber",
            div(class = "metric-label", "Specificity"),
            div(class = "metric-value", paste0(spec, "%")),
            div(class = "metric-sub",   "True Negative Rate")
        ),
        div(class = "metric-card coral",
            div(class = "metric-label", "F1 Score"),
            div(class = "metric-value", paste0(f1, "%")),
            div(class = "metric-sub",   "Harmonic Mean")
        )
    )
  })
  
  output$knn_cm <- renderUI({
    r <- knn_results(); req(r)
    cm_html(r$cm)
  })
}
shinyApp(ui, server)