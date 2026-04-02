library(tidyverse)
library(shiny)
library(bslib)
mcols <- c("#325C6E", "#A8E5FF", "#68A2BA", "#6E4D27", "#BA9468", "#6A3D6E", "#FAC2FF", "#B57BBA", "#4D6E32", "#98BA7B")
the_mechs <- c("Research Projects", "Research Centers", "SBIR/STTR", "Other Research-Related", "Training, Individual", "Training, Institutional", "R and D Contracts", "Other")

# read in the data
mech_data <- read.csv("data/mech_data.csv", stringsAsFactors = FALSE)
mech_data$funding_mechanism <- factor(mech_data$funding_mechanism, levels = rev(the_mechs))
the_states <- sort(unique(mech_data$organization_org_state[mech_data$organization_org_state != "All"]))
the_states <- c("All", the_states)
awdType_data <- read.csv("data/awardType_data.csv", stringsAsFactors = FALSE)
awdType_data$award_type_desc <- factor(awdType_data$award_type_desc, levels = sort(unique(awdType_data$award_type_desc), decreasing = TRUE))
nofo_data <- read.csv("data/nofo_data.csv", stringsAsFactors = FALSE)
nofo_data$nofo_type <- factor(nofo_data$nofo_type, levels = c("OTH", "OTA", "PAS", "PAR", "RFA", "PA"))
state_data <- read.csv("data/state_summary.csv", stringsAsFactors = FALSE)
theme_data <- read.csv("data/theme_data.csv", stringsAsFactors = FALSE)
theme_data$short_label <- factor(theme_data$short_label, levels = rev(c("Developmental Biology", "Reproductive Health", "Pregnancy", "Pediatrics", "Pharmacology", "Rehabilitation", "Unclassified")))
crosscut_data <- read.csv("data/crosscut_data.csv", stringsAsFactors = FALSE)
crosscut_data$short_label <- factor(crosscut_data$short_label, levels = rev(c("Data Science & AI", "Global Health", "Health Disparities", "Infectious Diseases", "Nutrition")))
orgType_data <- read.csv("data/orgType_data.csv", stringsAsFactors = FALSE)
orgType_data$organization_type_code_desc <- factor(orgType_data$organization_type_code_desc, levels = sort(unique(orgType_data$organization_type_code_desc), decreasing = TRUE))
org_data <- read.csv("data/org_data.csv", stringsAsFactors = FALSE)
org_data$org_name_clean <- factor(org_data$org_name_clean, levels = sort(unique(org_data$org_name_clean), decreasing = TRUE))
branch_theme_data <- read.csv("data/branch_theme_data.csv", stringsAsFactors = FALSE)
branch_theme_data$short_label <- factor(branch_theme_data$short_label, levels = c("Developmental Biology", "Reproductive Health", "Pregnancy", "Pediatrics", "Pharmacology", "Rehabilitation", "Unclassified"))
branch_theme_data$major_topic <- factor(branch_theme_data$major_topic, levels = sort(unique(branch_theme_data$major_topic), decreasing = TRUE))
## save as .csv file?
#us_base <- rnaturalearth::ne_states("united states of america", returnclass = "sf")
#us_base <- readRDS("data/us_base_sf_data.rds")
us_base <- sf::st_read("data/us_base_data.shp")

# create the ui
ui <- page_sidebar(
  title = "NICHD Active Extramural Awards Dashboard",
  theme = bs_theme(bootswatch = "cerulean", primary = mcols[1]),
  sidebar = sidebar(
    title = "Filter Options",
    radioButtons(
      "measure",
      label = "Measure",
      inline = TRUE,
      choices = c("Awards", "Funding"),
      selected = "Funding"
    ),
    selectInput(
      "state",
      label = "State",
      choices = the_states
    ),
    hr(),
    p("Data source: RePORTER"),
    p(paste("Data updated:", unique(state_data$run_date)))
    ## maybe budget mechanism
  ),
  navset_pill(
    nav_panel(
      title = "Overview",
      hr(),
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Quick Figures"),
          layout_column_wrap(
            width = 1/2,
            value_box(title = "Awards", value = textOutput("num_awards")),
            value_box(title = "Funding", value = textOutput("tot_fund")),
            value_box(title = "Investigators", value = textOutput("num_invest")),
            value_box(title = "Organizations", value = textOutput("num_orgs")),
          )
        ),
        card(
          card_header("Budget Mechanisms"),
          plotOutput("mech_plot")
        ),
        card(
          card_header("Award Types"),
          plotOutput("awdType_plot")
        ),
        card(
          card_header("NOFO Types"),
          plotOutput("nofo_plot")
        )
      )
    ),
    #nav_panel(
    #  title = "Investigators",
    #  hr(),
    #  "Cards"
    #),
    nav_panel(
      title = "Organizations",
      hr(),
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Organization Types"),
          plotOutput("orgType_plot")
        ),
        card(
          card_header("Top Organizations"),
          plotOutput("orgList_plot")
        )
      ),
      card(
        card_header("Funded Organization Map"),
        plotOutput("org_map")
      )
    ),
    nav_panel(
      title = "Research Topics",
      hr(),
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Strategic Plan Themes"),
          plotOutput("theme_plot")
        ),
        card(
          card_header("Crosscutting Themes"),
          plotOutput("crosscut_plot")
        )
      ),
      card(
        card_header("Strategic Plan Themes by NICHD Branch"),
        plotOutput("branch_theme_plot")
      )
    )
  ),
  id = "tab"
)

server <- function(input, output) {
  filtered_state <- reactive({
    if (input$state == "All") {
      state_data %>% 
        summarise(
          Awards = sum(Awards),
          Funding = sum(Funding),
          funded_orgs = sum(funded_orgs),
          funded_invest = sum(funded_invest)
        )
    }
    else {
      state_data %>% 
      filter(organization_org_state == input$state)
    }
  })
  output$num_awards <- renderText({
    scales::number(filtered_state()$Awards, big.mark = ",")
  })
  output$tot_fund <- renderText({
    scales::number(filtered_state()$Funding, big.mark = ",", prefix = "$", suffix = "M")
  })
  output$num_invest <- renderText({
    scales::number(filtered_state()$funded_invest, big.mark = ",")
  })
  output$num_orgs <- renderText({
    scales::number(filtered_state()$funded_orgs, big.mark = ",")
  })
  filtered_mech <- reactive({
    if (input$state == "All") {
      mech_data %>% 
        filter(measure == input$measure) %>% 
        group_by(funding_mechanism) %>% 
        summarise(
          amount = sum(amount),
          .groups = "drop"
        )
    }
    else {
      mech_data %>% 
      filter(organization_org_state == input$state, measure == input$measure)
    }
  })
  output$mech_plot <- renderPlot({
    p <- ggplot(filtered_mech(), aes(amount, funding_mechanism)) + 
      geom_col(fill = mcols[1]) + 
      labs(x = "Amount", y = NULL) + 
      theme_gray(base_size = 16) + 
      if (input$measure == "Funding") {scale_x_continuous(labels = scales::label_currency(prefix = "$", suffix = "M"))}
    p
  })
  filtered_nofo <- reactive({
    if (input$state == "All") {
      nofo_data %>% 
        filter(measure == input$measure) %>% 
        group_by(nofo_type) %>% 
        summarise(
          amount = sum(amount),
          .groups = "drop"
        )
    }
    else {
      nofo_data %>% 
        filter(organization_org_state == input$state, measure == input$measure)
    }
  })
  output$nofo_plot <- renderPlot({
    p <- ggplot(filtered_nofo(), aes(amount, nofo_type)) + 
      geom_col(fill = mcols[1]) + 
      labs(x = "Amount", y = NULL) + 
      theme_gray(base_size = 16) + 
      if (input$measure == "Funding") {scale_x_continuous(labels = scales::label_currency(prefix = "$", suffix = "M"))}
    p
  })
  filtered_awdType <- reactive({
    if (input$state == "All") {
      awdType_data %>% 
        filter(measure == input$measure) %>% 
        group_by(award_type_desc) %>% 
        summarise(
          amount = sum(amount),
          .groups = "drop"
        )
    }
    else {
      awdType_data %>% 
        filter(organization_org_state == input$state, measure == input$measure)
    }
  })
  output$awdType_plot <- renderPlot({
    p <- ggplot(filtered_awdType(), aes(amount, award_type_desc)) + 
      geom_col(fill = mcols[1]) + 
      labs(x = "Amount", y = NULL) + 
      theme_gray(base_size = 16) + 
      if (input$measure == "Funding") {scale_x_continuous(labels = scales::label_currency(prefix = "$", suffix = "M"))}
    p
  })
  filtered_theme <- reactive({
    if (input$state == "All") {
      theme_data %>% 
        filter(measure == input$measure) %>% 
        group_by(short_label) %>% 
        summarise(
          amount = sum(amount),
          .groups = "drop"
        )
    }
    else {
      theme_data %>% 
        filter(organization_org_state == input$state, measure == input$measure)
    }
  })
  output$theme_plot <- renderPlot({
    p <- ggplot(filtered_theme(), aes(amount, short_label)) + 
      geom_col(fill = mcols[1]) + 
      labs(x = "Amount", y = NULL) + 
      theme_gray(base_size = 16) + 
      if (input$measure == "Funding") {scale_x_continuous(labels = scales::label_currency(prefix = "$", suffix = "M"))}
    p
  })
  filtered_crosscut <- reactive({
    if (input$state == "All") {
      crosscut_data %>% 
        filter(measure == input$measure) %>% 
        group_by(short_label) %>% 
        summarise(
          amount = sum(amount),
          .groups = "drop"
        )
    }
    else {
      crosscut_data %>% 
        filter(organization_org_state == input$state, measure == input$measure)
    }
  })
  output$crosscut_plot <- renderPlot({
    p <- ggplot(filtered_crosscut(), aes(amount, short_label)) + 
      geom_col(fill = mcols[1]) + 
      labs(x = "Amount", y = NULL) + 
      theme_gray(base_size = 16) + 
      if (input$measure == "Funding") {scale_x_continuous(labels = scales::label_currency(prefix = "$", suffix = "M"))}
    p
  })
  filtered_branch_theme <- reactive({
    if (input$state == "All") {
      branch_theme_data %>% 
        filter(measure == input$measure) %>% 
        group_by(short_label, major_topic) %>% 
        summarise(
          amount = sum(amount),
          .groups = "drop"
        )
    }
    else {
      branch_theme_data %>% 
        filter(organization_org_state == input$state, measure == input$measure)
    }
  })
  output$branch_theme_plot <- renderPlot({
    p <- ggplot(filtered_branch_theme(), aes(short_label, major_topic, fill = amount)) + 
      geom_tile() + scale_x_discrete(position = "top", guide = guide_axis(n.dodge = 2)) + 
      labs(x = "Strategic Plan Theme", y = "NICHD Branch", fill = "Amount") + 
      theme_gray(base_size = 16) + 
      if (input$measure == "Funding") {scale_fill_viridis_c(labels = scales::label_currency(prefix = "$", suffix = "M"))}
      else {scale_fill_viridis_c()}
    p
  })
  filtered_orgType <- reactive({
    if (input$state == "All") {
      orgType_data %>% 
        filter(measure == input$measure) %>% 
        group_by(organization_type_code_desc) %>% 
        summarise(
          amount = sum(amount),
          .groups = "drop"
        )
    }
    else {
      orgType_data %>% 
        filter(organization_org_state == input$state, measure == input$measure)
    }
  })
  output$orgType_plot <- renderPlot({
    p <- ggplot(filtered_orgType(), aes(amount, organization_type_code_desc)) + 
      geom_col(fill = mcols[1]) + 
      labs(x = "Amount", y = NULL) + 
      theme_gray(base_size = 16) + 
      if (input$measure == "Funding") {scale_x_continuous(labels = scales::label_currency(prefix = "$", suffix = "M"))}
    p
  })
  filtered_orgData <- reactive({
    orgType_data %>% 
      filter(organization_org_state == input$state, measure == input$measure)
  })
  filtered_orgData_list <- reactive({
    if (input$state == "All") {
      org_data %>% 
        filter(measure == input$measure) %>% 
        group_by(org_name_clean, org_zip, INTPTLAT, INTPTLONG) %>% 
        summarise(
          amount = sum(amount),
          .groups = "drop"
        ) %>% 
        slice_max(amount, n = 15)
    }
    else {
      org_data %>% 
        filter(organization_org_state == input$state, measure == input$measure) %>% 
        slice_max(amount, n = 15)
    }
  })
  output$orgList_plot <- renderPlot({
    p <- ggplot(filtered_orgData_list(), aes(amount, org_name_clean)) + 
      geom_col(fill = mcols[1]) + 
      labs(x = "Amount", y = NULL) + 
      theme_gray(base_size = 16) + 
      if (input$measure == "Funding") {scale_x_continuous(labels = scales::label_currency(prefix = "$", suffix = "M"))}
    p
  })
  filtered_orgData_map <- reactive({
    if (input$state == "All") {
      org_data %>% 
        filter(measure == input$measure) %>% 
        group_by(org_name_clean, org_zip, INTPTLAT, INTPTLONG) %>% 
        summarise(
          amount = sum(amount),
          .groups = "drop"
        ) %>% 
        filter(is.na(INTPTLAT) == FALSE, is.na(INTPTLONG) == FALSE)
    }
    else {
      org_data %>% 
        filter(organization_org_state == input$state, measure == input$measure, is.na(INTPTLAT) == FALSE, is.na(INTPTLONG) == FALSE)
    }
  })
  output$org_map <- renderPlot({
    if (input$state == "All") {
      p2 <- ggplot(us_base) + geom_sf(color = "grey75") + coord_sf(xlim = c(-123, -69), ylim = c(25,50)) + 
      geom_point(data = filtered_orgData_map(), aes(x = INTPTLONG, y = INTPTLAT, size = amount), color = mcols[1], alpha = 0.7) + 
        labs(size = "Amount") + theme_void(base_size = 18) + 
        if (input$measure == "Funding") {scale_size_continuous(range = c(1,10), labels = scales::label_currency(prefix = "$", suffix = "M"))}
        else {scale_size_continuous(range = c(1,10))}
      p2
    }
    else {
      p2 <- ggplot(us_base[us_base$postal == input$state,]) + geom_sf(color = "grey75") +
      geom_point(data = filtered_orgData_map()[filtered_orgData_map()$organization_org_state == input$state,], aes(x = INTPTLONG, y = INTPTLAT, size = amount), color = mcols[1], alpha = 0.7) + 
        labs(size = "Amount") + theme_void(base_size = 18) + 
        if (input$measure == "Funding") {scale_size_continuous(range = c(1,10), labels = scales::label_currency(prefix = "$", suffix = "M"))}
        else {scale_size_continuous(range = c(1,10))}
      p2
    }
  })
}

# launch the app
shinyApp(ui = ui, server = server)