library(shinydashboard)

source('./src/shiny/finance/portfolio/portfolio_ui.R', local=TRUE)
source('./src/shiny/finance/portfolio/theory_ui.R', local=TRUE)

app_layout = function(instrument_choices, instruments_selection, risk_free_choices, risk_free_selection, analisys_period) {

  sidebarWidth = 250

  dashboardPage(skin = "black" ,
    dashboardHeader(title = "Portfolio Allocation", titleWidth = sidebarWidth),
    dashboard_Sidebar(sidebarWidth),
    dashboard_body(instrument_choices, instruments_selection, risk_free_choices, risk_free_selection, analisys_period)
  )
}

dashboard_Sidebar = function(width) {
  dashboardSidebar(width = width,
    sidebarUserPanel("Ron Pontes", image = "avatar.png"),
    sidebarMenu(
      menuItem("Portfolio", tabName = "portfolio", icon = icon("line-chart"),
               menuSubItem("Portfolio Allocation", tabName = "portfolio_allocation"),
               menuSubItem("Allocation Comparison", tabName = "portfolio_optmisation")

               ),
      menuItem("Theory", tabName = "theory", icon = icon("graduation-cap"),
               menuSubItem("How to use the App", tabName = "how_to_use", icon = icon("book")),
               menuSubItem("Risk/Return Ratio", tabName = "risk_return_theory"),
               menuSubItem("Optimal Portfolio", tabName = "efficient_frontier_theory"),
               menuSubItem("Performance Measures", tabName = "performance_measures")
               ),
      menuItem("The Author", tabName = "author", icon = icon("user")),
      menuItem("Disclaimers", tabName = "discl", icon = icon("exclamation-triangle")))
  )
}

dashboard_body = function(instrument_choices, instruments_selection, risk_free_choices, risk_free_selection, analisys_period) {
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tabItems(
      tabItem(tabName = "portfolio_allocation",
        portfolio_allocation_ui(instrument_choices, instruments_selection, risk_free_choices, risk_free_selection, analisys_period)),

      tabItem(tabName = "portfolio_optmisation",       portfolio_optmisation_ui() ),

      tabItem(tabName = "how_to_use",                  portfolio_app_how_to_use_ui() ),
      tabItem(tabName = "risk_return_theory",          portfolio_risk_return_theory_ui() ),
      tabItem(tabName = "efficient_frontier_theory",   portfolio_efficient_frontier_theory_ui() ),
      tabItem(tabName = "performance_measures",        portfolio_performance_measures_ui() ),

      tabItem(tabName = "author",                      htmlOutput("author")),
      tabItem(tabName = "discl",                       htmlOutput("disclaimer")))
  )
}
