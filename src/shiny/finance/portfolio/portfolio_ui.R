library(plotly)
library(DT)
library(shiny)
library(shinyWidgets)

portfolio_allocation_ui = function(instrument_choices, instruments_selection, risk_free_choices, risk_free_selection, analisys_period) {
	tagList(
		fluidRow(div(
			column(6, selectizeInput( "tickers",
				label = "Instruments: (select or type a new one)",
				choices = instrument_choices,
				selected = instruments_selection,
				multiple = TRUE,
				options = list(create = TRUE, maxItems = 50)),				),
			column(2, uiOutput("benchmark")),
			column(3, selectizeInput("risk_free",
				label = "Risk-Free Rates",
				choices = risk_free_choices,
				selected = risk_free_selection,
				multiple = FALSE,
				options = list(create = FALSE))) )),
		fluidRow(div(
			column(6, h4("Portfolio Allocation:", align = "left"),                              ),
			column(3, h4("Select Rebalance Schedule:", align = "left")),
			column(3, h4("Allocation", align = "center")))),
		fluidRow(
			column(6, uiOutput("sliders")),
			column(3,
				fluidRow(radioButtons(inputId="rebalance",
					label=NULL,
					choices=c("Monthly","Quarterly", "Annually", "Never"),
					selected = "Never")),
				fluidRow(br(),br(),br())),
			column(3, div(plotlyOutput("graph5"), align = "center", style = "height:250px"))),
		fluidRow(column(12, div(
					sliderTextInput(
						inputId = "date_range", label = h4("Time interval:"), width = "80%",
						choices = analisys_period, selected = range(analisys_period),
						grid = TRUE, dragRange = FALSE),
					align = "center"))),
		fluidRow(column(6, h4("Compound Return", align="center")),
			 column(6, h4("Performance Measures", align="center"))),
		fluidRow(column(6, div(plotlyOutput("graph6"), align="center")),
			 column(6, div(tableOutput("bt_table1"), align="center")))
	)
}

portfolio_optmisation_ui = function() {
	tagList(
		fluidRow(column(4, h4("Your Allocation", align="center")),
				column(4, h4("Similar Return", align="center")),
				column(4, h4("Similar Risk", align="center"))
				),
		fluidRow(column(4, div(plotlyOutput("graph7"), align="center")),
				column(4, div(plotlyOutput("graph8"), align="center")),
				column(4, div(plotlyOutput("graph9"), align="center"))
				),
		fluidRow(column(6, h4("Compound Return", align = "center")),
				column(6, h4("Performance Measures", align="center"))
			),
		fluidRow(column(6, div(plotlyOutput("graph10"), allign = "center")),
				column(6, div(br(),tableOutput("bt_table2"), align="center"))
				)
		)
}

portfolio_app_how_to_use_ui = function() {
	fluidRow(column(8,div(htmlOutput("how_to_use"))))
}

portfolio_risk_return_theory_ui = function() {
	fluidPage(h1("Risk/Return Ratio"),
		p("In 1952 Harry Markowitz suggested that assets should be evaluated based on their risk/return ratio.
			For the purposes of this app, I look at the asset returns measured by corresponding indices from 2000
			to the present day. "),
		p("The assets are:"),
			p(em("Equities:")),
				tags$div(tags$ul(
					tags$li("S&P 500"),
					tags$li("MSCI Europian Stock Index"),
					tags$li("MSCI Emerging Market Stock Index"))),
			p(em("Bonds:")),
				tags$div(tags$ul(
					tags$li("Barclays US Treasury Total Return Index"),
					tags$li("Barclays US Corporate Bonds Total Return Index"))),
			p(em("Real Estate:")),
				tags$div(tags$ul(
					tags$li("Dow Jones Real Estate Index"))),
		tabsetPanel(
			tabPanel("Whole Period", br(), plotlyOutput("graph1")),
			tabPanel("By Years",  plotlyOutput("graph2")),
			tabPanel("Compound Return",  plotlyOutput("graph3")))
	)
}

portfolio_efficient_frontier_theory_ui = function() {
	fluidPage(
		fluidRow(column(6,h1("Optimal portfolio"),
			p("Asset returns are not perferctly correlated. Therefore, we can combine assets into portfolios, and harverst
				the results of the diversification."),
			p("However, diversification is not limitless. For each expected risk there will be a portfolio with
				a maximum achievable return.The graph below shows risk/return profiles of simulated portfolios (gray) and
				a line (blue) depicting portfolios offering highest return for a given risk."),
			p("In Harry Markowitz (1952) framework, such line is called the Efficient Frontier. However, Markowitz' theory
				assumes that investors hold long-short portfolios. In our analysis, we limit ourselves to long-only portfolios,
				as it is the type of portfolios retail investors usually hold. Therefore, we will refer to portfolios on this line as
				'Optimal Portfolios', and the line itself as the 'Optimal Line'."),
			br(),
			plotlyOutput("graph4")))
	)
}

portfolio_performance_measures_ui = function() {
	fluidRow(column(8,div(htmlOutput("measures"))))
}
