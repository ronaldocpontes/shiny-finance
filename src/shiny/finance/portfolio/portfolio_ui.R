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
				options = list(create = TRUE, maxItems = 50)), ),
			column(2, uiOutput("benchmark")),
			column(1),
			column(3, selectizeInput("risk_free",
				label = "Risk-Free Rate",
				choices = risk_free_choices,
				selected = risk_free_selection,
				multiple = FALSE,
				options = list(create = FALSE))) )),
		fluidRow(div(
			column(6, h4("Portfolio Allocation:", align = "left"), ),
			column(3, h4("Select Rebalance Schedule:", align = "left")),
			column(3, h4("Allocation", align = "left")))),
		fluidRow(
			column(6, uiOutput("sliders")),
			column(3,
				fluidRow(radioButtons(inputId="rebalance",
					label=NULL,
					choices=c("Monthly","Quarterly", "Annually", "Never"),
					selected = "Never")),
				fluidRow(br(),br(),br())),
			column(3, div(plotlyOutput("graph5"), align = "left", style = "height:250px"))),
		fluidRow(
			column(6,
				sliderTextInput(
					inputId = "date_range", label="", width = "95%",
					choices = analisys_period, selected = range(analisys_period),
					grid = TRUE, dragRange = FALSE),
				align = "right")),
		fluidRow(
			column(6, h4("Compound Return", align="left")),
			column(6, h4("Performance Measures", align="left"))),
		fluidRow(
			column(6, div(plotlyOutput("graph6"), align="center")),
			column(3, div(tableOutput("port_measures1"), align="center")),
			column(3, div(tableOutput("port_measures2"), align="center")))
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
				 column(6, h4("Performance Measures", align="center"))),
		fluidRow(column(6, div(plotlyOutput("graph10"), allign = "center")),
				column(6, div(tableOutput("opt_measures1"), align="center"))),
		fluidRow(column(6),
				column(6, div(tableOutput("opt_measures2"), align="center")))
		)
}

portfolio_app_how_to_use_ui = function() {
	fluidRow(column(8,div(htmlOutput("how_to_use"))))
}
