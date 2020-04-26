# Shiny Finance
Shiny Project for illustrating Asset Management Principles.

The App is running from
https://o-o-o.shinyapps.io/finance/


## File structure
/app.R - in entry point for the Shiny App

/app_layout.R - define the overall app UI structure and import any necessary UI modules

/src

  /src/finance - standalone financial functions

  /func/shiny/finance/porfolio - reactive shine module for portifolio allocation and optimisation

/html - static html pages

/www - static web assets

/data - cache folder for downloaded data


## Usage
The application illustrates the key principles of portfolio optimisation.


In the sections below, we talk about portfolio theory, diversification and portfolio composition. Also, we introduce key performance measures that are later used in our backtesting.


The Portfolio allocation section allows you to chose financial instruments and build a portfolio to be simulated between the selected date range and a desired rebalancing schedule. The performance obtained is then compared to a selected benchmark.


On the Allocation Comparison section, the portfolio is compared to two possible optmised portfolios which had similar performances on the date range: a portfolio with the same return and lower risk, and a portfolio with the same risk and higher return.


IMPORTANT!: Please be informed that information in this application is provided for illustrative purposes only and does not constitute a financial advice. For more information please see the Disclaimer.
