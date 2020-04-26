## Welcome to GitHub Pages

You can use the [editor on GitHub](https://github.com/Bcogan0/covid_tr/edit/master/README.md) to maintain and preview the content for your website in Markdown files.

Whenever you commit to this repository, GitHub Pages will run [Jekyll](https://jekyllrb.com/) to rebuild the pages in your site, from the content in your Markdown files.

### Markdown

Markdown is a lightweight and easy-to-use syntax for styling your writing. It includes conventions for

```markdown
---
title: "Covid-19 Dashboard Turkey"
author: "by Bcogan"
output: 
  flexdashboard::flex_dashboard:
    orientation: rows
    social: ["facebook", "twitter", "linkedin"]
   # source_code: embed
    vertical_layout: fill
---

```{r setup, include=FALSE}
#------------------ Packages ------------------
library(flexdashboard)
library(devtools)
#checks if there is data update on the Github version
coronavirus::update_datasets(silence = TRUE)
# install.packages("devtools")
#devtools::install_github("RamiKrispin/coronavirus", force = TRUE)
library(coronavirus)
library(readxl)
library(deSolve)
data(coronavirus)
#update_datasets()
# View(coronavirus)
max(coronavirus$date)

`%>%` <- magrittr::`%>%`
#------------------ Parameters ------------------
# Set colors
# https://www.w3.org/TR/css-color-3/#svg-color
confirmed_color <- "blue"
active_color <- "#1f77b4"
recovered_color <- "forestgreen"
death_color <- "red"
#------------------ Data ------------------
df <- coronavirus %>%
  # dplyr::filter(date == max(date)) %>%
  dplyr::filter(Country.Region == "Turkey") %>%
  dplyr::group_by(Country.Region, type) %>%
  dplyr::summarise(total = sum(cases)) %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = total
  ) %>%
  # dplyr::mutate(unrecovered = confirmed - ifelse(is.na(recovered), 0, recovered) - ifelse(is.na(death), 0, death)) %>%
  dplyr::mutate(unrecovered = confirmed - ifelse(is.na(death), 0, death)) %>%
  dplyr::arrange(-confirmed) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(country = dplyr::if_else(Country.Region == "United Arab Emirates", "UAE", Country.Region)) %>%
  dplyr::mutate(country = dplyr::if_else(country == "Mainland China", "China", country)) %>%
  dplyr::mutate(country = dplyr::if_else(country == "North Macedonia", "N.Macedonia", country)) %>%
  dplyr::mutate(country = trimws(country)) %>%
  dplyr::mutate(country = factor(country, levels = country))


df_daily <- coronavirus %>%
  dplyr::filter(Country.Region == "Turkey") %>%
  dplyr::group_by(date, type) %>%
  dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = total
  ) %>%
  dplyr::arrange(date) %>%
  dplyr::ungroup() %>%
  #dplyr::mutate(active = confirmed - death - recovered) %>%
  dplyr::mutate(active = confirmed - death) %>%
  dplyr::mutate(
    confirmed_cum = cumsum(confirmed),
    death_cum = cumsum(death),
    # recovered_cum = cumsum(recovered),
    active_cum = cumsum(active),
    id = row_number()
  )
df_daily <- df_daily %>%
  dplyr::filter(id > 44)

df1 <- coronavirus %>% dplyr::filter(date == max(date))
#pr <- read_excel("predict2.xlsx")
pr <- read_excel("predict3.xlsx")
distance3 <- read_excel("distance3.xlsx")
predict33 <- read_excel("predict33.xlsx")
distance6 <- read_excel("distance6.xlsx")
thirtyfirst <- read_excel("thirtyfirst.xlsx")
testnr <- read_excel("testnr.xlsx")
table_tr <- read_excel("table_tr.xlsx")
table_tr <- table_tr%>%
   dplyr::filter(id > 7)
```

OVERVIEW (TR)
=======================================================================

Row {data-width=400}
-----------------------------------------------------------------------

### confirmed {.value-box}

```{r}

valueBox(
  value = paste(format(sum(df$confirmed), big.mark = ","), "", sep = " "),
  #value = 13531,
  caption = "Total confirmed cases",
  icon = "fas fa-user-md",
  color = confirmed_color
)
```




### death {.value-box}

```{r}

valueBox(
  value = paste(format(sum(df$death, na.rm = TRUE), big.mark = ","), " (",
    round(100 * sum(df$death, na.rm = TRUE) / sum(df$confirmed), 1),
    "%)",
    sep = ""),
  #value = 214,
  caption = "Deaths (Case fatality rate)",
  icon = "fas fa-bed",
  color = death_color
)
```




### inf {.value-box}

```{r}

valueBox(
  value = paste(format(round(2*(sum(df$confirmed)*(1.25)^5)), big.mark = ","), " (",
    round((sum(df$confirmed)*100/(2*(sum(df$confirmed)*(1.25)^5))), 1),
    "%)",
    sep = ""),
  caption = "Estimated number of infected (Detection rate)",color = "gray",
  icon = "far fa-sun"
  
)
```




### Ifr {.value-box}

```{r}

valueBox(
  value = paste(format(round(100 * sum(df$death, na.rm = TRUE) / ((2*sum(df$confirmed))*(1.25)^5), 1), big.mark = ","),
  "%", sep = ""),
  caption = "Infection fatality rate",color = "red",
  icon = "fas fa-stethoscope"
)
```


Row
-----------------------------------------------------------------------

### **Daily cumulative cases by type** (Turkey only)
    
```{r}
plotly::plot_ly(data = df_daily) %>%
  plotly::add_trace(
    x = ~date,
    y = ~death_cum,
    type = "bar",
    mode = "markers",
    name = "Cumulative death", 
    line = list(color = death_color),
    marker = list(color = death_color)
  ) %>%
  plotly::add_trace(
    x = table_tr$date,
    y = table_tr$new_test,
    type = "bar",
    #mode = "markers",
    name = "Daily number of tests", text=table_tr$new_test, textposition = 'auto',
    line = list(color = "orange", dash="dash"),
    marker = list(color = "orange", size=3)
  ) %>%
  plotly::add_trace(
    x = predict33$date,
    y = predict33$confirmed_cum,
    type = "scatter",
    mode = "lines+markers",
    name = "*Predicted cases",
    line = list(color = "black", dash="dash"),
    marker = list(color = "black", size=3)
  ) %>%
  plotly::add_trace(
    x = distance3$date,
    y = distance3$confirmed_cum,
    type = "scatter",
    mode = "lines+markers",
    name = "50% less social exposure",
    line = list(color = "gray", dash="dash"),
    marker = list(color = "gray", size=3)
  ) %>%
  # plotly::add_trace(
  #   x = distance6$date,
  #   y = distance6$confirmed_cum,
  #   type = "scatter",
  #   mode = "lines+markers",
  #   name = "75% less social exposure",
  #   line = list(color = "green", dash="dash"),
  #   marker = list(color = "green", size=3)
  # ) %>%
  plotly::add_trace(
    x = ~date,
    # y = ~active_cum,
    y = ~confirmed_cum,
    type = "scatter",
    mode = "lines+markers",
    # name = "Active",
    name = "Confirmed cases",
    line = list(color = active_color),
    marker = list(color = active_color)
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-03-11"),
    y = 1,
    text = paste("First case"),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = -90,
    ay = -90
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-03-17"),
    y = 3,
    text = paste("First death"),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = -90,
    ay = -90
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-04-07"),
    y = 31833,
    text = paste("Apr 7:","Confirmed=34109","Predicted=31833",sep="<br>"),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = -10,
    ay = -90
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-04-01"),
    y = 15679,
    text = paste("Apr 1:","Confirmed=15679","Predicted=15290",sep="<br>"),
    xref = "x",
    yref = "y",
    arrowhead = 5,
    arrowhead = 3,
    arrowsize = 1,
    showarrow = TRUE,
    ax = -10,
    ay = -90
  # ) %>%
  # plotly::add_annotations(
  #   x = as.Date("2020-03-20"),
  #   y = 130000,
  #   text = paste("Approximate number of lives saved between 31.March-18.April thanks to restrictions and measures = 850"),
  #   xref = "x",
  #   yref = "y",
  #   arrowhead = 5,
  #   arrowhead = 3,
  #   arrowsize = 1,
  #   showarrow = FALSE,
  #   ax = -10,
  #   ay = -90
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-04-19"),
    y = 122107,
    text = paste("Apr 18:","Confirmed=82329","Predicted=122107",sep="<br>"),
    xref = "x",
    yref = "y",
    showarrow=FALSE
  ) %>%
  plotly::add_annotations(
    x = as.Date("2020-03-20"),
    y = 120000,
    showarrow=FALSE,
    text = paste(
      "*Predictions:","","calculated","","on","","31st March",
      "","based","","on","","the","","growth","","trend","","between","","25-31.March"
    ),
    xref = "x",
    yref = "y"
  ) %>%
  plotly::layout(
    title = "",
    yaxis = list(title = ""),
    xaxis = list(title = "Date",dtick=86400000*7),
    legend = list(x = 0.1, y = 0.9), barmode='stack',
    hovermode = "compare"
  )


# #SIR
# N <- 83154997 # population
# old <- par(mfrow = c(1, 2))
# 
# Infected <- c(1, 1,5  ,   5 ,    6  ,  18  ,  47   , 98 ,  192 ,  359 ,  670 , 1236 , 1529,  1872 , 2433 , 3629 , 5698,7402, 9217, 10827 ,13531 ,15679 ,18135, 20921, 23934, 27069 ,30217, 34109 ,38226, 42282, 47029, 52167)
# #Infected <- c(16,17,27,46,48,79,130  ,  159 ,   196,    262  ,  482 ,   670 ,   799 ,  1040  , 1176,
# #   1457 ,  1908 ,  2078 ,  3675,4585,5795,7272 ,  9257,12327,15320 , 19848,  22213,  24873,  29056,  32986,
# #  37323,43938 , 50871,57695 , 62095 , 66885 , 71808,77872 , 84794,91159 , 96092, 100123 ,103374, 107663, 113296,
# #   118181,122171,124908)
# date_de <- 1:(length(Infected))
# 
# 
# #Function
# SIR <- function(time, state, parameters) {
#   par <- as.list(c(state, parameters))
#   with(par, {
#     dS <- -beta/N * I * S
#     dI <- beta/N * I * S - gamma * I
#     dR <- gamma * I
#     list(c(dS, dI, dR))
#   })
# }
# 
# init <- c(S = N-Infected[1], I = Infected[1], R = 0)
# RSS <- function(parameters) {
#   names(parameters) <- c("beta", "gamma")
#   out <- ode(y = init, times = date_de, func = SIR, parms = parameters)
#   fit <- out[ , 3]
#   sum((Infected - fit)^2)
# }
# 
# Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1)) # optimize with some sensible conditions
# 
# Opt_par <- setNames(Opt$par, c("beta", "gamma"))
# 
# t <- 1:100 # time in days
# fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
# col <- 1:3 # colour
# 
# 
# matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Days passed after 11th of March", ylab = "Number of subjects", lwd = 2, lty = 1, col = col)
# matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Days passed after 11th of March", ylab = "Number of subjects", lwd = 2, lty = 1, col = col, log = "y")
# ## Warning in xy.coords(x, y, xlabel, ylabel, log = log): 1 y value <= 0
# ## omitted from logarithmic plot
# 
# points(date_de, Infected)
# #legend("bottomright", c("Susceptibles", "Infecteds", "Recovereds"))
# title("SIR model Covid-19 Turkey", outer = TRUE, line = -2)
# 
# 
# par(old)
# 
# R0 <- setNames(Opt_par["beta"] / Opt_par["gamma"], "R0")
# 
# #fit[fit$I == max(fit$I), "I", drop = FALSE] # height of pandemic
# 
# max_infected <- max(fit$I)
```

SIR MODEL
=======================================================================

Column {data-width=400}
-------------------------------------

### **SIR Turkey: Based on the data from 1st of April to 11th of April **
    
```{r}
#SIR
N <- 83154997 # population
old <- par(mfrow = c(1, 2))

Infected <- c(15679 ,18135, 20921, 23934, 27069 ,30217, 34109 ,38226, 42282, 47029, 52167)

date_de <- 1:(length(Infected))


#Function
SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/N * I * S
    dI <- beta/N * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

init <- c(S = N-Infected[1], I = Infected[1], R = 0)
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = date_de, func = SIR, parms = parameters)
  fit <- out[ , 3]
  sum((Infected - fit)^2)
}

Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1)) # optimize with some sensible conditions

Opt_par <- setNames(Opt$par, c("beta", "gamma"))

t <- 1:100 # time in days
fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
col <- 1:3 # colour


matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Days passed after 1st of April", ylab = "Number of subjects", lwd = 2, lty = 1, col = col)
matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Days passed after 1st of April", ylab = "Number of subjects", lwd = 2, lty = 1, col = col, log = "y")
## Warning in xy.coords(x, y, xlabel, ylabel, log = log): 1 y value <= 0
## omitted from logarithmic plot

points(date_de, Infected)
legend("bottomleft",inset=c(0.3,1.02),xpd=TRUE, c("Susceptible", "Infected", "Recovered"),lty = 1, lwd = 2, col = col,cex = 0.75)
title("SIR model Covid-19 Turkey", outer = TRUE, line = -2)


par(old)

R0 <- setNames(Opt_par["beta"] / Opt_par["gamma"], "R0")

#fit[fit$I == max(fit$I), "I", drop = FALSE] # height of pandemic

max_infected <- max(fit$I)
max_infected = max_infected + 15679

```


### **SIR Model results for Turkey**

An SIR model is an epidemiological model that computes the theoretical number of people infected with a contagious illness in a closed population over time. The model divides the population into compartments: Susceptible, Infectious, Recovered. Between S and I, the transition rate βI/N, where β is the average number of contacts per person per time, multiplied by the probability of disease transmission in a contact between a susceptible and an infectious subject, and I/N is the fraction of contact occurrences that involve an infectious individual.Between I and R, the transition rate is γ (simply the rate of recovery or mortality,that is, γ = 1/D, where D is the duration of the infection. This analysis is done for the purpose of learning the basics of SIR modelling and the dynamics of the spread of a pandemic. See more details [here](https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology){target="_blank"} and for its example for covid-19 [here](https://www.lewuathe.com/covid-19-dynamics-with-sir-model.html){target="_blank"}. 
![Compartments.](C:/Users/baris/OneDrive/Documents/R/Dashboard/sir.png)
<!-- ![Differential equations](C:/Users/baris/OneDrive/Documents/R/Dashboard/sir2.png) -->

**Data and assumptions**

The analysis is done on 12th of April. The covid-19 case data used in this modelling are from 1st of April until the 11th of April.(The raw data is pulled from Johns Hopkins University Center for Systems Science and Engineering).

Population of Turkey: 83154997 (TUİK)

 Mild symptomps= 80.9%, Severe cases= 13.8%, Intensive care= 4.7%. (According to this [source](https://www.statista.com/chart/20856/coronavirus-case-severity-in-china/){target="_blank"};)

The case fatality rate= 2.1%, the infection fatality rate = 0.3%. (See About tab for the estimation).


**Model statistics**
```{r}
  cat("- Peak of the pandemic:", "Middle of May", sep = " ")
  cat("- Ro:", R0, sep = " ")
  cat("- Maximum number of infected:", round(max_infected), sep = " ")
  cat("- Total severe cases:", round(max_infected / 7.24), sep = " ")
  cat("- Total number of cases in intensive care:", round(max_infected*0.047), sep = " ")
  cat("- Total deaths:", round(max_infected * 0.021 + 277), sep = " ")

```

TREND
=======================================================================

 Column {data-width=400}
-------------------------------------


### **Case growth after 100 cases on the logarithmic scale **


```{r}
comp_de <- coronavirus %>%
  dplyr::filter(Country.Region == "Germany") %>%
  dplyr::group_by(date, type) %>%
  dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = total
  ) %>%
  dplyr::arrange(date) %>%
  dplyr::ungroup() %>%
  #dplyr::mutate(active = confirmed - death - recovered) %>%
  dplyr::mutate(active = confirmed - death) %>%
  dplyr::mutate(
    confirmed_cum_de = cumsum(confirmed),
    death_cum_de = cumsum(death),
    # recovered_cum = cumsum(recovered),
    active_cum_de = cumsum(active),
    id = row_number()
  )
comp_it <- coronavirus %>%
  dplyr::filter(Country.Region == "Italy") %>%
  dplyr::group_by(date, type) %>%
  dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = total
  ) %>%
  dplyr::arrange(date) %>%
  dplyr::ungroup() %>%
  #dplyr::mutate(active = confirmed - death - recovered) %>%
  dplyr::mutate(active = confirmed - death) %>%
  dplyr::mutate(
    confirmed_cum_it = cumsum(confirmed),
    death_cum_it = cumsum(death),
    # recovered_cum = cumsum(recovered),
    active_cum_it = cumsum(active),
    id = row_number()
  )
comp_us <- coronavirus %>%
  dplyr::filter(Country.Region == "US") %>%
  dplyr::group_by(date, type) %>%
  dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = total
  ) %>%
  dplyr::arrange(date) %>%
  dplyr::ungroup() %>%
  #dplyr::mutate(active = confirmed - death - recovered) %>%
  dplyr::mutate(active = confirmed - death) %>%
  dplyr::mutate(
    confirmed_cum_us = cumsum(confirmed),
    death_cum_us = cumsum(death),
    # recovered_cum = cumsum(recovered),
    active_cum_us = cumsum(active),
    id = row_number()
  )
comp_fr <- coronavirus %>%
  dplyr::filter(Country.Region == "France") %>%
  dplyr::group_by(date, type) %>%
  dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = total
  ) %>%
  dplyr::arrange(date) %>%
  dplyr::ungroup() %>%
  #dplyr::mutate(active = confirmed - death - recovered) %>%
  dplyr::mutate(active = confirmed - death) %>%
  dplyr::mutate(
    confirmed_cum_fr = cumsum(confirmed),
    death_cum_fr = cumsum(death),
    # recovered_cum = cumsum(recovered),
    active_cum_fr = cumsum(active),
    id = row_number()
  )
comp_sp <- coronavirus %>%
  dplyr::filter(Country.Region == "Spain") %>%
  dplyr::group_by(date, type) %>%
  dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = total
  ) %>%
  dplyr::arrange(date) %>%
  dplyr::ungroup() %>%
  #dplyr::mutate(active = confirmed - death - recovered) %>%
  dplyr::mutate(active = confirmed - death) %>%
  dplyr::mutate(
    confirmed_cum_sp = cumsum(confirmed),
    death_cum_sp = cumsum(death),
    # recovered_cum = cumsum(recovered),
    active_cum_sp = cumsum(active),
    id = row_number()
  )
comp_jp <- coronavirus %>%
  dplyr::filter(Country.Region == "Japan") %>%
  dplyr::group_by(date, type) %>%
  dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = total
  ) %>%
  dplyr::arrange(date) %>%
  dplyr::ungroup() %>%
  #dplyr::mutate(active = confirmed - death - recovered) %>%
  dplyr::mutate(active = confirmed - death) %>%
  dplyr::mutate(
    confirmed_cum_jp = cumsum(confirmed),
    death_cum_jp = cumsum(death),
    # recovered_cum = cumsum(recovered),
    active_cum_jp = cumsum(active),
    id = row_number()
  )
#Cases above 100
comp_de_c <- comp_de %>%
  dplyr::filter(confirmed_cum_de > 100)%>%
  dplyr::mutate(id=row_number())
df_daily_c <- df_daily %>%
  dplyr::filter(confirmed_cum > 100)%>%
  dplyr::mutate(id=row_number())
comp_it_c <- comp_it %>%
  dplyr::filter(confirmed_cum_it > 100)%>%
  dplyr::mutate(id=row_number())
comp_us_c <- comp_us %>%
  dplyr::filter(confirmed_cum_us > 100)%>%
  dplyr::mutate(id=row_number())
comp_fr_c <- comp_fr %>%
  dplyr::filter(confirmed_cum_fr > 100)%>%
  dplyr::mutate(id=row_number())
comp_sp_c <- comp_sp %>%
  dplyr::filter(confirmed_cum_sp > 100)%>%
  dplyr::mutate(id=row_number())
comp_jp_c <- comp_jp %>%
  dplyr::filter(confirmed_cum_jp > 100)%>%
  dplyr::mutate(id=row_number())
#Trend lines
days = c(1:12)
daily = c(100,200,400,800,1600,3200,6400, 12800, 25600, 51200, 102400, 204800)
days_2 = c(2,4,6,8,10,12,14,16,18,20,22,24)
#twodays = c(100,150,200,300,400,600,800,1200,1600,2400,3200,4800,6400,9600,12800,19200,25600,38400,51200,76800,102400,153600 )
daily_seven = c(100,200,400,800,1600,3200,6400)
days_seven = c(1,7,14,21,28,35,42)


plotly::plot_ly(data = df_daily_c) %>%
    plotly::add_trace(x = ~id,
      y = ~log(confirmed_cum),
      type = "scatter",
      mode = "lines+markers",
      name = "Turkey",
      line = list(color = "red"),
      marker = list(color = "red", size=3)
      ) %>%
    plotly::add_trace(
      x = comp_de_c$id,
      y = ~log(comp_de_c$confirmed_cum_de),
      type = "scatter",
      mode = "lines+markers",
      name = "Germany",
      line = list(color = "black"),
      marker = list(color = "black", size=3)
    ) %>%
  plotly::add_trace(
      x = comp_it_c$id,
      y = ~log(comp_it_c$confirmed_cum_it),
      type = "scatter",
      mode = "lines+markers",
      name = "Italy",
      line = list(color = "green"),
      marker = list(color = "green", size=3)
    ) %>%
    plotly::add_trace(
      x = comp_us_c$id,
      y = ~log(comp_us_c$confirmed_cum_us),
      type = "scatter",
      mode = "lines+markers",
      name = "US",
      line = list(color = "blue"),
      marker = list(color = "blue", size=3)
    ) %>%
  plotly::add_trace(
      x = comp_fr_c$id,
      y = ~log(comp_fr_c$confirmed_cum_fr),
      type = "scatter",
      mode = "lines+markers",
      name = "France",
      line = list(color = "cyan"),
      marker = list(color = "cyan", size=3)
    ) %>%
  plotly::add_trace(
      x = comp_sp_c$id,
      y = ~log(comp_sp_c$confirmed_cum_sp),
      type = "scatter",
      mode = "lines+markers",
      name = "Spain",
      line = list(color = "orange"),
      marker = list(color = "orange", size=3)
    ) %>%
  plotly::add_trace(
      x = comp_jp_c$id,
      y = ~log(comp_jp_c$confirmed_cum_jp),
      type = "scatter",
      mode = "lines+markers",
      name = "Japan",
      line = list(color = "pink"),
      marker = list(color = "pink", size=3)
    ) %>%
   plotly::add_trace(
       x = days,
       y = log(daily),
       type = "scatter",
       mode = "lines",
       showlegend = FALSE,
       name = "doubles every day",
       line = list(color = "gray", dash="dash")
     ) %>%
  plotly::add_trace(
       x = days_2,
       y = log(daily),
       type = "scatter",
       mode = "lines",
       showlegend = FALSE,
       name = "doubles every two days",
       line = list(color = "purple", dash="dash")
     ) %>%
  plotly::add_trace(
       x = days_seven,
       y = log(daily_seven),
       type = "scatter",
       mode = "lines",
       showlegend = FALSE,
       name = "doubles every seven days",
       line = list(color = "brown", dash="dash")
     ) %>%
   plotly::add_annotations(
     x = 10,
     y = 1.1,
     text = paste("doubles every day"),
     xref = "x",
     yref = "y",
     showarrow = FALSE,
     font=list(size=9,color = "gray")
   ) %>%
  plotly::add_annotations(
     x = 24,
     y = 1.1,
     text = paste("doubles every two days"),
     xref = "x",
     yref = "y",
     showarrow = FALSE,
     font=list(size=9,color = "purple")
   ) %>%
  plotly::add_annotations(
     x = 36,
     y = 0.95,
     text = paste("doubles every seven days"),
     xref = "x",
     yref = "y",
     showarrow = FALSE,
     font=list(size=9,color = "brown")
   ) %>%
  plotly::add_annotations(
     x = 16,
     y = 1.01,
     text = paste("TR"),
     xref = "x",
     yref = "y",
     showarrow = FALSE,
     font=list(size=9,color = "red")
   ) %>%
  plotly::add_annotations(
     x = 32,
     y = 1.11,
     text = paste("US"),
     xref = "x",
     yref = "y",
     showarrow = FALSE,
     font=list(size=9,color = "blue")
   ) %>%
  plotly::add_annotations(
     x = 34,
     y = 1.087,
     text = paste("SP"),
     xref = "x",
     yref = "y",
     showarrow = FALSE,
     font=list(size=9,color = "orange")
   ) %>%
  plotly::add_annotations(
     x = 36,
     y = 1.07,
     text = paste("GER"),
     xref = "x",
     yref = "y",
     showarrow = FALSE,
     font=list(size=9,color = "black")
   ) %>%
  plotly::add_annotations(
     x = 36,
     y = 1.05,
     text = paste("FR"),
     xref = "x",
     yref = "y",
     showarrow = FALSE,
     font=list(size=9,color = "cyan")
   ) %>%
  plotly::add_annotations(
     x = 43,
     y = 1.07,
     text = paste("IT"),
     xref = "x",
     yref = "y",
     showarrow = FALSE,
     font=list(size=9,color = "green")
   ) %>%
  plotly::add_annotations(
     x = 44,
     y = 0.92,
     text = paste("JP"),
     xref = "x",
     yref = "y",
     showarrow = FALSE,
     font=list(size=9,color = "pink")
   ) %>%
  plotly::layout(
      title = "",
      yaxis = list(type = "log", title = "Confirmed cases", ticktext=c("0","150","400","1.000","3.000","8.000","20.000","60.000","160.000","400.000"), tickvals=c(0,5.01,5.991,6.907,8,8.987,9.90,11.002,11.9,12.89)),
      xaxis = list(title = "Days passed after 100 cases", tick0=0, dtick=3),
      legend = list(x = 0.65, y = 0.03)
      #hovermode = "compare"
      )


```


### **Death growth after 5 deaths on the logarithmic scale **
    
```{r}
#Deaths above 5
comp_de_d <- comp_de %>%
  dplyr::filter(death_cum_de > 5)%>%
  dplyr::mutate(id=row_number())
df_daily_d <- df_daily %>%
  dplyr::filter(death_cum > 5)%>%
  dplyr::mutate(id=row_number())
comp_it_d <- comp_it %>%
  dplyr::filter(death_cum_it > 5)%>%
  dplyr::mutate(id=row_number())
comp_us_d <- comp_us %>%
  dplyr::filter(death_cum_us > 5)%>%
  dplyr::mutate(id=row_number())
comp_fr_d <- comp_fr %>%
  dplyr::filter(death_cum_fr > 5)%>%
  dplyr::mutate(id=row_number())
comp_sp_d <- comp_sp %>%
  dplyr::filter(death_cum_sp > 5)%>%
  dplyr::mutate(id=row_number())
comp_jp_d <- comp_jp %>%
  dplyr::filter(death_cum_jp > 5)%>%
  dplyr::mutate(id=row_number())
#Trend lines
days = c(1:12)
daily = c(5,10,20,40,80,160,320, 640, 1280, 2560, 5120, 10240)
days_2 = c(1,3,5,7,9,11,13,15,17,19,21,23)
daily_seven = c(5,10,20,40,80,160,320,640)
days_seven = c(1,8,15,22,29,36,43,50)


plotly::plot_ly(data = df_daily_d) %>%
    plotly::add_trace(x = ~id,
      y = ~log(death_cum),
      type = "scatter",
      mode = "lines+markers",
      name = "Turkey",
      line = list(color = "red"),
      marker = list(color = "red", size=3)
      ) %>%
    plotly::add_trace(
      x = comp_de_d$id,
      y = ~log(comp_de_d$death_cum_de),
      type = "scatter",
      mode = "lines+markers",
      name = "Germany",
      line = list(color = "black"),
      marker = list(color = "black", size=3)
    ) %>%
  plotly::add_trace(
      x = comp_it_d$id,
      y = ~log(comp_it_d$death_cum_it),
      type = "scatter",
      mode = "lines+markers",
      name = "Italy",
      line = list(color = "green"),
      marker = list(color = "green", size=3)
    ) %>%
    plotly::add_trace(
      x = comp_us_d$id,
      y = ~log(comp_us_d$death_cum_us),
      type = "scatter",
      mode = "lines+markers",
      name = "US",
      line = list(color = "blue"),
      marker = list(color = "blue", size=3)
    ) %>%
  plotly::add_trace(
      x = comp_fr_d$id,
      y = ~log(comp_fr_d$death_cum_fr),
      type = "scatter",
      mode = "lines+markers",
      name = "France",
      line = list(color = "cyan"),
      marker = list(color = "cyan", size=3)
    ) %>%
  plotly::add_trace(
      x = comp_sp_d$id,
      y = ~log(comp_sp_d$death_cum_sp),
      type = "scatter",
      mode = "lines+markers",
      name = "Spain",
      line = list(color = "orange"),
      marker = list(color = "orange", size=3)
    ) %>%
  plotly::add_trace(
      x = comp_jp_d$id,
      y = ~log(comp_jp_d$death_cum_jp),
      type = "scatter",
      mode = "lines+markers",
      name = "Japan",
      line = list(color = "pink"),
      marker = list(color = "pink", size=3)
    ) %>%
   plotly::add_trace(
       x = days,
       y = log(daily),
       type = "scatter",
       mode = "lines",
       showlegend = FALSE,
       name = "doubles every day",
       line = list(color = "gray", dash="dash")
     ) %>%
  plotly::add_trace(
       x = days_2,
       y = log(daily),
       type = "scatter",
       mode = "lines",
       showlegend = FALSE,
       name = "doubles every two days",
       line = list(color = "purple", dash="dash")
     ) %>%
  plotly::add_trace(
       x = days_seven,
       y = log(daily_seven),
       type = "scatter",
       mode = "lines",
       showlegend = FALSE,
       name = "doubles every seven days",
       line = list(color = "brown", dash="dash")
     ) %>%
   plotly::add_annotations(
     x = 10,
     y = 1.01,
     text = paste("doubles every day"),
     xref = "x",
     yref = "y",
     showarrow = FALSE,
     font=list(size=9,color = "gray")
   ) %>%
  plotly::add_annotations(
     x = 24,
     y = 1.01,
     text = paste("doubles every two days"),
     xref = "x",
     yref = "y",
     showarrow = FALSE,
     font=list(size=9,color = "purple")
   ) %>%
  plotly::add_annotations(
     x = 44,
     y = 0.82,
     text = paste("doubles every seven days"),
     xref = "x",
     yref = "y",
     showarrow = FALSE,
     font=list(size=9,color = "brown")
   ) %>%
  plotly::add_annotations(
     x = 27,
     y = 0.85,
     text = paste("TR"),
     xref = "x",
     yref = "y",
     showarrow = FALSE,
     font=list(size=9,color = "red")
   ) %>%
  plotly::add_annotations(
     x = 45,
     y = 1.011,
     text = paste("US"),
     xref = "x",
     yref = "y",
     showarrow = FALSE,
     font=list(size=9,color = "blue")
   ) %>%
  plotly::add_annotations(
     x = 39,
     y = 1.01,
     text = paste("SP"),
     xref = "x",
     yref = "y",
     showarrow = FALSE,
     font=list(size=9,color = "orange")
   ) %>%
  plotly::add_annotations(
     x = 36,
     y = 0.9,
     text = paste("GER"),
     xref = "x",
     yref = "y",
     showarrow = FALSE,
     font=list(size=9,color = "black")
   ) %>%
  plotly::add_annotations(
     x = 44,
     y = 0.98,
     text = paste("FR"),
     xref = "x",
     yref = "y",
     showarrow = FALSE,
     font=list(size=9,color = "cyan")
   ) %>%
  plotly::add_annotations(
     x = 48,
     y = 1.02,
     text = paste("IT"),
     xref = "x",
     yref = "y",
     showarrow = FALSE,
     font=list(size=9,color = "green")
   ) %>%
  plotly::add_annotations(
     x = 44,
     y = 0.65,
     text = paste("JP"),
     xref = "x",
     yref = "y",
     showarrow = FALSE,
     font=list(size=9,color = "pink")
   ) %>%
  plotly::layout(
      title = "",
      yaxis = list(type = "log", title = "deaths",
                   ticktext= c("5","15","40","110","299","812","2208","6003","16318") 
                   ,tickvals=c(1.7:10)
                   ),
      xaxis = list(title = "Days passed after 5 deaths", tick0=0, dtick=3),
      legend = list(x = 0.65, y = 0.03)
      #hovermode = "compare"
      )

```




PROGRESSION
=======================================================================


Column {data-width=400}
-------------------------------------


### **Daily case progression as % of daily cases and linear trend line**
    
```{r}
df_prog <- coronavirus %>%
  dplyr::filter(Country.Region == "Germany") %>%
  dplyr::group_by(date, type) %>%
  dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = total
  ) %>%
  dplyr::arrange(date) %>%
  dplyr::ungroup() %>%
  #dplyr::mutate(active = confirmed - death - recovered) %>%
  dplyr::mutate(active = confirmed - death) %>%
  dplyr::mutate(
    confirmed_cum = cumsum(confirmed),
    death_cum = cumsum(death),
    # recovered_cum = cumsum(recovered),
    active_cum = cumsum(active),
    id = row_number()
    #predicted = cumsum(predicted)
  )
df_prog <- df_prog %>%
  dplyr::filter(id > 46)
#df_prog = df_prog[-c(54,58),]
df_prog <- df_prog %>%
  dplyr::mutate(variant_case = round(100*(confirmed - (dplyr::lag(confirmed, n=1L)))/(dplyr::lag(confirmed, n=1L))))%>%
  dplyr::mutate(variant_death = round(100*(death - (dplyr::lag(death, n=1L)))/(dplyr::lag(death, n=1L))))%>%
  dplyr::mutate(variant_case = ifelse(is.nan(variant_case), 0,variant_case))%>%
  dplyr::mutate(variant_death = ifelse(is.nan(variant_death), 0,variant_death))%>%
  dplyr::mutate(variant_death = ifelse(!is.finite(variant_death), 100,variant_death))%>%
  dplyr::mutate(variant_case = ifelse(!is.finite(variant_case), 100,variant_case))

df_prog_uk <- coronavirus %>%
  dplyr::filter(Country.Region == "United Kingdom") %>%
  dplyr::group_by(date, type) %>%
  dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = total
  ) %>%
  dplyr::arrange(date) %>%
  dplyr::ungroup() %>%
  #dplyr::mutate(active = confirmed - death - recovered) %>%
  dplyr::mutate(active = confirmed - death) %>%
  dplyr::mutate(
    confirmed_cum = cumsum(confirmed),
    death_cum = cumsum(death),
    # recovered_cum = cumsum(recovered),
    active_cum = cumsum(active),
    id = row_number()
    #predicted = cumsum(predicted)
  )
df_prog_uk <- df_prog_uk %>%
   dplyr::filter(id > 52)
#df_prog_uk = df_prog_uk[-c(54,58),]
df_prog_uk <- df_prog_uk %>%
  dplyr::mutate(variant_case = round(100*(confirmed - (dplyr::lag(confirmed, n=1L)))/(dplyr::lag(confirmed, n=1L))))%>%
  dplyr::mutate(variant_death = round(100*(death - (dplyr::lag(death, n=1L)))/(dplyr::lag(death, n=1L))))%>%
  dplyr::mutate(variant_case = ifelse(is.nan(variant_case), 0,variant_case))%>%
  dplyr::mutate(variant_death = ifelse(is.nan(variant_death), 0,variant_death))%>%
  dplyr::mutate(variant_death = ifelse(!is.finite(variant_death), 100,variant_death))%>%
  dplyr::mutate(variant_case = ifelse(!is.finite(variant_case), 100,variant_case))%>%
  dplyr::mutate(variant_case = ifelse((variant_case)>100, 100,variant_case))

df_prog_tr <- coronavirus %>%
  dplyr::filter(Country.Region == "Turkey") %>%
  dplyr::group_by(date, type) %>%
  dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = total
  ) %>%
  dplyr::arrange(date) %>%
  dplyr::ungroup() %>%
  #dplyr::mutate(active = confirmed - death - recovered) %>%
  dplyr::mutate(active = confirmed - death) %>%
  dplyr::mutate(
    confirmed_cum = cumsum(confirmed),
    death_cum = cumsum(death),
    # recovered_cum = cumsum(recovered),
    active_cum = cumsum(active),
    id = row_number()
    #predicted = cumsum(predicted)
  )
df_prog_tr <- df_prog_tr %>%
   dplyr::filter(id > 60)
#df_prog_tr = df_prog_tr[-c(54,58),]
df_prog_tr <- df_prog_tr %>%
  dplyr::mutate(variant_case = round(100*(confirmed - (dplyr::lag(confirmed, n=1L)))/(dplyr::lag(confirmed, n=1L))))%>%
  dplyr::mutate(variant_death = round(100*(death - (dplyr::lag(death, n=1L)))/(dplyr::lag(death, n=1L))))%>%
  dplyr::mutate(variant_case = ifelse(is.nan(variant_case), 0,variant_case))%>%
  dplyr::mutate(variant_death = ifelse(is.nan(variant_death), 0,variant_death))%>%
  dplyr::mutate(variant_death = ifelse(!is.finite(variant_death), 100,variant_death))%>%
  dplyr::mutate(variant_case = ifelse(!is.finite(variant_case), 100,variant_case))%>%
  dplyr::mutate(variant_case = ifelse((variant_case)>100, 100,variant_case))

df_prog_de_c = df_prog
df_prog_de_d = df_prog

df_prog_uk_c = df_prog_uk
df_prog_uk_d = df_prog_uk

df_prog_tr_c = df_prog_tr
df_prog_tr_d = df_prog_tr

plotly::plot_ly(data = df_prog_de_c) %>%
     plotly::add_trace(x = ~df_prog_de_c$date,
       y = ~df_prog_de_c$variant_case,
       type = "bar",
      # mode = "lines",
       name = "Germany",
       line = list(color = "black", dash="dash"),
    marker = list(color = "black", size=3), opacity =0.1
       ) %>%
     
    plotly::add_trace(x = ~df_prog_uk_c$date,
       y = ~df_prog_uk_c$variant_case,
       type = "bar",
      # mode = "lines",
       name = "UK", color="blue",
       line = list(color = "blue", dash="dash"),
    marker = list(color = "blue", size=3), opacity =0.2
       ) %>%
    plotly::add_trace(x = ~df_prog_tr_c$date,
       y = ~df_prog_tr_c$variant_case,
       type = "bar",
      # mode = "lines",
       name = "Turkey", color="red",
       line = list(color = "red", dash="dash"),
    marker = list(color = "red", size=3), opacity =0.2
       ) %>%
     plotly::add_lines(x = ~df_prog_de_c$date,
        y= fitted(lm(df_prog_de_c$variant_case~df_prog_de_c$date)), name="trend GER", line = list(color = "black", dash="dash") 
       ) %>%
     plotly::add_lines(x = ~df_prog_uk_c$date,
        y= fitted(lm(df_prog_uk_c$variant_case~df_prog_uk_c$date)), name="trend UK", line = list(color = "blue", dash="dash") 
       ) %>%
     plotly::add_lines(x = ~df_prog_tr_c$date,
        y= fitted(lm(df_prog_tr_c$variant_case~df_prog_tr_c$date)), name="trend TR", line = list(color = "red", dash="dash") 
       ) %>%
    plotly::layout( 
        title = "", 
        yaxis = list(title = "Per cent (%)", range= c(-200,200)),
        xaxis = list(title = "-Starting as of 1000 total confirmed cases in each country-", size=8), 
        legend = list(x = 0.1, y = 0.2,orientation = 'h'),
        hovermode = "compare" 
        )
 

```

### **Daily death progression as % of daily deaths and linear trend line **



```{r}
 plotly::plot_ly(data = df_prog_de_d) %>%
      plotly::add_trace(x = ~df_prog_de_d$date,
        y = ~df_prog_de_d$variant_death,
        type = "bar",
        #mode = "lines",
        name = "Germany", color="black",
        line = list(color = "black", dash="dash"),
    marker = list(color = "black", size=3), opacity =0.1
        ) %>%
      plotly::add_trace(x = ~df_prog_uk_d$date,
        y = ~df_prog_uk_d$variant_death,
        type = "bar",
        #mode = "lines",
        name = "UK", color ="blue",
        line = list(color = "blue", dash="dash"),
    marker = list(color = "blue", size=3), opacity =0.2
        ) %>%
     plotly::add_trace(x = ~df_prog_tr_d$date,
        y = ~df_prog_tr_d$variant_death,
        type = "bar",
        #mode = "lines",
        name = "Turkey", color="red",
       line = list(color = "red", dash="dash"),
    marker = list(color = "red", size=3), opacity =0.2
        ) %>%
       plotly::add_lines(x = ~df_prog_de_d$date,
         y= fitted(lm(df_prog_de_d$variant_death~df_prog_de_d$date)), name = "trend GER", line = list(color = "black", dash="dash") 
        ) %>%
       plotly::add_lines(x = ~df_prog_uk_d$date,
         y= fitted(lm(df_prog_uk_d$variant_death~df_prog_uk_d$date)), name = "trend UK", line = list(color = "blue", dash="dash") 
        ) %>%
      plotly::add_lines(x = ~df_prog_tr_d$date,
         y= fitted(lm(df_prog_tr_d$variant_death~df_prog_tr_d$date)), name = "trend TR", line = list(color = "red", dash="dash") 
        ) %>%
      plotly::layout( 
         title = "", 
         yaxis = list(title = "Per cent (%)", range= c(-200,200)),
         xaxis = list(title = "-Starting as of 1000 total confirmed cases in each country-"), 
         legend = list(x = 0.1, y = 0.2,orientation ='h'),
         hovermode = "compare" 
         )

# ```
# 
# Column {data-width=400}
# -------------------------------------
# 
# ### **Daily death progression as % of daily deaths (Linear trend) -Starting as of 1000 confirmed cases**
#     
# ```{r}
# plotly::plot_ly(data = df_prog_de_c) %>%
#     plotly::add_lines(x = ~df_prog_de_c$date,
#        y= fitted(lm(df_prog_de_c$variant_case~df_prog_de_c$date)), name="trend GER", line = list(color = "black", dash="dash") 
#       ) %>%
#     plotly::add_lines(x = ~df_prog_uk_c$date,
#        y= fitted(lm(df_prog_uk_c$variant_case~df_prog_uk_c$date)), name="trend UK", line = list(color = "blue", dash="dash") 
#       ) %>%
#     plotly::add_lines(x = ~df_prog_tr_c$date,
#        y= fitted(lm(df_prog_tr_c$variant_case~df_prog_tr_c$date)), name="trend TR", line = list(color = "red", dash="dash") 
#       ) %>%
#     plotly::layout( 
#         title = "", 
#         yaxis = list(title = "Per cent (%)", range= c(-100,100)),
#         xaxis = list(title = "Date"), 
#         legend = list(x = 0.1, y = 0.1), 
#         hovermode = "compare" 
#         )

# ```
# 
# ### **Daily death progression as % of daily deaths (Linear trend) -Starting as of 1000 confirmed cases**
# 
# 
# 
# ```{r}
# plotly::plot_ly(data = df_prog_de_d) %>%
#     plotly::add_lines(x = ~df_prog_de_d$date,
#         y= fitted(lm(df_prog_de_d$variant_death~df_prog_de_d$date)), name = "trend GER", line = list(color = "black", dash="dash")
#        ) %>%
#       plotly::add_lines(x = ~df_prog_uk_d$date,
#         y= fitted(lm(df_prog_uk_d$variant_death~df_prog_uk_d$date)), name = "trend UK", line = list(color = "blue", dash="dash") 
#        ) %>%
#      plotly::add_lines(x = ~df_prog_tr_d$date,
#         y= fitted(lm(df_prog_tr_d$variant_death~df_prog_tr_d$date)), name = "trend TR", line = list(color = "red", dash="dash") 
#        ) %>%
#       plotly::layout( 
#          title = "", 
#          yaxis = list(title = "Per cent (%)", range= c(-100,100)),
#          xaxis = list(title = "Date"), 
#          legend = list(x = 0.1, y = 0.1), 
#          hovermode = "compare" 
#          )


```

DAILY
=======================================================================


Column {data-width=400}
-------------------------------------


### **Daily new confirmed cases (`r  max(coronavirus$date)`) - Top 10 Countries**
    
```{r}
max_date <- max(coronavirus$date)
coronavirus %>% 
  dplyr::filter(type == "confirmed", date == max_date) %>%
  dplyr::group_by(Country.Region) %>%
  dplyr::summarise(total_cases = sum(cases)) %>%
  dplyr::arrange(-total_cases) %>%
  dplyr::mutate(country = factor(Country.Region, levels = Country.Region)) %>%
  dplyr::ungroup() %>%
  dplyr::top_n(n = 10, wt = total_cases) %>%
  plotly::plot_ly(x = ~ country,
                  y = ~ total_cases,
                  text = ~ total_cases,
                  textposition = 'auto',
                  type = "bar") %>%
  plotly::layout(yaxis = list(title = "Number of Cases"),
                 xaxis = list(title = "",autorange="reversed"),
                 margin =  list(
                   l = 10,
                   r = 10,
                   b = 10,
                   t = 10,
                   pad = 2
                 ))

``` 



### **Daily new deaths (`r  max(coronavirus$date)`) - Top 10 Countries**



```{r}
max_date <- max(coronavirus$date)
coronavirus %>% 
  dplyr::filter(type == "death", date == max_date) %>%
  dplyr::group_by(Country.Region) %>%
  dplyr::summarise(total_cases = sum(cases)) %>%
  dplyr::arrange(-total_cases) %>%
  dplyr::mutate(country = factor(Country.Region, levels = Country.Region)) %>%
  dplyr::ungroup() %>%
  dplyr::top_n(n = 10, wt = total_cases) %>%
  plotly::plot_ly(x = ~ country,
                  y = ~ total_cases,
                  text = ~ total_cases,                       textposition = 'auto',
                  type = "bar", marker = list(color='red')) %>%
  plotly::layout(yaxis = list(title = "Deaths"),
                 xaxis = list(title = "",autorange="reversed"),
                 margin =  list(
                   l = 10,
                   r = 10,
                   b = 10,
                   t = 5,
                   pad = 2)
  )
### **Total cases and deaths**

```

Column {data-width=400}
-------------------------------------

### **Daily new confirmed cases over time - Turkey**
    
```{r}
#Total number of cases and deaths 
#df_EU <- coronavirus %>%
#   # dplyr::filter(date == max(date)) %>%
#   dplyr::filter(Country.Region == "Germany" |
#     Country.Region == "France" |
#     Country.Region == "Italy" |
#     Country.Region == "Spain"|
#     Country.Region == "US"|
#     Country.Region == "Iran"|
#     Country.Region == "China"|
#     Country.Region == "United Kingdom"|
#     Country.Region == "Belgium"|
#     Country.Region == "Switzerland"|
#     Country.Region == "Turkey") %>%
#   dplyr::group_by(Country.Region, type) %>%
#   dplyr::summarise(total = sum(cases)) %>%
#   tidyr::pivot_wider(
#     names_from = type,
#     values_from = total
#   ) %>%
#   # dplyr::mutate(unrecovered = confirmed - ifelse(is.na(recovered), 0, recovered) - ifelse(is.na(death), 0, death)) %>%
#   dplyr::mutate(unrecovered = confirmed - ifelse(is.na(death), 0, death)) %>%
#   dplyr::arrange(confirmed) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(country = dplyr::if_else(Country.Region == "United Arab Emirates", "UAE", Country.Region)) %>%
#   dplyr::mutate(country = dplyr::if_else(country == "Mainland China", "China", country)) %>%
#   dplyr::mutate(country = dplyr::if_else(country == "North Macedonia", "N.Macedonia", country)) %>%
#   dplyr::mutate(country = trimws(country)) %>%
#   dplyr::mutate(country = factor(country, levels = country))
# 
# plotly::plot_ly(
#   data = df_EU,
#   x = ~country,
#   # y = ~unrecovered,
#   y = ~ confirmed,
#   # text =  ~ confirmed,
#   textposition = 'auto',
#   type = "bar",
#   name = "Confirmed",
#   marker = list(color = active_color)
# ) %>%
#   plotly::add_trace(
#     y = ~death,
#     # text =  ~ death,
#     # textposition = 'auto',
#     name = "Death",
#     marker = list(color = death_color)
#   ) %>%
#   plotly::layout(
#     barmode = "stack",
#     yaxis = list(title = "Total cases"),
#     xaxis = list(title = ""),
#     hovermode = "compare",
#     margin = list(
#       # l = 60,
#       # r = 40,
#       b = 10,
#       t = 10,
#       pad = 2)
#     )
plotly::plot_ly(data = df_daily) %>%
    plotly::add_trace(x = ~date,
      y = ~confirmed,
      type = "bar",
      name = "Turkey",text = ~ confirmed, textposition = 'auto'
      #line = list(color = "blue"),
      #marker = list(color = "blue", size=3)
      ) %>%
    plotly::layout( 
       title = "", 
       yaxis = list(title = "Confirmed cases"), 
       xaxis = list(title = "Date", dtick=86400000*3), 
       legend = list(x = 0.1, y = 0.8) 
       #hovermode = "compare" 
       )
```



### **Daily new deaths over time - Turkey**



```{r}
plotly::plot_ly(data = df_daily) %>%
    plotly::add_trace(x = ~date,
      y = ~death,
      type = "bar",
      name = "Turkey",text = ~ death, textposition = 'auto',
      marker = list(color = "red", size=3)
      ) %>%
    plotly::layout( 
       title = "", 
       yaxis = list(title = "Deaths"), 
       xaxis = list(title = "Date", dtick=86400000*3), 
       legend = list(x = 0.1, y = 0.8) 
       #hovermode = "compare" 
       )
```



CASE PROFILE
=======================================================================


Column {data-width=200}
-------------------------------------

### **% share of critical cases (`r  max(table_tr$date)`) - Turkey** (Source: TR Ministry of Health)

```{r}
#Active cases
# table_tr <- table_tr%>%
#   dplyr::filter(id > 16)
# tube_share = (max(table_tr$tube)*100)/max(table_tr$active)
# icu_share = (max(table_tr$icu)*100)/max(table_tr$active)
# others_share = 100-(tube_share+icu_share)
# 
# label_share = c(others_share,tube_share,icu_share)
# text_share = c('Non-critical','Intubated','Intensive care')
# patient_data <- data.frame(label_share, text_share)
# 
# plotly::plot_ly(data = patient_data) %>%
#   plotly::add_trace(labels = ~text_share,
#                     values = ~label_share,
#                     marker= list(colors = c('#C0C0C0','#4682B4', '#FF8C00')),
#                     type = 'pie',textposition ='auto', insidetextorientation='auto', name = "TR", text = ~text_share
#   ) 

#patients share
table_tr <- table_tr%>%
   dplyr::filter(id > 16)
plotly::plot_ly(data = table_tr) %>%
     plotly::add_trace(x = ~date,
       y = ~round(tube_share,2),
       type = 'bar',
       name = "% share of intubated patients",text = ~ round(tube_share,2), textposition = 'auto'
       ) %>%
   plotly::add_trace(x=~date, y = ~round(icu_share,1), name = '% share of intensive care', text = ~ round(icu_share,1),type = 'bar', textposition = 'auto'
      ) %>%
     plotly::layout( 
        title = "", xaxis = list(title=''),
        yaxis = list(title='Share in total active cases as %'), barmode='stack'
        ,legend = list(x=0.4, y=0.9)
        )

``` 



### **Active cases and deaths (`r  max(table_tr$date)`) - Turkey** (Source: TR Ministry of Health)



```{r} 
table_tr <- table_tr%>%
   dplyr::filter(id > 16)
plotly::plot_ly(data = table_tr) %>%
     plotly::add_trace(x = ~date,
       y = ~tube,
       type = 'bar',
       name = "Total intubated patients",text = ~ tube, textposition = 'auto'
       ) %>%
   plotly::add_trace(x=~date, y = ~icu, name = 'Total intensive care', text = ~ icu,type = 'bar', textposition = 'auto'
      ) %>%
     plotly::layout( 
        title = "", xaxis = list(title=''),
        yaxis = list(title='Cases'), barmode='stack',legend = list(x=0.03, y=0.93)
        ) %>%
  # plotly::add_trace(
  #   x = ~date,
  #   y = ~total_recovered,
  #   type = "scatter",
  #   mode = "lines+markers",
  #   name = "Total recovered",
  #   line = list(color = "green"),
  #   marker = list(color = "green", size=3)
  #   )%>%
  plotly::add_trace(
    x = ~date,
    y = ~total_death,
    type = "scatter",
    mode = "lines+markers",
    name = "Total death",
    line = list(color = "black"),
    marker = list(color = "black", size=3)
    )
  # plotly::add_annotations(x = as.Date("2020-04-01"),
  #                  y = 3000,
  #                  text = paste("Total intubated patients=", sum(table_tr$tube), sep = " "),
  #                  xref = "6",showarrow = FALSE )%>%
  # plotly::add_annotations(text = paste("Total intensive care patients=", sum(table_tr$isc), sep = " "),
  #                   x = as.Date("2020-04-01"),
  #                   y = 2900,
  #                   xref = "6",showarrow = FALSE )%>%
  # plotly::add_annotations(text = paste("Total recovered patients=", max(table_tr$total_recovered), sep = " "),
  #                   x = as.Date("2020-04-01"),
  #                   y = 2800,
  #                   xref = "6",showarrow = FALSE )%>%
  #  plotly::add_annotations(text = paste("Total deaths=", sum(table_tr$new_death), sep = " "),
  #                   x = as.Date("2020-04-01"),
  #                   y = 2700,
  #                   xref = "6",showarrow = FALSE )
# ``` 
# 
# Column {data-width=200}
# -------------------------------------
# 
# ### **Active and closed cases (`r  max(table_tr$date)`) - Turkey** (Source: TR Ministry of Health)
# 
# ```{r}
# x_2 <- list(overlaying ="x", title="% case fatality rate", side ="top", tickvals= list(1,1.6,1.7,1.8,1.9,2.0,2.11,2.13,2.15,2.17,2.2))
# table_tr <- table_tr%>%
#   dplyr::filter(id > 16)  
# plotly::plot_ly(data = table_tr) %>%
#   dplyr::mutate(death_rate = round(total_death*100/total_confirmed,digits=2))%>%
#   plotly::add_trace(x = ~active,
#                     y = ~date,
#                     type = 'bar',orientation = 'h',marker = list(color='tomato'),
#                     name = "Active cases"
#   ) %>%
#   plotly::add_trace(x=~total_recovered, y = ~date,orientation = 'h', name = 'Recoveries',type = 'bar',marker = list(color='green')
#   ) %>%
#   plotly::add_trace(x=~total_death, y = ~date,orientation = 'h', name = 'Deaths', marker = list(color='black'), type = 'bar'
#   )%>%
#   # plotly::add_trace(x=~death_rate, y = ~date, xaxis="x2", orientation = 'h', name = 'Case fatality rate %', text=~death_rate, marker = list(color='cyan', size=7), type = 'scatter', mode ='markers'
#   # )%>%
#   plotly::layout( 
#     title = "", xaxis = list(title=''), xaxis2 = x_2,
#     yaxis = list(title='Date', autorange="reversed",dtick=86400000*3), barmode='stack'
#   )
# 
# ``` 
# 
# 
# 
# ### ** 
# 
# 
# 
# ```{r} 
# 

```



FATALITY
=======================================================================


Column {data-width=200}
-------------------------------------

### **% change in the daily deaths plotted with the case fatality rate over time- Turkey** (Source: JHU)

```{r}
df_prog_tr <- coronavirus %>%
  dplyr::filter(Country.Region == "Turkey") %>%
  dplyr::group_by(date, type) %>%
  dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = total
  ) %>%
  dplyr::arrange(date) %>%
  dplyr::ungroup() %>%
  #dplyr::mutate(active = confirmed - death - recovered) %>%
  dplyr::mutate(active = confirmed - death) %>%
  dplyr::mutate(
    confirmed_cum = cumsum(confirmed),
    death_cum = cumsum(death),
    # recovered_cum = cumsum(recovered),
    active_cum = cumsum(active),
    id = row_number()
    #predicted = cumsum(predicted)
  )
df_prog_tr_d <- df_prog_tr %>%
  dplyr::filter(id > 55)
df_prog_tr_d <- df_prog_tr_d %>%
  dplyr::mutate(variant_case = round(100*(confirmed - (dplyr::lag(confirmed, n=1L)))/(dplyr::lag(confirmed, n=1L))))%>%
  dplyr::mutate(variant_death = round(100*(death - (dplyr::lag(death, n=1L)))/(dplyr::lag(death, n=1L))))%>%
  dplyr::mutate(variant_case = ifelse(is.nan(variant_case), 0,variant_case))%>%
  dplyr::mutate(variant_death = ifelse(is.nan(variant_death), 0,variant_death))%>%
  dplyr::mutate(variant_death = ifelse(!is.finite(variant_death), 100,variant_death))%>%
  dplyr::mutate(variant_case = ifelse(!is.finite(variant_case), 100,variant_case))%>%
  dplyr::mutate(variant_case = ifelse((variant_case)>100, 100,variant_case))

cfr = round((df_prog_tr_d$death_cum*100)/df_prog_tr_d$confirmed_cum,digits=2)
y_2 <- list(overlaying ="y", title="Case fatality rate (%)", side ="right", range= c(-3,3)
            ,tickvals= list(-3,-2,-1,0,1,1.5,2,2.3,2.6,3)
            )
plotly::plot_ly(data = df_prog_tr_d) %>%
      plotly::add_trace(x = ~df_prog_tr_d$date,
        y = ~df_prog_tr_d$variant_death,
        type = "bar",
        #mode = "lines",
        name = "Change in the daily deaths (%)", color="red",  text=df_prog_tr_d$variant_death,textposition = 'auto',  textcolor= 'blue',
        groupnorm ='percent',
        line = list(color = "red", dash="dash"),
        marker = list(color = "red", size=3)
    )%>%
  plotly::add_trace(x = ~df_prog_tr_d$date,
        y = ~cfr,
        type = "scatter",
        mode = "lines+markers",
        name = "Case fatality rate (%)", color="black", yaxis="y2", name="Case fatality rate", text=cfr, textposition = 'auto',
        line = list(color = "black"),
        marker = list(color = "black", size=5)
    )%>%
  plotly::add_annotations(x = as.Date("2020-03-20"),
                    y = 49,
                    text = paste("1.1%")
                    ,showarrow = FALSE 
    )%>%
  plotly::add_annotations(x = as.Date("2020-03-22"),
                    y = 89,
                    text = paste("2.43%")
                    ,showarrow = FALSE 
    )%>%
  plotly::add_annotations(x = as.Date("2020-03-29"),
                    y = 59,
                    text = paste("1.42%")
                    ,showarrow = FALSE 
    )%>%
  plotly::add_annotations(x = as.Date("2020-04-04"),
                    y = 79,
                    text = paste("2.09%")
                    ,showarrow = FALSE 
    )%>%
  plotly::add_annotations(x = as.Date("2020-04-10"),
                    y = 79,
                    text = paste("2.14%")
                    ,showarrow = FALSE 
    )%>%
  plotly::add_annotations(x = as.Date("2020-04-14"),
                    y = 79,
                    text = paste("2.15%")
                    ,showarrow = FALSE 
    )%>%
  plotly::add_annotations(x = as.Date("2020-04-25"),
                    y = 100,
                    text = paste("2.51%")
                    ,showarrow = FALSE 
    )%>%
    plotly::layout( 
         title = "", 
         yaxis = list(color ="red",title = "Daily change of deaths (%)", range= c(-100,100)), yaxis2=y_2,
         xaxis = list(title = "-Dates starting from the first death-",dtick=86400000*2), 
         legend = list(x=0.83, y=0.105),
         hovermode = "compare" 
         )

``` 

Column {data-width=200}
-------------------------------------

### **% change in daily cases- Turkey (Source: JHU)**


```{r} 
df_prog_tr <- coronavirus %>%
  dplyr::filter(Country.Region == "Turkey") %>%
  dplyr::group_by(date, type) %>%
  dplyr::summarise(total = sum(cases, na.rm = TRUE)) %>%
  tidyr::pivot_wider(
    names_from = type,
    values_from = total
  ) %>%
  dplyr::arrange(date) %>%
  dplyr::ungroup() %>%
  #dplyr::mutate(active = confirmed - death - recovered) %>%
  dplyr::mutate(active = confirmed - death) %>%
  dplyr::mutate(
    confirmed_cum = cumsum(confirmed),
    death_cum = cumsum(death),
    # recovered_cum = cumsum(recovered),
    active_cum = cumsum(active),
    id = row_number()
    #predicted = cumsum(predicted)
  )
df_prog_tr_c <- df_prog_tr %>%
  dplyr::filter(id > 55)
df_prog_tr_c <- df_prog_tr_c %>%
  dplyr::mutate(variant_case = round(100*(confirmed - (dplyr::lag(confirmed, n=1L)))/(dplyr::lag(confirmed, n=1L))))%>%
  dplyr::mutate(variant_death = round(100*(death - (dplyr::lag(death, n=1L)))/(dplyr::lag(death, n=1L))))%>%
  dplyr::mutate(variant_case = ifelse(is.nan(variant_case), 0,variant_case))%>%
  dplyr::mutate(variant_death = ifelse(is.nan(variant_death), 0,variant_death))%>%
  dplyr::mutate(variant_death = ifelse(!is.finite(variant_death), 100,variant_death))%>%
  dplyr::mutate(variant_case = ifelse(!is.finite(variant_case), 100,variant_case))%>%
  dplyr::mutate(variant_case = ifelse((variant_case)>100, 100,variant_case))

cfr = round((df_prog_tr_c$death_cum*100)/df_prog_tr_c$confirmed_cum,digits=2)
y_2 <- list(overlaying ="y", title="Case fatality rate (%)", side ="right", range= c(-3,3)
            ,tickvals= list(-3,-2,-1,0,1,1.5,2,2.3,2.6,3)
            )
plotly::plot_ly(data = df_prog_tr_c) %>%
      plotly::add_trace(x = ~df_prog_tr_c$date,
        y = ~df_prog_tr_c$variant_case,
        type = "bar",
        #mode = "lines",
        name = "Change in the daily cases (%)", color="blue",  text=df_prog_tr_c$variant_case,textposition = 'auto',
        groupnorm ='percent',
        line = list(color = "blue", dash="dash"),
        marker = list(color = "blue", size=3)
  )%>%
    plotly::layout( 
         title = "", 
         yaxis = list(color ="blue",title = "Daily change of cases (%)", range= c(-100,100)), yaxis2=y_2,
         xaxis = list(title = "-Dates starting from the first death-",dtick=86400000*2), 
         showlegend = TRUE,legend = list(x=0.81, y=0.1),
         hovermode = "compare" 
          )

```


About
=======================================================================

**The Coronavirus Dashboard: Turkey**

This Coronavirus dashboard: the case of Turkey provides an overview of the 2019 Novel Coronavirus COVID-19 (2019-nCoV) epidemic for Turkey This dashboard is built with R using the R Markdown framework by [Bcogan](https://github.com/Bcogan0/){target="_blank"}. 

**Code**

This dashboard is adapted from [here](https://github.com/RamiKrispin){target="_blank"} and [here](https://github.com/AntoineSoetewey/coronavirus_dashboard){target="_blank"}.

**Data and Method**

The input data for this dashboard is the dataset available from the [`{coronavirus}`](https://github.com/RamiKrispin/coronavirus){target="_blank"} R package.The data and dashboard are updated on a daily basis.The raw data is pulled from the Johns Hopkins University Center for Systems Science and Engineering (JHU CCSE) Coronavirus [repository](https://github.com/RamiKrispin/coronavirus-csv){target="_blank"}. 

Projected cases: The forecast is calculated based on the logarithmic scale of the same data. The applied linear regression on the log scale is used to interpolate the raw data as an exponential model.

Estimated number of infected: The actual number of people who are infected is roughly estimated based on the calculated case growth rate and an assumed period of 5 days for symptoms to develop. It is also assumed that only 50% of infected people are symptomatic according to this [source](https://edition.cnn.com/2020/04/01/europe/iceland-testing-coronavirus-intl/index.html){target="_blank"}. This [source](http://covid19.science.unimelb.edu.au/){target="_blank"}uses a different method for estimating detection rate. 

Infection fatality rate: See more detailed information about Infection fatality rate and case fatality rate [here](https://ourworldindata.org/coronavirus#what-we-want-to-know-isn-t-the-case-fatality-rate-it-s-the-infection-fatality-rate){target="_blank"}.

Progression: Progression rate for daily number of cases and deaths (second derivative of the daily number of cases and deaths) gives us information about the acceleration or deceleration rate of the daily cases and deaths. If the this rate is positive, that is if growth of the new cases per day is accelerating, the outbreak is exponentially growing. If the rate of variation is negative, even if the number of cases is still growing, the growth of the daily new cases is slowing down. For example, in the graph for the confirmed case progression; on 21st of March there were 19% less cases than the day before in UK, but 40% more deaths than the day before. In Germany on the same day, there were 48% less cases than the day before, and 26% less deaths. See more info [here](https://towardsdatascience.com/whats-wrong-with-covid-19-data-visualizations-and-how-to-fix-it-3cdc9adc774d){target="_blank"}.

The Map tab was contributed by [Art Steinmetz](@adababbage) on this [pull request](https://github.com/RamiKrispin/coronavirus_dashboard/pull/1). 

**Update**

The data is as of `r format(max(coronavirus$date), "%A %B %d, %Y")` 

[Link](url) and ![Image](src)
```

For more details see [GitHub Flavored Markdown](https://guides.github.com/features/mastering-markdown/).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/Bcogan0/covid_tr/settings). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://help.github.com/categories/github-pages-basics/) or [contact support](https://github.com/contact) and we’ll help you sort it out.
