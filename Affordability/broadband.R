# Required libraries
library(dplyr)
library(OECD)
library(ggplot2)
library(viridis)

broadband <- read_csv("~/Documents/GitHub/PhD/Data/OECD.STI.DEP,DSD_ICT_HH_IND@DF_HH,1.0+AUS+ITA+CHE+ESP+FRA+DEU+GBR+USA.A.B21_HH+B21B_HH+B1_HH.PT_HH._T....HH_Q4+HH_Q3+HH_Q2+INCOME_HH+HH_Q1+_T..csv")

broadband_total <- broadband %>%
        dplyr::select(`Reference area`, `Measure`,`Income group`, `TIME_PERIOD`, `OBS_VALUE` ) %>%
        group_by(`Reference area`, `TIME_PERIOD`) %>%
        filter(`Income group` == "Total" & `Measure` == "Households with broadband Internet access at home")

broadband_Q1 <- broadband %>%
        dplyr::select(`Reference area`, `Measure`,`Income group`, `TIME_PERIOD`, `OBS_VALUE` ) %>%
        group_by(`Reference area`, `TIME_PERIOD`) %>%
        filter(`Income group` == "Households with income in first quartile" & `Measure` == "Households with broadband Internet access at home")

internet_total <- broadband %>%
        dplyr::select(`Reference area`, `Measure`,`Income group`, `TIME_PERIOD`, `OBS_VALUE` ) %>%
        group_by(`Reference area`, `TIME_PERIOD`) %>%
        filter(`Income group` == "Total" & `Measure` == "Households with Internet access at home" & `TIME_PERIOD` <= "2021")

internet_Q1 <- broadband %>%
        dplyr::select(`Reference area`, `Measure`,`Income group`, `TIME_PERIOD`, `OBS_VALUE` ) %>%
        group_by(`Reference area`, `TIME_PERIOD`) %>%
        filter(`Income group` == "Households with income in first quartile" & `Measure` == "Households with Internet access at home")

ggplot(internet_total, aes(x = `TIME_PERIOD`, y = `OBS_VALUE`, color = `Reference area`)) + 
               geom_line() + 
               theme_minimal() + 
        labs(title = "% of Households with internet access at home",
             x = "Year",
             y = "Country") + 
        scale_y_continuous(expand = c(0,0), limits = c(0, 100))
        