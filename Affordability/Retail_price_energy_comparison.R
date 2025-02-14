# Required libraries
library(dplyr)
library(tidyverse)
library(eurostat)
library(RColorBrewer)
library(viridis)
library(ggplot2)
library(gridExtra)

# Expand country selection to include UK and comparison countries
select_countries <- c("United Kingdom", "Germany", "France", "Italy", "Spain", "Netherlands")

# Get electricity price data
elec_price <- get_eurostat("nrg_pc_204_c", type = "label", time_format = "num")
elec_price_average <- elec_price %>%
        filter(nrg_cons == "Consumption of kWh - all bands", 
               currency == "Euro") %>%
        rename("PricePkWh" = values)

# Create comparison for selected countries - using 2019 data
average_elec_composition_2019 <- elec_price_average %>%
        filter(geo %in% select_countries, 
               TIME_PERIOD == "2019", 
               PricePkWh != 0)

# Create electricity price comparison chart
Elec_Chart_2019 <- average_elec_composition_2019 %>%
        ggplot(aes(x = geo, y = PricePkWh, fill = nrg_prc)) +
        geom_bar(stat = "identity") +
        labs(title = "Residential Electricity Price Comparison 2019",
             x = "Country",
             y = "Euro per kWh",
             fill = "Components of Price") +
        scale_fill_viridis(discrete = TRUE) +
        theme_minimal() +
        ylim(0, 0.40) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Get gas price data
gas_price <- get_eurostat("nrg_pc_202_c", type = "label", time_format = "num")
gas_price_average <- gas_price %>%
        filter(nrg_cons == "Consumption of GJ - all bands", 
               currency == "Euro", 
               unit == "Kilowatt-hour") %>%
        rename("PricePkWh" = values)

# Create gas comparison for selected countries - using 2019 data
average_gas_composition_2019 <- gas_price_average %>%
        filter(geo %in% select_countries, 
               TIME_PERIOD == "2019", 
               PricePkWh != 0)

# Create gas price comparison chart
Gas_Chart_2019 <- average_gas_composition_2019 %>%
        ggplot(aes(x = geo, y = PricePkWh, fill = nrg_prc)) +
        geom_bar(stat = "identity") +
        labs(title = "Residential Gas Price Comparison 2019",
             x = "Country",
             y = "Euro per kWh",
             fill = "Components of Price") +
        scale_fill_viridis(discrete = TRUE) +
        theme_minimal() +
        ylim(0, 0.40) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine charts side by side
Elect_Chart_Grob <- ggplotGrob(Elec_Chart_2019)
Gas_Chart_Grob <- ggplotGrob(Gas_Chart_2019)
Combined_Price <- grid.arrange(Elect_Chart_Grob, Gas_Chart_Grob, 
                               ncol = 2,
                               top = "European Residential Energy Prices Comparison 2019")

# Optional: Add data summary table
summary_stats <- average_elec_composition_2019 %>%
        group_by(geo) %>%
        summarise(
                total_electricity_price = sum(PricePkWh),
                .groups = 'drop'
        ) %>%
        left_join(
                average_gas_composition_2019 %>%
                        group_by(geo) %>%
                        summarise(
                                total_gas_price = sum(PricePkWh),
                                .groups = 'drop'
                        ),
                by = "geo"
        )

png("combined_energy_prices_2019.png", width = 15, height = 7, units = "in", res = 300)
grid.arrange(Elect_Chart_Grob, Gas_Chart_Grob, 
             ncol = 2,
             top = "European Residential Energy Prices Comparison 2019")
dev.off()