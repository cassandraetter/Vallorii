
# Load libraries
library(quantmod)
library(ggplot2)
library(lubridate)
library(tidyr)
library(httr)
library(jsonlite)
library(dplyr)

#### Stock Data #### 
## Source: Yahoo Finance
# Define the ticker symbols
tickers <- c("SVT.L", "UU.L", "PNN.L")
company_names <- c("Severn Trent", "United Utilities", "Pennon Group")

# Set the time period (you can adjust these dates)
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2025-01-01")
#end_date <- Sys.Date()

# Create an empty list to store the data
stock_data <- list()

# Fetch data for each company
for(i in seq_along(tickers)) {
        stock_data[[i]] <- try(getSymbols(tickers[i], 
                                          src = "yahoo",
                                          from = start_date,
                                          to = end_date,
                                          auto.assign = FALSE))
}

# Create a data frame for plotting
plot_data <- data.frame(Date = index(stock_data[[1]]))

# Add closing prices for each company
for(i in seq_along(stock_data)) {
        plot_data[, company_names[i]] <- as.numeric(Cl(stock_data[[i]]))
}

# Reshape data for ggplot
library(tidyr)
plot_data_long <- gather(plot_data, Company, Price, -Date)

# Create the plot
p <- ggplot(plot_data_long, aes(x = Date, y = Price, color = Company)) +
        geom_line(size = 1.5) +                                         # Thicker lines
        theme_minimal() +
        labs(title = "",
             x = "",
             y = "Nominal Price (GBP)",
             color = "Company") +
        theme(
                axis.title.y = element_text(size = 50),                 # Larger y-axis title
                axis.text.y = element_text(size = 40),
                axis.text.x = element_text(size = 40),
                legend.position = "none",                               # Remove legend
                panel.grid.major = element_line(size = 0.2),           # More visible gridlines
                panel.grid.minor = element_blank()                      # Remove minor gridlines
        )
ggsave("water_companies_prices.png", p, width = 13.33, height = 10, dpi = 300)


# Print the latest prices
latest_prices <- tail(plot_data, 1)
print("Latest Stock Prices:")
print(latest_prices)

# Calculate basic statistics
for(company in company_names) {
        cat("\nSummary statistics for", company, ":\n")
        print(summary(plot_data[[company]]))
}

#### Inflation Adjusted Stock Data #### 
### Source: ONS 

# Get latest version of CPIH data
base_url <- "https://api.beta.ons.gov.uk/v1/datasets/cpih01"
editions_response <- GET(paste0(base_url, "/editions/time-series/versions"))
editions_data <- fromJSON(rawToChar(editions_response$content))
latest_version <- editions_data$items$version[1]  # Get the most recent version

# Now get the CPIH data using latest version
url <- paste0(base_url, "/editions/time-series/versions/", latest_version)
response <- GET(url)
data <- fromJSON(rawToChar(response$content))
download_url <- data$downloads$csv$href
cpih_response <- GET(download_url)
cpih_raw <- read.csv(text = rawToChar(cpih_response$content))

# Clean CPIH data
cpih_clean <- cpih_raw %>%
        filter(Aggregate == "Overall Index") %>%
        select(Time, v4_0) %>%
        mutate(date = as.Date(paste0("01 ", Time), format="%d %b-%y")) %>%
        rename(cpih = v4_0) %>%
        select(date, cpih)

print("CPIH date range:")
print(paste("From:", format(min(cpih_clean$date), "%B %Y")))
print(paste("To:", format(max(cpih_clean$date), "%B %Y")))

# Create a sequence of first days of months for the past 5 years
dates <- seq(floor_date(Sys.Date() - years(5), "month"), 
             floor_date(Sys.Date(), "month"), 
             by = "month")

# Function to get stock data just for these dates
get_monthly_stock <- function(ticker) {
        # Get all data
        stock <- getSymbols(ticker, src="yahoo", 
                            from = min(dates) - days(5),  # Get a few days before to ensure we have data
                            to = max(dates) + days(5),    # Get a few days after to ensure we have data
                            auto.assign = FALSE)
        
        # Convert to data frame
        stock_df <- data.frame(
                date = index(stock),
                price = as.numeric(Cl(stock))
        )
        
        # For each first of month, get the closest available trading day
        monthly_data <- data.frame(date = dates) %>%
                mutate(
                        trading_date = date
                )
        
        # For each target date, find the closest available trading day price
        result <- data.frame(
                date = dates,
                price = sapply(dates, function(d) {
                        available_dates <- stock_df$date
                        closest_date <- available_dates[which.min(abs(available_dates - d))]
                        stock_df$price[stock_df$date == closest_date]
                })
        )
        
        return(result)
}

svt_data <- get_monthly_stock("SVT.L")
uu_data <- get_monthly_stock("UU.L")
pnn_data <- get_monthly_stock("PNN.L")

stocks_monthly <- data.frame(
        date = svt_data$date,
        SVT.L = svt_data$price,
        UU.L = uu_data$price,
        PNN.L = pnn_data$price
)

print("Sample of monthly stock data:")
print(head(stocks_monthly))

final_data <- stocks_monthly %>%
        left_join(cpih_clean, by = "date") %>%
        na.omit() %>%
        mutate(
                adj_factor = last(cpih) / cpih,
                SVT.L_adj = SVT.L * adj_factor,
                UU.L_adj = UU.L * adj_factor,
                PNN.L_adj = PNN.L * adj_factor
        )

plot_data <- final_data %>%
        select(date, SVT.L_adj, UU.L_adj, PNN.L_adj) %>%
        gather(key = "Company", value = "Price", -date) %>%
        mutate(Company = case_when(
                Company == "SVT.L_adj" ~ "Severn Trent",
                Company == "UU.L_adj" ~ "United Utilities",
                Company == "PNN.L_adj" ~ "Pennon Group"
        ))

# Create plot
ggplot(plot_data, aes(x = date, y = Price, color = Company)) +
        geom_line() +
        theme_minimal() +
        labs(title = "UK Water Companies Stock Prices (Last 5 Years)",
             subtitle = paste0("Monthly Data, Inflation-Adjusted using CPIH (Latest: ", 
                               format(max(cpih_clean$date), "%B %Y"), ")"),
             x = "Date",
             y = "Price (GBP, adjusted to latest month prices)",
             color = "Company") +
        theme(
                plot.title = element_text(size = 20, face = "bold"),  # Larger title
                plot.subtitle = element_text(size = 16),              # Larger subtitle
                legend.position = "bottom"
        )

##### Water Costs OECD ####


# National average data from OECD and International Water Association (2023)
water_costs <- data.frame(
        country = c("CHE", "DEU", "GBR", "NLD", "AUS", "FRA", "JPN", "CAN", "USA"),
        cost_per_m3 = c(6.45, 5.11, 4.89, 4.82, 4.35, 4.20, 3.85, 3.65, 3.04)
)

water_costs_2 <- data.frame(
        country = c("CHE", "DEU", "GBR", "NLD", "AUS", "FRA", "JPN", "CAN", "USA"),
        cost_per_m3 = c(2.43, 3.92, 2.82, 4.5, 3.1, 3.4, 3.85, 3.65, 3.04)
)

# Create the visualization
ggplot(water_costs, aes(x = reorder(country, -cost_per_m3), y = cost_per_m3)) +
        geom_bar(stat = "identity", 
                 fill = ifelse(water_costs$country == "GBR", "#c70266", "#5d56cf"),
                 width = 0.7) +
        geom_text(aes(label = sprintf("€%.2f", cost_per_m3)), 
                  vjust = -0.5, 
                  size = 12,  # Increased from 4
                  fontface = "bold") +
        labs(
                title = "National Average Water Costs",
                subtitle = "Combined water supply and wastewater charges (EUR/m³)",
                x = NULL,
                y = "Cost (EUR/m³)",
        ) +
        theme_minimal() +
        theme(
                axis.text.x = element_text(angle = 45,  # Added angle to prevent overlap with larger text
                                           hjust = 1, 
                                           size = 24),    # Increased from 14
                axis.text.y = element_text(size = 22),  # Increased from 12
                axis.title.y = element_text(size = 26), # Added explicit size for y-axis title
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                plot.title = element_text(face = "bold", 
                                          size = 36,      # Increased from 24
                                          margin = margin(b = 20)), # Added space below title
                plot.subtitle = element_text(size = 28, # Increased from 16
                                             color = "grey40",
                                             margin = margin(b = 0)), # Added space below subtitle
                plot.caption = element_text(size = 20,  # Increased from 10
                                            color = "grey40"),
                plot.margin = margin(t = 40, r = 40, b = 40, l = 40) # Increased margins
        ) +
        scale_y_continuous(
                labels = function(x) sprintf("€%.2f", x),
                expand = expansion(mult = c(0, 0.2))    # Increased top expansion for larger value labels
        )

# PowerPoint standard dimensions (16:9 aspect ratio) with increased height
ggsave("national_water_costs.png", width = 15, height = 7, dpi = 300)


#### Water Quality ####

# Create the new data frame
water_quality <- data.frame(
        country = c("GBR", " USA", "DEU", "NLD", 
                    "AUS", "CAN", "JPN", "CHE", "FRA"),
        quality_percentage = c(33, 42, 77, 13, 84, 88, 57, 79, 85)
)


ggplot(water_quality, aes(x = reorder(country, -quality_percentage), y = quality_percentage)) +
        geom_bar(stat = "identity", 
                 fill = ifelse(water_quality$country == "GBR", "#c70266", "#5d56cf"),
                 width = 0.7) +
        geom_text(aes(label = sprintf("%d%%", quality_percentage)), 
                  vjust = -0.5, 
                  size = 12,
                  fontface = "bold") +
        labs(
                title = "National Water Quality Ratings",
                subtitle = "Percentage of water bodies with good ambient water quality",
                x = NULL,
                y = "Quality Rating (%)",
        ) +
        theme_minimal() +
        theme(
                axis.text.x = element_text(angle = 45,
                                           hjust = 1, 
                                           size = 24),
                axis.text.y = element_text(size = 22),
                axis.title.y = element_text(size = 26),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                plot.title = element_text(face = "bold", 
                                          size = 36,
                                          margin = margin(b = 0)),
                plot.subtitle = element_text(size = 28,
                                             color = "grey40",
                                             margin = margin(b = 20)),
                plot.caption = element_text(size = 20,
                                            color = "grey40"),
                plot.margin = margin(t = 40, r = 40, b = 40, l = 40)
        ) +
        scale_y_continuous(
                limits = c(0, 100),  # Set y-axis from 0-100 since it's percentage
                breaks = seq(0, 100, 20),  # Breaks every 20%
                expand = expansion(mult = c(0, 0.1))
        )

# Save with same dimensions as before
ggsave("national_water_quality.png", width = 15, height = 7, dpi = 300)

# Create combined data frame
combined_water_data <- data.frame(
        country = water_quality$country,
        quality_percentage = water_quality$quality_percentage,
        cost_per_m3 = water_costs$cost_per_m3  # Assuming countries are in same order
)

# Add source information as attributes
attr(combined_water_data, "quality_source") <- "UN Water"
attr(combined_water_data, "cost_source") <- "OECD Water Statistics"

# Export to CSV
write.csv(combined_water_data, "water_metrics.csv", row.names = FALSE)
writexl::write_xlsx(combined_water_data, "water_metric.xlsx" , row.names(FALSE))

ggplot(combined_water_data, 
       aes(x = quality_percentage, 
           y = cost_per_m3)) +
        geom_smooth(method = "lm", 
                    color = "grey40",
                    alpha = 0.2,  # More transparent fill
                    size = 2) +  # Thicker line
        # Add points
        geom_point(aes(fill = country == "GBR"),
                   size = 14,
                   shape = 21,
                   color = "white",
                   stroke = 2) +
        geom_text(aes(label = country,
                      vjust = ifelse(country == "GBR", 1.5, -1.2)),
                  size = 12,
                  fontface = "bold") +
        scale_fill_manual(values = c("FALSE" = "#5d56cf", "TRUE" = "#c70266"),
                          guide = "none") +
        labs(
                x = "Water Quality Rating (%)",
                y = "Cost (EUR/m³)"
        ) +
        theme_minimal() +
        theme(
                axis.text = element_text(size = 28),
                axis.title = element_text(size = 32),
                plot.margin = margin(t = 40, r = 40, b = 40, l = 40)
        ) +
        scale_x_continuous(limits = c(0, 100),
                           breaks = seq(0, 100, 20)) +
        scale_y_continuous(labels = function(x) sprintf("€%.2f", x))

# Save with dimensions optimized for scatter plot
ggsave("water_quality_vs_cost.png", width = 15, height = 12, dpi = 300)


