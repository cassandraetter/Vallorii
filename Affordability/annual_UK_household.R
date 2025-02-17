library(haven)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(scales)
library(gridExtra)
library(ggthemes)
library(plotly)
library(viridis)
library(ggrepel)


setwd("~/Documents/GitHub/Vallorii/LCF")



# Function to normalize column names to lowercase
normalize_colnames <- function(data) {
        names(data) <- tolower(names(data))
        return(data)
}

process_year <- function(year) {
        file_path <- paste0(year, "_dvhh_ukanon.dta")
        print(paste("Processing:", file_path))
        
        if(!file.exists(file_path)) {
                print(paste("File not found:", file_path))
                return(NULL)
        }
        
        tryCatch({
                # Read data and normalize column names
                data <- read_stata(file_path) %>% normalize_colnames()
                print(paste("Successfully read file for year:", year))
                print(paste("Number of rows:", nrow(data)))
                
                # Check column names (now using lowercase)
                print("Checking columns...")
                required_cols <- tolower(c("a049", "a103", "a128", "a129", "a130"))
                missing_cols <- setdiff(required_cols, names(data))
                if(length(missing_cols) > 0) {
                        print(paste("Missing columns:", paste(missing_cols, collapse=", ")))
                        print("Available columns:")
                        print(names(data))
                        stop("Missing required columns")
                }
                
                # Select columns (using lowercase names)
                processed_data <- data %>%
                        select(any_of(tolower(c("case", "a049", "a103", "a128", "a129", "a130",
                                                "a150", "a151", "a152", "a153", "a154", "a155", "a156",
                                                "b010", "b020", "b029p", "b050", "b055p",
                                                "b110", "b160", "b166b", "b1661b", "b167",
                                                "b170", "b489", "b231", "b181", "b187", "b188", "b195b",
                                                "b229", "b270", "b480", "b4804", "b481", "b4815",
                                                "b487", "b488", "b1701", "b233", "b490",
                                                "ctsproxy", "ctwproxy",
                                                "p515tp", "p538t", "p540t", "p539t", "p607t", "p542", "p551tp",
                                                "c12111t", "c12121t", "c11251t", "c11141t", "c72211t",
                                                "cc1311t", "cc3111t", "cc1316t", "c45112t", "c45212t",
                                                "c73112t", "c93411t", "c93412t", "c45214t", "c45222t",
                                                "p116t", "p118t", "p119t",
                                                "p352p", "p344p", "p389p", "p392p", "p515p", "weighta")))) %>%
                        mutate(year = year)
                
                print(paste("Columns after select:", paste(names(processed_data), collapse=", ")))
                
                # Rename columns (using lowercase for original names)
                renamed_data <- processed_data %>%
                        rename(
                                household_size = tolower("a049"),
                                gas_electric_included = tolower("a103"),
                                gas_method = tolower("a128"),
                                duel_method = tolower("a129"),
                                elec_method = tolower("a130"),
                                heat_electric = tolower("a150"),
                                heat_gas = tolower("a151"),
                                heat_oil = tolower("a152"),
                                heat_solid = tolower("a153"),
                                heat_solidoil = tolower("a154"),
                                heat_calor = tolower("a155"),
                                heat_other = tolower("a156"),
                                rent_net = tolower("b010"),
                                rent_service = tolower("b020"),
                                council_tax = tolower("b029p"),
                                water_charge = tolower("b050"),
                                sewage_charge = tolower("b055p"),
                                housing_insurance = tolower("b110"),
                                education_expense = tolower("b160"),
                                telephone_exp = tolower("b166b"),
                                mobile_exp = tolower("b1661b"),
                                comms_expenses = tolower("b167"),
                                gas_expenses = tolower("b170"),
                                gas_meter = tolower("b1701"),
                                gas_payment = tolower("b490"),
                                elec_expenses = tolower("b489"),
                                duel_energy = tolower("b231"),
                                duel_prepayment = tolower("b233"),
                                tv_a_exp = tolower("b181"),
                                vehicle_road_tax = tolower("b187"),
                                vehicle_insurance = tolower("b188"),
                                internet_wk = tolower("b195b"),
                                insurance_exp = tolower("b229"),
                                furniture = tolower("b270"),
                                holiday_UK = tolower("b480"),
                                cruise_UK = tolower("b4804"),
                                holiday_outside = tolower("b481"),
                                cruise_outside = tolower("b4815"),
                                domestic_flights = tolower("b487"),
                                int_flight = tolower("b488"),
                                council_water = tolower("ctwproxy"),
                                food_exp = tolower("p538t"),
                                tobacco_exp = tolower("p540t"),
                                alcohol_exp = tolower("p539t"),
                                transport_costs = tolower("p607t"),
                                clothing_exp = tolower("p542"),
                                total_exp = tolower("p551tp"),
                                coffee_exp = tolower("c12111t"),
                                tea_exp = tolower("c12121t"),
                                sausage_exp = tolower("c11251t"),
                                cakes_exp = tolower("c11141t"),
                                petrol_exp = tolower("c72211t"),
                                tp_exp = tolower("cc1311t"),
                                jewelry_exp = tolower("cc3111t"),
                                cosmetics_exp = tolower("cc1316t"),
                                elec_slot_payment = tolower("c45112t"),
                                gas_slot_payment = tolower("c45214t"),
                                gas_bottle_slot = tolower("c45222t"),
                                railway_exp = tolower("c73112t"),
                                pet_food = tolower("c93411t"),
                                pet_acc = tolower("c93412t"),
                                housing_expenses = tolower("p515tp"),
                                food_expenses = tolower("p118t"),
                                alcohol_expenses = tolower("p119t"),
                                gross_income = tolower("p352p"),
                                gross_normal = tolower("p344p"),
                                disposable_income = tolower("p389p"),
                                taxes = tolower("p392p")
                        )
                
                print("Successfully renamed columns")
                
                # Clean and process data
                cleaned_data <- renamed_data %>%
                        mutate(across(where(is.numeric), ~if_else(. > 0, ., NA_real_)))
                
                # Calculate annual values
                annual_data <- cleaned_data %>%
                        mutate(
                                annual_gross_income = gross_income * 52,
                                annual_gross_normal = gross_normal * 52,
                                annual_disposable_income = disposable_income * 52,
                                annual_taxes = taxes * 52,
                                annual_rent_net = rent_net * 52,
                                annual_water_charge = water_charge * 52,
                                annual_council_water = council_water,
                                annual_sewage_charge = sewage_charge * 52,
                                annual_housing_expenses = housing_expenses * 52,
                                annual_gas_expenses = gas_expenses * 52,
                                annual_elec_expenses = elec_expenses * 52,
                                annual_duel_energy = duel_energy * 52,
                                annual_elec_slot = elec_slot_payment * 52,
                                annual_gas_slot = gas_slot_payment * 52,
                                annual_gas_meter = gas_meter * 52,
                                annual_duel_prepay = duel_prepayment * 52,
                                annual_gas_payment = gas_payment * 52,
                                annual_gas_bottle = gas_bottle_slot * 52,
                                annual_telephone = telephone_exp * 52,
                                annual_mobile = mobile_exp * 52,
                                annual_comms = comms_expenses * 52,
                                annual_internet = internet_wk * 52,
                                annual_food = food_exp * 52,
                                annual_food_personal = food_expenses * 52,
                                annual_alcohol = alcohol_exp * 52,
                                annual_alcohol_personal = alcohol_expenses * 52,
                                annual_tobacco = tobacco_exp * 52,
                                annual_coffee = coffee_exp * 52,
                                annual_tea = tea_exp * 52,
                                annual_sausage = sausage_exp * 52,
                                annual_cakes = cakes_exp * 52,
                                annual_transport = transport_costs * 52,
                                annual_railway = railway_exp * 52,
                                annual_petrol = petrol_exp * 52,
                                annual_domestic_flights = domestic_flights * 12,
                                annual_int_flights = int_flight * 12,
                                annual_holiday_UK = holiday_UK * 12,
                                annual_cruise_UK = cruise_UK * 12,
                                annual_holiday_outside = holiday_outside *4,
                                annual_cruise_outside = cruise_outside *4,
                                annual_vehicle_tax = vehicle_road_tax,
                                annual_vehicle_insurance = vehicle_insurance * 52,
                                annual_housing_insurance = housing_insurance * 52,
                                annual_medical_insurance = insurance_exp * 52,
                                annual_council_tax = council_tax * 52,
                                annual_tv_license = tv_a_exp * 52,
                                annual_clothing = clothing_exp *4,
                                annual_furniture = furniture * 4,
                                annual_jewelry = jewelry_exp *4,
                                annual_cosmetics = cosmetics_exp * 52,
                                annual_tp = tp_exp * 52,
                                annual_pet_food = pet_food * 52,
                                annual_pet_acc = pet_acc,
                                annual_education = education_expense * 52,
                                annual_total_exp = total_exp * 52
                        ) %>%
                        mutate(
                                across(starts_with("annual_"), ~round(., 2)),
                                standard_bill_energy = rowSums(select(., annual_gas_expenses, annual_elec_expenses), na.rm = TRUE),
                                dual_fuel_energy = annual_duel_energy,
                                prepayment_energy = rowSums(select(., annual_elec_slot, annual_gas_slot, annual_duel_prepay), na.rm = TRUE),
                                energy_expenditure = pmax(standard_bill_energy, dual_fuel_energy, prepayment_energy, na.rm = TRUE),
                                water_expenditure = rowSums(select(., annual_council_water, annual_water_charge, annual_sewage_charge), na.rm = TRUE),
                                telecomms_expenditure = rowSums(select(., annual_telephone, annual_internet, annual_mobile, annual_comms), na.rm = TRUE),
                                rail_expenditure = annual_railway
                        )
                
                print(paste("Successfully processed data for year:", year))
                return(annual_data)
                
        }, error = function(e) {
                print(paste("Error in processing year", year, ":", e$message))
                return(NULL)
        })
}

# Process all years
years <- 2010:2022
all_data <- list()

for(year in years) {
        year_data <- process_year(year)
        if(!is.null(year_data)) {
                all_data[[as.character(year)]] <- year_data
                print(paste("Successfully added year:", year))
        }
}

# Combine all years
combined_data <- bind_rows(all_data)
print(paste("Total rows in combined data:", nrow(combined_data)))
print("Unique years in combined data:")
print(unique(combined_data$year))

# Create summary by year
summary_by_year <- combined_data %>%
        group_by(year) %>%
        summarise(across(c(starts_with("annual_"),
                           energy_expenditure, water_expenditure,
                           telecomms_expenditure, rail_expenditure),
                         ~weighted.mean(., w = weighta, na.rm = TRUE))) %>%
        mutate(across(everything(), ~round(., 2)))

# Save the summary
write_csv(summary_by_year, "UK_expenditure_by_year.csv")
print("Summary by year completed and saved.")


# Create a long format dataset for plotting
plot_data <- summary_by_year %>%
        select(year, energy_expenditure, water_expenditure, 
               telecomms_expenditure, rail_expenditure) %>%
        pivot_longer(cols = -year, 
                     names_to = "expenditure_type",
                     values_to = "amount")

# Create the plot
ggplot(plot_data, aes(x = year, y = amount, color = expenditure_type)) +
        geom_line(size = 1, alpha = 0.8) +
        geom_point(size = 3, shape = 21, fill = "white") +
        scale_y_continuous(
                expand = c(0, 0),
                limits = c(0, 2000),
                labels = scales::label_dollar(prefix = "£", big.mark = ",")
        ) +
        scale_x_continuous(breaks = unique(plot_data$year)) +
        scale_color_brewer(
                palette = "Set2",
                labels = c("Energy", "Rail", "Telecoms", "Water") 
        ) +
        theme_minimal() +
        theme(
                plot.title = element_text(face = "bold", size = 14, margin = margin(b = 20)),
                plot.subtitle = element_text(size = 11, color = "grey40", margin = margin(b = 20)),
                axis.title = element_text(size = 11),
                axis.text = element_text(size = 10),
                legend.title = element_text(size = 11),
                legend.text = element_text(size = 10),
                panel.grid.minor = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_line(color = "grey90"),
                legend.position = "bottom",
                legend.box = "horizontal",
                legend.margin = margin(t = 20),
                legend.key.width = unit(2, "cm"),
                plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
        ) +
        labs(
                title = "UK Household Expenditure (2018-2022)",
                x = "Year",
                y = "Annual Expenditure",
                color = "Category"
        )

# Save the plot
ggsave("expenditure_trends.png", width = 12, height = 8)

### 2010- 2017 have different column names so they didn't load, but I will come back to this later if necessary

###___________________________________
###----------------------------------%%---------%%%


# Calculate utilities bundle and other major components
components_data <- summary_by_year %>%
        mutate(
                utilities = energy_expenditure + water_expenditure + 
                        telecomms_expenditure + rail_expenditure,
                housing = annual_housing_expenses,
                food = annual_food,
                taxes = annual_taxes
        ) %>%
        select(year, utilities, housing, food, taxes) %>%
        pivot_longer(-year, 
                     names_to = "component",
                     values_to = "expenditure") %>%
        mutate(
                component = str_to_title(component),
                # Reorder factors for stacked area chart
                component = factor(component, 
                                   levels = c("Taxes", "Food", "Utilities", "Housing"))
        )

# Create stacked area chart
components_plot <- ggplot(components_data, 
                          aes(x = year, y = expenditure, fill = component)) +
        geom_area() +
        scale_fill_viridis(discrete = TRUE, direction = -1) +
        theme_minimal() +
        labs(title = "Major Household Expense Components (2018 - 2022)",
             subtitle = "Utilities includes energy, water, telecommunications, and rail",
             x = "Year",
             y = " Average Annual HH Expenditure (£, Current)",
             fill = "Component") +
        scale_y_continuous(labels = scales::comma_format()) +
        theme(
                legend.position = "bottom",
                plot.title = element_text(face = "bold", size = 14),
                plot.subtitle = element_text(size = 10, color = "gray50"),
                axis.title = element_text(face = "bold"),
                legend.title = element_text(face = "bold")
        )

# Create percentage stacked area chart
components_pct_plot <- components_data %>%
        group_by(year) %>%
        mutate(total = sum(expenditure),
               pct = expenditure / total * 100) %>%
        ungroup() %>%
        ggplot(aes(x = year, y = pct, fill = component)) +
        geom_area() +
        scale_fill_viridis(discrete = TRUE, direction = -1) +
        theme_minimal() +
        labs(title = "Relative Share of Major Household Expenses (2018-2022)",
             subtitle = "Utilities includes energy, water, telecommunications, and rail",
             x = "Year",
             y = "Percentage of Total Expenses (£, Current)",
             fill = "Component") +
        scale_y_continuous(labels = function(x) paste0(x, "%")) +
        theme(
                legend.position = "bottom",
                plot.title = element_text(face = "bold", size = 14),
                plot.subtitle = element_text(size = 10, color = "gray50"),
                axis.title = element_text(face = "bold"),
                legend.title = element_text(face = "bold")
        )


# Calculate utilities trends
utilities_trends <- summary_by_year %>%
        mutate(
                utilities = energy_expenditure + water_expenditure + 
                        telecomms_expenditure + rail_expenditure
        ) %>%
        select(year, utilities) %>%
        mutate(
                yoy_change = (utilities - lag(utilities)) / lag(utilities) * 100
        )


# Save all plots
ggsave("major_components.png", components_plot, width = 12, height = 8)

components_plotly <- ggplotly(components_plot)
htmlwidgets::saveWidget(components_plotly, "major_components.html")

# Calculate and print summary statistics
summary_stats <- components_data %>%
        group_by(year) %>%
        mutate(total = sum(expenditure),
               pct_of_total = expenditure / total * 100) %>%
        filter(year == latest_year) %>%
        arrange(desc(pct_of_total)) %>%
        select(component, expenditure, pct_of_total) %>%
        mutate(
                expenditure = scales::comma(round(expenditure, 2)),
                pct_of_total = round(pct_of_total, 1)
        )

# Print summaries
print("Latest Year Summary Statistics:")
print(summary_stats)

print("\nUtilities Trend Summary:")
print(utilities_trends %>% 
              select(year, utilities, yoy_change) %>%
              mutate(across(where(is.numeric), ~round(., 2))) %>%
              arrange(desc(year)))