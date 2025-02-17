library(haven)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(ggforce)
library(plotly)

## import data from 2023 Cost and Food Survey --- NOTE THIS IS NOT COMMERCIAL DATA. It is available to be purchased for £50 per dataset (year) + £450 for the project. 
data <- read_stata("LCF/dvhh_ukanon_2022.dta")
df <- read_stata("LCF/dvhh_urbanrural_ukanon_2022.dta")

filtered_subset <- data %>%
        select(case, a049, a103, a128, a129, a130, 
               a150, a151, a152, a153, a154, a155, a156,
               b010, b020, b029p, b050, b055p,
               b110, b160, B166b, B1661b, b167,
               b170, b489, b231, b181, b187, b188, B195b,
               b229, b270, b480, b4804, b481, b4815,
               b487, b488, b1701, b233, b490, 
               # Council and utilities
               ctsproxy, ctwproxy,
               # Expenditure categories
               p515tp, p538t, p540t, p539t, p607t, p542, p551tp, p607t,
               p071h, b228,
               # Specific items
               c12111t, c12121t, c11251t, c11141t, c72211t,
               cc1311t, cc3111t, cc1316t, c45112t, c45212t,
               c73112t, c93411t, c93412t, c45214t, c45222t, 
               # Personal expenditure and income
               p116t, p118t, p119t,
               P352p, p344p, P389p, P392p, p515p,
               
               ##weight
               weighta) %>%
        rename(
                household_size = a049,#household size 
                gas_electric_included = a103, #gas and electric in accomodation
                gas_method = a128, # gas payment method 1 = Direct 2 = receipt of bill 3 = prepayment
                duel_method = a129, # combined method 1 = DD, 2 = receipt 3= pre-payment 4 = rent 5 = cash 6 = benefits 7 = annual 8 = other  9 = someone outside house
                elec_method = a130, # elec method (same as duel method)
                heat_electric = a150, # 1= heat by electric
                heat_gas = a151, # 1 = heat by gas
                heat_oil = a152, # 1 = heat by oil
                heat_solid = a153, # 1 = heat by solid fuel
                heat_solidoil = a154, #1 = heat by solid fule and oil
                heat_calor = a155, #1 = eaht by calor gas
                heat_other = a156, # heat by other gas 
                rent_net = b010, # last rent payment net
                rent_service = b020, # rent service charge deducted net
                council_tax = b029p, # council tax - payable after status discount
                water_charge = b050, # water charges - last net payment
                sewage_charge = b055p, # sewage basic weekly charge 
                housing_insurance = b110,    # Hstructure insurance last payment
                education_expense = b160, # amount paid for education last quarter
                telephone_exp = B166b, # telephone amount
                mobile_exp = B1661b, #mobile single bill 
                comms_expenses = b167, # combined tv telecom and internet bills
                gas_expenses = b170, #gas amount paid last bill 
                gas_meter = b1701, # separate gas meter payment
                gas_payment = b490, # gas amount paid in last account - less rebate
                elec_expenses = b489, # electricity amount paid in last account
                duel_energy = b231, # combined electricity and gas - account
                duel_prepayment =b233, # combined elec and gas.  -- prepayment
                tv_a_exp = b181, # annual TV license 
                vehicle_road_tax = b187, # vehicle road tax last year
                vehicle_insurance = b188, # vehicle insurance last year
                internet_wk = B195b, # internet weekly - single bill 
                insurance_exp = b229, # medical insurance -total amount premium
                furniture = b270, # furniture
                holiday_UK = b480, # holiday package in the UK
                cruise_UK = b4804, # cruise package in UK
                holiday_outside = b481, # holiday package outside of the UK
                cruise_outside = b4815, # cruise package outside UK
                domestic_flights = b487, # domestic flights
                int_flight = b488, # international flights 
                council_water = ctwproxy, # anonymized council water charge 
                food_exp = p538t,    # Food ONS adults and children 
                tobacco_exp = p540t, # tobacco ONS adults and children
                alcohol_exp = p539t, # alcoholic drinks adults and children
                transport_costs = p607t,  # Transport expenditure
                clothing_exp = p542,     # Clothing and footwear
                total_exp = p551tp, # total expenditure 
                coffee_exp = c12111t, # coffee -- children and adults
                tea_exp = c12121t, # tea - children and adults 
                sausage_exp = c11251t, # sausage -- children and adults
                cakes_exp = c11141t, # cakes and puddings - adults and children 
                petrol_exp = c72211t, # petrol -adults and children  
                tp_exp = cc1311t, # toilet paper  - adults and children
                jewelry_exp = cc3111t, # jewellery, clocks and watches -- adults and children
                cosmetics_exp = cc1316t, # cosmetics and related accessories -- adults and children
                elec_slot_payment = c45112t, # electricity slot meter payment - children and adults
                gas_slot_payment = c45214t, # gas slot meeter payment - children and adults
                gas_bottle_slot = c45222t, # bottled gas- children and adults
                railway_exp = c73112t, # railway and tube fares other than season tickets - adults and children
                pet_food = c93411t, # petfood children and adults 
                pet_acc = c93412t, # pet purcahse and accessories-- children and adults
                housing_expenses = p515tp, # housing personal expenditure -- childre nadn adults
                food_expenses = p118t, # food personal expenditure children and adults
                alcohol_expenses = p119t, # alcoholic drink 
                gross_income = P352p, # Gross current income of household - top coded
                gross_normal = p344p, #gross normal weekly household income - top coded 
                disposable_income = P389p, # normal weekly disposable income top coded
                taxes = P392p, # income tax payments less refunds top coded 
                transport_costs = p607t, # COICOP: Total transport costs - children and adults 
                pension = p071h, # pension and superannuation contribution -- household 
                pension2 = b228 # personal pension
        )

filtered_subset <- filtered_subset %>%
        mutate(across(where(is.numeric), ~if_else(. > 0, ., NA_real_)))
##### -----------------------------

annual_expenditure <- filtered_subset %>%
        mutate(
                # Income and taxes (weekly to annual)
                annual_gross_income = gross_income * 52,
                annual_gross_normal = gross_normal * 52,
                annual_disposable_income = disposable_income * 52,
                annual_taxes = taxes * 52,
                annual_pension = pension * 52,
                annual_pension2 = pension2 * 52,
                
                # Housing and utilities (weekly to annual)
                annual_rent_net = rent_net * 52,
                annual_water_charge = water_charge * 52,
                annual_council_water = council_water,
                annual_sewage_charge = sewage_charge * 52,
                annual_housing_expenses = housing_expenses * 52,
                
                # Energy expenses (converting bill amounts appropriately)
                annual_gas_expenses = gas_expenses * 52, 
                annual_elec_expenses = elec_expenses * 52,  
                annual_duel_energy = duel_energy * 52,  
                annual_elec_slot = elec_slot_payment * 52,  
                annual_gas_slot = gas_slot_payment * 52, 
                annual_gas_meter = gas_meter * 52,  
                annual_duel_prepay = duel_prepayment * 52, 
                annual_gas_payment = gas_payment * 52,  
                annual_gas_bottle = gas_bottle_slot * 52,
                
                # Communications (weekly to annual)
                annual_telephone = telephone_exp * 52,
                annual_mobile = mobile_exp * 52,
                annual_comms = comms_expenses * 52,
                annual_internet = internet_wk * 52,
                
                # Food and beverages (weekly to annual)
                annual_food = food_exp * 52,
                annual_food_personal = food_expenses * 52,
                annual_alcohol = alcohol_exp * 52,
                annual_alcohol_personal = alcohol_expenses * 52,
                annual_tobacco = tobacco_exp * 52,
                annual_coffee = coffee_exp * 52,
                annual_tea = tea_exp * 52,
                annual_sausage = sausage_exp * 52,
                annual_cakes = cakes_exp * 52,
                
                # Transport (weekly to annual)
                annual_transport = transport_costs * 52,
                annual_railway = railway_exp * 52,
                annual_petrol = petrol_exp * 52,
                
                # Travel and holidays (already annual)
                annual_domestic_flights = domestic_flights * 52,
                annual_int_flights = int_flight * 52,
                annual_holiday_UK = holiday_UK,
                annual_cruise_UK = cruise_UK,
                annual_holiday_outside = holiday_outside,
                annual_cruise_outside = cruise_outside,
                
                # Insurance and tax
                annual_vehicle_tax = vehicle_road_tax,
                annual_vehicle_insurance = vehicle_insurance * 52,
                annual_housing_insurance = housing_insurance * 52,
                annual_medical_insurance = insurance_exp * 52,
                annual_council_tax = council_tax * 52,
                annual_tv_license = tv_a_exp,
                
                # Other expenses (weekly to annual)
                annual_clothing = clothing_exp,
                annual_furniture = furniture,
                annual_jewelry = jewelry_exp,
                annual_cosmetics = cosmetics_exp,
                annual_tp = tp_exp * 52,
                annual_pet_food = pet_food * 52,
                annual_pet_acc = pet_acc,
                
                # Education
                annual_education = education_expense * 52,
                
                # Total expenditure
                annual_total_exp = total_exp * 52
        ) %>%
        # Round all values to 2 decimal places
        mutate(across(starts_with("annual_"), ~round(., 2))) %>%
        mutate(
                standard_bill_energy = rowSums(select(., annual_gas_expenses, annual_elec_expenses), na.rm = TRUE),
                dual_fuel_energy = annual_duel_energy,
                prepayment_energy = rowSums(select(., annual_elec_slot, annual_gas_slot, annual_duel_prepay), na.rm = TRUE),
                energy_expenditure = pmax(standard_bill_energy, dual_fuel_energy, prepayment_energy, na.rm = TRUE),
                water_expenditure = rowSums(select(., annual_council_water, annual_water_charge, annual_sewage_charge), na.rm = TRUE),
                telecomms_expenditure = rowSums(select(., annual_telephone, annual_internet, annual_mobile, annual_comms), na.rm = TRUE),
                rail_expenditure = rowSums(select(., annual_railway), na.rm = TRUE), 
                airfare_expenditure = rowSums(select(., annual_domestic_flights, annual_int_flights), na.rm = TRUE), 
        )


summary_expenditure <- annual_expenditure %>%
        # Summarize all annual columns and combined expenditures to get means
        summarise(across(c(starts_with("annual_"), 
                           energy_expenditure, water_expenditure,
                           telecomms_expenditure, rail_expenditure, housing_expenses, airfare_expenditure), 
                         ~weighted.mean(., w = weighta, na.rm = TRUE))) %>%
        # Round all values to 2 decimal places
        mutate(across(everything(), ~round(., 2))) %>%
        select(annual_gross_income, annual_taxes, annual_food, annual_total_exp, annual_pension, annual_housing_expenses, energy_expenditure,telecomms_expenditure, rail_expenditure, water_expenditure, airfare_expenditure )




# Convert to long format and create visualization
summary_expenditure_long <- summary_expenditure %>%
        pivot_longer(
                cols = everything(),
                names_to = "category",
                values_to = "amount"
        ) %>%
        mutate(
                category = str_remove(category, "annual_"),
                category = str_replace_all(category, "_", " "),
                category = str_to_title(category),
                category = fct_reorder(category, amount)
        )

ggplot(summary_expenditure_long, aes(x = category, y = amount)) +
        geom_col(fill = "#2E86C1", alpha = 0.8) +
        coord_flip() +  
        scale_y_continuous(
                labels = scales::comma_format(prefix = "£"),
                expand = expansion(mult = c(0, 0.1))
        ) +
        labs(
                title = "Average Annual Household Expenditure",
                x = NULL,
                y = "Amount (£)"
        ) +
        theme_minimal() +
        theme(
                plot.title = element_text(face = "bold", size = 14),
                axis.text.y = element_text(size = 10),
                axis.text.x = element_text(size = 10),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.x = element_blank()
        )
print(summary_expenditure_long)


# Calculate total income for center label
avg_income <- summary_expenditure_long %>%
        filter(category == "Gross Income") %>%
        pull(amount)


# Prepare donut data
library(plotly)

# Prepare the data with formatted labels
summary_expenditure_donut <- summary_expenditure_long %>%
        filter(!category %in% c("Total Exp", "Gross Income", "Council Tax")) %>%
        mutate(
                percentage = amount / sum(amount) * 100,
                # Create hover text with full details
                hover_text = paste0(
                        category, "<br>",
                        "£", format(round(amount), big.mark=","), "<br>",
                        round(percentage, 1), "%"
                ),
                # Create simpler label for the boxes
                label = paste0(
                        category, "<br>",
                        round(percentage, 1), "%"
                )
        )

p <- plot_ly(summary_expenditure_donut, 
             labels = ~category, 
             values = ~percentage,
             type = 'pie',
             hole = 0.6,
             textposition = 'outside',
             textinfo = 'label+percent',
             hovertext = ~label,
             hoverinfo = 'text',
             insidetextfont = list(color = '#FFFFFF'),
             marker = list(
                     colors = RColorBrewer::brewer.pal(nrow(summary_expenditure_donut), "Set3"),
                     line = list(color = '#FFFFFF', width = 1)
             )
) %>%
        layout(
                title = list(
                        text = "UK Household Expenditure (2022)",
                        font = list(size = 20)
                ),
                showlegend = FALSE,
                annotations = list(
                        list(
                                x = 0.5,
                                y = 0.5,
                                text = "Household<br>Expenditure<br>2022",
                                showarrow = FALSE,
                                font = list(size = 14)
                        )
                ),
                margin = list(t = 50, l = 50, r = 50, b = 50)
        )

# Save as HTML file
htmlwidgets::saveWidget(p, "expenditure_donut.html")


print(summary_expenditure_donut %>% 
              select(category, amount, percentage) %>% 
              arrange(desc(percentage)))

ggsave("expend_donut.png", width = 12, height = 8)


write_csv(summary_expenditure_donut, "UK_expenditure.csv")