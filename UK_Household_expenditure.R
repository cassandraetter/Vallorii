UK_expenditure <- UK %>%
        dplyr::select(pidp, year, fihhmnnet1_dv, hh_incomey, rent_dv, xpfood1_g3, houscost1_dv, ttlener) %>%
        group_by(year) %>%
        summarise(
                average_income = mean(hh_incomey, na.rm = TRUE),
                housing = mean(coalesce(rent_dv, houscost1_dv), na.rm = TRUE) * 12,
                energy = mean(ttlener, na.rm = TRUE),
                petrol = mean(xpgasy, na.rm = TRUE),
                food = mean(xpfood1_g3, na.rm = TRUE) * 12
        )
table_expenditure <- tableGrob(UK_expenditure)
ggsave("expenditure_UK.png", table_expenditure, width = 10, height = 6)
write.csv(UK_expenditure, "expenditure_UK.csv", row.names = FALSE)