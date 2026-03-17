## bezettingsgraden per markt en totaal

library(tidyverse)
library(lubridate)

data_bezettingsgraden <- openxlsx::read.xlsx(
    "01 data raw/Kopie van Bezettingsgraden.xlsx"
) |>
    pivot_longer(cols = c("2601":"Eindtotaal"))

source("04 scripts 26/00 scr/script 00 plot functies.R")


data_bezettingsgraden |>
    filter(
        name != 'Eindtotaal',
        markt == 'totaal'
    ) |>
    mutate(name = ym(name)) |>
    ungroup() |>
    ggplot(aes(y = value, x = name, group = markt)) +
    geom_line(
        linewidth = 1,
        color = blauw_pal[1]
    ) +
    geom_text(aes(label = label_percent(accuracy = 1)(value)), vjust = -1) +
    scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.2),
        labels = label_percent(),
        expand = c(0, 0)
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +

    labs(y = NULL, x = NULL) +
    theme_os_line(legend_position = 'bottom')
ggsave("06 output figuren/fig_bezettingsgraad.svg", width = 10, height = 4)

data_bezettingsgraden |>
    filter(name == 'Eindtotaal') |>
    fun_totaal_een(
        xvar = value,
        yvar = fct_relevel(fct_reorder(Markt_bez, value), 'totaal')
    ) +
    scale_x_continuous(labels = scales::percent)
ggsave("06 output figuren/fig_bezettingsgraad2.svg", width = 6, height = 6)


write_rds(data_bezettingsgraden, "03 intermediate/tabel_bezettingsgraden.rds")

#####################################################
## vergrijzing ,  duur op de markt , vast of soll ###
#####################################################

data_bezettingsgraden <- openxlsx::read.xlsx(
    "01 data raw/Ondernemers op peildatum 11-12-2025.xlsx",
)


my_markt_rename <- function(x) {
    x |>
        mutate(
            markt = case_when(
                markt == 'Weesp dinsdag ambulante handel' ~ 'Weesp',
                markt == "Plein '40 - '45" ~ "Plein 40-45",
                markt == 'Buikslotermeerplein dagmarkt' ~ 'Buikslotermeerplein',
                markt == "Ten Katestraat 2024" ~ 'Ten Katestraat',
                markt == "Waterloopleinmarkt 2022" ~ 'Waterloopleinmarkt',
                markt == "Noordermarkt Zaterdag" ~ 'Noordermarkt',
                markt == 'Albert Cuyp' ~ 'Albert Cuypmarkt',
                markt == "Eesterenlaan Biomarkt" ~ "Biomarkt Zeeburg",
                markt == 'totaal' ~ 'alle markten',
                TRUE ~ markt
            )
        )
}


data_bez <- data_bezettingsgraden |>
    my_markt_rename() |>
    mutate(
        reg_datum = lubridate::as_date(
            registratie_datum,
            origin = "1899-12-30"
        ),
        geb_datum = lubridate::as_date(geboortedatum, origin = "1899-12-30"),
        reg_jaar = year(reg_datum),
        geb_jaar = year(geb_datum),
        duur = 2026 - reg_jaar,
        leeftijd = 2026 - geb_jaar
    ) |>
    filter(leeftijd > 2) |>
    mutate(
        duur_quant = gtools::quantcut(duur),
        lft_quant = gtools::quantcut(leeftijd),

        type_ondernemer = case_when(
            type %in% c("TVPL", "TVPLZ") ~ 'tijd. v.p.',
            type %in% c("VPL", "EB") ~ 'verg.',
            type == 'SOLL' ~ 'soll.'
        ),

        leeftijdsklasse = case_when(
            lft_quant == '[20,36]' ~ 'tot 36 jaar',
            lft_quant == '(36,49]' ~ '36 jaar tot 49 jaar',
            lft_quant == '(49,60]' ~ '49 jaar tot 60 jaar',
            lft_quant == '(60,120]' ~ '60 jaar en ouder'
        ),

        leeftijdsklasse = factor(
            leeftijdsklasse,
            levels = c(
                'tot 36 jaar',
                '36 jaar tot 49 jaar',
                '49 jaar tot 60 jaar',
                '60 jaar en ouder'
            )
        ),

        duur_klasse = case_when(
            duur_quant == "1" ~ 'korter dan 1 jaar',
            duur_quant == "(1,4]" ~ 'tussen 1 en 4 jaar',
            duur_quant == "(4,17]" ~ 'tussen 4 en 17 jaar',
            duur_quant == "(17,60]" ~ '17 jaar of langer'
        ),

        duur_klasse = factor(
            duur_klasse,
            levels = c(
                'korter dan 1 jaar',
                'tussen 1 en 4 jaar',
                'tussen 4 en 17 jaar',
                '17 jaar of langer'
            )
        )
    )

## gemiddelde leeftijd
leeft_per_markt <- bind_rows(
    data_bez |>
        group_by(markt, type_ondernemer) |>
        summarise(
            aantal = n(),
            gem_duur = mean(duur),
            gem_lft = mean(leeftijd)
        ) |>
        group_by(markt) |>
        mutate(aandeel = aantal / sum(aantal)),

    data_bez |>
        group_by(type_ondernemer) |>
        summarise(
            aantal = n(),
            gem_duur = mean(duur),
            gem_lft = mean(leeftijd)
        ) |>
        ungroup() |>
        mutate(aandeel = aantal / sum(aantal)) |>
        add_column(markt = 'alle markten')
)


# leeftijd op markt
leeft_per_cat <- bind_rows(
    data_bez |>
        group_by(markt, leeftijdsklasse) |>
        summarise(aantal = n()) |>
        group_by(markt) |>
        mutate(aandeel = aantal / sum(aantal)),

    data_bez |>
        group_by(leeftijdsklasse) |>
        summarise(aantal = n()) |>
        ungroup() |>
        mutate(aandeel = aantal / sum(aantal)) |>
        add_column(markt = 'alle markten')
)


## lengte op de markt
duur_per_cat <- bind_rows(
    data_bez |>
        group_by(markt, duur_klasse) |>
        summarise(aantal = n()) |>
        group_by(markt) |>
        mutate(aandeel = aantal / sum(aantal)),

    data_bez |>
        group_by(duur_klasse) |>
        summarise(aantal = n()) |>
        ungroup() |>
        mutate(aandeel = aantal / sum(aantal)) |>
        add_column(markt = 'alle markten')
)

## lengte op de markt
duur_per_cat <- bind_rows(
    data_bez |>
        group_by(markt, duur_klasse) |>
        summarise(aantal = n()) |>
        group_by(markt) |>
        mutate(aandeel = aantal / sum(aantal)),

    data_bez |>
        group_by(duur_klasse) |>
        summarise(aantal = n()) |>
        ungroup() |>
        mutate(aandeel = aantal / sum(aantal)) |>
        add_column(markt = 'alle markten')
)

## categorie sollicitant of vaste plek
type_ond <- bind_rows(
    data_bez |>
        group_by(markt, type_ondernemer) |>
        summarise(aantal = n()) |>
        group_by(markt) |>
        mutate(aandeel = aantal / sum(aantal)),

    data_bez |>
        group_by(type_ondernemer) |>
        summarise(aantal = n()) |>
        ungroup() |>
        mutate(aandeel = aantal / sum(aantal)) |>
        add_column(markt = 'alle markten')
)


### figuren ---

#  aantal jaren dat ondernemers op de markt staan
a <- duur_per_cat |>
    filter(markt == 'alle markten') |>
    fun_totaal_een(
        verm_factor = 100,
        grenswaarde = 0.1,
        xvar = aandeel,
        yvar = fct_rev(duur_klasse)
    ) +
    scale_x_continuous(labels = scales::percent) +
    labs(x = 'aantal jaren op markt')

# leeftijdsklasse van ondernemers
b <- leeft_per_cat |>
    filter(markt == 'alle markten') |>
    fun_totaal_een(
        verm_factor = 100,
        grenswaarde = 0.1,
        xvar = aandeel,
        yvar = fct_rev(leeftijdsklasse)
    ) +
    scale_x_continuous(labels = scales::percent) +
    labs(x = 'leeftijdsverdeling')


# ltype ondernemer
c <- type_ond |>
    filter(markt == 'alle markten') |>

    fun_totaal_een(
        verm_factor = 100,
        grenswaarde = 0.1,
        xvar = aandeel,
        yvar = type_ondernemer
    ) +
    scale_x_continuous(labels = scales::percent) +
    labs(x = 'type ondernemer')

library(patchwork)

b + a + c
ggsave("06 output figuren/fig_kenmerken_ond.svg", width = 10, height = 4)


write.xlsx(
    list(leeft_per_markt, leeft_per_cat, duur_per_cat),
    "05 output tabellen/tabel_lft_marktond.xlsx"
)

write_rds(
    list(
        gem_lft = leeft_per_markt,
        cat_lft = leeft_per_cat,
        cat_duur = duur_per_cat
    ),
    "03 intermediate/tabel_lft_ondernemers_mb.rds"
)
