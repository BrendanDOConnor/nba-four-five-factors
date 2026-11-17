full_player_seasons_fivestat <- read.csv("C:/Users/bdoc1/OneDrive/NBA/full_player_seasons_fivestat.csv", header=FALSE)
View(full_player_seasons_fivestat)
colnames(full_player_seasons_fivestat) <- full_player_seasons_fivestat[1,]
full_player_seasons_fivestat <- full_player_seasons_fivestat[-1, ]
five_stat_plus_minus <- lm(full_player_seasons_fivestat$`On-off per 48_lineup` ~
                           full_player_seasons_fivestat$`TS% Gap` + full_player_seasons_fivestat$`Opp TS% Gap` + 
                             full_player_seasons_fivestat$`TOV% Gap` + full_player_seasons_fivestat$`Opp TOV% Gap` +
                             full_player_seasons_fivestat$`REB% Gap`)
summary(five_stat_plus_minus)
typeof(full_player_seasons_fivestat$`TS% Gap`)
full_player_seasons_fivestat <- as.data.frame(full_player_seasons_fivestat)
full_player_seasons_fivestat$`TS% Gap` <- as.numeric(full_player_seasons_fivestat$`TS% Gap`)
full_player_seasons_fivestat$`Opp TS% Gap` <- as.numeric(full_player_seasons_fivestat$`Opp TS% Gap`)
full_player_seasons_fivestat$`TOV% Gap` <- as.numeric(full_player_seasons_fivestat$`TOV% Gap`)
full_player_seasons_fivestat$`Opp TOV% Gap` <- as.numeric(full_player_seasons_fivestat$`Opp TOV% Gap`)
full_player_seasons_fivestat$`REB% Gap` <- as.numeric(full_player_seasons_fivestat$`REB% Gap`)
