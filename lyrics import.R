library(tidyverse)
library(tidytext)
library(geniusR)

import_album <- function(artist, album, genre){
  df = genius_album(artist = artist, album = album) %>% 
    unnest_tokens(word, text)
  
  df[is.na(df)] <- 0
  
  df$Song_Genre <- genre
  df$Song_Artist <- artist
  
  return(df)
}


blink_jacket <- import_album("Blink 182", "Take off your pants and jacket", "Pop Punk")
write_csv(blink_jacket, "blink jacket.csv")
blink_enema <- import_album("Blink 182", "Enema of the state", "Pop Punk")
write_csv(blink_enema, "blink enema.csv")
blink_blink <- import_album("Blink 182", "Blink 182", "Pop Punk")
write_csv(blink_blink, "blink blink.csv")
blink_dude <- import_album("Blink 182", "Dude ranch", "Pop Punk")
write_csv(blink_dude, "blink dude.csv")

GD_21 <- import_album("Green day", "21st century breakdown", "Pop Punk")
write_csv(GD_21, "GD 21.csv")
GD_idiot <- import_album("Green day", "American idiot", "Pop Punk")
write_csv(GD_idiot, "GD idiot.csv")
GD_warning <- import_album("Green day", "Warning", "Pop Punk")
write_csv(GD_warning, "GD warning.csv")
GD_nimrod <- import_album("Green day", "Nimrod", "Pop Punk")
write_csv(GD_nimrod, "GD nimrod.csv")
GD_insomniac <- import_album("Green day", "Insomniac", "Pop Punk")
write_csv(GD_insomniac, "GD insomniac.csv")
GD_dookie <- import_album("Green day", "Dookie", "Pop Punk")
write_csv(GD_dookie, "GD dookie.csv")

# problem child AT_gd <- import_album("Alkaline trio", "Goddamnit", "Pop Punk")
AT_maybe <- import_album("Alkaline trio", "Maybe i ll catch fire", "Pop Punk")
write_csv(AT_maybe, "AT maybe.csv")
AT_AT <- import_album("Alkaline trio", "Alkaline trio", "Pop Punk")
write_csv(AT_AT, "AT AT.csv")
AT_from <- import_album("Alkaline trio", "From here to infirmary", "Pop Punk")
write_csv(AT_from, "AT from.csv")
AT_gm <- import_album("Alkaline trio", "Good mourning", "Pop Punk")
write_csv(AT_gm, "AT gm.csv")
AT_crimson <- import_album("Alkaline trio", "Crimson", "Pop Punk")
write_csv(AT_crimson, "AT crimson.csv")

ramones_ramones <- import_album("Ramones", "Ramones", "Punk")
write_csv(ramones_ramones, "ramones.csv")
ramones_LH <- import_album("Ramones", "Leave home", "Punk")
write_csv(ramones_LH, "ramones LH.csv")
ramones_rocket <- import_album("Ramones", "Rocket to russia", "Punk")
write_csv(ramones_rocket, "ramones rocket.csv")
ramones_road <- import_album("Ramones", "Road to ruin", "Punk")
write_csv(ramones_road, "ramones road.csv")


behemoth_demigod <- import_album("Behemoth", "Demigod", "Death Metal")
write_csv(behemoth_demigod, "behemoth demigod.csv")
behemoth_apostasy <- import_album("Behemoth", "The apostasy", "Death Metal")
write_csv(behemoth_apostasy, "behemoth apostasy.csv")
behemoth_satanist <- import_album("Behemoth", "The satanist", "Death Metal")
write_csv(behemoth_satanist, "behemoth satanist.csv")
behemoth_evangelion <- import_album("Behemoth", "Evangelion", "Death Metal")
write_csv(behemoth_evangelion, "behemoth evangelion.csv")

opeth_orchid <- import_album("Opeth", "Orchid", "Death Metal")
write_csv(opeth_orchid, "opeth orchid.csv")
opeth_SL <- import_album("Opeth", "Still life", "Death Metal")
write_csv(opeth_SL, "opeth SL.csv")
opeth_GR <- import_album("Opeth", "Ghost reveries", "Death Metal")
write_csv(opeth_GR, "opeth GR.csv")
opeth_watershed <- import_album("Opeth", "Watershed", "Death Metal")
write_csv(opeth_watershed, "opeth watershed.csv")
opeth_deliverance <- import_album("Opeth", "Deliverance", "Death Metal")
write_csv(opeth_deliverance, "opeth deliverance.csv")
opeth_heritage <- import_album("Opeth", "Heritage", "Death Metal")
write_csv(opeth_heritage, "opeth heritage.csv")

bloodbath_NMF <- import_album("Bloodbath", "Nightmares made flesh", "Death Metal")
write_csv(bloodbath_NMF, "bloodbath NMF.csv")
RC_AEALO <- import_album("Rotting Christ", "Aealo", "Death Metal")
write_csv(RC_AEALO, "RC AEALO.csv")

db_DCA <- import_album("Dimmu borgir", "Death cult armageddon", "Death Metal")
write_csv(db_DCA, "db DCA.csv")
db_diaboli <- import_album("Dimmu borgir", "In sorte diaboli", "Death Metal")
write_csv(db_diaboli, "db diaboli.csv")


atheist_jupiter <- import_album("Atheist", "Jupiter", "Death Metal")
write_csv(atheist_jupiter, "atheist jupiter.csv")
atheist_UP <- import_album("Atheist", "Unquestionable presence", "Death Metal")
write_csv(atheist_UP, "atheist UP.csv")

emperor_eclipse <- import_album("Emperor", "In the nightside eclipse", "Death Metal")
write_csv(emperor_eclipse, "emperor eclipse.csv")
emperor_anthems <- import_album("Emperor", "Anthems to the welkin at dusk", "Death Metal")
write_csv(emperor_anthems, "emperor anthems.csv")
emperor_equilibrium <- import_album("Emperor", "Ix equilibrium", "Death Metal")
write_csv(emperor_equilibrium, "emperor equilibrium.csv")

btbam_btbam <- import_album("Between the buried and me", "Between the buried and me", "Metal")
write_csv(btbam_btbam, "btbam btbam.csv")
btbam_circus <- import_album("Between the buried and me", "The silent circus", "Metal")
write_csv(btbam_circus, "btbam circus.csv")
btbam_alaska <- import_album("Between the buried and me", "Alaska", "Metal")
write_csv(btbam_alaska, "btbam alaska.csv")
btbam_colors <- import_album("Between the buried and me", "Colors", "Metal")
write_csv(btbam_colors, "btbam colors.csv")

metallica_RTL <- import_album("Metallica", "Ride the lightning", "Metal")
write_csv(metallica_RTL, "metallica RTL.csv")
metallica_ajfa <- import_album("Metallica", "And justice for all", "Metal")
write_csv(metallica_ajfa, "metallica ajfa.csv")
metallica_metallica <- import_album("Metallica", "Metallica", "Metal")
write_csv(metallica_metallica, "metallica.csv")
metallica_mop <- import_album("Metallica", "Master of puppets", "Metal")
write_csv(metallica_mop, "metallica mop.csv")
metallica_kea <- import_album("Metallica", "Kill em all", "Metal")
write_csv(metallica_kea, "metallica kea.csv")
