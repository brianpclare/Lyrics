library(tidyverse)
library(tidytext)
library(geniusR)
library(keras)
library(tensorflow)
use_condaenv("r-tensorflow")

library(permute)

import_album <- function(artist, album, genre){
  df = genius_album(artist = artist, album = album) %>% 
    unnest_tokens(word, text)
  
  df[is.na(df)] <- 0
  
  df$Song_Genre <- genre
  df$Song_Artist <- artist
  
  return(df)
}

format <- function(album_lyrics){
  split <- album_lyrics %>% split(.$title) %>% unname()
  
  for(i in 1:length(split)){
    split[[i]] <- split[[i]] %>% select(word) %>% 
      t() %>% unname()
  }
  
  max_length <- 0
  for(i in 1:length(split)){
    max_length <- max(max_length, length(split[[i]]))
  }
  
  for(i in 1:length(split)){
    while(length(split[[i]]) < max_length){
      split[[i]] <- append(split[[i]], 0, after = 0)
    }
    split[[i]] <- unname(split[[i]])
  }
  
  df <- do.call(rbind, split)
  return(df)
}

blink_jacket <- read_csv("blink jacket.csv")
blink_enema <- read_csv("blink enema.csv")
blink_blink <- read_csv("blink blink.csv")
blink_dude <- read_csv("blink dude.csv")

GD_21 <- read_csv("GD 21.csv")
GD_idiot <- read_csv("GD idiot.csv")
GD_warning <- read_csv("GD warning.csv")
GD_nimrod <- read_csv("GD nimrod.csv")
GD_insomniac <- read_csv("GD insomniac.csv")
GD_dookie <- read_csv("GD dookie.csv")

# problem child AT_gd <- import_album("Alkaline trio", "Goddamnit", "Pop Punk")
AT_maybe <- read_csv("AT maybe.csv")
AT_AT <- read_csv("AT AT.csv")
AT_from <- read_csv("AT from.csv")
AT_gm <- read_csv("AT gm.csv")
AT_crimson <- read_csv("AT crimson.csv")

ramones_ramones <- read_csv("ramones.csv")
ramones_LH <- read_csv("ramones LH.csv")
ramones_rocket <- read_csv("ramones rocket.csv")
ramones_road <- read_csv("ramones road.csv")



behemoth_demigod <- read_csv("behemoth demigod.csv")
behemoth_apostasy <- read_csv("behemoth apostasy.csv")
behemoth_satanist <- read_csv("behemoth satanist.csv")
behemoth_evangelion <- read_csv("behemoth evangelion.csv")

opeth_orchid <- read_csv("opeth orchid.csv")
opeth_SL <- read_csv("opeth SL.csv")
opeth_GR <- read_csv("opeth GR.csv")
opeth_watershed <- read_csv("opeth watershed.csv")
opeth_deliverance <- read_csv("opeth deliverance.csv")
opeth_heritage <- read_csv("opeth heritage.csv")

bloodbath_NMF <- read_csv("bloodbath NMF.csv")
RC_AEALO <- read_csv("RC AEALO.csv")

db_DCA <- read_csv("db DCA.csv")
db_diaboli <- read_csv("db diaboli.csv")

atheist_jupiter <- read_csv("atheist jupiter.csv")
atheist_UP <- read_csv("atheist UP.csv")

emperor_eclipse <- read_csv("emperor eclipse.csv")
emperor_anthems <- read_csv("emperor anthems.csv")
emperor_equilibrium <- read_csv("emperor equilibrium.csv")

btbam_btbam <- read_csv("btbam btbam.csv")
btbam_circus <- read_csv("btbam circus.csv")
btbam_alaska <- read_csv("btbam alaska.csv")
btbam_colors <- read_csv("btbam colors.csv")

metallica_RTL <- read_csv("metallica RTL.csv")
metallica_ajfa <- read_csv("metallica ajfa.csv")
metallica_black <- read_csv("metallica.csv")
metallica_mop <- read_csv("metallica mop.csv")
metallica_kea <- read_csv("metallica kea.csv")


all_songs <- list(blink_blink, blink_dude, blink_enema, blink_jacket, 
                  GD_21, GD_idiot, GD_warning, GD_nimrod, GD_insomniac, GD_dookie,
                  behemoth_demigod, behemoth_apostasy, behemoth_satanist, 
                  behemoth_evangelion, opeth_orchid, opeth_SL, opeth_GR, opeth_watershed, 
                  opeth_deliverance, bloodbath_NMF, RC_AEALO, db_DCA, db_diaboli,
                  atheist_jupiter, emperor_eclipse, emperor_anthems, emperor_equilibrium,
                  atheist_UP, AT_crimson, AT_gm, AT_from, AT_AT, AT_maybe, 
                  opeth_heritage, btbam_alaska, btbam_circus, btbam_colors, btbam_btbam,
                  metallica_RTL, metallica_ajfa, metallica_black, metallica_mop, metallica_kea,
                  ramones_LH, ramones_ramones, ramones_road, ramones_rocket)

num_albums <- length(all_songs)

# Randomly rearrange the albums
# set.seed(12)
# random_order <- shuffle(num_albums)
# 
# all_songs <- all_songs[c(random_order)]

all_matrices <- lapply(all_songs, format)

unique_words <- bind_rows(all_songs) %>% select(word) %>% unique()
num_words <- length(unique_words$word)
unique_words$index <- 1:num_words


max_words <- 0

# replacing words with indices

for(i in 1:num_albums){
  
  num_songs <- dim(all_matrices[[i]])[1]
  song_words <- dim(all_matrices[[i]])[2]  
  max_words <- max(song_words, max_words)
  
  for(k in 1:num_songs){
    for(j in 1:song_words){
      if(all_matrices[[i]][[k, j]] != "0"){
        x <- all_matrices[[i]][[k, j]]
        y <- unique_words$index[unique_words$word == x]
        all_matrices[[i]][[k, j]] <- y
      }
    }
  }
}

for(i in 1:num_albums){
  while(dim(all_matrices[[i]])[2] < max_words){
    all_matrices[[i]] <- cbind(0, all_matrices[[i]])
  }
}

final_train_input <- do.call(rbind, all_matrices)


songs_artists <- bind_rows(all_songs) %>%
  select(title, Song_Genre, Song_Artist) %>% unique()

# The real labels
punk <- ifelse(
  songs_artists$Song_Artist == "Blink 182" | songs_artists$Song_Artist == "Green day" |
    songs_artists$Song_Artist == "Alkaline trio" | songs_artists$Song_Artist == "Ramones", 1, 0)

# Randomize song order instead of just album order
set.seed(538)
total_songs <- length(punk)
random_order <- shuffle(total_songs)

final_train_input <- final_train_input[c(random_order), ]
punk <- punk[c(random_order)]
songs_artists <- songs_artists[c(random_order), ]



# Random labels for testing purposes
# set.seed(100)
# punk <- rbinom(length(songs_artists$title), 1, 0.5)

rec_dim <- round(length(unique_words$word)^(0.25))

set.seed(1977)

model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = num_words + 1, output_dim = rec_dim) %>% 
  layer_lstm(units = 32, dropout = 0.3, recurrent_dropout = 0.3) %>% 
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = optimizer_rmsprop(lr = 0.005),
  loss = "binary_crossentropy",
  metrics = c("acc")
)

history <- model %>% fit(
  final_train_input, punk,
  epochs = 5,
  batch_size = 20,
  validation_split = 0.2
)

summary(model)

model %>% evaluate(final_train_input, punk)

y_train_hat <- model %>% predict_classes(final_train_input)
table(punk, y_train_hat)

song_names <- songs_artists %>% select(title, Song_Artist)
song_names$pred <- y_train_hat
song_names$true <- punk

misclass <- song_names %>% filter(pred != true)
