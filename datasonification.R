# Author: Jeff Hemsley jhemsley at uw dot edu
# Twitter: @JeffHemsley
# Created: Sometime in 2013
# 
# the location of the files I used are at:
# http://somelab.net/wp-content/uploads/2013/05/test_tweet_like_data.txt
# Grab these files or make your own and fix the path info below
#
# load the tuneR package
library(tuneR)

dir.path <- "c:/r/rt_nets/"
dir.path.dat <- "c:/r/rt_nets/dat/"
#dir.path.dat <- "c:/r/sound/"
tweet.data.file.name <- "earthRTs.txt"
#tweet.data.file.name <- "test_tweet_like_data.txt"
tweet.data.file <- paste(dir.path.dat, tweet.data.file.name, sep="")

# tweet data is stored in a file, often a big one with a tab as the separater
tweet.data <- data.frame(read.delim(file=tweet.data.file, sep='\t', stringsAsFactors=F, row.names=NULL))

# ok, just so we can see what we got...
colnames(tweet.data)
dim(tweet.data)
tweet.data[1,]

# now, we want have certain characters given some sounds, everything else we
# treat as a pause so as to sort of mimic language and bird chirps. So
# here is a list of the characters we will create sound for. Spaces give us
# us something like different words.
chars.to.sonify <- c("#", "@", "-", ",", ".", "'", letters, as.character(0:9))
chars.to.sonify.length <- length(chars.to.sonify)

# sampling rate. this is how many data points (I think) per second
sampeling.rate <- 6000

# how rich the sound is?
bits <- 8

long.pause <- .5 # in seconds
short.pause <- .1 # in seconds
character.sound.length <- 0.01

# we are going to setup a range of tones for each user
# to do that we need to find the total range, how many users, and a min and max for each user
# and then stuff it in a dataframe so we can get those ranges depending on who is "talking"
# note, Hz low values are deep song, and high values are high pitched
min.Hz <- 600
max.Hz <- 8000
Hz.range <- max.Hz - min.Hz

# get the users from the original dataframe
users.vector <- sort(unique(tweet.data$user.screen_name))
users.vector.length <- length(users.vector)
num.tone.start.buckets <- floor(Hz.range/users.vector.length)

# ok, now make the dataframe
user.voice.df <- data.frame(screen.name=users.vector, min.tone=rep(0, users.vector.length), max.tone=rep(0, users.vector.length))
user.voice.df$min.tone <- seq(from=min.Hz, by=num.tone.start.buckets, length=users.vector.length)
user.voice.df$max.tone <- seq(to=max.Hz, by=num.tone.start.buckets, length=users.vector.length)
user.voice.df[1,] # whats the first row look like?
user.voice.df[users.vector.length,] # whats the last row look like?

#ok. Now, we don't want to do all of the tweets, just a sample, for experimenting
# do like 3 to 10.
num.tweets.to.sonify <- 10
num.obs <- dim(tweet.data)[1]

if (num.obs < num.tweets.to.sonify) {
  num.tweets.to.sonify <- num.obs
}
tweet.rows.to.sing <- sample(x=1:num.obs, size=num.tweets.to.sonify)

# here is the sonify loop: for each user sing thier tweet
for (i in 1:num.tweets.to.sonify) {

  if (i == 1) {
    # wait! if this is the first iteration, lets make a wave object: a "coversation" of tweets
    w.conversation <- silence(duration = long.pause, xunit = c("samples", "time")[2], bit=bits, samp.rate=sampeling.rate)
  }
  
  # i-th sample
  df.tweet.row <- tweet.rows.to.sing[i]
  
  # get the user and set their range
  the.user <- tweet.data$user.screen_name[df.tweet.row]
  the.user.index <- which(user.voice.df$screen.name == the.user)
  user.min.Hz <- user.voice.df$min.tone[the.user.index]
  user.max.Hz <- user.voice.df$max.tone[the.user.index]
  user.Hz.range <- user.max.Hz - user.min.Hz

  # get the tweet text
  the.tweet <- tweet.data$text[df.tweet.row]
  the.tweet.length <- nchar(the.tweet)

  # break into a vector of characters
  # lowercase the letters. stuff it all in a vector
  tweet.text.vec <- unlist(strsplit(tolower(the.tweet), ""))
  
  # For each character in the tweet, find it's index in the chars.to.sonify 
  # (see above for our 'alphabet' of chars we are sounding out)
  tmp.index <- match(tweet.text.vec, chars.to.sonify)

  # each 'talker' starts with a pause of silence
  wobj <- silence(duration = long.pause, xunit = c("samples", "time")[2], bit=bits, samp.rate=sampeling.rate)

  # ok. for each character in the tweet, make a little wave for it.
  for (j in 1:the.tweet.length) {
    # j <- 1 + j
    if (is.na(tmp.index[j])) {
      w <- silence(duration = short.pause, xunit = c("samples", "time")[2], bit=bits, samp.rate=sampeling.rate)
    } else {
      tweet.char.freq <- (tmp.index[j] * (user.Hz.range/chars.to.sonify.length)) + user.min.Hz
      w <- sine(tweet.char.freq, duration=character.sound.length, xunit = c("samples", "time")[2], bit=bits, samp.rate=sampeling.rate)
    }
    
    # add each part of the wave to the wave object
    wobj <- bind(wobj, w)
  }
  
  # add each talker's tweet to the conversasion
  w.conversation <- bind(w.conversation, wobj)
}

play(w.conversation)
# write it all to a wav file.
writeWave(w.conversation, "c:/r/sound/tweet_data_sonification.wav")
