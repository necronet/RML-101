library(tuneR)
library(matlab)
library(e1071)

setWavPlayer('/usr/bin/afplay')

target_path = "/Users/joseayerdis/Documents/repos/free-spoken-digit-dataset/recordings/" 
wave <- readWave(paste0(target_path,"0_jackson_0.wav"))
play(wave)


str(wave)
s <- wave@left/2^(wave@bit -1)
timeArray <- (seq_len(length(wave))-1) / wave@samp.rate
plot(timeArray, s, type='l', col='black', xlab="Time (ms)", ylab='Amplitude')


print(paste0(length(wave)/wave@samp.rate," secs"))


normalize_audio <- wave@left/max(abs(wave@left))
plot(timeArray, normalize_audio, type='l', col='blue', xlab="Time (ms)", ylab='Amplitude')

#padarray(1:10,c(0,2), "symmetric")

chunk_size = 2048
sample_rate = wave@samp.rate
hop_size <- 15

audio = padarray(wave@left, c(0, chunk_size / 2), 'symmetric')
frame_len = round(sample_rate * hop_size / 1000)
frame_num = as.integer( ((length(audio) - chunk_size) / frame_len)) + 1

frames = matrix(0L, ncol = frame_num, nrow = chunk_size)
dim(frames)
length(audio)
merge(frames, 1:65, all = T)

for(i in 1:frame_num) {
  x1 <- max(0, i*frame_len + 1)
  x2 <- min(i*frame_len+chunk_size,length(audio))
  #print(paste("i:",i,"From:",x1,"To:",x2, " = ", (x2-x1), " | ", length(audio[x1:x2]) ))
  
  frames[1:length(audio[x1:x2]), i] = audio[x1:x2]
}

audio_frames <- frames
print(paste("Generated audio chunks:", paste0(dim(frames), collapse="x")))


# Explore hanning window
hanning_window <- hanning.window(chunk_size)
plot(hanning.window(chunk_size), type='l', col='black',  ylab='Hanning')


audio_win = audio_frames * hanning_window

graphics.off()
par(mfrow = c(2,1))
plot(audio_frames[,1], type = 'l',  main='Original', xlab = "",  ylab='')
plot(audio_win[,1], type = 'l',  main='Windowed sequence', xlab = "",  ylab='')

audio_win_t <- t(audio_win)

audio_fft = matrix(0L, ncol = nrow(audio_win_t), nrow = (1 + as.integer(chunk_size/2)))

dim(audio_win_t)
dim(audio_fft)

for (i in 0:(nrow(audio_win_t)) ) {
  print(length(audio_win_t[,i]))
  audio_fft[i ,] <- fft(audio_win_t[,i])
}

audio_power <- abs(audio_fft)^2

freq_min <- 0
freq_high <- sample_rate / 2
mel_filter_num <- 10

freq_to_mel <- function(freq) {
  2595.0 * log10(1.0 + freq / 700.0)
}

mel_to_freq <- function(mels) {
    700.0 * (10.0**(mels / 2595.0) - 1.0)
}

get_filter_points <- function(fmin, fmax, mel_filter_num, FFT_size, sample_rate=44100) {
  fmin_mel <- freq_to_mel(fmin)
  fmax_mel <- freq_to_mel(fmax)
  
  print(paste("MEL min ", fmin_mel))
  print(paste("MEL nax ", fmax_mel))
  
  mels <- seq(from = fmin_mel, to = fmax_mel, length.out= mel_filter_num+2)
  freqs <- mel_to_freq(mels)
  list( filter_points = floor( (chunk_size + 1) / sample_rate*freqs), freqs = freqs)
}

fp <- get_filter_points(freq_min, 22050, mel_filter_num, chunk_size, 44100)
fp$filter_points

get_filters <- function(filter_points, FFT_size) {
  filters <- matrix(0L, nrow = length(filter_points)-2, ncol = as.integer((FFT_size/2) +1) )
  print(dim(filters))
  for ( i in 1:(length(filter_points)-2)) {
    
    #print( length( seq(from = 0, to = 1, length.out = filter_points[i + 1] - filter_points[i] ) ))
    #print( length( filter_points[i] : filter_points[i + 1] ))
    
    num_output <- filter_points[i + 1] - filter_points[i]
    
    print(num_output)
    filters[i, (filter_points[i]+1) : filter_points[i + 1]] = seq(from = 0, to = 1, length.out =  num_output)
    filters[i, (filter_points[i + 1]+1) : filter_points[i + 2]] = seq(from = 1, to = 0, length.out = filter_points[i + 2] - filter_points[i + 1])
  }
  
  filters
}

mel_freqs <- fp$freqs
filters <- get_filters(fp$filter_points, chunk_size)

graphics.off()

#plot.new()
for (i in 1:nrow(filters) ) {
    print(i)
    par(new=TRUE)
    plot(filters[i,], col=i, type = "l", ylab ='', xlab = '')
}

enorm <- 2.0 / mel_freqs[1:mel_filter_num+2] - mel_freqs[1:mel_filter_num]

new_filters =  filters * enorm

new_filters[1,3]

graphics.off()
for (i in 1:nrow(new_filters) ) {
  par(new=TRUE)
  plot(new_filters[,i], col=i, type = "l", ylab ='', xlab = '')
}
new_filters

a = 13
b = 28

a:b
#13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28

a+1:b
#14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41

