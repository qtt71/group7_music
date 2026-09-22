require("audio")

note_to_hz <- function(note, accidental, octave){
  note_data <- c(A = 12, B = 14, C = 3, D = 5, E = 7, F = 8, G = 10, R = NA, "|"=NA, "||"=NA)
  accidental_data <- c("bb" = -2, "b" = -1, "n" = 0, "#" = 1, "##" = 2)
  
  note_value <- note_data[match(toupper(note),names(note_data))]
  accidental[accidental==""] <- "n"
  accidental_value <- accidental_data[match(accidental,names(accidental_data))]
  octave_value <- as.numeric(octave)
  note_num = note_value + accidental_value + octave_value*12
  freq = 440 * 2^((note_num - 60)/12)
  return(unname(freq))
}

duration_to_num <- function(duration){
  duration_data <- c(b = 8, w = 4, h = 2, q = 1, e = 0.5, s = 0.25, "|"=0, "||"=0)  
  dur_char <- substr(duration,1,1)
  dur_value <- duration_data[match(tolower(dur_char),names(duration_data))]
  triple <- grepl("t",duration,fixed=TRUE)
  dur_value[triple] <- dur_value[triple]*2/3
  dub_dot <- grepl("..",duration,fixed=TRUE)
  dot <- grepl(".",duration,fixed=TRUE)
  dur_multiple <- 1 + as.numeric(dot)*0.5 + as.numeric(dub_dot)*0.25
  dur_value <- dur_value * dur_multiple
  return(unname(dur_value))
}

make_note_waveform <- function(freq, duration, bpm, sample_rate = 44100) {
  if(duration == 0){
    return(NULL)
  }
  time <- seq(0, duration / bpm * 60, 1 / sample_rate)
  if(is.na(freq)) return(rep(0,length(time)))
  wave <- sin(time * freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  waveform <- wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
  return(waveform)
}

str_to_waveform <- function(str, bpm = 120, add_pad=TRUE){
  if(substr(str,nchar(str)-1,nchar(str)) == " |"){
    str <- paste0(str,"|")
  }
  str <- gsub(" ||"," ||0||",str,fixed=TRUE)
  str <- gsub(" | ", " |0| ",str,fixed=TRUE)
  str <- gsub(" R", " R0",str,fixed=TRUE)
  if(substr(str,1,1)=="R"){
    str <- paste0("R0",substr(str,2,nchar(str)))
  }
  tokens <- strsplit(str," ")[[1]]
  m <- regexec("^([^0-9]+)([0-9]+)(.*)$", tokens)
  parts <- regmatches(tokens, m)
  df <- data.frame(
    note  = sapply(parts, `[`, 2),
    octave = as.integer(sapply(parts, `[`, 3)),
    duration   = sapply(parts, `[`, 4),
    stringsAsFactors = FALSE
  )
  df$accidental <- substr(df$note,2,3)
  df$note <- substr(df$note,1,1)
  df$note[df$duration=="||"] <- "||"
  df$accidental[df$duration=="||"] <- ""
  df$hz <- note_to_hz(df$note, df$accidental,df$octave)
  df$dur <- duration_to_num(df$duration)
  
  df <- df[,c("hz","dur")]
  if(add_pad){
    pad <- data.frame(hz = NA, dur = 2)
    df <- rbind(pad,df,pad)
  }
  wave_notes <- mapply(make_note_waveform, freq=df$hz, 
                       duration=df$dur, bpm=bpm)
  if(is.matrix(wave_notes)) wave_notes <- as.list(as.data.frame(wave_notes))
  wave_tune <- unlist(wave_notes)
  return(wave_tune)
}


if(FALSE){
  ## Example
  str <- "C4s D4s F4s D4s | A4e. A4e. G4q. C4s D4s F4s D4s | G4e. G4e. F4q. C4s D4s F4s D4s | F4q G4e E4e. D4s C4q C4e | G4q F4h Re ||"
  waveform <- str_to_waveform(str)
  play(waveform)
}