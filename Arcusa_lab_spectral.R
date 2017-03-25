## Week 9: Spectral Analysis

#=================================================
## PART 1 : WORKING WITH THE DATA

## 1.1. Use the read.csv function to read the wavedata.csv file

d <- read.csv("wavedata.csv", header = TRUE, sep = ",")

## This is sea surface elevation data measured on a beach 
## (specifically, it came from Praa Sands, England, and it was measured by yours truly)
### The two columns are: 1) time in seconds, and 2) sea surface elevation, eta, in metres
### We're going to use spectral analysis to work out basic quantities from this time series, such as wave heights, periods, speeds and wavelengths
## These same techniques can be used to analyze many other random signals in nature such temperature and topography

## 1.2. Use R to work out the sample frequency? (number of sea surface elevation samples per second, or Hz) Round to the nearest integer

freq.df <- matrix(data = NA, nrow = length(seq(round(min(d$Time)), round(max(d$Time)), by = 1)), ncol= 1)
freq.df[1] <- sum(d$Time <2000 +1)-1

for(i in 2001:3000){
  freq.df[i] <- sum(d$Time >i & d$Time <i +1)-1
}

freq.df <- na.omit(freq.df)

Fs <- round(mean(freq.df/1))
print(Fs) #samples per second


## 1.3. Recast the time vector so it starts from zero

t.diff <- mean(diff(d$Time))
duration = max(d$Time)-min(d$Time)
recast.time <- seq(0, duration, by = t.diff)

## 1.4. Show what proportion of the sea surface elevations are NAs

nas <- round((sum(is.na(d$Eta))/length(d$Eta))*100, digits = 3)
print(nas) #percentage of NAs in the sea surface elevations

## 1.5. We don't want the NA values but we want to keep the regular time stamp. This means replacing NAs with an estimated value, rather than removing them
# load the 'zoo' library and use the 'na.approx' method to replace each NA with interpolated values

#install.packages("zoo")
library(zoo)

cleaned <- na.approx(d$Eta)

## 1.6. Check that there are no NAs in the sea surface elevation data

which(is.na(cleaned)) #no NAs

## 1.7. What is the mean water depth?

print(mean(cleaned))

## 1.8. Recast this no-NA sea surface elevation so it has zero mean

scaled <- cleaned - mean(cleaned)

## 1.9. Make a plot of the time-series of time in minutes versus demeaned sea surface elevation. 
##    Make it appropriately beautiful with labels containing units

library(ggplot2)

df <- cbind(recast.time, scaled)
colnames(df) <- c("Time", "ETA")
df <- as.data.frame(df)

min.period = seq(0, 1000, by = 1000/16)
min.labels = seq(0, 16, by = 1)
#hrs.freqs = 1/hrs.period

my.plot <- ggplot(data = df, aes(x = Time, y = ETA)) +
  geom_line(colour = "blue")+
  scale_x_continuous(breaks = min.period, labels = min.labels)+
  labs(title = "Praa Sands, England", x = "Minutes", y = "Demeaned Sea surface elevation (m)")

my.plot
##1.10. Later, we're going to use the Fourier transform to transform the series from the time to frequency domain. 
##     What will be the frequency resolution (the interval between adjacent frequency bins, or bin width)?

freq.res = Fs/length(df$ETA)

#=================================================
## PART 2 : AUTOCORRELATION ANALYSIS OF WAVES

## 2.1. Let's have a look at the autocorrelation function of the trace. Compute the autocorrelation up to lag corresponding to 10 seconds

ACF <- acf(df$ETA, lag.max = length(df$ETA) , plot = F)

## 2.2 Make a plot of the autocorrelation function (ACF) versus time in seconds. 

plot(x = df$Time, y = ACF$acf, type = "l", ylab = "ACF", xlab = "Time (seconds)", xlim = c(0,100))

## Notice that the ACF dips below zero then comes back up in a "2nd bump" 
## (between the first zero up-crossing and second zero down-crossing). 
## The range of periods in this second bump is an indication of what the dominant wave periods are present in the data

## 2.3 annotate the plot with a markers at the frst 3 points where the curve crosses zero 
##     (i.e., first down-crossing, first up-crossing, and second down-crossing)
##     What range of periods do you expect the dominant waves to have?

cross = which(ACF$acf < 0)

plot(x = df$Time, y = ACF$acf, type = "l", ylab = "ACF", xlab = "Time (seconds)", xlim = c(0,100))
abline(v = 2.479256, col = "blue")
abline(v = 8.637408, col = "blue")
abline(v = 15.79526, col = "blue")
abline(h = 0, col = "grey")

#I would expect a range between 8 and 15 seconds
#=================================================
## PART 3 : SPECTRAL ANALYSIS OF WAVES

## 3.1. Compute the r.m.s wave amplitude (m) from the time-series

RMS.amp = sqrt(mean(df$ETA^2))

## 3.2. Compute the r.m.s wave height (m) from the amplitude

RMS.height = RMS.amp*2

## 3.3. Make a plot of just the first 103 seconds of wave data, and annotate the plot with a bar showing the r.m.s. wave height
##      use ggplot and save the plot as an object (variable)

wave.plot = ggplot(df)+
  geom_line(aes(x = Time, y = ETA))+
  xlim(0, 103)+
  annotate("text", x = 10, y = 0.9, label = "RMS wave height 0.780 m")+
  geom_segment(x = 0, y = 0, xend = 0, yend = RMS.height, colour = "blue", arrow = arrow() )

wave.plot

## 3.4. Compute a raw periodogram of the data using the spec.pgram function
##      Taper 10% of the data. Don't pad or smooth the time-series 

pg = spec.pgram(df$ETA, taper = 0.1) 

## 3.5. Show that the rms amplitude computed from the spectrum is roughly equivalent to the rms amplitude computed in 3.1
##      remember the spec.pgram function doesn't normalize by (half) the length of the signal, and that the periodogram is one-sided

RMS.spec = sqrt(mean(pg$spec))
print(RMS.spec)
print(RMS.amp)

## 3.6. Make a dataframe of the spectrum and create a plot. 
##     Note that the usual range of wave periods on beaches is ~4 to ~30 seconds
##     Also note that the frequency vector needs to be multiplied by the sample frequency

normal = which(df$Time >4 & df$Time < 30)
min(normal)
max(normal)

df2 = cbind(pg$freq*Fs, pg$spec)
colnames(df2) = c("Frequency", "Spectrum")
df2 = as.data.frame(df2[402:3001,])

spec.plot = ggplot(df2)+
  geom_line(aes(x = df2$Frequency, y = df2$Spectrum))+
  scale_x_log10(limits = c(0.4, 1), breaks = c(0.4, 0.5, 0.7, 1))+
  scale_y_continuous(limits = c(0, 2.5))+
  labs(x = "Frequency * sample frequency", y = "Power spectrum", title = "RAW Spectral analysis")
  
spec.plot

## 3.7. Compute the spectrum with 10% tapering and 15% padding (hint: ?spec.pgram)

pg2 = spec.pgram(df$ETA, taper = 0.1, pad = 0.15)
df3 = cbind(pg2$freq*Fs, pg2$spec)
colnames(df3) = c("Frequency", "Spectrum")
df3 = as.data.frame(df3[402:3001,])

## 3.8 Use the gridExtra package to make a (2 column) side-by-side graph of the raw and unpadded spectrum plots. What is the major difference? 

library(gridExtra)

spec.plot2 = ggplot(df3)+
  geom_line(aes(x = df3$Frequency, y = df3$Spectrum))+
  scale_x_log10(limits = c(0.4, 1), breaks = c(0.4, 0.5, 0.7, 1))+
  scale_y_continuous(limits = c(0, 2.5))+
  labs(x = "Frequency * sample frequency", y = "Power spectrum", title = "PADDED")

spec.plot2

my.power.plot = grid.arrange(spec.plot, spec.plot2, ncol = 1, nrow = 2)

#the major difference is that the pwer spectrum values are exaggerated in the padded analysis

## 3.9. Compute the spectrum with 10% tapering, 15% padding and a modified Daniell (or rectangular) window to be used to smooth the periodogram. 
##      Use a width of 5 for the smoothing function
##      Make a (3 column) side by side plot of raw, unpadded, and filtered spectrum plots

pg3 = spec.pgram(df$ETA, taper = 0.1, pad = 0.15, spans = 5)
df4 = cbind(pg3$freq*Fs, pg3$spec)
colnames(df4) = c("Frequency", "Spectrum")
df4 = as.data.frame(df4[402:3001,])

spec.plot3 = ggplot(df4)+
  geom_line(aes(x = df4$Frequency, y = df4$Spectrum))+
  scale_x_log10(limits = c(0.4, 1), breaks = c(0.4, 0.5, 0.7, 1))+
  scale_y_continuous(limits = c(0, 2.5))+
  labs(x = "Frequency * sample frequency", y = "Power spectrum", title = "MODIFIED DANIELL")

spec.plot3

my.power.plot = grid.arrange(spec.plot, spec.plot2, spec.plot3, ncol = 3, nrow = 1)

## 3.10. Compute the 95% confidence intervals for the filtered spectrum

alpha = 0.05
U = qchisq(alpha/2, df = pg3$df)
L = qchisq(1-(alpha/2), df = pg3$df)

pg3$CIlower = 2*pg3$spec/L
pg3$CIupper = 2*pg3$spec/U

df5 = cbind(pg3$freq*Fs, pg3$spec, pg3$CIlower, pg3$CIupper)
colnames(df5) = c("Frequency", "Spectrum", "Lower", "Upper")
df5 = as.data.frame(df5[402:3001,])

## 3.11. Make a (3 column) side by side plot of raw, unpadded, and filtered spectrum plots. 
##       On the plot of filtered spectrum, add the lines for the upper and lower confidence intervals, and log scale the y axis

spec.plot4 = ggplot() + 
  geom_line(aes(x = df5$Frequency, y = df5$Spectrum, col = "Estimate"))+
  geom_line(aes(x = df5$Frequency, y = df5$Lower, col = "Lower CI"))+
  geom_line(aes(x = df5$Frequency, y = df5$Upper, col = "Upper CI"))+
  scale_y_log10()+
  labs(x = "Frequency * sample frequency", y = "Power spectrum", title = "MODIFIED DANIELL")
  


#=================================================
## PART 4 : STATISTICS AND SIMULATION OF WAVES

## 4.1. What is the dominant wave period, in seconds? (associated with the peak in the filtered spectrum)


## 4.2. Annotate the plot you made of just the first 103 seconds of wave data (in Q 3.3), with a bar showing the wave period


## 4.3. What is the amplitude of waves (in metres) associated with the dominant wave period?
##      Note the units of the power spectrum are m^2 / Hz, so to find m you need sqrt(power in m^2) * Freq (in Hz)
##      But also remember you have to undo the 2*n factor that was applied to satisfy Parseval's theorem (see lesson notes)



## 4.4. What is the period (in seconds) and amplitude (in metres) of waves associated with the SECOND most dominant wave period?



## 4.5. Using a linear superposition of 2 sine waves, simulate a synthetic time series of waves that have the amplitude and period
##      of the 2 most dominant wave periods in the record


## 4.6. Annotate the plot you made of just the first 103 seconds of wave data (in Q 3.3), with 
##      the first 103 seconds of the synthetic time-series you generated in Q 4.5


## 4.7. Make a graph that shows the histograms of both the real and simulated sea surface elevations (on the same plot)
##      Explain the major difference between the distributions, using what you know about the power spectrum


## 4.8. The so-called 'significant wave height' is the average height of the largest 1/3 of waves. 
##      Wave theory tells us that it can be computed from as 4 times the standard deviation of the detrended sea surface elevation series. Compute it.



## 4.9. Deep water wave phase speed (also called celerity), Co, is (g / 2*pi) * T, 
##       in metres per second, where T is wave period, and g is gravitational acceleration (= 9.81 m/s^2).
##       Compute the wave phase speed for the first and second most dominant wave periods (Q 4.1 and Q 4.4)


## 4.10. Waves start to slow down significantly in shallow water because of bottom friction and is controlled by water depth
##       Phase speed is: C = sqrt(g*h), where h is mean water depth (Q 1.7)
##       Compute the shallow wave phase speed


## 4.11. Wavelength is given by C / f, where f is wave frequency. Make one final beautiful plot, this time of power spectral density versus wavelength
