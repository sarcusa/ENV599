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

t.diff <- diff(d$Time)
duration = max(d$Time)-min(d$Time)
recast.time <- seq(0, duration-1, by = t.diff)

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

scaled <- scale(cleaned)
scaled <- scaled [-100001]

## 1.9. Make a plot of the time-series of time in minutes versus demeaned sea surface elevation. 
##    Make it appropriately beautiful with labels containing units


#MUST RECAST TIME INTO MINUTES

library(ggplot2)

df <- cbind(recast.time, scaled)
colnames(df) <- c("Time", "ETA")
df <- as.data.frame(df)

my.plot <- ggplot(data = df, aes(x = Time, y = ETA)) +
  geom_line(colour = "blue")+
  scale_x_continuous(breaks = seq(0, 1000, by = 10))+
  labs(title = "Praa Sands, England", x = "Minutes", y = "Sea surface elevation (m)")

scale_x_continuous(breaks = round(seq(min(df$Time), max(df$Time), by = 60),1))


my.plot
##1.10. Later, we're going to use the Fourier transform to transform the series from the time to frequency domain. 
##     What will be the frequency resolution (the interval between adjacent frequency bins, or bin width)?


#=================================================
## PART 2 : AUTOCORRELATION ANALYSIS OF WAVES

## 2.1. Let's have a look at the autocorrelation function of the trace. Compute the autocorrelation up to lag corresponding to 10 seconds


## 2.2 Make a plot of the autocorrelation function (ACF) versus time in seconds. 


## Notice that the ACF dips below zero then comes back up in a "2nd bump" 
## (between the first zero up-crossing and second zero down-crossing). 
## The range of periods in this second bump is an indication of what the dominant wave periods are present in the data

## 2.3 annotate the plot with a markers at the frst 3 points where the curve crosses zero 
##     (i.e., first down-crossing, first up-crossing, and second down-crossing)
##     What range of periods do you expect the dominant waves to have?


#=================================================
## PART 3 : SPECTRAL ANALYSIS OF WAVES

## 3.1. Compute the r.m.s wave amplitude (m) from the time-series


## 3.2. Compute the r.m.s wave height (m) from the amplitude


## 3.3. Make a plot of just the first 103 seconds of wave data, and annotate the plot with a bar showing the r.m.s. wave height
##      use ggplot and save the plot as an object (variable)



## 3.4. Compute a raw periodogram of the data using the spec.pgram function
##      Taper 10% of the data. Don't pad or smooth the time-series 


## 3.5. Show that the rms amplitude computed from the spectrum is roughly equivalent to the rms amplitude computed in 3.1
##      remember the spec.pgram function doesn't normalize by (half) the length of the signal, and that the periodogram is one-sided



## 3.6. Make a dataframe of the spectrum and create a plot. 
##     Note that the usual range of wave periods on beaches is ~4 to ~30 seconds
##     Also note that the frequency vector needs to be multiplied by the sample frequency



## 3.7. Compute the spectrum with 10% tapering and 15% padding (hint: ?spec.pgram)


## 3.8 Use the gridExtra package to make a (2 column) side-by-side graph of the raw and unpadded spectrum plots. What is the major difference? 


## 3.9. Compute the spectrum with 10% tapering, 15% padding and a modified Daniell (or rectangular) window to be used to smooth the periodogram. 
##      Use a width of 5 for the smoothing function
##      Make a (3 column) side by side plot of raw, unpadded, and filtered spectrum plots


## 3.10. Compute the 95% confidence intervals for the filtered spectrum


## 3.11. Make a (3 column) side by side plot of raw, unpadded, and filtered spectrum plots. 
##       On the plot of filtered spectrum, add the lines for the upper and lower confidence intervals, and log scale the y axis



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



