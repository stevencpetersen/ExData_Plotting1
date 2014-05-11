plot2<-function() {
      
# Set-up date conversion for use in read.table
      setClass('myDate')
      setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y"))
      
# Read in table - data must be one subdirectory from current working directory
      p<-read.table("./exdata_data_household_power_consumption/household_power_consumption.txt",
                    header=TRUE,sep=";",na.strings="?",as.is=TRUE, 
                    colClasses=c("myDate","character","numeric","numeric",
                                 "numeric","numeric","numeric","numeric","numeric"))
      
# Select rows where date are 2007-02-01 and 2007-02-02
      p<-p[p$Date=="2007-02-01"|p$Date=="2007-02-02",]
      
# Add columns for date concatenated with time and weekday abbreviation for the date given
      p<-data.frame(DateTime=as.character(paste(p$Date,p$Time)),
                    WeekDay=as.character(weekdays(p$Date,abbreviate=TRUE)),p)
      
# Set-up the png output file
      png(file = "plot2.png", bg = "white")
 
# Plot global active power in kilowatts over time
      plot(p$DateTime,p$Global_active_power,type="n",ylab="Global Active Power (kilowatts)",
           xaxt="n")
      lines(p$DateTime,p$Global_active_power)
      axis(1,at=c(min(which(p$WeekDay == "Thu")),
            min(which(p$WeekDay == "Fri")),
            max(which(p$WeekDay == "Fri"))+1),
            labels=c("Thu","Fri","Sat"))

# Close the output
      dev.off()
}