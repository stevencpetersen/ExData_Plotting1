plot4<-function() {

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
      png(file = "plot4.png", bg = "white")

# Set-up window for 2x2 charts
      par(mfrow=c(2,2),pty="m")

# Plot global active power in kilowatts over time     
      plot(p$DateTime,p$Global_active_power,type="n",ylab="Global Active Power",
           xaxt="n")
      lines(p$DateTime,p$Global_active_power)
      axis(1,at=c(min(which(p$WeekDay == "Thu")),
            min(which(p$WeekDay == "Fri")),
            max(which(p$WeekDay == "Fri"))+1),
            labels=c("Thu","Fri","Sat"))

# Plot voltage over time
      plot(p$DateTime,p$Voltage,type="n",ylab="Voltage",xaxt="n",xlab="datetime")
      lines(p$DateTime,p$Voltage)
      axis(1,at=c(min(which(p$WeekDay == "Thu")),
            min(which(p$WeekDay == "Fri")),
            max(which(p$WeekDay == "Fri"))+1),
      labels=c("Thu","Fri","Sat"))

# Plot sub_metering values 1,2,3 over time     
      plot(p$DateTime,p$Sub_metering_1,xaxt="n",ylab="Energy sub metering")
      lines(p$DateTime,p$Sub_metering_1)
      lines(p$DateTime,p$Sub_metering_2,col="red")
      lines(p$DateTime,p$Sub_metering_3,col="blue")
      legend("topright",pch=c(32,32,32),col=c("black","red","blue"),
            legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
            lwd=1,lty=1)
      axis(1,at=c(min(which(p$WeekDay == "Thu")),
            min(which(p$WeekDay == "Fri")),
            max(which(p$WeekDay == "Fri"))+1),
            labels=c("Thu","Fri","Sat"))

# Plot global reactive power over time
      plot(p$DateTime,p$Global_reactive_power,type="n",xaxt="n",
           ylab="Global_reactive_power", xlab="datetime")
      lines(p$DateTime,p$Global_reactive_power)
      axis(1,at=c(min(which(p$WeekDay == "Thu")),
            min(which(p$WeekDay == "Fri")),
            max(which(p$WeekDay == "Fri"))+1),
      labels=c("Thu","Fri","Sat"))

# Reset par to 1x1
      par(mfrow=c(1,1))

# Close the output 
      dev.off()
}