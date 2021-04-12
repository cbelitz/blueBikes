
# read in trip data for Sep 2019. A bit slow but only needs to be done once
temp = tempfile()
download.file("https://s3.amazonaws.com/hubway-data/201909-bluebikes-tripdata.zip", temp)
trip_zip = unz(temp, "201909-bluebikes-tripdata.csv")
trip_data = read.csv(trip_zip, header=TRUE)
unlink(temp)
summary(trip_data)

