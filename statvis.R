plot_gantt_plfs_fileview <- function (df)
{
	df$ORG.PID = as.factor(df$ORG.PID)

	ggplot(df, aes(colour=ORG.PID)) +
		geom_segment(aes(x=Logical_offset, xend=Logical_tail, y=ORG.PID, yend=ORG.PID ), size=3) +
		xlab("Logical Offset") + ylab("Rank")
}



plot_gantt_plfs_fileview_sort <- function (df)
{	
	# order PID by the first logical offset
	df.hds = ddply(df, .(ORG.PID), function(x) head(x,1))
	df.hds = arrange(df.hds, Logical_offset)
	#orderedPid = factor(df.hds$ORG.PID, levels = df.hds$ORG.PID)
	orderedPid = df.hds$ORG.PID
	
	# reorder the ORG.PID levels
	df$ORG.PID = factor(df$ORG.PID, levels = orderedPid)

	ggplot(df, aes(colour=ORG.PID)) +
		geom_segment(aes(x=Logical_offset, xend=Logical_tail, y=ORG.PID, yend=ORG.PID ), size=3) +
		xlab("Logical Offset") + ylab("Rank")
}


############################################################
plot_gantt_plfs <- function (df)
{
	df$ORG.PID = as.factor(df$ORG.PID)
	themin = min(df$Begin_timestamp)

	df$Begin_timestamp = df$Begin_timestamp - themin
	df$End_timestamp = df$End_timestamp - themin

	ggplot(df, aes(colour=ORG.PID)) +
		geom_segment(aes(x=Begin_timestamp, xend=End_timestamp, y=Logical_offset, yend=Logical_tail), size=1) +
		xlab("Time (sec)")   #+coord_flip()
}


############################################################
plot_gantt <- function (df)
{
 df$pid = as.factor(df$pid)
 themin = min(df$starttime)
 sn = 1:nrow(df)
 df = cbind(sn, df)
 df$starttime = df$starttime - themin
 df$endtime = df$endtime - themin
 df$tail = df$offset + df$length

 for ( mypid in levels(df$pid) ) {
  df.toplot = subset( df, pid == mypid )
  p = ggplot(df.toplot, aes()) +
   geom_segment(aes(x=offset, xend=tail, y=sn, yend=sn), size=1) +
   facet_wrap(~pid, ncol=4)
  print(p)
  readline()
 }
}

############################################################
# Output basic statistics of the map
# Write range
# Overall duration time
# Number of Writes
# Length statistics
map_stat <- function(df)
{
	unitdiv = 1024 #show KB results
	
	df$duration = df$End_timestamp - df$Begin_timestamp
	
	print("Write range")
	print(c(min(df$Logical_offset)/unitdiv, 
	        max(df$Logical_tail)/unitdiv) )
	
	print("Overall duation time")
	print( 
			max(df$End_timestamp)-min(df$Begin_timestamp)
		 )
	
	print("Total number of writes (may not be accurate due to merging)")
	print(nrow(df))
	
	print("Length summary 1")
	print(summary(df$Length))
	
	print("Length summary 2")
	print(summary(as.factor(df$Length)))
	
	phist_len = ggplot(df, aes(Length)) +
			geom_histogram(aes(y  =  ..count..), binwidth  =  1024)
	print(phist_len)
	
	
	phist_duration = ggplot(df, aes(duration)) +
			geom_histogram(aes(y  =  ..count..))
	windows()
	print(phist_duration)
}


















