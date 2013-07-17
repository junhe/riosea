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

