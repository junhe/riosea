# data frame list
# df.btio = rdmine()
# df.chombo = rdmine()
# df.flash64 = rdmine()
# df.lanlapp.1 = rdmine()
# df.lanlapp2.appio = rdmine()
# df.lanlapp2.col = rdmine()
# df.lanlapp2.ind = rdmine()
# df.lanlapp3.50000 = rdmine()
# df.fstest64 = rdmine()
# df.flash64.hdf5.chk = rdmine()
# df.flash64.hdf5.cnt = rdmine()
# df.flash64.hdf5.crn = rdmine()

create_list <- function ()
{
	mylist <- list()
	mylist[[1]] = df.btio 
	mylist[[2]] = df.chombo 
	mylist[[3]] = df.flash64 
	mylist[[4]] = df.lanlapp.1 
	mylist[[5]] = df.lanlapp2.appio 
	mylist[[6]] = df.lanlapp2.col 
	mylist[[7]] = df.lanlapp2.ind 
	mylist[[8]] = df.lanlapp3.50000 
	mylist[[9]] = df.fstest64 
	mylist[[10]] = df.flash64.hdf5.chk 
	mylist[[11]] = df.flash64.hdf5.cnt 
	mylist[[12]] = df.flash64.hdf5.crn 
	mylist
}


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
	stime = min(df$Begin_timestamp)
	df$Begin_timestamp = df$Begin_timestamp - stime
	df$End_timestamp = df$End_timestamp - stime
	
	
	df$duration = df$End_timestamp - df$Begin_timestamp
	
	
	pfileview = plot_gantt_plfs_fileview_sort(df)
	windows()
	print(pfileview)
	
	
	pgannt = plot_gantt_plfs(df)
	windows()
	print(pgannt)
	
	
	print("Write range")
	print(paste(min(df$Logical_offset), " to ",
	        max(df$Logical_tail)) )
	
	print("Overall duation time")
	overalltime = max(df$End_timestamp)-min(df$Begin_timestamp)
	print( overalltime )
	
	print("Aggragated write size")
	overallsize = sum(as.numeric(df$Length))
	print( paste(overallsize, "bytes or ", overallsize/(1024*1024), "MB") )
	
	print("Overall bandwidth (may be lower than peak)")
	overallbw = overallsize/overalltime
	print( paste(overallbw/(1024*1024), "MB/s") )
	
	print("Total number of writes (may not be accurate due to merging before tracing)")
	nwrites = nrow(df)
	print(nwrites)
	
	print("index size (48 bytes per entry)")
	print(paste(nwrites*48/(1024*1024), " MB"))
	
	print("Length summary 1")
	print(summary(df$Length))
	
	#print("Length summary 2")
	#print(summary(as.factor(df$Length)))
	
	phist_len = ggplot(df, aes(Length)) +
			geom_histogram(aes(y  =  ..count..), binwidth  =  1024) +
			ggtitle("Overall Length histogram")
	windows()
	print(phist_len)
	
	
	phist_duration = ggplot(df, aes(duration)) +
			geom_histogram(aes(y  =  ..count..)) + 
			ggtitle("Overall duration histogram")
	windows()
	print(phist_duration)
	
	# Holes and overlaps
	df = find_hole(df)
	print("Number of holes")
	print( nrow(subset(df, prefix_hole>0)) )
	print("Number of Overlap. May be already handled by PLFS")
	print( nrow(subset(df, prefix_hole<0)) )
	
	# IOPS histogram (op issue rate)
	phist_iops = ggplot(df, aes(Begin_timestamp)) +
			geom_histogram(aes(y  =  ..count..), binwidth  =  1.0) + ylab("Number of IO ops issued")
	windows()
	print(phist_iops)
	
	# plot holes of a single pid
	df.pid = subset(df, ORG.PID == 3)
	df.pid = find_hole(df.pid)
	phole_pid = ggplot(df.pid, aes(x=Logical_offset, y=prefix_hole)) +
			geom_point() + ggtitle("From local data")
	windows()
	print(phole_pid)
	
	print("Length summary of a PID")
	print(summary(as.factor(df.pid$Length)))
	
	plength_pid = ggplot(df.pid, aes(x=Logical_offset, y=Length)) +
			geom_point() + ggtitle("From local data")
	windows()
	print(plength_pid)	
	
	# Merge contiguous
	#df.merged = merge_contiguous(df)
	#print("number of rows after merging")
	#print(nrow(df.merged))
}

############################################
########## To find hole in each PID group
############################################

find_hole <- function(map)
{
	# sort by logical offset
	map = arrange(map, Logical_offset)

	tail_len = length(map$Logical_tail)
	map$pre_tail = c(map$Logical_offset[1], map$Logical_tail[1:tail_len-1]+1) 
	map$prefix_hole = map$Logical_offset - map$pre_tail
	# If prefix hole is a postive nubmer
	#    then there is a hole
	# else if it is negative
	#    then there is AT LEAST an overlap
	# else 0
	#    then it is the first write or it is contiguous with the previous one
	map
}

find_hole_bypid <- function(df)
{
	ddply(df, .(ORG.PID), find_hole_in_pid)
}

##############################################
# df should have 1 row at least
mark_contiguous_seg <- function(df)
{
	df = find_hole(df)
	n = nrow(df)
	df$seg = rep(-1, n)
	segidx = 0
	for ( i in 1:n) {
		if (df$prefix_hole != 0 || i == 1) {
			segidx = segidx + 1
		}
		df$seg[i] = segidx
	}
	df
}

##############################################################################
############# merge local(per process) contiguous
##############################################################################

# the pid has to be the same
merge_contiguous_withinpid <- function(df)
{

	tail_len = length(df$Logical_tail)
	df$pre_tail = c(df$Logical_offset[1], df$Logical_tail[1:tail_len-1]+1) 
	df$prefix_hole = df$Logical_offset - df$pre_tail

	# mark all segment-start entry
	df$segstart = (df$prefix_hole != 0)
	df$segstart[1] = TRUE # this first one is always a start
	
	df$segend = c(df$segstart[-1], TRUE) # the last one is always an end
	
	starts = subset(df, segstart == TRUE)
	ends = subset(df, segend == TRUE)
	
	starts$Length = ends$Logical_offset + ends$Length - starts$Logical_offset
	starts$End_timestamp = ends$End_timestamp
	starts$Logical_tail = starts$Logical_offset + starts$Length - 1
	
	starts
}

merge_local_contig <- function(df)
{
	ddply(df, .(ORG.PID), merge_contiguous_withinpid)
}








# all rows in df should have the same seg number
shrink_contiguous_seg <- function(df)
{
	n = nrow(df)
	if ( n <= 1 ) {
		df
	} else {
		first = head(df,1)
		last = tail(df,1)
		first$Length = sum(as.numeric(df$Length))
		first$End_timestamp = last$End_timestamp
		first$Logical_tail = last$Logical_tail
		#first$Chunk_offset = NA
		first
	}
}


merge_contiguous <- function(df)
{
	df = mark_contiguous_seg(df)
	ddply(df, .(seg), shrink_contiguous_seg)
}













