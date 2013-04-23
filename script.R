play_movie <- function(df, mypid, save.pic=F)
{
	# Sort by time, then by logical offset
	df = df[with(df, order(Begin_timestamp, Logical_offset)), ]
	df$ORG.PID = as.factor(df$ORG.PID)
	df$WriteID = row.names(df)
	# select the pid you want
	df = subset(df, ORG.PID == mypid)
	# convert from wide to long
	df.melt = melt(df, id = c("WriteID", "Length"), measure=c("Logical_offset", "Chunk_offset"),
	               variable_name="OffsetType")
	df.melt$WriteID = interaction(df.melt$WriteID, drop=T)
	endtmp = df.melt$value + df.melt$Length
	myxmax = max(endtmp, na.rm = T)
	
	p <- ggplot(df.melt, aes()) +
		#geom_segment(aes(x=value, xend=value+Length, y=WriteID, yend=WriteID, color=WriteID), size=5, alpha=0.5) +
		xlab("Offset") + ylab("Write ID") + facet_grid(OffsetType~., drop=F) + opts(legend.position = "none") + 
		xlim(0, 1.1*myxmax) + ylim(levels(df.melt$WriteID))
	myrowsize = nrow(df.melt)
	for ( i in 1:(myrowsize/2) ) {
		print( last_plot() + geom_segment(data = df.melt[c(i,i+(myrowsize/2)),], aes(x=value, xend=value+Length, y=WriteID, yend=WriteID, color=WriteID  ), size=5, alpha=0.5)   )
		if (save.pic == T) {
		  ggsave(file=paste("mywid", i, "png", sep="."))
		} 
	}	
}
play_movie(df, 53)



fill.frame <- function(df)
{
  # Sort by time, then by logical offset
  df = df[with(df, order(Begin_timestamp, Logical_offset)), ]
  df$ORG.PID = as.factor(df$ORG.PID)
  df$WriteID = row.names(df)
  
  
  fend = max(df$Logical_tail, na.rm=T)
  frame.length = ceiling(sqrt(fend))
  print( c("frame.length:", frame.length) )
  
  p <- ggplot(df, aes()) + ylim(c(0, frame.length)) + xlim(c(0, frame.length)) 
  p
  
  
  n = nrow(df)
  for ( i in 1:n ) {
    entry = df[i,]
	print(entry)
    # sx: segment x
    # rx: row x
    sx = entry$Logical_offset %% frame.length
    sxend = entry$Logical_tail %% frame.length
    sy = floor(entry$Logical_offset / frame.length)
    syend = floor(entry$Logical_tail / frame.length)
    print( c("sx, sxend, sy, syend", sx, sxend, sy, syend) )
	
	df.seg = data.frame(rx=NULL,rxend=NULL,ry=NULL,ryend=NULL)
	
    for (yi in sy:syend) {
      ry = yi
      ryend = yi
      
      rx = 0
      rxend = frame.length
      if ( yi == sy ) {
        # The starting row
        rx = sx
      } else if (yi == syend) {
        # The ending row
        rxend = sxend        
      }
      df.seg = rbind(df.seg, c(rx,rxend,ry,ryend))
    }
	names(df.seg) = c("rx","rxend","ry","ryend")
	newp <- last_plot() + 
            geom_segment(data=df.seg, aes(x=rx, y=ry, xend=rxend, yend=ryend))
    print(newp)	
  }
}
fill.frame(df)


plot.speed <-function(df, period=10)
{
  # Sort by time, then by logical offset
  df = df[with(df, order(Begin_timestamp, Logical_offset)), ]
  df$ORG.PID = as.factor(df$ORG.PID)
  df$WriteID = row.names(df)
  
  df.speed = data.frame(t(rep(NA, 4)))
  df.speed = df.speed[-1,]
  
  
  n = nrow(df)
  for ( i in 1:n ) {
    cur = i
    pre = cur - period
    if ( pre < 1 ) {
      pre = 1
    }
    time = df[cur,]$End_timestamp - df[pre,]$Begin_timestamp
    size = sum( df[pre:cur,"Length"])
    ops = cur - pre + 1
    bandwidth = size/time
    iops = ops/time
    df.speed = rbind(df.speed, c(cur, df[cur,]$End_timestamp, bandwidth, iops))    
	
  }
  
  names(df.speed) = c("writeid", "time", "bandwidth", "iops") 
  df.speed$bandwidth = df.speed$bandwidth/(1024*1024)
  nspeed = nrow(df.speed)
  df.melt = melt(data=df.speed, id=c("writeid", "time"))
  #print(nspeed)
  #print(nrow(df.melt))
  for ( i in 1:nspeed ) {
    bw = as.character(df.melt[i,]$value)
	print(bw)
    sel = c(1:i, (nspeed+1):(nspeed+i))
    p <- ggplot(data=df.melt[sel,], aes()) +
	  geom_line(aes(x=writeid, y=value)) + 	  
	  scale_y_log10() + facet_grid(variable~., scale="free")+
	  geom_text(data = NULL, x = 50, y = 50, label = bw ) + xlim(0, 100) + ylim(0,100)
	print(p)
	Sys.sleep(1)
  }
}
plot.speed(df)


plot.rankdensity <- function(df)
{
  df = df[with(df, order(Begin_timestamp, Logical_offset)), ]
  df$ORG.PID = as.factor(df$ORG.PID)
  df$WriteID = row.names(df)
  
}
