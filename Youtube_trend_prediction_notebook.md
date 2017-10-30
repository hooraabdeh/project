# Something fascinating
Jenny Bryan  
`r format(Sys.Date())`  


  

```r
#output: github_document
# html_document:
  #  toc: yes
  # html_preview
  # html_notebook:
  #  toc: yes
 # pdf_document:
  #  highlight: tango
   # toc: yes
```


```r
inputfile=read.csv("/Users/hoora/Documents/Interview/Data_Incubator/Semifinal/Challenge/youtube/USvideos.csv",header = TRUE)
#summary(inputfile)
attach(inputfile)
category_name=read.table("/Users/hoora/Documents/Interview/Data_Incubator/Semifinal/Challenge/youtube/US_category_id.json")
```


```r
# survaival days

a <- table(video_id)
survaival = array(1:dim(inputfile)[1]) 
id_d = array(1:dim(inputfile)[1])
id = character(dim(inputfile)[1])

for (i in 1:dim(inputfile)[1]) {
  survaival[i]=a[names(a)==video_id[i]]
  id_d[i] <- category_id[i]
  id[i] <- as.character(category_name$V3[category_name$V1 == category_id[i]] )
} 

# Category histogram
library(ggplot2)
#library(plotly)

b <- table(id)
hist(a, col = "red", breaks=8, xlim=c(0,8), 
     main="My Histogram", las=2, xlab = "Values", cex.lab = 1.3)
```

![](Youtube_trend_prediction_notebook_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#plot_ly( x = ~names(b), y = ~b, type = 'bar')
```




```r
b
```

```
## id
##      Autos & Vehicles                Comedy             Education 
##                   116                   756                   334 
##         Entertainment      Film & Animation                Gaming 
##                  1601                   378                    82 
##         Howto & Style                 Music       News & Politics 
##                   870                  1252                   626 
## Nonprofits & Activism        People & Blogs        Pets & Animals 
##                    14                   883                   116 
##  Science & Technology                 Shows                Sports 
##                   512                     2                   410 
##       Travel & Events 
##                    48
```


```r
plot(category_id, likes, xlim=c(0,30), xlab="Category ID", ylab="Likes")
```

![](Youtube_trend_prediction_notebook_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

