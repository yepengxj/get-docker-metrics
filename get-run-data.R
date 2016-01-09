# Load the package required to read JSON files.
library("rjson")      #install.packages("rjson")
library("httr")         #install.packages("httr")
library(mailR)
library(xtable)


#调用cadvisor API返回html报文
result<-GET("http://$host_id:$port/api/v1.0/machine")  #httr包
#从html报文中提取json数据/html正文
result <- fromJSON(content(result, "text"))  #content来自httr包，fromJSON来自rjson包
memory_capacity <- result$memory_capacity

result<-GET("http://$host_id:$port/api/v2.0/ps/")
# Give the input file name to the function.
result <- fromJSON(content(result, "text"))

idx<-1
ps_info<-NULL
for(ps_info_t in result)
{
  if(idx == 1)
  {
    ps_info<-as.data.frame(ps_info_t)
  }
  else
  {
    ps_info<-rbind(ps_info,as.data.frame(ps_info_t))
  }
  
  idx<- idx+1
} 
ps_info_table<-print(xtable(ps_info, caption = 'ps_info'), 
      type="html",
      caption.placement = 'top',
      #html.table.attributes=""
      )

# Convert JSON file to a data frame.
#调用cadvisor API返回html报文
result <- GET("http://$host_id:$port/api/v2.0/storage")

result <- fromJSON(content(result, "text"))
file_system_df<-NULL
result[[1]]
idx<-1
for(filesystem_info in result)
{
  if(idx == 1)
  {
    file_system_df<-cbind(
      data.frame("device"=filesystem_info$device),
      data.frame("mountpoint"=filesystem_info$mountpoint),
      data.frame("capacity"=filesystem_info$capacity),
      data.frame("available"=filesystem_info$available),
      data.frame("usage"=filesystem_info$usage)
    )
  }
  else
  {
    file_system_df<-rbind(file_system_df,
                          cbind(
                            data.frame("device"=filesystem_info$device),
                            data.frame("mountpoint"=filesystem_info$mountpoint),
                            data.frame("capacity"=filesystem_info$capacity),
                            data.frame("available"=filesystem_info$available),
                            data.frame("usage"=filesystem_info$usage)
                          )
                          )
  }
  
  idx<- idx+1
}
View(file_system_df)
file_system_df<-file_system_df[-grep("/dev/mapper/docker",file_system_df[,"device"]),]
file_system_df$available<-paste0( format((file_system_df$available/file_system_df$capacity)*100 ,digits =2), "_%" )
file_system_df$capacity<- paste0(format(file_system_df$capacity/1024/1024/1024  ,digits =2),"_GB")
file_system_df$usage<-paste0(format(file_system_df$usage/1024/1024/1024 ,digits =2),"_GB")

fs_info_table<-print(xtable(file_system_df[,c("device","mountpoint","capacity","usage","available")], caption = '<H2>file_system_info</H2>'), 
      type="html",
      #html.table.attributes="",
      caption.placement = 'top', 
      )

result <- GET("http://$host_id:$port/api/v2.0/stats?count=1")

result <- fromJSON(content(result, "text"))

as.data.frame(result$`/`[[1]]$cpu)

cpu_table <- print(xtable(as.data.frame(result$`/`[[1]]$cpu), caption = '<H2>cpu_info</H2>'), 
      type="html",
      #html.table.attributes="",
      caption.placement = 'top', 
    )

memory<-as.data.frame(result$`/`[[1]]$memory)
memory<-cbind(data.frame("memory_capacity"=memory_capacity),memory)
memory$available<- paste0( format((1- memory$usage/memory$memory_capacity) * 100,digits =2 ), "_%" )
memory$memory_capacity<-paste0(format(as.numeric(memory$memory_capacity/1024/1024/1024),digits =2 ),"_GB")
memory$usage<-paste0(format(memory$usage/1024/1024/1024,digits=2),"_GB")
memory$working_set<-paste0(format(memory$working_set/1024/1024/1024,digits=2),"_GB")

mem_table<-print(xtable(memory[,c("memory_capacity","usage","available")], caption = '<H2>memory_info</H2>'), 
      type="html",
      #html.table.attributes="",
      caption.placement = 'top', 
     )

as.data.frame(result$`/`[[1]]$network)

nt_table<-print(xtable(as.data.frame(result$`/`[[1]]$network), caption = '<H2>network_info</H2>'), 
      type="html",
      #html.table.attributes="",
      caption.placement = 'top', 
      )

result <- GET("http://$host_id:$port/api/v1.3/docker")
result <- fromJSON(content(result, "text"))

idx<-1
cont_metric<- NULL
for(subcon in result)
{
    cont_metric_t<-cbind(#as.data.frame(subcon$name),
                       as.data.frame(subcon$aliases[1]),
                       #as.data.frame(subcon$aliases[2]),
                       #as.data.frame(subcon$namespace),
                       as.data.frame(subcon$spec$creation_time),
                       #as.data.frame(subcon$spec$labels ),
                       as.data.frame(subcon$spec$image),
                       #as.data.frame(subcon$stats[[1]]$timestamp),
                       #as.data.frame(subcon$stats[[1]]$cpu),
                       as.data.frame(subcon$stats[[1]]$memory$usage),
                       as.data.frame(subcon$stats[[1]]$memory$working_set))
    
    colnames(cont_metric_t)<-c(
      #"cadvisor_id",
      "name",
     #"container_id",
      #  "namespace",
      "creation_time",
      #  "labels",
      "image",
      #  "timestamp",
      #   "cpu",
      "memory_usage",
      "memory_working_set"
      #"memory_container_data.pgfault",
      #"memory_container_data.pgmajfault",
      #"memory_hierarchical_data.pgfault",
      #"memory_hierarchical_data.pgmajfault"
    )
    
    if(idx == 1)
    {
      cont_metric <- cont_metric_t
    }
    else
    {
      cont_metric <- rbind(cont_metric,cont_metric_t)
    }
  idx<- idx+1
}
cont_metric<-cont_metric[order(-cont_metric[,"memory_usage"]),]
cont_metric$memory_usage<-  paste0(format(cont_metric$memory_usage/1024/1024,digits = 2  ),"_MB")
cont_metric$memory_working_set<-paste0(format(cont_metric$memory_working_set/1024/1024 ,digits = 2  ),"_MB")
cont_metric_table<-print(xtable(cont_metric[,c("name","memory_usage","image","creation_time")], 
                                caption = '<H2>container_info</H2>'), 
      type="html",
      #html.table.attributes="",
      caption.placement = 'top', 
      file="/Users/yepeng/Desktop/example.html")

all_info<-paste(cpu_table,
                mem_table,
                fs_info_table,
                nt_table,
                ps_info_table,
                cont_metric_table,
                "<br><p><br>")

write(all_info,file="/Users/yepeng/Desktop/example.html")

#mailR包
send.mail(from = "13552330677@139.com",
          to = c("yepeng@asiainfo.com"),
          subject = "主机性能日常监控",
          body = all_info,
          html = TRUE,
          inline = TRUE,
          encoding="utf-8",
          smtp = list(host.name = "smtp.139.com", port = 25, user.name = "13552330677", passwd = "yepeng@1", ssl = F),
          authenticate = TRUE,
          send = TRUE)
