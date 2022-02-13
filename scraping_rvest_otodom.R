
library(devtools)
library(dplyr)
library(stringr)
library(gtools)
library(rvest)
library(xml2)

LinksVector<-c()
for ( i in 1: 2){
  newUrl<- paste0("https://www.otodom.pl/pl/oferty/sprzedaz/dom/lublin?page=",i)
  print(newUrl)
  page <-read_html(newUrl)
  temp<- page%>%html_nodes(".css-14cy79a")%>%html_children()%>%html_nodes("a")%>%xml_attr("href")
  temp<- paste0("https://www.otodom.pl",temp)#łaczymy nazwy linków
  LinksVector<-c(LinksVector,temp)
}

LinksVectorU<- LinksVector%>%unique()



doRvest<- function(link){
  newUrl<-link
  page<-read_html(newUrl)
  Price<-html_node(page,".css-b114we")%>%html_text()
  
  ColumnName<-html_node(page,".css-17vqyja")%>%html_node(".css-1sxg93g")%>%html_nodes(".css-18h1kfv")%>%html_attr("aria-label")
  value<-html_node(page,".css-17vqyja")%>%html_node(".css-1sxg93g")%>%html_nodes(".css-18h1kfv")%>%html_node(".css-1ytkscc")%>%html_attr("title")
  
  df1<- data.frame  (matrix(value,nrow = 1,ncol=length(value)) )
  names(df1) <- ColumnName
  df1<-cbind(Price,df1)
  df1
}

houses<-NULL
for(w in 1: length(LinksVectorU)){
  skip<-FALSE
  tryCatch(
    df1<-doRvest(LinksVectorU[w]),error=function(e){skip<<-TRUE}
  )
  if(skip){next}
  
  if(is.null(houses)){
    houses<-df1
  }
  else{
    houses<-smartbind(houses,df1)
  }
}

write.csv(houses,"otodom.csv", row.names = FALSE)
