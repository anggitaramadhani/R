library(dplyr)
library(rtweet)
library(tidyr)
library(tidytext)
library(ggplot2)
library(textdata)
library(purrr)

baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"
api_key <- "xsV1Ntr65ZM9wPjoMMqRGd7Uf"
api_secret_key <- "BldXwi2f4IeiXDAgnG8McKTt3LaE3iXwU4VmdfZCuRuAPipJia"

f1 = list(family="Courier New, monospace", size=12, 
          color="rgb(30,30,30)")
minutesSinceLastUpdate = function(fileName) {
  (as.numeric(as.POSIXlt(Sys.time())) -  
     as.numeric(file.info(fileName)$ctime)) / 60
}

kata <- search_tweets("#COVID19", n=1000, include_rts=FALSE)

## proses setiap set tweet menjadi teks rapi atau objek corpus
tweet.Kata = kata %>% select (screen_name, text)

## menghapus element http
tweet.Kata$stripped_text1 <- gsub("http\\S+","",tweet.Kata$text)

## gunakan fungsi unnest_tokens() untuk konversi menjadi huruf kecil
## hapus tanda baca, dan id untuk setiap tweet
tweet.Kata_stem <- tweet.Kata %>%
  select(stripped_text1) %>%
  unnest_tokens(word, stripped_text1)

## hapus kata-kata stopwords dari daftar kata-kata
cleaned_tweets.Kata <- tweet.Kata_stem %>%
  anti_join(stop_words)

## untuk melakukan analisis sentimen menggunakan Bing di tweet COVID19,
## perintah berikut ini mengembalikan sebuah tibble.
bing_kata = cleaned_tweets.Kata %>% inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>% ungroup()

## visualisasi jumlah kata untuk memfilter dan memplot kata-kata 
## bersebelahan untuk membandingkan emosi positif dan negatif.
bing_kata %>% group_by(sentiment) %>% top_n(10) %>% ungroup() %>%
  mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets containing '#COVID19'", y = "Contribution to sentiment",
       x = NULL) + coord_flip() + theme_bw()

## fungsi untuk mendapatkan skor sentimen untuk setiap tweet
sentiment_bing = function(twt){
  twt_tbl = tibble(text = twt) %>%
    mutate(
      stripped_text = gsub("http\\S+","",text)
    )%>%
    unnest_tokens(word, stripped_text) %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("bing")) %>%
    count(word,sentiment, sort = TRUE) %>%
    ungroup() %>%
    ## buat kolom "skor", yang menetapkan -1 untuk semua kata negatif, 
    ## dan 1 untuk kata positif
    mutate(
      score = case_when(
        sentiment == 'negative'~n*(-1),
        sentiment == 'positive'~n*1)
    )
  ## menghitung total score
  sent.score = case_when(
    nrow(twt_tbl)==0~0, #jika tidak ada kata, skor adalah 0
    nrow(twt_tbl)>0~sum(twt_tbl$score) #selainnya, jumlah positif dan negatif
  )
  ## untuk melacak tweet mana yang tidak mengandung kata sama sekali dari daftar bing
  zero.type = case_when(
    nrow(twt_tbl)==0~"Type 1", #Type 1 : tidak ada kata sama sekali, zero = nol
    nrow(twt_tbl)>0~"Type 2" #Type 2 : nol berarti jumlah kata = 0
  )
  list(score = sent.score, type = zero.type, twt_tbl = twt_tbl)
}

## fungsi lapply untuk mengembalikan list semua skor sentimen, jenis, dan tabel tweet
kata_sent = lapply(kata$text, function(x){sentiment_bing(x)})

## membuat tibble yang menentukan kata, skor, dan jenisnya
kata_sentiment = bind_rows(tibble(kata = '#COVID19', score = unlist(map(kata_sent, 'score')), 
                                  type = unlist(map(kata_sent, 'type'))))

## mengecek data apakah file sudah data terbaru bila sudah maka file akan 
## disave dengan data terbaru bila belum maka akan menggunakan dataset yang lama
loadData = function(fileName, columnName) {
  if(!file.exists(fileName) || 
     minutesSinceLastUpdate(fileName) > 10) {
    data = read.csv(file.path(baseURL, fileName), 
                    check.names=FALSE, stringsAsFactors=FALSE) %>%
      select(-Lat, -Long) %>% 
      pivot_longer(-(1:2), names_to="date", values_to=columnName)%>%
      mutate(
        date=as.Date(date, format="%m/%d/%y"),
       
      )
    save(data, file=fileName)  
  } else {
    load(file=fileName)
  }
  return(data)
}

## join dari 3 dataset antara data jumlah kasus, kematian dan kesembuhan
allData = 
  loadData(
    "time_series_covid19_confirmed_global.csv", "CumConfirmed") %>%
  inner_join(loadData(
    "time_series_covid19_deaths_global.csv", "CumDeaths")) %>%
  inner_join(loadData(
    "time_series_covid19_recovered_global.csv","CumRecovered"))

## memfilter inputan dari data yang diinput pada ui
function(input, output, session) {
  
  data = reactive({
    d = allData %>%
      filter(`Country/Region` == input$country)
   
      d = d %>% 
        group_by(date) %>% 
        summarise_if(is.numeric, sum, na.rm=TRUE)

    
    d %>%
      ## Menambah kolom untuk data yang ada.
      mutate(
        dateStr = format(date, format="%b %d, %Y"),    # Jan 20, 2020
        NewConfirmed=CumConfirmed - lag(CumConfirmed, default=0),
        NewRecovered=CumRecovered - lag(CumRecovered, default=0),
        NewDeaths=CumDeaths - lag(CumDeaths, default=0)
      )
  })
  
  
  countries = sort(unique(allData$`Country/Region`))
  
  #untuk membuat negara china sebagai negara default pada pertama kali menjalankan program
  updateSelectInput(session, "country", choices=countries, selected="China")
  
  #fungsi untuk konfigurasi grafik dan legenda pada tampilan 
  renderBarPlot = function(varPrefix, legendPrefix, yaxisTitle) {
    renderPlotly({
      data = data()
      plt = data %>% 
        plot_ly() %>%
        config(displayModeBar=FALSE) %>%
        layout(
          barmode='group', 
          xaxis=list(
            title="", tickangle=-90, type='category', ticktext=as.list(data$dateStr), 
            tickvals=as.list(data$date), gridwidth=1), 
          yaxis=list(
            title=yaxisTitle
          ),
          legend=list(x=0.05, y=0.95, font=list(size=15), bgcolor='rgba(240,240,240,0.5)'),
          font=f1
        )
      for(metric in input$metrics) 
        plt = plt %>%
        add_trace(
          x= ~date, y=data[[paste0(varPrefix, metric)]], type='bar', 
          name=paste(legendPrefix, metric, "Cases"),
          marker=list(
            color=switch(metric, Deaths='rgb(200,30,30)', Recovered='rgb(30,200,30)', Confirmed='rgb(100,140,240)'),
            line=list(color='rgb(8,48,107)', width=1.0)
          )
        )
      plt
    })
        
  }

  
  ## menampilkan grafik pada plotly yang ada pada ui.R
  output$dailyMetrics = renderBarPlot("New", legendPrefix="New", yaxisTitle="New Cases per Day")
  output$cumulatedMetrics = renderBarPlot("Cum", legendPrefix="Cumulated", yaxisTitle="Cumulated Cases")
  output$sentimentResult = renderPlotly(bing_kata %>% group_by(sentiment) %>% top_n(10) %>% ungroup() %>%
                                          mutate(word = reorder(word, n)) %>% ggplot(aes(word, n, fill = sentiment)) +
                                          geom_col(show.legend = FALSE) + facet_wrap(~sentiment, scales = "free_y") +
                                          labs(title = "Tweets containing '#COVID19'", y = "Contribution to sentiment",
                                               x = NULL) + coord_flip() + theme_bw())
  output$sentimentMetrics = renderPlotly(ggplot(kata_sentiment, aes(x=score, fill=kata)) +
                                           geom_histogram(bins = 15, alpha= .6) + facet_grid(~kata) + theme_bw())
}
