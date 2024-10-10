import finnhub
import pandas as pd
import numpy as np
finnhub_client = finnhub.Client(api_key="cf0ntpqad3i62koq2cb0cf0ntpqad3i62koq2cbg")
ticker=pd.read_csv('/Users/peterguo/Desktop/Bitcoin 1/output_next2.csv')
#1. General Information 
# first 60
list1=[]
first=ticker.iloc[0:60,0]
for a in first:
  list1.append(finnhub_client.company_profile2(symbol=a))
list1=pd.DataFrame(list1)
# generate excel
list1.to_excel('first.xlsx')

# last
list2=[]
last=ticker.iloc[60:100,0]
for b in last:
  list2.append(finnhub_client.company_profile2(symbol=b))
list2=pd.DataFrame(list2)

# generate excel
list2.to_excel('last.xlsx')


#2. Analysts Recommendation Trends:
list3=[]
trend_first=ticker.iloc[0:60,0]
for c in trend_first:
  list3.append(finnhub_client.recommendation_trends(c))
list3=pd.DataFrame(list3)  
list3.to_excel('ART first.xlsx')

list4=[]
trend_second=ticker.iloc[61:100,0]
for d in trend_second:
  list4.append(finnhub_client.recommendation_trends(d))
list4=pd.DataFrame(list4)  
list4.to_excel('ART second.xlsx')





#3.Metrics
import json
ticker=pd.read_csv('/Users/peterguo/Desktop/Bitcoin 1/output_next2.csv')
list1=[]
first=ticker.iloc[0:60,0]
for a in first:
  list1.append(finnhub_client.company_basic_financials(symbol=a, metric=all))
  
b=list(range(60)) 
lista=[]
for c in b:
  lista.append(list1[c]['series']['annual'])

json_data=json.dumps(lista)
text = json.loads(json_data)
list=pd.DataFrame(text)
list.to_excel('firstmetric.xlsx')

list2=[]
second=ticker.iloc[61:100,0]
for a in second:
  list2.append(finnhub_client.company_basic_financials(symbol=a, metric=all))

c=list(range(37)) 
listb=[]
for d in c:
  listb.append(list2[d]['series']['annual'])
json_data2=json.dumps(listb)
text2 = json.loads(json_data2)
list2=pd.DataFrame(text2)
list2.to_excel('secondmetric.xlsx')






from yahoo_fin.stock_info import get_data
ticker_list = ["amzn", "aapl", "ba"]
historical_datas = {}
for ticker in ticker_list:
    historical_datas[ticker] = get_data(ticker)

historical_datas["aapl"]

amazon_weekly= get_data("AE", start_date="12/30/2022", end_date="01/01/2023",  interval="1d")
amazon_weekly



# price
#GTS
ticker=pd.read_csv('/Users/peterguo/Desktop/Bitcoin 1/All_final version.csv')
list1=[]
first=ticker.iloc[0:36,11]
for ac in first:
  list1.append(get_data(ac, start_date="11/10/2022", end_date="01/01/2023",  interval="1d"))
list1=np.squeeze(list1)
list1=pd.DataFrame(list1)
list1.to_excel('price_first.xlsx')


#MNRL
list2=[]
second=ticker.iloc[37:48,11]
for be in second:
  list2.append(get_data(be, start_date="11/10/2022", end_date="01/01/2023",  interval="1d"))
list2=np.squeeze(list2)
list2=pd.DataFrame(list2)
list2.to_excel('price_second.xlsx')

#TWTR
list3=[]
third=ticker.iloc[49:85,11]
for qw in third:
  list3.append(get_data(qw, start_date="12/30/2022", end_date="01/01/2023",  interval="1d"))
list3=np.squeeze(list3)
list3=pd.DataFrame(list3)
list3.to_excel('price_third.xlsx')

list4=[]
fourth=ticker.iloc[86:96,11]
for er in fourth:
  list4.append(get_data(er, start_date="12/30/2022", end_date="01/01/2023",  interval="1d"))
list4=np.squeeze(list4)
list4=pd.DataFrame(list4)
list4.to_excel('price_fourth.xlsx')


get_data("AE", start_date="11/10/2022", end_date="01/01/2023",  interval="1d")



GTS,MNRL,TWTR


#4.Period Price
import yfinance as yf
from yahoo_fin.stock_info import get_data
ticker=pd.read_csv('/Users/peterguo/Desktop/Bitcoin 1/All_final version.csv')
perioda={}
firsta=ticker.iloc[0:36,11]
for c in firsta:
  perioda[c]=(get_data(c, start_date="01/01/2021", end_date="01/01/2022",  interval="1d"))
combined_statsa = pd.concat(perioda)
combined_statsa = combined_statsa.reset_index()
combined_statsa.to_excel('period_price_first.xlsx')

periodb={}
secondb=ticker.iloc[37:48,11]
for d in secondb:
  periodb[d]=(get_data(d, start_date="01/01/2021", end_date="01/01/2022",  interval="1d"))
combined_statsb = pd.concat(periodb)
combined_statsb = combined_statsb.reset_index()
combined_statsb.to_excel('period_price_second.xlsx')

periodc={}
thirdc=ticker.iloc[49:85,11]
for c in thirdc:
  periodc[c]=(get_data(c, start_date="01/01/2021", end_date="01/01/2022",  interval="1d"))
combined_statsc = pd.concat(periodc)
combined_statsc = combined_statsc.reset_index()
combined_statsc.to_excel('period_price_third.xlsx')

periodd={}
fourthd=ticker.iloc[86:97,11]
for d in fourthd:
  periodd[d]=(get_data(d, start_date="01/01/2021", end_date="01/01/2022",  interval="1d"))
combined_statsd = pd.concat(periodd)
combined_statsd = combined_statsd.reset_index()
combined_statsd.to_excel('period_price_fourth.xlsx')

#5. marketcapitalization
ticker=pd.read_csv('/Users/peterguo/Desktop/Bitcoin 1/94 Tickers.csv')
mkticker=ticker["ticker"]
market_cap = {}
for aze in mkticker:
  stock = yf.Ticker(aze)
  data = yf.download(aze, start="2021-12-31", end="2022-01-01")
  closing_price = data.at['2021-12-31','Close']
  shares_outstanding = stock.info["sharesOutstanding"]
  market_cap[aze] = closing_price * shares_outstanding
market_cap = pd.DataFrame.from_dict(market_cap, orient='index', columns=['Market Capitalization'])
market_cap.to_excel('marketcapitalization for 94 tickers.xlsx')





stock = yf.Ticker("KBNT")
data = yf.download("KBNT", start="2021-12-31", end="2022-01-01")
closing_price = data.at['2021-12-31','Close']
shares_outstanding = stock.info["sharesOutstanding"]
market_cap["KBNT"] = closing_price * shares_outstanding
market_cap["KBNT"]


