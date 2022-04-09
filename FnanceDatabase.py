import sqlalchemy
import pandas as pd
import yfinance as yf

# Getting Data
wiki = 'https://en.wikipedia.org/wiki/'
ticker = pd.read_html(wiki+'Dow_Jones_Industrial_Average')[1].Symbol.to_list()

def getdata(tickers):
  data = []
  for ticker in tickers:
    data.append(yf.download(ticker).reset_index())
  return data

# Using Sqlite engine. Using mysql or other requires a schema
def createengine(name):
  engine = sqlalchemy.create_engine('sqlite:///'+name)
  return engine

#Convert to db
def TOSQL(frames,symbols,engine):
  for frame,symbol in zip(frames,symbols):
    frame.to_sql(symbol, engine, index = False)
  print('Done')

# Program
dow = getdata(ticker)
DOWengine = createengine('DOW.db')
TOSQL(dow,ticker,DOWengine)