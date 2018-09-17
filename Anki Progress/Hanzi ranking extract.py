# -*- coding: utf-8 -*-
"""
Created on Tue Aug 28 13:50:38 2018

@author: pierr

Reference:
    https://srome.github.io/Parsing-HTML-Tables-in-Python-with-BeautifulSoup-and-pandas/
"""
from bs4 import BeautifulSoup
import requests
import pandas as pd

class HTMLTableParser:
   
    def parse_url(self, url):
        r = requests.get(url)
        r.raise_for_status()
        r.encoding = "utf-8"
        soup = BeautifulSoup(r.text, 'lxml').find('table')

        n_columns = 0
        n_rows=0
        column_names = []

        # Find number of rows and columns
        # we also find the column titles if we can
        for row in soup.find_all('tr'):
            
            # Determine the number of rows in the table
            td_tags = row.find_all('td')
            if len(td_tags) > 0:
                n_rows+=1
                if n_columns == 0:
                    # Set the number of columns for our table
                    n_columns = len(td_tags)
                    
            # Handle column names if we find them
            th_tags = row.find_all('th') 
            if len(th_tags) > 0 and len(column_names) == 0:
                for th in th_tags:
                    column_names.append(th.get_text())

        # Safeguard on Column Titles
        if len(column_names) > 0 and len(column_names) != n_columns:
            raise Exception("Column titles do not match the number of columns")

        columns = column_names if len(column_names) > 0 else range(0,n_columns)
        df = pd.DataFrame(columns = columns,
                          index= range(0,n_rows))
        row_marker = 0
        for row in soup.find_all('tr'):
            column_marker = 0
            columns = row.find_all('td')
            for column in columns:
                df.iat[row_marker,column_marker] = column.get_text()
                column_marker += 1
            if len(columns) > 0:
                row_marker += 1
                
        # Convert to float if possible
        for col in df:
            try:
                df[col] = df[col].astype(float)
            except ValueError:
                pass
        
        return df

hp = HTMLTableParser()
url = "http://hanzidb.org/character-list/by-frequency?page="

table = hp.parse_url(url+str(1))

for i in range(2, 101, 1):
    urlp = url+str(i)
    t = hp.parse_url(urlp)
    table = table.append(t, ignore_index=True)
    print(urlp)
    
table.info()

table.to_csv("C:/Users/pierr/Desktop/Hanzi.csv")
table.to_json("C:/Users/pierr/Desktop/Hanzi.JSON")