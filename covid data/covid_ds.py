import pandas as pd
import numpy as np
import matplotlib.pyplot as plot


file_path = 'archive/usa_county_wise.csv'
df = pd.read_csv(file_path)
df["Date"] = pd.to_datetime(df["Date"])
df = df.sort_values(by='Date')

daily_totals = df.groupby("Date")[['Confirmed', 'Deaths']].sum().reset_index()

df.plot(x="Date", 
y=["Confirmed", "Deaths"], 
kind="line", 
title="US Confirmed COVID-19 Cases Vs. Deaths")

plot.xlabel("Date")
plot.ylabel("Number of Cases")
plot.grid(True)
plot.legend()
plot.tight_layout()
plot.show()