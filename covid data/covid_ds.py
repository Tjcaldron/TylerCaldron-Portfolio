import pandas as pd
import numpy as np
import matplotlib.pyplot as plot


file_path = 'archive/usa_county_wise.csv'
df = pd.read_csv(file_path)

df.plot(x="Date", y="Confirmed", kind="line", title="US Confirmed COVID-19 Cases")
plot.show()