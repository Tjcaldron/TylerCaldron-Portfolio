import pandas as pd
import numpy as np

s = pd.Series([1, 3, 5, np.nan, 6, 8])
dates = pd.date_range("20130101", periods=6)

df = pd.DataFrame(np.random.randn(6,4), index=dates, columns=list("ABCD"))
#print(df)
df2 = pd.DataFrame(
    {
        "A": 1.0,
        "B": pd.Timestamp("20130102"),
        "C": pd.Series(1, index=list(range(4)), dtype="float32"),
        "D": np.array([3] * 4, dtype="int32"),
        "E": pd.Categorical(["test", "train","test", 'train']),
        "F": "foo",
    }
)
#print(df2.clip)
df.head()
df.tail(3)
#print(df.index)
#print(df.columns)

df["A"]
df[0:3]
df["20130102":"20130104"]
df.loc[dates[0]]
df.loc[:, ["A", "B"]]
df.loc["20130102":"20130104", ["A", "B"]]
df.loc[dates[0], "A"]
df.at[dates[0], "A"]
df.iloc[3]
df.iloc[3:5, 0:2]
df.iloc[[1,2,4], [0, 2]]
#for slicing rows
df.iloc[1:3, :]
#for slicing columns
df.iloc[:, 1:3]
#for getting a value 
df.iloc[1,1]
#for getting fast access to a scalar (equiv to the prior method)
df.iat[1,1]
#Boolean indexing
df[df["A"] > 0]
#selects the values that meet the condition
df[df > 0]
#using isin()
df2 = df.copy()
df2["E"] = ["one", "one", "two", "three", "four", "three"]
df2
df2[df2["E"].isin(["two", "four"])]
#setting
s1 = pd.Series([1, 2, 3, 4, 5, 6], index=pd.date_range("20130102", periods=6))
s1 
df["F"] = s1
#set values by label
df.at[dates[0], "A"] = 0
#set values by position
df.iat[0,1] = 0
#sety by assigning numpy array
df.loc[:, "D"] = np.array([5] * len(df))
df
#where operations
df2 = df.copy()
df2[df2 > 0] = -df2
print(df2)
#missing data
df1 = df.reindex(index=dates[0:4], columns=list(df.columns) + ["E"])
df1.loc[dates[0] : dates[1], "E"] = 1