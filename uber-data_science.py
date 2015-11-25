##python code for uber-data_science_challenge

import pandas as pd
import json
import matplotlib.pyplot as plt

path = (r"C:\Users\Nj_neeraj\Documents\Data_Science\uber_data_challenge.json")
text = open(path,'r')
x = text.read()
y =json.loads(x)
data = pd.DataFrame(y)
#print(data.head)
uber= data.dropna() #remove missing values
print(len(uber)) #check the length now
print(uber.dtypes) #datatypes
headers = uber.dtypes.index #column names
print(headers)
coruber = pd.DataFrame.corr(uber)
print(coruber)
plt.matshow(coruber) #correlation plot
print(len(headers))
