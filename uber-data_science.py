##python code for uber-data_science_challenge

import pandas as pd
import json
path = (r"C:\Users\Nj_neeraj\Documents\Data_Science\uber_data_challenge.json")
text = open(path,'r')
x = text.read()
y =json.loads(x)
data = pd.DataFrame(y)
#print(data.head)
