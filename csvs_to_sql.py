import os
from glob import glob
import sqlite3
import pandas as pd
  
conn = sqlite3.connect('data/SQLData.db')

for csv in glob(os.path.join(os.getcwd(), "data/checkouts/*")):
    print(csv.split("\\")[-1])
    checkouts_data = pd.read_csv(csv, dtype = {'UsageClass': 'str', 
                                               'CheckoutType': 'str',
                                               'MaterialType': 'str',
                                               'CheckoutYear': 'int',
                                               'CheckoutMonth': 'int',
                                               'Checkouts': 'int',
                                               'Title': 'str',
                                               'ISBN': 'str',
                                               'Creator': 'str',
                                               'Subjects': 'str',
                                               'Publisher': 'str',
                                               'PublicationYear': 'str'})
    checkouts_data.to_sql('checkouts', conn, if_exists='append', index=False)

conn.close()