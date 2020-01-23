import pandas as pd
df = pd.read_csv('ambientais_ufms.csv')
df.to_csv('ambientais_ufms_OK.csv', index=False)