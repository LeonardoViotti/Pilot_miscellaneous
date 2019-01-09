#### Pomodoro timer draft

# import libraries
from datetime import datetime  
from datetime import timedelta  

time0 = datetime.now()
timei = datetime.now()
while timei < time0 + timedelta(seconds=2):
	print(timei)
	timei = datetime.now()