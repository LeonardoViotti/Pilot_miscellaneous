
#### Import Libraries
import time
import sys

from datetime import datetime  as dt
from datetime import timedelta as td

print("Start working!")

pause_gb = ''

def subcycle(dur, message):
		t0 = dt.now()
		ti = dt.now()
		while ti < t0 + td(seconds=dur):
			ti = dt.now()
			while True:
				pause_gb = input("type 'pause'")
				if ti == t0 + td(seconds=dur):
					break 
		print(message)
		print(pause_gb)

		
subcycle(2, "time's up")