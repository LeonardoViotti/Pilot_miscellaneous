
#### Import Libraries
import time
import sys

from datetime import datetime  as dt
from datetime import timedelta as td

print("Start working!")


def subcycle(dur, message):
		t0 = dt.now()
		ti = dt.now()
		while ti < t0 + td(seconds=dur):
			ti = dt.now()
		print(message)

		
subcycle(2, "foo")