import threading
import time
import sys

from datetime import datetime  as dt
from datetime import timedelta as td


timeOut_bol = None
answer = None
	
def userInp(prompt):
		while True:
			answer = input(prompt)
			if answer == "p" or timeOut_bol:
				break
			#else: 
			#	print(answer)
	
def subcycle(dur, message):
		t0 = dt.now()
		ti = dt.now()
		while ti < t0 + td(seconds=dur):
			ti = dt.now()
		print(message)

def fullCycle(foo):
		
	subcycle(5, "\n c1")
	print(foo)
	subcycle(2, "\n b1")
	print(foo)
	subcycle(5, "\n c2")	
	subcycle(2, "\n b2")
	
		
		
		
print("Start!")	

# user input thread
thread1 = threading.Thread(target=userInp, args=("Shall we pause?",))

# first subcycle thread
thread2 = threading.Thread(target=fullCycle, args=(answer,))



thread2.start()

thread1.daemon = True
thread1.start()


#thread1.stop()