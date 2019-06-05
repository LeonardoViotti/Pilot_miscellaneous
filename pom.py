	#----------------------------------------------------------------------------#
#
# 						Pomodoro timer draft
#
#----------------------------------------------------------------------------#


#----------------------------------------------------------------------------#
#### Settings
#----------------------------------------------------------------------------#

#### Import Libraries
import time
import sys
#import os

from datetime import datetime  as dt
from datetime import timedelta as td

#### System settings
sys.stdout.flush()

#----------------------------------------------------------------------------#
#### Globals
#----------------------------------------------------------------------------#

#### Cycle time
Wcy_dur = 2 # Work
Bcy_dur = 1 # Break
LBcy_dur = 2 # Long break

#### Messages
Wcy_mes1 = "Work cycle finished! Have a break."
Wcy_mes2 = "Last work cycle finished! Long break now."
Bcy_mes = "Break finished. Go back to work!"
LBcy_mes = "Full cycle finished!"

#### Acceptable answers
start_ansY = ["y", "yes", "oui", "sim", "si", "yeah"]
start_ansN = ["n",  "no", "nao", "nop", "not"]

start_ansYN = start_ansY + start_ansN


#### Subcycle function (in seconds replace with minutes)
def subcycle(dur, message):
		t0 = dt.now()
		ti = dt.now()
		while ti < t0 + td(seconds=dur):
			ti = dt.now()
		print(message)
		#os.system("beep -f 555 -l 460")


#----------------------------------------------------------------------------#
#### Work cycle
#----------------------------------------------------------------------------#

start_gb = input('Shall we start working? (Y/n) ').strip().lower()

while start_gb not in start_ansYN:
	print("Sorry, I didn't get that.")
	start_gb = input('Shall we start working now? (Y/n) ')

if start_gb in start_ansY:
	print("Start working!")

	# Repeat work subcycle and break 3x
	for i in range(0,3):
		# Work cycle
		subcycle(Wcy_dur, Wcy_mes1)

		# Break cycle
		subcycle(Bcy_dur, Bcy_mes)


	# Last work cycle before long break
	subcycle(Wcy_dur, Wcy_mes2)

	# Long break!
	subcycle(LBcy_dur, LBcy_mes)

else:
	print("Ok, have fun!")
	time.sleep(1)
