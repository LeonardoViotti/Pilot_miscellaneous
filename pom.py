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

from datetime import datetime  as dt
from datetime import timedelta as td

#### System settings
sys.stdout.flush()

#----------------------------------------------------------------------------#
#### Globals
#----------------------------------------------------------------------------#

w_cycle_dur = 3
b_cycle_dur = 1

time_0 = dt.now()
time_i = dt.now()


#----------------------------------------------------------------------------#
#### Work cycle
#----------------------------------------------------------------------------#

start_gb = input('Shall we start working? (Y/n) ')
pause_gb = ''

if start_gb == 'Y':
	print("Start working!")

	for i in range(0,3):
		# Work cycle
		time_0 = dt.now()
		time_i = dt.now()
		while time_i < time_0 + td(seconds=w_cycle_dur):
			#print(timei)
			time_i = dt.now()
			#pause_gb = input('Type 'pause' if you want to pause')
		print("Work cycle finished! Have a break.")
		
		# Break cycle
		#time.sleep(1)
		time_0 = dt.now()
		time_i = dt.now()
		while time_i < time_0 + td(seconds=b_cycle_dur):
			#print(timei)
			time_i = dt.now()
		print("Break finished. Go back to work!")

		time.sleep(1)

	# Last work cycle before long break
	time_0 = dt.now()
	time_i = dt.now()	
	while time_i < time_0 + td(seconds=w_cycle_dur):
		#print(timei)
		time_i = dt.now()
	print("Last work cycle finished! Long break now.")

	# Long break!
	time_0 = dt.now()
	time_i = dt.now()	
	while time_i < time_0 + td(seconds= 4*b_cycle_dur):
		#print(timei)
		time_i = dt.now()
	print("Full cycle finished!")

else:
	print("Ok, have fun!")
	time.sleep(2)
