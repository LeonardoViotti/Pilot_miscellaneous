import cv2
import numpy as np
import os
from utils import *

def stable_show(img, t = 5000):
    cv2.startWindowThread()
    cv2.namedWindow("preview")
    cv2.imshow('preview', img)
    cv2.waitKey(t)
    cv2.destroyAllWindows()

# Load video
path = 'C:/Users/wb519128/Videos/Captures/'

# # Frist frame
# sucess, img_1 = cap.read()
# stable_show(img_1)

# # Crop first frame
# stable_show(img_1[400:600,650:850])

out = cv2.VideoWriter(path + 'out.avi',
                      cv2.VideoWriter_fourcc('M','J','P','G'), 
                      10, 
                      (200,200))


cap = cv2.VideoCapture(path + 'Skype 2020-09-11 10-00-00.mp4')
while True:
    sucess, img_i = cap.read()
    crop = img_i[400:600,650:850]
    # if sucess:
    #     out.write(crop)
    cv2.imshow("Video", crop)
    # Break out by pressing 'q'
    if cv2.waitKey(1) & 0xFF == ord('q'):
        break

# When everything done, release the video capture and video write objects
cap.release()
out.release()

# Closes all the frames
cv2.destroyAllWindows() 

