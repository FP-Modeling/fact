import matplotlib.pyplot as plt 
import sys

xs = []
y1s = []
y2s = []
with open(sys.argv[1]) as file:
    for line in file:
        l = line.split(", ")
        values = l[1].split(' ')
        xs.append(float(l[0]))
        y1s.append(float(values[0]))
        y2s.append(float(values[1]))

plt.plot(xs, y1s)
plt.plot(xs, y2s)
        
plt.show()
