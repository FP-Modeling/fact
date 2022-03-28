import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D  
import sys

xs = []
ys = []
zs = []
with open(sys.argv[1]) as file:
    for line in file:
        l = line.split(", ")
        values = l[1].split(' ')
        xs.append(float(values[0]))
        ys.append(float(values[1]))
        zs.append(float(values[2]))

print(xs)
print(ys)
print(zs)
fig = plt.figure()
ax = Axes3D(fig)
ax.plot(xs, ys, zs)
plt.show()
