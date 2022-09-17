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

fig = plt.figure()
ax = Axes3D(fig)
ax.plot(xs, ys, zs)
ax.set_xlabel('$x$', fontsize=15)
ax.set_ylabel('$y$', fontsize=15)
ax.set_zlabel('$z$', fontsize=15)
plt.show()

plt.plot(xs, zs)
plt.axis('off')
plt.savefig('output3D.png')
