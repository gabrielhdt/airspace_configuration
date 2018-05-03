import sys
import matplotlib.pyplot as plt

filename = sys.argv[1]
opt = float(sys.argv[2])

x = []
y = []

def mean(l):
    sum = 0
    for i in l:
        sum += i
    return sum / len(l)

with open(filename, 'r') as f:
    for line in f:
        l = line.split()
        x.append(float(l[0]))
        y.append(mean(list(map(float, l[1:]))))

for i, j in zip(x, y):
    print(i, ' ', j)

plt.plot(x, y)
plt.plot([min(x), max(x)], [opt, opt])
plt.show()
