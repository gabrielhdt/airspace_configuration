import sys
import matplotlib.pyplot as plt

filename = sys.argv[1]
percent = float(sys.argv[2]) / 100

x = []
y = []

def mean(l):
    sum = 0
    for i in l:
        sum += i
    return sum / len(l)

def fivep(x):
    return (1+percent) * x

with open(filename, 'r') as f:
    for line in f:
        l = line.split()
        print(len(l))
        if l[0] == '#' and len(l) == 2 :
            opt = float(l[1])
        else:
            x.append(float(l[0]))
            y.append(mean(list(map(float, l[1:]))))

for i, j in zip(x, y):
    print(i, ' ', j)

plt.plot(x, y)
plt.plot([min(x), max(x)], [opt, opt])
plt.plot([min(x), max(x)], [fivep(opt), fivep(opt)])
plt.show()
