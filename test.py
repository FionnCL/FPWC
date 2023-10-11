x = 1
for i in range(1001):
    if i % 341 == 0:
        x = x * i
        print(i)

print(x)