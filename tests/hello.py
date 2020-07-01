def mult10(a):
    print(a % 100 == 0)

i = 0
limit = 1000

while (True):
    if (mult10(i)):
        print("hey:", i)
    i = i + 1
    if (i > limit):
        break
