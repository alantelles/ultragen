def t():
    print('my_func')
l = [0, 5, 8]
a_dict = {0: 'item1', 1: 'item2', 'func': t, 'list': l}
for e in a_dict:
    print(e,':', a_dict[e])