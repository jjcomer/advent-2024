import scipy

def parse(raw: str):
    for line in raw.splitlines():
        _, *buttons_str_list, joltage_str = line.split(' ')
        buttons = []
        for button_str in buttons_str_list:
            buttons.append(list(map(int, button_str.strip('()').split(','))))
        joltages = list(map(int, joltage_str.strip('{}').split(',')))
        yield buttons, joltages

def transpose(grid):
    grid = list(grid)
    return list(map(list, zip(*grid)))

def cost(joltages, buttons_index):
    buttons = [[int(i in b) for i in range(len(joltages))] for b in buttons_index]
    c = [1] * len(buttons)
    A_eq = transpose(buttons)

    result = scipy.optimize.linprog(c=c, A_eq=A_eq, b_eq=joltages, integrality=1)
    return result.fun

raw = open('input/2025/10.txt').read()
print(raw)
result = sum(cost(joltages, buttons) for buttons, joltages in parse(raw))
print(result)