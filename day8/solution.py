import numpy as np


def get_matrix(file_content: str) -> np.ndarray:
    lines = file_content.split()
    num_arr = map(lambda line: [int(c) for c in line], lines)
    return np.array(list(num_arr))


def get_input(filename: str) -> str:
    content = "";
    with open(filename, 'r') as file:
        content = file.read()
    return content


#inputFile = "test.txt"
inputFile = "input.txt";


def lookout_range(matrix):
    res = []
    for row in matrix:
        arr = []
        memory = dict.fromkeys(range(10), -1)
        for idx, val in enumerate(row):
            blockage = max([memory[x] for x in range(val, 10)])
            arr.append(idx if blockage == -1 else (idx - blockage))
            memory[val] = idx
        res.append(arr)
    return res


def tallest_tree(matrix):
    res = []
    for row in matrix:
        max_val = -1
        arr = []
        for val in row:
            if val > max_val:
                max_val = val
                arr.append(True)
            else:
                arr.append(False)
        res.append(arr)
    return res


def process_matrix(matrix, preprocess_func, func):
    input_matrix = preprocess_func(matrix)
    res_matrix = func(input_matrix)
    return preprocess_func(np.array(res_matrix))


def main():
    input_content = get_input(inputFile)
    matrix = get_matrix(input_content)

    ml = process_matrix(matrix, lambda m: m, tallest_tree)
    mr = process_matrix(matrix, lambda m: np.flip(m), tallest_tree)
    mt = process_matrix(matrix, lambda m: m.T, tallest_tree)
    mb = process_matrix(matrix, lambda m: np.flip(m.T), tallest_tree)

    cumulative = ml + mr + mt + mb
    print(sum(np.reshape(cumulative, -1)))

    # part 2

    ml2 = process_matrix(matrix, lambda m: m, lookout_range)
    mr2 = process_matrix(matrix, lambda m: np.flip(m), lookout_range)
    mt2 = process_matrix(matrix, lambda m: m.T, lookout_range)
    mb2 = process_matrix(matrix, lambda m: np.flip(m.T), lookout_range)

    cumulative2 = ml2 * mr2 * mt2 * mb2
    print(cumulative2)
    print(np.max(cumulative2))


if __name__ == "__main__":
    main()
