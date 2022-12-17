#inputFile = "test.txt"
#inputFile = "test2.txt"
inputFile = "input.txt";


def get_input(filename):
    content = "";
    with open(filename, 'r') as file:
        content = file.read()
    return content


def get_moves(filename):
    input_content = get_input(filename)
    els = input_content.split()
    directions = els[::2]
    lengths = [int(x) for x in els[1::2]]
    return list(zip(directions, lengths))


def move_head(head_position: (int, int), next_move: (str, int)) -> (int, int):
    match next_move[0]:
        case "U":
            return head_position[0] + next_move[1], head_position[1]
        case "D":
            return head_position[0] - next_move[1], head_position[1]
        case "R":
            return head_position[0], head_position[1] + next_move[1]
        case "L":
            return head_position[0], head_position[1] - next_move[1]


def move_tail_toward_head(tail_position, head_position):
    vertical_diff = head_position[0] - tail_position[0]
    horizontal_diff = head_position[1] - tail_position[1]
    move_v = vertical_diff // abs(vertical_diff) if vertical_diff != 0 else 0
    move_h = horizontal_diff // abs(horizontal_diff) if horizontal_diff != 0 else 0
    return tail_position[0] + move_v, tail_position[1] + move_h


def move_tail(tail_position, head_position) -> [(int, int)]:
    path = [tail_position]

    while get_distance(head_position, tail_position) > 1:
        tail_position = move_tail_toward_head(tail_position, head_position)
        path.append(tail_position)
    return path


def get_head_positions(head_position: (int, int), moves: [(str, int)]) -> [(int, int)]:
    visited_places = [head_position]
    for move in moves:
        head_position = move_head(head_position, move)
        visited_places.append(head_position)
    return visited_places


def process_moves(head_visited_positions: [(int, int)], tail_position: (int, int)) -> [(int, int)]:
    visited_places = []
    for head_position in head_visited_positions:
        # 2 move tail and get positions
        tail_path = move_tail(tail_position, head_position)
        tail_position = tail_path[-1]
        # 3 store positions in set
        visited_places.extend(tail_path)
    return visited_places


def get_unique_places(head_position, tail_position, moves):
    head_visited_places = get_head_positions(head_position, moves)
    visited_places = process_moves(head_visited_places, tail_position)
    unique_places = set(visited_places)
    return len(unique_places)


def get_distance(head_position, tail_position):
    return max(abs(head_position[0] - tail_position[0]), abs(head_position[1] - tail_position[1]));


def get_positions_for_last_tail(head_position: (int, int), tail_position: (int, int), num_of_tails: int, moves: (str, int)) -> [(int, int)]:
    prev_positions = get_head_positions(head_position, moves)
    for tail in range(num_of_tails):
        prev_positions = process_moves(prev_positions, tail_position)
    return prev_positions

def main():
    moves = get_moves(inputFile)
    h = (0, 0)
    t = (0, 0)
    # part 1
    res1 = get_unique_places(h, t, moves)
    print(res1)
    # part 2
    res2 = get_positions_for_last_tail(h, t, 9, moves)
    print(len(set(res2)))


if __name__ == "__main__":
    main()