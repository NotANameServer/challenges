players = "ox"

def play(board, player, col):
    assert 1 <= col <= 7
    for row in range(6, 0, -1):
        if board[row][col] == ' ':
            board[row][col] = player
            break
    else:
        return player, False

    return players[1 - players.index(player)], is_gameover(board, row, col)

def is_gameover(board, row, col):
    player = board[row][col]

    for arow, acol in [(-1, -1), (-1, 0), (-1, 1), (0, -1)]:
        length = 1
        for sign in (1, -1):
            dist = 1
            while player == board[row + arow * sign * dist][col + acol * sign * dist]:
                dist += 1
            length += dist - 1
        if length >= 4:
            return True

    return False

def main():
    new_player = players[0]
    gameover = False
    board = [
        [' ' for _ in range(7 + 2)]
        for _ in range(6 + 2)
    ]

    while True:
        print()
        print('#', '.'.join("1234567"), '#', sep="")
        for row in board[1:-1]:
            print('#', '|'.join(row[1:-1]), '#', sep="")
        print("###############")

        if gameover:
            break

        player = new_player
        while True:
            col = input(f"{player}, 1-7 ? ")
            if col in set('1234567'):
                new_player, gameover = play(board, player, int(col))
                if new_player != player:
                    break
            print("Wrong input!")

    print(f"Player {player} won!")

if __name__ == '__main__':
    main()
