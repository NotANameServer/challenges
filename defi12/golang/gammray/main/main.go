package main

import (
	"sudoku_nan"
	"regexp"
	"time"
	"fmt"
	"os"
)


func main() {
	if len(os.Args[1:]) == 0 {
		fmt.Println("Usage: ", os.Args[0], " <grid>")
		fmt.Println("Ex: ", os.Args[0], " 096040001100060004504810390007950043030080000405023018010630059059070830003590007")
		os.Exit(1)
	}
	data := os.Args[1:][0]
	if !ValidArgs(data) {
		fmt.Println("The grid provided must be composed of numbers from 0 to 9 and must be 81 chars long.")
		os.Exit(2)
	}
	
	board := StringToBoard(data)
	results := [][9][9]int{}
	t1 := time.Now()
	Solve(board, &results)
	fmt.Println("Time spent: ", time.Since(t1))
	if (len(results) == 1 && sudoku_nan.SudokuChecker(results[0])) {
		sudoku_nan.PrintBoard(results[0])
	} else {
		fmt.Println("The board could not be solved or contains multiple solutions.")
	}
}

func ValidArgs(seq string) bool {
	reg, _ := regexp.Match(`[0-9]{81}`, []byte(seq))
	return len(seq) == 81 && reg
}

func StringToBoard(args string) [9][9]int{
	res := [9][9]int{}
	for i, value := range args {
		res[i/9][i%9] = int(value - 48)
	}
	return res
}

func NextEmpty(board [9][9]int) (int, int) {
	for y := 0; y < 9; y++ {
		for x := 0; x < 9; x++ {
			if board[y][x] == 0 {
				return x, y
			}
		}
	}
	return -1, -1
}

func Solve(currentBoard [9][9]int, validBoards *[][9][9]int) {
	if (len(*validBoards) > 1) {
		return
	}
	targetX, targetY := NextEmpty(currentBoard)
	if targetX == -1 && targetY == -1 {
		*validBoards = append(*validBoards, currentBoard)
	} else {
		for testValue := 1; testValue <= 9; testValue++ {
			if sudoku_nan.IsValid(currentBoard, targetX, targetY, testValue) {
				currentBoard[targetY][targetX] = testValue
				Solve(currentBoard, validBoards)
			}
		}
	}
}


