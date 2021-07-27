//  .. .. .. .. .. .. ..        TOP                      0 0 0 0 0 0 0  0 0 0 0 0 0 0
//  05 12 19 26 33 40 47                  . . . . . . .  0 0 0 0 0 0 0  0 0 0 0 0 0 0
//  04 11 18 25 32 39 46                  . . . . . . .  0 0 0 0 0 0 0  0 0 0 0 0 0 0
//  03 10 17 24 31 38 45                  . . . . . . .  0 0 0 0 0 0 0  0 0 0 0 0 0 0
//  02 09 16 23 30 37 44                  . . . X . . .  0 0 0 1 0 0 0  0 0 0 0 0 0 0
//  01 08 15 22 29 36 43                  . . . X . . .  0 0 0 1 0 0 0  0 0 0 0 0 0 0
//  00 07 14 21 28 35 42      BOTTOM      . . O X O O .  0 0 0 1 0 0 0  0 0 1 0 1 1 0

//  X: 0000000 0000000 0000000 1110000 0000000 0000000 0000000  234881024
//  O: 0000000 1000000 1000000 0000000 1000000 0000000 0000000  2216204173312

export const WIDTH = 7n
export const HEIGHT = 6n

const HEIGHT1 = HEIGHT + 1n
const BOTTOM = ((1n << (HEIGHT1 * WIDTH)) - 1n) / ((1n << HEIGHT1) - 1n)
const TOP = BOTTOM << HEIGHT
const FULL = ((1n << (WIDTH + WIDTH * HEIGHT)) - 1n) ^ TOP

const DIRECTIONS = [1n, 7n, 6n, 8n]

const bitboard = [0n, 0n]
export const heights = Array.from({ length: Number(WIDTH) }, (_, i) => BigInt(i) * WIDTH)
export let player = 0

export function isFull() {
  return (bitboard[0] | bitboard[1]) === FULL
}

export function hasWon() {
  const target = bitboard[1 - player]
  for (const dir of DIRECTIONS) {
    const bb = target & (target >> dir)
    if (bb & (bb >> (2n * dir))) return true
  }
}

function willOponentNotWin() {
  for (const move of getMoves()) {
    makeMove(move)
    if (hasWon()) return undoMove(move)
    undoMove(move)
  }
  return true
}

export function makeMove(column) {
  bitboard[player] ^= 1n << heights[column]++
  player = 1 - player
}

function undoMove(column) {
  player = 1 - player
  bitboard[player] ^= 1n << --heights[column]
}

function evaluate() {
  let score = 0
  const target = bitboard[1 - player]
  for (const dir of DIRECTIONS) {
    if (target & (target >> dir)) score += 20
    if (target & (target >> dir) & (target >> (2n * dir))) score += 50
  }
  return score
}

export function* getMoves() {
  for (let i = 0; i < WIDTH; i++) {
    if (heights[i] < BigInt(i) * WIDTH + HEIGHT) yield i
  }
}

export function getNextMove() {
  let bestMove
  let bestScore = -1

  for (const move of getMoves()) {
    makeMove(move)
    if (hasWon()) {
      // if the move is winning, directly return it
      undoMove(move)
      return move
    } else if (willOponentNotWin()) {
      const score = evaluate()
      if (score > bestScore) {
        // if the move is better than the current best, make it the best
        bestMove = move
        bestScore = score
      }
    } else if (bestMove === undefined) {
      bestMove = move
    }
    undoMove(move)
  }

  return bestMove
}
