import './index.css'
import * as core from './core'

const HEIGHT = Number(core.HEIGHT)
const WIDTH = Number(core.WIDTH)

const COLORS = ['red', 'yellow']

const boardTable = document.getElementById('board')
boardTable.tBodies[0].innerHTML = `<tr>${'<td></td>'.repeat(WIDTH)}</tr>`.repeat(HEIGHT)

const turnIndicator = document.getElementById('turn')
const startButton = document.getElementById('start')
const result = document.getElementById('result')

const CONTROLLERS = [
  () =>
    new Promise((resolve) =>
      boardTable.addEventListener('click', function userInput(event) {
        const column = event.target.cellIndex
        if (column !== undefined && core.heights[column] < HEIGHT + column * WIDTH) {
          boardTable.removeEventListener('click', userInput)
          resolve(column)
        }
      })
    ),
  async () => core.getNextMove(),
  async () => {
    const moves = [...core.getMoves()]
    return moves[Math.floor(Math.random() * moves.length)]
  },
]

function getPlayerController(n) {
  return CONTROLLERS[document.getElementById(`player-${n}`).selectedIndex]
}

startButton.addEventListener(
  'click',
  async () => {
    startButton.classList.replace('px-7', 'px-5')
    startButton.textContent = 'Recommencer'
    startButton.addEventListener('click', () => location.reload())
    turnIndicator.hidden = false
    result.hidden = false

    const players = [getPlayerController(1), getPlayerController(2)]

    // as long as the board is not full and the game is not won by the last player
    while (!core.isFull() && !core.hasWon()) {
      const move = await players[core.player]()
      boardTable.rows[HEIGHT - 1 - Number(core.heights[move]) + move * WIDTH].cells[move].classList.add(
        COLORS[core.player]
      )
      core.makeMove(move)
      turnIndicator.classList.replace(COLORS[1 - core.player], COLORS[core.player])
    }

    turnIndicator.hidden = true
    result.textContent = core.isFull() ? 'Partie nulle !' : `Le joueur ${-core.player + 2} a gagné !`
    result.hidden = false
  },
  { once: true }
)
