const WALL = '#'
const EMPTY = ' '
const START = '.'

class Maze {
    constructor(width, height) {
        this.width = width
        this.height = height
        this.grid = Array.from({ length: height }, () => Array(width).fill(WALL))
        this.end = this.randomEnd()
        this.previousPositions = []
        this.previousStuckPositions = []
        this.genPath()
    }

    randomEnd() {
        const x = Math.floor(Math.random() * this.width)
        const y = Math.floor(Math.random() * this.height)
        if (!this.isBorder({ x, y }) || this.isCorner({ x, y })) {
            return this.randomEnd()
        }
        return { x, y }
    }

    isBorder({ x, y }) {
        return y === 0 ||
            x === 0 ||
            y === this.height - 1 ||
            x === this.width - 1 ?
            true :
            false
    }

    isCorner({ x, y }) {
        return x === 0 && y === 0 ||
            x === 0 && y === this.height - 1 ||
            x === this.width - 1 && y === 0 ||
            x === this.width - 1 && y === this.height - 1
    }

    genPath() {
        let { x, y } = this.end
        this.grid[y][x] = EMPTY

        if (x === 0) x += 1
        else if (y === 0) y += 1
        else if (x === this.width - 1) x -= 1
        else if (y === this.height - 1) y -= 1

        this.grid[y][x] = EMPTY

        this.position = { x, y }
        this.nextPosition()
    }

    nextPosition() {
        if (!this.position) {
            this.addStartPoint()
            return
        }
        let { x, y } = this.position

        const sideCells = this.sideCells({ x, y })
        const availablePositions = this.availablePositions(sideCells)
        if (availablePositions.length === 0) {
            this.previousStuckPositions.push({ x, y })
            this.position = this.previousPositions.pop()
            return this.nextPosition()
        }
        this.previousPositions.push({ x, y })
        this.position = this.randomPosition(availablePositions)
        this.grid[this.position.y][this.position.x] = EMPTY
        this.nextPosition()
    }

    sideCells({ x, y }) {
        const left = { x: x - 1, y: y }
        const up = { x: x, y: y - 1 }
        const right = { x: x + 1, y: y }
        const down = { x: x, y: y + 1 }

        return [left, up, right, down]
    }

    availablePositions(positions) {
        const availablePositions = positions.filter(pos => {
            return !this.isSameAsPreviousPosition(pos) && !this.isBorder(pos)
        })
        return availablePositions.filter(position => {
            return this.sideCells(position)
                .filter(pos => pos.x !== this.position.x || pos.y !== this.position.y)
                .every(pos => !this.isSameAsPreviousPosition(pos))
        })
    }

    isSameAsPreviousPosition({ x, y }) {
        return this.previousPositions.some(pos => x === pos.x && y === pos.y) ||
        this.previousStuckPositions.some(pos => x === pos.x && y === pos.y)
    }

    randomPosition(positions) {
        const randomIndex = Math.floor(Math.random() * positions.length)
        return positions[randomIndex]
    }

    addStartPoint() {
        let axe = 'horizontal'
        let { x, y } = this.opposite(this.end)

        if (x === 0) x += 1, axe = 'vertical'
        else if (y === 0) y += 1
        else if (x === this.width - 1) x -= 1, axe = 'vertical'
        else if (y === this.height - 1) y -= 1

        while(!this.isAvailable({ x, y })) {
            if (axe === 'horizontal') {
                x = x >= this.width / 2 ? x - 1 : x + 1
            } else {
                y = y >= this.height / 2 ? y - 1 : y + 1
            }
        }
        this.grid[y][x] = START
    }

    opposite({ x, y }) {
        return { x: this.width - 1 - x, y: this.height - 1 - y}
    }

    isAvailable({ x, y }) {
        return this.grid[y][x] === EMPTY
    }

    render() {
        return this.grid.map(row => `${row.join('')}\n`).join('')
    }
}

const maze = new Maze(30, 8)
console.log(maze.render())
