class Maze {
  constructor(width, height) {
      this.width = width
      this.height = height
      this.grid = Array.from({ length: height }, () => Array.from({ length: width }, () => '#'))
      this.start = this.randomStart()
      this.end = this.opposite(this.start)
      this.previousPositions = []
      this.previousStuckPositions = []
      this.genPath()
  }

  randomStart() {
      const x = Math.floor(Math.random() * this.width)
      const y = Math.floor(Math.random() * this.height)
      if (!this.isWall({ x, y }) || this.isCorner({ x, y })) {
          return this.randomStart()
      }
      return { x, y }
  }

  isWall({ x, y }) {
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

  opposite({ x, y }) {
      return { x: this.width - 1 - x, y: this.height - 1 - y}
  }

  genPath() {
      // Maze end
      this.grid[this.end.y][this.end.x] = ' '

      let { x, y } = this.start

      if (x === 0) x += 1
      else if (y === 0) y += 1
      else if (x === this.width - 1) x -= 1
      else if (y === this.height - 1) y -= 1

      // Maze start
      this.grid[y][x] = '.'

      this.position = { x, y }
      this.nextPosition()
  }

  nextPosition() {
      if (!this.position) {
          this.checkEndCanBeReached()
          this.render()
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
      this.grid[this.position.y][this.position.x] = ' '
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
          return !this.isSameAsPreviousPosition(pos) && !this.isWall(pos)
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

  // Check if end is blocked, remove wall if needed
  // Won't work in every case
  checkEndCanBeReached() {
      let { x, y } = this.end

      if (x === 0) x += 1
      else if (y === 0) y += 1
      else if (x === this.width - 1) x -= 1
      else if (y === this.height - 1) y -= 1

      if (this.grid[y][x] === '#') {
          this.grid[y][x] = ' '
      }
  }

  render() {
      console.log(this.grid.map(row => `${row.join('')}\n`).join(''))
  }
}

const maze = new Maze(30, 8)