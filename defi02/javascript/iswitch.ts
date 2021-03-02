type Point = {
  row: number,
  column: number
};

enum Direction {
  TOP = 0,
  RIGHT = 1,
  BOTTOM = 2,
  LEFT = 3
}

class Maze {

  protected _grid: string[][];
  protected _width: number;
  protected _height: number;
  protected _end: Point;
  protected _start: Point;
  protected _path: Point[];

  constructor(width: number, height: number) {
      this._grid = [];
      this._height = height;
      this._width = width;
      this._init();
      this._buildSurroundings();
      this._start = this._generateStartPoint();
      this._grid[this._start.row][this._start.column] = '.';
      this._end = this._generateEndPoint();
      this._grid[this._end.row][this._end.column] = ' ';
      this._path = this._createSolution();
      this._addInnerWalls();
      this._removeHints();
  }

  public toString(): string {
      let stringRepresentation: string = '';
      for (let row = 0; row < this._height; ++row) {
          stringRepresentation += this._grid[row].join('');
      }
      return stringRepresentation;
  }

  protected _init(): void {
      for (let row = 0; row < this._height; ++row) {
          this._grid[row] = [];
          for (let column = 0; column < this._width; ++ column) {
              this._grid[row][column] = ' ';
          }
          this._grid[row][this._width] = '\n';
      }
  }

  protected _buildSurroundings(): void {
      for (let row = 0; row < this._width; ++row) {
          this._grid[0][row] = '#';
          this._grid[this._height - 1][row] = '#';
      }
      for (let row = 0; row < this._height; ++row) {
          this._grid[row][0] = '#';
          this._grid[row][this._width - 1] = '#';
      }
  }

  protected _generateStartPoint(): Point {
      const start: Point = { row: 0, column: 0 };
      switch (Math.floor(Math.random() * 4) % 4) {
          case Direction.TOP:
              start.row = 1;
              start.column = Math.floor(Math.random() * (this._width - 2)) + 1;
              break;
          case Direction.RIGHT:
              start.row = Math.floor(Math.random() * (this._height - 2)) + 1;
              start.column = this._width - 2;
              break;
          case Direction.BOTTOM:
              start.row = this._height - 2;
              start.column = Math.floor(Math.random() * (this._width - 2)) + 1;
              break;
          case Direction.LEFT:
              start.row = Math.floor(Math.random() * (this._height - 2)) + 1;
              start.column = 1;
              break;
      }
      return start;
  }

  protected _generateEndPoint(): Point {
      const end: Point = { row: 0, column: 0 };
      switch (Math.floor(Math.random() * 4) % 4) {
          case Direction.TOP:
              end.row = 0;
              end.column = Math.floor(Math.random() * (this._width - 2)) + 1;
              break;
          case Direction.RIGHT:
              end.row = Math.floor(Math.random() * (this._height - 2)) + 1;
              end.column = this._width - 1;
              break;
          case Direction.BOTTOM:
              end.row = this._height - 1;
              end.column = Math.floor(Math.random() * (this._width - 2)) + 1;
              break;
          case Direction.LEFT:
              end.row = Math.floor(Math.random() * (this._height - 2)) + 1;
              end.column = 0;
              break;
      }
      return end;
  }

  protected _randomUnitMove(): Point {
      const unitMove: Point = { row: 0, column: 0 };
      switch(Math.floor(Math.random() * 4) % 4) {
          case Direction.TOP:
              unitMove.row = -1;
              unitMove.column = 0;
              break;
          case Direction.RIGHT:
              unitMove.row = 0;
              unitMove.column = 1;
              break;
          case Direction.BOTTOM:
              unitMove.row = 1;
              unitMove.column = 0;
              break;
          case Direction.LEFT:
              unitMove.row = 0;
              unitMove.column = -1;
              break;
      }
      return unitMove;
  }

  protected _createSolution(): Point[] {
      const path: Point[] = [];
      path.push(this._start);
      while (path[path.length - 1].row !== this._end.row || path[path.length - 1].column !== this._end.column) {
          const currentPosition: Point = path[path.length - 1];
          const unitMove: Point = this._randomUnitMove();
          const nextPosition: Point = {
              row: currentPosition.row + unitMove.row,
              column: currentPosition.column + unitMove.column
          };
          if (this._grid[nextPosition.row][nextPosition.column] !== '#' && (Math.sqrt(Math.pow(nextPosition.row - this._end.row, 2) + Math.pow(nextPosition.column - this._end.column, 2)) <= Math.sqrt(Math.pow(currentPosition.row - this._end.row, 2) + Math.pow(currentPosition.column - this._end.column, 2)))) {
              path.push(nextPosition);
              this._grid[nextPosition.row][nextPosition.column] = 'o';
          }
      }
      return path;
  }

  protected _addInnerWalls(): void {
      for (let row = 1; row < this._height - 1; row += 2) {
          for (let column = 1; column < this._width - 1; column += 2) {
              if (this._grid[row][column] === ' ') {
                  this._grid[row][column] = '#';
              }
          }
      }
      for (let row = 1; row < this._height - 1; ++row) {
          for (let column = 1; column < this._width - 1; ++column) {
              if (this._grid[row][column] === ' ' && Math.random() < 0.25) {
                  this._grid[row][column] = '#';
              }
          }
      }
  }

  protected _removeHints(): void {
      for (let row = 0; row < this._height; ++row) {
          for (let column = 0; column < this._width; ++column) {
              if (this._grid[row][column] === 'o') {
                  this._grid[row][column] = ' ';
              }
          }
      }
  }

}

const [width, height] = process.argv.slice(2);
console.log(new Maze(parseInt(width, 10) || 21, parseInt(height, 10) || 11).toString());
