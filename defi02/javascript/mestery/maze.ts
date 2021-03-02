import { Renderer, RendererConstructor, TextRenderer } from './renderer'

export const enum CellType {
  EMPTY,
  WALL,
  ENTRY_POINT,
  // EXIT,
}
export class Cell {
  // public visited = false;
  public constructor(public type: CellType) {}

  public setType(type: CellType): this {
    this.type = type;
    return this;
  }

  /* public setVisited(visited = true): this {
    this.visited = visited;
    return this;
  } */
}

function shuffle<T extends unknown[]>(array: T): T  {
  for (let i = array.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [array[i], array[j]] = [array[j], array[i]];
  }
  return array;
}

function average(a: number, b: number): number {
  return Math.floor((a + b) / 2);
}

export class Maze<R extends Renderer = TextRenderer> {
  private readonly $grid;
  private readonly renderer: R;
  public render: R['render'];
  public toString: R['toString'];
  public readonly height: number;
  public readonly width: number;

  public constructor(width: number, height: number, renderer: RendererConstructor = TextRenderer) {
    if (this.width < 5 || this.height < 5) {
      throw new RangeError('width and height must be >= 5.');
    }
    if (this.width % 2 === 0 || this.height % 2 === 0) {
      throw new Error('width and height must be odd.');
    }
    this.width = width;
    this.height = height;
    this.$grid = this.generate();
    this.renderer = new renderer(this.grid) as R;
    this.render = this.renderer.render;
    this.toString = this.renderer.toString;
  }

  private *neighbours(seen: Set<string>, [x, y]: [number, number]) {
    for (const [xp, yp] of shuffle([[x - 2, y], [x, y - 2], [x + 2, y], [x, y + 2]])) {
      if (1 <= xp && xp < this.width - 1 && 1 <= yp && yp < this.height - 1 && !seen.has([xp, yp].toString())) {
        yield [xp, yp];
      }
    }
  }

  private walk(grid: Cell[][], seen: Set<string>, [x, y]: [number, number]): void {
    seen.add([x, y].toString());
    for (const [nx, ny] of this.neighbours(seen, [x, y])) {
      grid[average(y, ny)][average(x, nx)].type = CellType.EMPTY;
      grid[ny][nx].type = CellType.EMPTY;
      this.walk(grid, seen, [nx, ny]);
    }
  }

  private generate(): Cell[][] {
    const grid = Array.from({ length: this.height }, () => Array.from({ length: this.width }, () => new Cell(CellType.WALL)));
    const seen = new Set<string>();

    this.walk(grid, seen, [this.width, this.height - 2]);
    grid[1][1].type = CellType.ENTRY_POINT;

    return grid;
  }

  public get grid(): CellType[][] {
    return this.$grid.map((row) => row.map((cell) => cell.type));
  }

  public toJSON(): CellType[][] {
    return this.grid;
  }
}
