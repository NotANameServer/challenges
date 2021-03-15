enum Cell {
    START = '.',
    WALL = '#',
    ROAD = ' ',
    VISITED = 'x',
    SOLUTION = 'o'
}

type Position = {
    row: number,
    column: number
};

type Neighbors = {
    top?: Position,
    left?: Position,
    right?: Position,
    bottom?: Position
};

class Tree {
    protected _root: TreeNode;

    public get root(): TreeNode {
        return this._root;
    }

    constructor(rootValue: Position, rootChilds?: TreeNodeChilds) {
        this._root = new TreeNode(rootValue, undefined, rootChilds);
    }
}

class TreeNode {
    protected _value: Position;
    protected _parent?: TreeNode;
    protected _childs: TreeNodeChilds;

    public get value(): Position {
        return this._value;
    }

    public get childs(): TreeNodeChilds {
        return this._childs;
    }

    public set childs(childs: TreeNodeChilds) {
        this._childs = childs;
    }

    public get parent(): TreeNode | undefined {
        return this._parent;
    }

    constructor (value: Position, parent?: TreeNode, childs?: TreeNodeChilds) {
        this._value = {
            row: value.row,
            column: value.column
        };
        this._parent = parent;
        this._childs = childs || {};
    }

    public isLeaf(): boolean {
        return !this._childs.top && !this._childs.left && !this._childs.right && !this._childs.bottom;
    }

}

type TreeNodeChilds = {
    top?: TreeNode,
    left?: TreeNode,
    right?: TreeNode,
    bottom?: TreeNode
}

class Maze {
    protected _grid: string[][];
    protected _start: Position;
    protected _end: Position;
    protected _solved: boolean;
    protected _solution: Position[];
    protected _width: number;
    protected _height: number;

    constructor(mazeStringRepresentation: string) {
        this._grid = mazeStringRepresentation.split('\n').map((line: string) => line.split(''));
        this._width = this._grid[0].length;
        this._height = this._grid.length;
        this._start = this._findStart()!;
        this._end = this._findEnd()!;
        this._solved = false;
        this._solution = [];
        this._findPath();
        this._removeVisitedMarks();
        this._displaySolutionPath();
    }

    protected _findStart(): Position | null {
        for (let row = 1; row < this._height - 1; ++row) {
            for (let column = 1; column < this._width - 1; ++column) {
                if (this._grid[row][column] === Cell.START) {
                    return {
                        row,
                        column
                    };
                }
            }
        }
        return null;
    }

    protected _findEnd(): Position | null {
        for (let column = 0; column < this._width; ++column) {
            if (this._grid[0][column] === Cell.ROAD) {
                return {
                    row: 0,
                    column
                };
            }
            if (this._grid[this._height - 1][column] === Cell.ROAD) {
                return {
                    row: this._height - 1,
                    column
                };
            }
        }
        for (let row = 0; row < this._height; ++row) {
            if (this._grid[row][0] === Cell.ROAD) {
                return {
                    row,
                    column: 0
                };
            }
            if (this._grid[row][this._width - 1] === Cell.ROAD) {
                return {
                    row,
                    column: this._width - 1
                };
            }
        }
        return null;
    }

    protected _neighborsOf(cell: Position): Neighbors {
        const top: Position = {
            row: cell.row - 1,
            column: cell.column
        };
        const left: Position = {
            row: cell.row,
            column: cell.column - 1
        };
        const right: Position = {
            row: cell.row,
            column: cell.column + 1
        };
        const bottom: Position = {
            row: cell.row + 1,
            column: cell.column
        };
        return {
            top: this._grid[top.row][top.column] === Cell.ROAD ? top : undefined,
            left: this._grid[left.row][left.column] === Cell.ROAD ? left : undefined,
            right: this._grid[right.row][right.column] === Cell.ROAD ? right : undefined,
            bottom: this._grid[bottom.row][bottom.column] === Cell.ROAD ? bottom : undefined
        };
    }

    protected _findPath(): void {
        const tree: Tree = new Tree(this._start);
        this._walk(tree.root);
    }

    protected _walk(currentTreeNode: TreeNode): void {
        if (!this._solved) {
            this._grid[currentTreeNode.value.row][currentTreeNode.value.column] = Cell.VISITED;
            if (currentTreeNode.value.row !== this._end.row || currentTreeNode.value.column !== this._end.column) {
                const neighbors: Neighbors = this._neighborsOf(currentTreeNode.value);
                currentTreeNode.childs = {
                    top: neighbors.top ? new TreeNode(neighbors.top, currentTreeNode) : undefined,
                    left: neighbors.left ? new TreeNode(neighbors.left, currentTreeNode) : undefined,
                    right: neighbors.right ? new TreeNode(neighbors.right, currentTreeNode) : undefined,
                    bottom: neighbors.bottom ? new TreeNode(neighbors.bottom, currentTreeNode) : undefined
                };
                if (currentTreeNode.childs.top) {
                    this._walk(currentTreeNode.childs.top);
                }
                if (currentTreeNode.childs.left) {
                    this._walk(currentTreeNode.childs.left);
                }
                if (currentTreeNode.childs.right) {
                    this._walk(currentTreeNode.childs.right);
                }
                if (currentTreeNode.childs.bottom) {
                    this._walk(currentTreeNode.childs.bottom);
                }
            }
            else {
                this._solved = true;
                this._buildPath(currentTreeNode);
            }
        }
    }

    protected _buildPath(reverseTree?: TreeNode) {
        while (reverseTree) {
            this._solution.push(reverseTree.value);
            reverseTree = reverseTree.parent;
        }
    }

    protected _removeVisitedMarks(): void {
        this._grid.forEach((line: string[], row: number) => {
            line.forEach((character: string, column: number) => {
                if (character === Cell.VISITED) {
                    this._grid[row][column] = Cell.ROAD;
                }
            });
        })
    }

    protected _displaySolutionPath(): void {
        this._solution.forEach((solutionCell: Position) => {
            this._grid[solutionCell.row][solutionCell.column] = Cell.SOLUTION;
        });
        this._grid[this._start.row][this._start.column] = Cell.START;
    }

    public toString(): string {
        return `\n${this._grid.map((line: string[]) => line.join('')).join('\n')}\n`;
    }

}
