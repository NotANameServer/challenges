import { Piece } from '.';
import { GAME_PROPERTIES } from '../data';
import { PlayerNumber } from '../enums';

export class Board {
  private _grid: Piece[][];
  private _moves: Piece[];
  private _isEnded: boolean;
  private _isDraw: boolean;
  private _afterPlay: (moveDone: Piece) => void;
  private _afterEnd: (moveDone: Piece) => void;
  private _possibleMoves: Piece[];

  public get numberOfMoves(): number {
    return this._moves.length;
  }

  public get numberOfTurnsDone(): number {
    return Math.floor(this.numberOfMoves / 2);
  }

  public get playerTurn(): PlayerNumber {
    if (this.numberOfMoves > 0) {
      const lastMove: Piece = this._moves[this.numberOfMoves - 1];
      return lastMove.playerNumber === PlayerNumber.FIRST
        ? PlayerNumber.SECOND
        : PlayerNumber.FIRST;
    } else {
      return PlayerNumber.FIRST;
    }
  }

  public get previousPlayerTurn(): PlayerNumber {
    if (this.numberOfMoves > 0) {
      const lastMove: Piece = this._moves[this.numberOfMoves - 1];
      return lastMove.playerNumber!;
    } else {
      return PlayerNumber.FIRST;
    }
  }

  public get possibleMoves(): Piece[] {
    return this._possibleMoves;
  }

  public get isEnded(): boolean {
    return this._isEnded;
  }

  public get isDraw(): boolean {
    return this._isDraw;
  }

  public get winner(): PlayerNumber | null {
    return this.isEnded && !this.isDraw
      ? this._moves[this.numberOfMoves - 1].playerNumber
      : null;
  }

  public set afterPlay(afterPlay: (moveDone: Piece) => void) {
    this._afterPlay = afterPlay;
  }

  public set afterEnd(afterEnd: (moveDone: Piece) => void) {
    this._afterEnd = afterEnd;
  }

  constructor(
    grid?: Piece[][],
    moves?: Piece[],
    isEnded?: boolean,
    isDraw?: boolean,
    afterPlay?: (moveDone: Piece) => void,
    afterEnd?: (moveDone: Piece) => void,
    possibleMoves?: Piece[]
  ) {
    if (
      grid &&
      moves &&
      isEnded !== undefined &&
      isDraw !== undefined &&
      afterPlay &&
      afterEnd &&
      possibleMoves
    ) {
      this._grid = this._copyGrid(grid);
      this._moves = this._copyMoves(moves);
      this._isEnded = isEnded;
      this._isDraw = isDraw;
      this._afterPlay = afterPlay;
      this._afterEnd = afterEnd;
      this._possibleMoves = possibleMoves;
    } else {
      this._grid = this._buildInitGrid();
      this._moves = [];
      this._isEnded = false;
      this._isDraw = false;
      this._afterPlay = () => { };
      this._afterEnd = () => { };
      this._possibleMoves = this._processPossibleMoves();
    }
  }

  private _constructEmptyGrid(): Piece[][] {
    const result: Piece[][] = [];
    for (let row = 0; row < GAME_PROPERTIES.rows; ++row) {
      result[row] = [];
    }
    return result;
  }

  private _copyGrid(grid: Piece[][]): Piece[][] {
    const result: Piece[][] = this._constructEmptyGrid();
    for (let row = 0; row < GAME_PROPERTIES.rows; ++row) {
      for (let column = 0; column < GAME_PROPERTIES.columns; ++column) {
        const piece: Piece = grid[row][column];
        result[row][column] = new Piece(row, column, piece.playerNumber);
      }
    }
    return result;
  }

  private _copyMoves(moves: Piece[]): Piece[] {
    const result: Piece[] = [];
    moves.forEach(move => {
      result.push(this.getPieceAt(move.row, move.column));
    });
    return result;
  }

  private _buildInitGrid(): Piece[][] {
    const result: Piece[][] = this._constructEmptyGrid();
    for (let row = 0; row < GAME_PROPERTIES.rows; ++row) {
      for (let column = 0; column < GAME_PROPERTIES.columns; ++column) {
        result[row][column] = new Piece(row, column);
      }
    }
    return result;
  }

  private _fullCheckVerticalVictory(): boolean {
    for (let column = 0; column < GAME_PROPERTIES.columns; ++column) {
      let firstPlayerStreak: number = 0;
      let secondPlayerStreak: number = 0;
      for (
        let row = 0;
        row <
        GAME_PROPERTIES.rows - GAME_PROPERTIES.pieceStreakWinCondition + 1;
        ++row
      ) {
        for (
          let offset = 0;
          offset < GAME_PROPERTIES.pieceStreakWinCondition;
          ++offset
        ) {
          const piece: Piece = this.getPieceAt(row + offset, column);
          if (piece.playerNumber === PlayerNumber.FIRST) {
            ++firstPlayerStreak;
          } else if (piece.playerNumber === PlayerNumber.SECOND) {
            ++secondPlayerStreak;
          }
        }
        if (
          firstPlayerStreak === GAME_PROPERTIES.pieceStreakWinCondition ||
          secondPlayerStreak === GAME_PROPERTIES.pieceStreakWinCondition
        ) {
          return true;
        } else {
          firstPlayerStreak = 0;
          secondPlayerStreak = 0;
        }
      }
    }
    return false;
  }

  private _fullCheckHorizontalVictory(): boolean {
    for (let row = 0; row < GAME_PROPERTIES.rows; ++row) {
      let firstPlayerStreak: number = 0;
      let secondPlayerStreak: number = 0;
      for (
        let column = 0;
        column <
        GAME_PROPERTIES.columns - GAME_PROPERTIES.pieceStreakWinCondition + 1;
        ++column
      ) {
        for (
          let offset = 0;
          offset < GAME_PROPERTIES.pieceStreakWinCondition;
          ++offset
        ) {
          const piece: Piece = this.getPieceAt(row, column + offset);
          if (piece.playerNumber === PlayerNumber.FIRST) {
            ++firstPlayerStreak;
          } else if (piece.playerNumber === PlayerNumber.SECOND) {
            ++secondPlayerStreak;
          }
        }
        if (
          firstPlayerStreak === GAME_PROPERTIES.pieceStreakWinCondition ||
          secondPlayerStreak === GAME_PROPERTIES.pieceStreakWinCondition
        ) {
          return true;
        } else {
          firstPlayerStreak = 0;
          secondPlayerStreak = 0;
        }
      }
    }
    return false;
  }

  private _fullCheckAntiDiagonalVictory(): boolean {
    for (
      let column = 0;
      column <
      GAME_PROPERTIES.columns - GAME_PROPERTIES.pieceStreakWinCondition + 1;
      ++column
    ) {
      let firstPlayerStreak: number = 0;
      let secondPlayerStreak: number = 0;
      for (
        let row = 0;
        row <
        GAME_PROPERTIES.rows - GAME_PROPERTIES.pieceStreakWinCondition + 1;
        ++row
      ) {
        for (
          let offset = 0;
          offset < GAME_PROPERTIES.pieceStreakWinCondition;
          ++offset
        ) {
          const piece: Piece = this.getPieceAt(row + offset, column + offset);
          if (piece.playerNumber === PlayerNumber.FIRST) {
            ++firstPlayerStreak;
          } else if (piece.playerNumber === PlayerNumber.SECOND) {
            ++secondPlayerStreak;
          }
        }
        if (
          firstPlayerStreak === GAME_PROPERTIES.pieceStreakWinCondition ||
          secondPlayerStreak === GAME_PROPERTIES.pieceStreakWinCondition
        ) {
          return true;
        } else {
          firstPlayerStreak = 0;
          secondPlayerStreak = 0;
        }
      }
    }
    return false;
  }

  private _fullCheckDiagonalVictory(): boolean {
    for (
      let column = GAME_PROPERTIES.pieceStreakWinCondition - 1;
      column < GAME_PROPERTIES.columns;
      ++column
    ) {
      let firstPlayerStreak: number = 0;
      let secondPlayerStreak: number = 0;
      for (
        let row = 0;
        row <
        GAME_PROPERTIES.rows - GAME_PROPERTIES.pieceStreakWinCondition + 1;
        ++row
      ) {
        for (
          let offset = 0;
          offset < GAME_PROPERTIES.pieceStreakWinCondition;
          ++offset
        ) {
          const piece: Piece = this.getPieceAt(row + offset, column - offset);
          if (piece.playerNumber === PlayerNumber.FIRST) {
            ++firstPlayerStreak;
          } else if (piece.playerNumber === PlayerNumber.SECOND) {
            ++secondPlayerStreak;
          }
        }
        if (
          firstPlayerStreak === GAME_PROPERTIES.pieceStreakWinCondition ||
          secondPlayerStreak === GAME_PROPERTIES.pieceStreakWinCondition
        ) {
          return true;
        } else {
          firstPlayerStreak = 0;
          secondPlayerStreak = 0;
        }
      }
    }
    return false;
  }

  /**
   * @deprecated Prefer "_heuristicCheckVictory" to check
   * after each move. Use this only for full grid naive check.
   */
  private _fullCheckVictory(): boolean {
    return (
      this._fullCheckVerticalVictory() ||
      this._fullCheckHorizontalVictory() ||
      this._fullCheckDiagonalVictory() ||
      this._fullCheckAntiDiagonalVictory()
    );
  }

  private _heuristicCheckVictory(moveDone: Piece): boolean {
    const {
      minRow,
      maxRow,
      minColumn,
      maxColumn
    } = moveDone.comboBoundary();
    // horizontal
    let streak: number = 0;
    for (
      let column = minColumn;
      column <= maxColumn &&
      maxColumn - column + 1 + streak >= GAME_PROPERTIES.pieceStreakWinCondition;
      ++column
    ) {
      const piece: Piece = this.getPieceAt(moveDone.row, column);
      if (piece.playerNumber === moveDone.playerNumber) {
        ++streak;
        if (streak === GAME_PROPERTIES.pieceStreakWinCondition) {
          return true;
        }
      }
      else {
        streak = 0;
      }
    }
    // vertical
    streak = 0;
    for (
      let row = minRow;
      row <= maxRow &&
      maxRow - row + 1 + streak >= GAME_PROPERTIES.pieceStreakWinCondition;
      ++row
    ) {
      const piece: Piece = this.getPieceAt(row, moveDone.column);
      if (piece.playerNumber === moveDone.playerNumber) {
        ++streak;
        if (streak === GAME_PROPERTIES.pieceStreakWinCondition) {
          return true;
        }
      }
      else {
        streak = 0;
      }
    }
    // diagonal
    streak = 0;
    const deltaMinRow: number = moveDone.row - minRow + 1;
    const deltaMinColumn: number = moveDone.column - minColumn + 1;
    let offsetMinRow: number = 0;
    let offsetMinColumn: number = 0;
    if (deltaMinRow > deltaMinColumn) {
      offsetMinRow = deltaMinRow - deltaMinColumn;
    }
    else if (deltaMinColumn > deltaMinRow) {
      offsetMinColumn = deltaMinColumn - deltaMinRow;
    }
    for (
      let row = minRow + offsetMinRow, column = minColumn + offsetMinColumn;
      row <= maxRow && column <= maxColumn
      && Math.min(maxRow - row, maxColumn - column) + 1 + streak >= GAME_PROPERTIES.pieceStreakWinCondition;
      ++row, ++column
    ) {
      const piece: Piece = this.getPieceAt(row, column);
      if (piece.playerNumber === moveDone.playerNumber) {
        ++streak;
        if (streak === GAME_PROPERTIES.pieceStreakWinCondition) {
          return true;
        }
      }
      else {
        streak = 0;
      }
    }
    // anti-diagonal
    streak = 0;
    let offsetMaxRow: number = 0;
    offsetMinColumn = 0;
    const deltaMaxRow: number = maxRow - moveDone.row + 1;
    if (deltaMaxRow > deltaMinColumn) {
      offsetMaxRow = deltaMaxRow - deltaMinColumn;
    }
    else if (deltaMinColumn > deltaMaxRow) {
      offsetMinColumn = deltaMinColumn - deltaMaxRow;
    }
    for (
      let row = maxRow - offsetMaxRow, column = minColumn + offsetMinColumn;
      row >= minRow && column <= maxColumn
      && Math.min(row - minRow, maxColumn - column) + 1 + streak >= GAME_PROPERTIES.pieceStreakWinCondition;
      --row, ++column
    ) {
      const piece: Piece = this.getPieceAt(row, column);
      if (piece.playerNumber === moveDone.playerNumber) {
        ++streak;
        if (streak === GAME_PROPERTIES.pieceStreakWinCondition) {
          return true;
        }
      }
      else {
        streak = 0;
      }
    }
    return false;
  }

  private _checkDraw(): boolean {
    return this._possibleMoves.length === 0;
  }

  private _processPossibleMoves(): Piece[] {
    const possibleMoves: Piece[] = [];
    for (let column = 0; column < GAME_PROPERTIES.columns; ++column) {
      for (let row = GAME_PROPERTIES.rows - 1; row > -1; --row) {
        const piece: Piece = this._grid[row][column];
        if (piece.playerNumber === null) {
          possibleMoves.push(piece);
          break;
        }
      }
    }
    return possibleMoves;
  }

  public copy(withListeners: boolean = false): Board {
    return new Board(
      this._grid,
      this._moves,
      this.isEnded,
      this.isDraw,
      withListeners ? this._afterPlay : () => { },
      withListeners ? this._afterEnd : () => { },
      this._possibleMoves
    );
  }

  public getPieceAt(row: number, column: number): Piece {
    return this._grid[row][column];
  }

  public play(row: number, column: number): void {
    if (!this.isEnded) {
      const piece: Piece = this.getPieceAt(row, column);
      piece.playerNumber = this.playerTurn;
      this._moves.push(piece);
      this._possibleMoves = this._processPossibleMoves();
      if (this._heuristicCheckVictory(piece)) {
        this._isEnded = true;
        this._afterEnd(piece);
      } else if (this._checkDraw()) {
        this._isEnded = true;
        this._isDraw = true;
        this._afterEnd(piece);
      }
      else {
        this._afterPlay(piece);
      }
    }
  }

  public reset(): void {
    this._isEnded = false;
    this._isDraw = false;
    this._moves = [];
    for (let row = 0; row < GAME_PROPERTIES.rows; ++row) {
      for (let column = 0; column < GAME_PROPERTIES.columns; ++column) {
        this._grid[row][column].playerNumber = null;
      }
    }
    this._possibleMoves = this._processPossibleMoves();
  }
}
