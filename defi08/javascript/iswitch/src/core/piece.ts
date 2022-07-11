import { GAME_PROPERTIES } from '../data';
import { PlayerNumber } from '../enums';

export class Piece {
  private _row: number;
  private _column: number;
  private _playerNumber: PlayerNumber | null;

  public get row(): number {
    return this._row;
  }

  public get column(): number {
    return this._column;
  }

  public get playerNumber(): PlayerNumber | null {
    return this._playerNumber;
  }

  public set playerNumber(playerNumber: PlayerNumber | null) {
    this._playerNumber = playerNumber;
  }

  constructor(
    row: number,
    column: number,
    playerNumber: PlayerNumber | null = null
  ) {
    this._row = row;
    this._column = column;
    this._playerNumber = playerNumber;
  }

  public comboBoundary(): {
    minRow: number,
    maxRow: number,
    minColumn: number,
    maxColumn: number
  } {
    const minRowLimit: number = 0;
    const maxRowLimit: number = GAME_PROPERTIES.rows - 1;
    const minColumnLimit: number = 0;
    const maxColumnLimit: number = GAME_PROPERTIES.columns - 1;
    let minRow: number = this.row - GAME_PROPERTIES.pieceStreakWinCondition + 1;
    let minColumn: number = this.column - GAME_PROPERTIES.pieceStreakWinCondition + 1;
    let maxRow: number = this.row + GAME_PROPERTIES.pieceStreakWinCondition - 1;
    let maxColumn: number = this.column + GAME_PROPERTIES.pieceStreakWinCondition - 1;
    minRow = Math.max(minRow, minRowLimit);
    minColumn = Math.max(minColumn, minColumnLimit);
    maxRow = Math.min(maxRow, maxRowLimit);
    maxColumn = Math.min(maxColumn, maxColumnLimit);
    return {
      minRow,
      maxRow,
      minColumn,
      maxColumn
    };
  }

  public comboPotential(): number {
    const {
      minRow,
      maxRow,
      minColumn,
      maxColumn
    } = this.comboBoundary();
    const verticalPotential: number = maxRow - minRow;
    const horizontalPotential: number = maxColumn - minColumn;
    const diagonalPotential: number = Math.min(maxRow - minRow, maxColumn - minColumn);
    const antiDiagonalPotential: number = Math.min(maxRow - this.row, this.column - minColumn) + Math.min(this.row - minRow, maxColumn - this.column);
    return verticalPotential + horizontalPotential + diagonalPotential + antiDiagonalPotential;
  }
}
