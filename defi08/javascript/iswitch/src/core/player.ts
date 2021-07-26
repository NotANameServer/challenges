import { Board } from '.';
import { PlayerNumber } from '../enums';

export class Player {
  protected _playerNumber: PlayerNumber;
  protected _isAI: boolean;

  public get playerNumber(): PlayerNumber {
    return this._playerNumber;
  }

  public get isAI(): boolean {
    return this._isAI;
  }

  constructor(playerNumber: PlayerNumber) {
    this._playerNumber = playerNumber;
    this._isAI = false;
  }

  public play(
    board: Board,
    selectedRow?: number,
    selectedColumn?: number
  ): { row: number; column: number } {
    return this._decideMove(board, selectedRow, selectedColumn);
  }

  protected _decideMove(
    board: Board,
    selectedRow?: number,
    selectedColumn?: number
  ): { row: number; column: number } {
    return {
      row: selectedRow!,
      column: selectedColumn!
    };
  }
}
