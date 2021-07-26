import { Board, Player } from '.';
import { GameAlgorithm } from '../algorithms';
import { PlayerNumber } from '../enums';

export class ArtificialIntelligence<T extends GameAlgorithm> extends Player {
  protected _AlgorithmType: new () => T;

  constructor(playerNumber: PlayerNumber, AlgorithmType: new () => T) {
    super(playerNumber);
    this._isAI = true;
    this._AlgorithmType = AlgorithmType;
  }

  protected _decideMove(
    board: Board,
    selectedRow?: number,
    selectedColumn?: number
  ): { row: number; column: number } {
    return new this._AlgorithmType().determineBestMove(board, this._playerNumber);
  }
}
