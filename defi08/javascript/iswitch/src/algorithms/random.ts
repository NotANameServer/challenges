import { GameAlgorithm } from '.';
import { Board } from '../core';
import { PlayerNumber } from '../enums';

export class Random extends GameAlgorithm {
  public determineBestMove(
    board: Board,
    player: PlayerNumber
  ): { row: number; column: number } {
    const randomIndex: number = Math.floor(
      Math.random() * board.possibleMoves.length
    );
    return {
      row: board.possibleMoves[randomIndex].row,
      column: board.possibleMoves[randomIndex].column
    };
  }
}
