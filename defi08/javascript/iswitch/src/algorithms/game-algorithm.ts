import { Board } from '../core';
import { PlayerNumber } from '../enums';

export abstract class GameAlgorithm {
  public abstract determineBestMove(
    board: Board,
    player: PlayerNumber
  ): { row: number; column: number };
}
