import { GameAlgorithm } from '.';
import { Board, Piece } from '../core';
import { GAME_PROPERTIES, MINIMAX_PROPERTIES, POTENTIALS_GRID } from '../data';
import { PlayerNumber } from '../enums';

export class Minimax extends GameAlgorithm {
  private _player: PlayerNumber | null;
  private _bestMove: { row: number, column: number } | null;

  constructor() {
    super();
    this._player = null;
    this._bestMove = null;
  }

  public determineBestMove(
    board: Board,
    playerNumber: PlayerNumber
  ): { row: number; column: number } {
    this._player = playerNumber;
    this._minimaxWithPruning(board.copy(), MINIMAX_PROPERTIES.depth, true, -Infinity, Infinity);
    return this._bestMove!;
  }

  private _minimax(
    board: Board,
    depth: number,
    isMaximizingPlayer: boolean
  ): number {
    if (depth === 0 || board.isEnded) {
      return this._evaluateCurrentGameState(board);
    }
    if (isMaximizingPlayer) {
      let maxEvaluation: number = -Infinity;
      for (const possibleMove of board.possibleMoves) {
        const boardCopy: Board = board.copy();
        boardCopy.play(possibleMove.row, possibleMove.column);
        const evaluation: number = this._minimax(boardCopy, depth - 1, false);
        if (evaluation > maxEvaluation) {
          maxEvaluation = evaluation;
          if (depth === MINIMAX_PROPERTIES.depth) {
            this._bestMove = {
              row: possibleMove.row,
              column: possibleMove.column
            };
          }
        }
      }
      return maxEvaluation;
    } else {
      let minEvaluation: number = Infinity;
      for (const possibleMove of board.possibleMoves) {
        const boardCopy: Board = board.copy();
        boardCopy.play(possibleMove.row, possibleMove.column);
        const evaluation: number = this._minimax(boardCopy, depth - 1, true);
        if (evaluation < minEvaluation) {
          minEvaluation = evaluation;
          if (depth === MINIMAX_PROPERTIES.depth) {
            this._bestMove = {
              row: possibleMove.row,
              column: possibleMove.column
            };
          }
        }
      }
      return minEvaluation;
    }
  }

  private _minimaxWithPruning(
    board: Board,
    depth: number,
    isMaximizingPlayer: boolean,
    alpha: number,
    beta: number
  ): number {
    if (depth === 0 || board.isEnded) {
      return this._evaluateCurrentGameState(board);
    }
    if (isMaximizingPlayer) {
      let maxEvaluation: number = -Infinity;
      for (const possibleMove of board.possibleMoves) {
        const boardCopy: Board = board.copy();
        boardCopy.play(possibleMove.row, possibleMove.column);
        const evaluation: number = this._minimaxWithPruning(boardCopy, depth - 1, false, alpha, beta);
        if (evaluation > maxEvaluation) {
          maxEvaluation = evaluation;
          if (depth === MINIMAX_PROPERTIES.depth) {
            this._bestMove = {
              row: possibleMove.row,
              column: possibleMove.column
            };
          }
        }
        alpha = Math.max(alpha, evaluation);
        if (beta <= alpha) {
          break; // pruning
        }
      }
      return maxEvaluation;
    } else {
      let minEvaluation: number = Infinity;
      for (const possibleMove of board.possibleMoves) {
        const boardCopy: Board = board.copy();
        boardCopy.play(possibleMove.row, possibleMove.column);
        const evaluation: number = this._minimaxWithPruning(boardCopy, depth - 1, true, alpha, beta);
        if (evaluation < minEvaluation) {
          minEvaluation = evaluation;
        }
        beta = Math.min(beta, evaluation);
        if (beta <= alpha) {
          break; // pruning
        }
      }
      return minEvaluation;
    }
  }

  private _evaluateCurrentGameState(board: Board): number {
    return this._defensiveEvaluation(board);
  }

  private _aggressiveEvaluation(board: Board): number {
    const evaluationSign: number = board.playerTurn === this._player ? 1 : -1;
    let playerScore: number = 0;
    let opponentScore: number = 0;
    for (let row = 0; row < GAME_PROPERTIES.rows; ++row) {
      for (let column = 0; column < GAME_PROPERTIES.columns; ++column) {
        const piece: Piece = board.getPieceAt(row, column);
        const piecePotential: number = POTENTIALS_GRID[row][column];
        if (piece.playerNumber === board.playerTurn) {
          playerScore += piecePotential;
        }
        else if (piece.playerNumber !== null) {
          opponentScore += piecePotential;
        }
      }
    }
    const score: number = playerScore - opponentScore;
    return score * evaluationSign;
  }

  private _defensiveEvaluation(board: Board): number {
    const evaluationSign: number = board.playerTurn === this._player ? 1 : -1;
    if (!board.isEnded) {
      let playerScore: number = 0;
      let opponentScore: number = 0;
      for (let row = 0; row < GAME_PROPERTIES.rows; ++row) {
        for (let column = 0; column < GAME_PROPERTIES.columns; ++column) {
          const piece: Piece = board.getPieceAt(row, column);
          const piecePotential: number = POTENTIALS_GRID[row][column];
          if (piece.playerNumber === board.playerTurn) {
            playerScore += piecePotential;
          }
          else if (piece.playerNumber !== null) {
            opponentScore += piecePotential;
          }
        }
      }
      const score: number = playerScore - opponentScore;
      return score * evaluationSign;
    }
    if (board.isDraw) {
      return MINIMAX_PROPERTIES.gameEvaluation.draw * evaluationSign;
    }
    if (board.winner === this._player) {
      return MINIMAX_PROPERTIES.gameEvaluation.win * evaluationSign;
    }
    return MINIMAX_PROPERTIES.gameEvaluation.loss * evaluationSign;
  }
}
