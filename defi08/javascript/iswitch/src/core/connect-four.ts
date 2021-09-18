import { ArtificialIntelligence, Board, Piece, Player, View } from '.';
import { Minimax, Random } from '../algorithms';
import { PlayerNumber } from '../enums';
import { GameMode } from '../types';

export class ConnectFour {
  private _board: Board;
  private _view: View;
  private _players: Player[];
  private _gameModes: GameMode[];

  private get _previousPlayer(): Player {
    return this._players.find(
      (player: Player) => player.playerNumber === this._board.previousPlayerTurn
    )!;
  }

  private get _currentPlayer(): Player {
    return this._players.find(
      (player: Player) => player.playerNumber === this._board.playerTurn
    )!;
  }

  constructor(gameWrapperElement: HTMLElement) {
    this._board = new Board();
    this._view = new View(gameWrapperElement);
    this._players = [];
    this._gameModes = [
      {
        name: 'Player vs. Player',
        onClick: (players: Player[]) => {
          this._players = players;
          this._attachUIClickBehavior();
          this._view.displayUI();
        },
        players: [
          new Player(PlayerNumber.FIRST),
          new Player(PlayerNumber.SECOND)
        ],
        slug: 'player-vs-player'
      },
      {
        name: 'Player vs. Random AI',
        onClick: (players: Player[]) => {
          this._players = players;
          this._attachUIClickBehavior();
          this._view.displayUI();
        },
        players: [
          new Player(PlayerNumber.FIRST),
          new ArtificialIntelligence(PlayerNumber.SECOND, Random)
        ],
        slug: 'player-vs-random-ai'
      },
      {
        name: 'Player vs. Minimax AI',
        onClick: (players: Player[]) => {
          this._players = players;
          this._attachUIClickBehavior();
          this._view.displayUI();
        },
        players: [
          new Player(PlayerNumber.FIRST),
          new ArtificialIntelligence(PlayerNumber.SECOND, Minimax)
        ],
        slug: 'player-vs-minimax-ai'
      }
    ];
    this._board.afterPlay = (moveDone: Piece) => {
      this._view.addPiece(moveDone.column, this._board.previousPlayerTurn);
      this._manageUI();
      if (this._currentPlayer.isAI) {
        this._view.showLoader("AI is thinking, please wait...");
        /**
         * play only on the next event loop iteration to
         * let the opportunity to the DOM to redraw before.
         */
        setTimeout(() => {
          const aiMove = this._currentPlayer.play(this._board);
          this._board.play(aiMove.row, aiMove.column);
          this._view.closeLoader();
        }, 0);
      }
    };
    this._board.afterEnd = (moveDone: Piece) => {
      this._view.addPiece(moveDone.column, this._board.previousPlayerTurn);
      this._view.displayEndMenu(this._board.winner, () => {
        this.start();
      });
    };
  }

  private _attachUIClickBehavior(): void {
    this._view.setBlurrerClickBehavior((column: number) => {
      const selectedRow: number = this._view.getBlurrerOf(column).row;
      const selectedColumn: number = column;
      const playerMove = this._currentPlayer.play(
        this._board,
        selectedRow,
        selectedColumn
      );
      this._board.play(playerMove.row, playerMove.column);
    });
  }

  private _manageUI(): void {
    if (this._currentPlayer.isAI && !this._previousPlayer.isAI) {
      this._view.hideUI();
    } else if (!this._currentPlayer.isAI && this._previousPlayer.isAI) {
      this._view.displayUI();
    }
  }

  public start(): void {
    this._board.reset();
    this._view.clear();
    this._view.displayStartMenu(this._gameModes);
  }
}
