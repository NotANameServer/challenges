import { Loader, Modal } from '.';
import { GAME_PROPERTIES } from '../data';
import { PlayerNumber } from '../enums';
import { GameMode } from '../types';

export class View {
  private _wrapper: HTMLElement;
  private _gameFragment: DocumentFragment;
  private _cellWidth: number;
  private _cellHeight: number;
  private _grid: HTMLDivElement[][];
  private _blurrers: { element: HTMLDivElement; row: number }[];
  private _blurrerBehaviors: ((mouseEvent: MouseEvent) => void)[];
  private _isBlurrerAttached: boolean;
  private _loader: Loader;
  private _modal: Modal;

  private get _gameWidth(): number {
    let gameWidth: number = this._wrapper.parentElement!.clientWidth;
    if (gameWidth > window.innerHeight) {
      gameWidth = window.innerHeight;
    }
    return gameWidth;
  }

  private get _gameHeight(): number {
    return (this._gameWidth / GAME_PROPERTIES.columns) * GAME_PROPERTIES.rows;
  }

  constructor(gameWrapperElement: HTMLElement) {
    this._wrapper = gameWrapperElement;
    this._cellWidth = 100 / GAME_PROPERTIES.columns;
    this._cellHeight = 100 / GAME_PROPERTIES.rows;
    this._grid = this._constructEmptyGrid();
    this._blurrers = [];
    this._blurrerBehaviors = [];
    this._isBlurrerAttached = false;
    this._loader = new Loader(this._wrapper);
    this._modal = new Modal(this._wrapper);
    this._gameFragment = this._prepareGameDisplay();
  }

  private _constructEmptyGrid(): HTMLDivElement[][] {
    const result: HTMLDivElement[][] = [];
    for (let row = 0; row < GAME_PROPERTIES.rows; ++row) {
      result[row] = [];
    }
    return result;
  }

  private _prepareGameDisplay(): DocumentFragment {
    const cells: DocumentFragment = this._buildCells();
    this._createBlurrers();
    this._attachBlurrers();
    return cells;
  }

  private _buildCells(): DocumentFragment {
    const cells: DocumentFragment = document.createDocumentFragment();
    for (let row = 0; row < GAME_PROPERTIES.rows; ++row) {
      for (let column = 0; column < GAME_PROPERTIES.columns; ++column) {
        const cell: HTMLDivElement = this._createCell(row, column);
        cells.appendChild(cell);
        this._grid[row][column] = cell;
      }
    }
    window.addEventListener('resize', (uiEvent: UIEvent) => {
      this._responsive();
    });
    return cells;
  }

  private _createCell(row: number, column: number): HTMLDivElement {
    const cell: HTMLDivElement = document.createElement('div');
    cell.style.width = `${this._cellWidth}%`;
    cell.style.height = `${this._cellHeight}%`;
    cell.classList.add('cell');
    cell.classList.add(`cellX-${column}`);
    cell.classList.add(`cellY-${row}`);
    return cell;
  }

  private _createBlurrers(): void {
    for (let column = 0; column < GAME_PROPERTIES.columns; ++column) {
      this._blurrers[column] = {
        element: this._createBlurrer(column),
        row: GAME_PROPERTIES.rows - 1
      };
    }
  }

  private _createBlurrer(
    column: number,
    playerNumber: PlayerNumber = PlayerNumber.FIRST
  ): HTMLDivElement {
    const blurrer: HTMLDivElement = document.createElement('div');
    blurrer.classList.add('blurrer');
    blurrer.classList.add(`blurrerX-${column}`);
    blurrer.classList.add(`blurrerY-${GAME_PROPERTIES.rows - 1}`);
    blurrer.classList.add(
      `blurrer-player-${playerNumber === PlayerNumber.FIRST ? 1 : 2}`
    );
    return blurrer;
  }

  private _attachBlurrers(): void {
    if (!this._isBlurrerAttached) {
      this._blurrers.forEach(
        (blurrer: { element: HTMLDivElement; row: number }, column: number) => {
          if (blurrer.row > -1) {
            this._grid[blurrer.row][column].appendChild(blurrer.element);
          }
        }
      );
      this._isBlurrerAttached = true;
    }
  }

  private _detachBlurrers(): void {
    if (this._isBlurrerAttached) {
      this._blurrers.forEach(
        (blurrer: { element: HTMLDivElement; row: number }, column: number) => {
          if (blurrer.row > -1) {
            this.getCellAt(blurrer.row, column).removeChild(blurrer.element);
          }
        }
      );
      this._isBlurrerAttached = false;
    }
  }

  private _responsive(): void {
    this._wrapper.style.width = `${this._gameWidth}px`;
    this._wrapper.style.height = `${this._gameHeight}px`;
  }

  private _createPiece(
    row: number,
    column: number,
    playerNumber: PlayerNumber
  ): HTMLDivElement {
    const piece: HTMLDivElement = document.createElement('div');
    piece.classList.add('piece');
    piece.classList.add(`pieceX-${column}`);
    piece.classList.add(`pieceY-${row}`);
    piece.classList.add(
      `piece-player-${playerNumber === PlayerNumber.FIRST ? 1 : 2}`
    );
    return piece;
  }

  private _buildStartMenu(gameModes: GameMode[]): DocumentFragment {
    const startMenu: DocumentFragment = document.createDocumentFragment();
    const title: HTMLHeadingElement = document.createElement('h1');
    title.classList.add('title');
    title.textContent = 'Connect 4';
    const subtitle: HTMLHeadingElement = document.createElement('h2');
    subtitle.classList.add('subtitle');
    subtitle.textContent = 'Select a game mode';
    const actions: HTMLDivElement = document.createElement('div');
    actions.classList.add('actions');
    gameModes.forEach((gameMode: GameMode) => {
      const button: HTMLButtonElement = document.createElement('button');
      button.classList.add('action');
      button.classList.add(`start-${gameMode.slug}`);
      button.textContent = gameMode.name;
      button.addEventListener('click', (mouseEvent: MouseEvent) => {
        gameMode.onClick(gameMode.players);
        this._modal.close();
      });
      actions.appendChild(button);
    });
    startMenu.appendChild(title);
    startMenu.appendChild(subtitle);
    startMenu.appendChild(actions);
    return startMenu;
  }

  private _buildEndMenu(
    winner: PlayerNumber | null,
    resetBehavior: () => void
  ): DocumentFragment {
    const endMenu: DocumentFragment = document.createDocumentFragment();
    const title: HTMLHeadingElement = document.createElement('h1');
    title.classList.add('title');
    title.textContent = 'Connect 4';
    const subtitle: HTMLHeadingElement = document.createElement('h2');
    subtitle.classList.add('subtitle');
    if (winner !== null) {
      subtitle.textContent = `Player ${winner == PlayerNumber.FIRST ? 1 : 2
        } won!`;
    } else {
      subtitle.textContent = 'Draw!';
    }
    const actions: HTMLDivElement = document.createElement('div');
    actions.classList.add('actions');
    const reset: HTMLButtonElement = document.createElement('button');
    reset.classList.add('action');
    reset.classList.add('reset');
    reset.textContent = 'Play again';
    reset.addEventListener('click', (mouseEvent: MouseEvent) => {
      resetBehavior();
    });
    actions.appendChild(reset);
    endMenu.appendChild(title);
    endMenu.appendChild(subtitle);
    endMenu.appendChild(actions);
    return endMenu;
  }

  private _initDisplay(): void {
    this._gameFragment = this._buildCells();
    this._createBlurrers();
    this._wrapper.appendChild(this._gameFragment);
    this._responsive();
  }

  public displayStartMenu(gameModes: GameMode[]): void {
    this._initDisplay();
    this._modal.content = this._buildStartMenu(gameModes);
    this._modal.show();
  }

  public displayUI(): void {
    this._attachBlurrers();
  }

  public hideUI(): void {
    this._detachBlurrers();
  }

  public displayEndMenu(
    winner: PlayerNumber | null,
    resetBehavior: () => void
  ): void {
    this.removeBlurrerClickBehavior();
    this._detachBlurrers();
    this._modal.content = this._buildEndMenu(winner, resetBehavior);
    this._modal.show();
  }

  public getCellAt(row: number, column: number): HTMLDivElement {
    return this._grid[row][column];
  }

  public getBlurrerOf(
    column: number
  ): { element: HTMLDivElement; row: number } {
    return this._blurrers[column];
  }

  public addPiece(column: number, playerNumber: PlayerNumber): void {
    const blurrer = this._blurrers[column];
    const cell = this.getCellAt(blurrer.row, column);
    if (this._isBlurrerAttached) {
      cell.removeChild(blurrer.element);
    }
    cell.appendChild(this._createPiece(blurrer.row, column, playerNumber));
    --blurrer.row;
    if (blurrer.row > -1 && this._isBlurrerAttached) {
      this.getCellAt(blurrer.row, column).appendChild(blurrer.element);
    }
    this._blurrers.forEach(
      (blurrer: { element: HTMLDivElement; row: number }) => {
        blurrer.element.classList.remove('blurrer-player-1');
        blurrer.element.classList.remove('blurrer-player-2');
        blurrer.element.classList.add(
          `blurrer-player-${playerNumber === PlayerNumber.FIRST ? 2 : 1}`
        );
      }
    );
  }

  public setBlurrerClickBehavior(behavior: (column: number) => void): void {
    this._blurrers.forEach(
      (blurrer: { element: HTMLDivElement; row: number }, column: number) => {
        this._blurrerBehaviors[column] = (mouseEvent: MouseEvent) => {
          behavior(column);
        };
        blurrer.element.addEventListener(
          'click',
          this._blurrerBehaviors[column]
        );
      }
    );
  }

  public removeBlurrerClickBehavior(): void {
    this._blurrers.forEach(
      (blurrer: { element: HTMLDivElement; row: number }, column: number) => {
        blurrer.element.removeEventListener(
          'click',
          this._blurrerBehaviors[column]
        );
      }
    );
  }

  public clear(): void {
    this._wrapper.innerHTML = '';
    this._isBlurrerAttached = false;
  }

  public showLoader(loadingMessage: string = ""): void {
    this._loader.loadingReason = loadingMessage;
    this._loader.show();
  }

  public closeLoader(): void {
    this._loader.close();
  }
}
