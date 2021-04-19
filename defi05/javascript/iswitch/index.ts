// ========================
// ===== DEFINITIONS ======
// ========================

enum State {
  NONE = "white",
  LOW = "#70F8BA",
  MEDIUM = "#368F8B",
  HIGH = "#246A73",
  ULTRA = "#1F2041"
}

type Weights = {
  low: {
    medium: number;
    high: number;
    ultra: number;
  };
  medium: {
    low: number;
    high: number;
    ultra: number;
  };
  high: {
    low: number;
    medium: number;
    ultra: number;
  };
  ultra: {
    low: number;
    medium: number;
    high: number;
  };
};

class Automaton {
  private _origin: Element;
  private _container: HTMLCanvasElement;
  private _toolbar: HTMLDivElement;
  private _grid: State[][];
  private _refreshSpeed: number;
  private _isStopped: boolean;
  private _margin: number;
  private _initialStateAt: (
    line: number,
    column: number,
    automaton: Automaton
  ) => State;
  private _behavior: (
    line: number,
    column: number,
    automaton: Automaton
  ) => State;

  public set refreshSpeed(refreshSpeed: number) {
    this._refreshSpeed = refreshSpeed;
  }

  public set margin(margin: number) {
    this._margin = margin;
  }

  public set initialStateAt(
    initialStateAt: (
      line: number,
      column: number,
      automaton: Automaton
    ) => State
  ) {
    this._initialStateAt = initialStateAt;
  }

  public set behavior(
    behavior: (line: number, column: number, automaton: Automaton) => State
  ) {
    this._behavior = behavior;
  }

  constructor(htmlElement: Element, lines: number, columns: number) {
    this._origin = htmlElement;
    this._container = document.createElement("canvas");
    this._toolbar = document.createElement("div");
    this._grid = [];
    this._refreshSpeed = 60;
    this._isStopped = true;
    this._margin = 0;
    this._initialStateAt = () => {
      return State.NONE;
    };
    this._behavior = () => {
      return State.NONE;
    };
    this._initView(lines, columns);
    this._attachView();
    this._updateView();
  }

  public init(): void {
    this._initStates();
    this._updateView();
  }

  public start(): void {
    this.init();
    this.resume();
  }

  public stop(): void {
    this._isStopped = true;
  }

  public resume(): void {
    this._isStopped = false;
    this._update();
  }

  public stateAt(line: number, column: number): State {
    return this._grid[line][column];
  }

  public statesAround(line: number, column: number): State[] {
    const states: State[] = [];
    for (let i = line - 1; i <= line + 1; ++i) {
      for (let j = column - 1; j <= column + 1; ++j) {
        const currentLine: number =
          i < 0 ? this._grid.length - 1 : i >= this._grid.length ? 0 : i;
        const currentColumn: number =
          j < 0 ? this._grid[0].length - 1 : j >= this._grid[0].length ? 0 : j;
        if (currentLine !== line || currentColumn !== column) {
          states.push(this._grid[currentLine][currentColumn]);
        }
      }
    }
    return states;
  }

  private _initView(lines: number, columns: number) {
    this._createViewContainer();
    this._createViewGrid(lines, columns);
    this._createViewToolbar();
  }

  private _createViewContainer(): void {
    this._container.width = this._origin.clientWidth;
    this._container.height = this._origin.clientWidth;
    window.addEventListener("resize", () => {
      this._container.width = this._origin.clientWidth;
      this._container.height = this._origin.clientWidth;
      this._updateView();
    });
  }

  private _createViewGrid(lines: number, columns: number): void {
    for (let line = 0; line < lines; ++line) {
      this._grid[line] = [];
      for (let column = 0; column < columns; ++column) {
        this._grid[line][column] = State.NONE;
      }
    }
  }

  private _createViewToolbar(): void {
    this._toolbar.style.display = "flex";
    this._toolbar.style.flexDirection = "row";
    this._toolbar.style.flexWrap = "wrap";
    this._toolbar.style.justifyContent = "center";
    const playPause = document.createElement("a");
    const reset = document.createElement("a");
    playPause.addEventListener("click", () => {
      if (this._isStopped) {
        playPause.innerText = "Pause";
        this.resume();
      } else {
        playPause.innerText = "Play";
        this.stop();
      }
    });
    reset.addEventListener("click", () => {
      this.start();
    });
    playPause.style.padding = "5px";
    playPause.style.cursor = "pointer";
    playPause.style.color = "#000000";
    playPause.innerText = "Play";
    reset.style.padding = "5px";
    reset.style.cursor = "pointer";
    reset.style.color = "#000000";
    reset.innerText = "Reset";
    this._toolbar.appendChild(playPause);
    this._toolbar.appendChild(reset);
  }

  private _attachView(): void {
    this._origin.appendChild(this._container);
    this._origin.appendChild(this._toolbar);
  }

  private _initStates(): void {
    this._grid.forEach((line: State[], lineIndex: number) => {
      line.forEach((state: State, columnIndex: number) => {
        this._grid[lineIndex][columnIndex] = this._initialStateAt(
          lineIndex,
          columnIndex,
          this
        );
      });
    });
  }

  private _updateStates(): void {
    const newStates: State[][] = [];
    this._grid.forEach((line: State[], lineIndex: number) => {
      newStates[lineIndex] = [];
      line.forEach((state: State, columnIndex: number) => {
        newStates[lineIndex][columnIndex] = this._behavior(
          lineIndex,
          columnIndex,
          this
        );
      });
    });
    this._grid = newStates;
  }

  private _updateView(): void {
    const canvasContext: CanvasRenderingContext2D = this._container.getContext(
      "2d"
    );
    const rectWidth: number = Math.round(
      this._container.clientWidth / this._grid[0].length - 2 * this._margin
    );
    const rectHeight: number = Math.round(
      this._container.clientHeight / this._grid.length - 2 * this._margin
    );
    this._grid.forEach((line: State[], lineIndex: number) => {
      line.forEach((state: State, columnIndex: number) => {
        const rectX: number =
          columnIndex * (rectWidth + 2 * this._margin) + this._margin;
        const rectY: number =
          lineIndex * (rectHeight + 2 * this._margin) + this._margin;
        canvasContext.fillStyle = state;
        canvasContext.fillRect(rectX, rectY, rectWidth, rectHeight);
      });
    });
  }

  private _update(): void {
    setTimeout(() => {
      this._updateOnce();
      if (!this._isStopped) {
        this._update();
      }
    }, this._refreshSpeed);
  }

  private _updateOnce(): void {
    this._updateStates();
    this._updateView();
  }
}

// ========================
// ====== BEHAVIORS =======
// ========================

const behaviorGenerator = (weights: any) => {
  return (line: number, column: number, automaton: Automaton) => {
    const currentState: State = automaton.stateAt(line, column);
    const neighborsStates = automaton.statesAround(line, column);
    const amount: { [key in State]: number } = {
      [State.NONE]: 0,
      [State.LOW]: 0,
      [State.MEDIUM]: 0,
      [State.HIGH]: 0,
      [State.ULTRA]: 0
    };
    neighborsStates.forEach((neighborState: State) => {
      amount[neighborState]++;
    });
    switch (currentState) {
      case State.LOW:
        if (amount[State.MEDIUM] > weights.low.medium) {
          return State.MEDIUM;
        }
        if (amount[State.HIGH] > weights.low.high) {
          return State.HIGH;
        }
        if (amount[State.ULTRA] > weights.low.ultra) {
          return State.ULTRA;
        }
        return State.LOW;
      case State.MEDIUM:
        if (amount[State.LOW] > weights.medium.low) {
          return State.LOW;
        }
        if (amount[State.HIGH] > weights.medium.high) {
          return State.HIGH;
        }
        if (amount[State.ULTRA] > weights.medium.ultra) {
          return State.ULTRA;
        }
        return State.MEDIUM;
      case State.HIGH:
        if (amount[State.LOW] > weights.high.low) {
          return State.LOW;
        }
        if (amount[State.MEDIUM] > weights.high.medium) {
          return State.MEDIUM;
        }
        if (amount[State.ULTRA] > weights.high.ultra) {
          return State.ULTRA;
        }
        return State.HIGH;
      case State.ULTRA:
        if (amount[State.LOW] > weights.ultra.low) {
          return State.LOW;
        }
        if (amount[State.MEDIUM] > weights.ultra.medium) {
          return State.MEDIUM;
        }
        if (amount[State.HIGH] > weights.ultra.high) {
          return State.HIGH;
        }
        return State.ULTRA;
      default:
        return State.NONE;
    }
  };
};

const spiralBehavior = behaviorGenerator({
  low: { medium: 2, high: 5, ultra: 7 },
  medium: { low: 6, high: 4, ultra: 2 },
  high: { low: 2, medium: 1, ultra: 5 },
  ultra: { low: 2, medium: 7, high: 1 }
});

const wavesBehavior = behaviorGenerator({
  low: { medium: 3, high: 2, ultra: 4 },
  medium: { low: 4, high: 4, ultra: 0 },
  high: { low: 4, medium: 1, ultra: 6 },
  ultra: { low: 5, medium: 7, high: 2 }
});

const oceanBehavior = behaviorGenerator({
  low: { medium: 7, high: 6, ultra: 3 },
  medium: { low: 4, high: 1, ultra: 7 },
  high: { low: 2, medium: 1, ultra: 1 },
  ultra: { low: 4, medium: 3, high: 6 }
});

const randomBehavior = behaviorGenerator({
  low: {
    medium: Math.floor(Math.random() * 8),
    high: Math.floor(Math.random() * 8),
    ultra: Math.floor(Math.random() * 8)
  },
  medium: {
    low: Math.floor(Math.random() * 8),
    high: Math.floor(Math.random() * 8),
    ultra: Math.floor(Math.random() * 8)
  },
  high: {
    low: Math.floor(Math.random() * 8),
    medium: Math.floor(Math.random() * 8),
    ultra: Math.floor(Math.random() * 8)
  },
  ultra: {
    low: Math.floor(Math.random() * 8),
    medium: Math.floor(Math.random() * 8),
    high: Math.floor(Math.random() * 8)
  }
});

// ========================
// ======== USAGE =========
// ========================

const element = document.querySelector(".automaton");
if (element) {
  const automaton = new Automaton(element, 100, 100);
  automaton.initialStateAt = (line: number, column: number) => {
    const random: number = Math.random();
    if (random > 0.75) {
      return State.LOW;
    }
    if (random > 0.5) {
      return State.MEDIUM;
    }
    if (random > 0.25) {
      return State.HIGH;
    }
    return State.ULTRA;
  };
  automaton.behavior = spiralBehavior;
  automaton.init();
}