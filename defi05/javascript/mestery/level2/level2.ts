import {  canvasRecord } from '../src/utils';

const canvas = document.getElementsByTagName('canvas')[0];
const stst = document.getElementById('stst') as HTMLButtonElement;
const reset = document.getElementById('reset') as HTMLButtonElement;
const record = document.getElementById('record') as HTMLInputElement;
const download = document.getElementById('download') as HTMLAnchorElement;
const widthSize = document.getElementById('width') as HTMLInputElement;
const heightSize = document.getElementById('height') as HTMLInputElement;
const speed = document.getElementById('speed') as HTMLInputElement;

const ctx = canvas.getContext('2d')!;

const scale = window.devicePixelRatio;
canvas.width = canvas.clientWidth * scale;
canvas.height = canvas.clientHeight * scale;

ctx.scale(scale, scale);

const enum State {
  YOUNG,
  OLD,
  START_OF_COMBUSTION,
  COMBUSTION,
  END_OF_COMBUSTION,
  ASH,
}
const COLORS = ['#4bb85a', '#054a29', '#fac748', '#ff591c', '#d00000', '#0c1618'];

let stopped = true;
let promise: Promise<void>;

reset.addEventListener('click', async () => {
  if (!stopped) {
    stopped = true;
    await promise;
  }

  download.hidden = true;
  ctx.clearRect(0, 0, canvas.width, canvas.height);
});

download.addEventListener('click', () => download.hidden = true);

stst.addEventListener('click', async () => {
  if (stopped) {
    stopped = false;
    stst.textContent = 'Stop';
    promise = main();
    return;
  }

  stopped = true;
  await promise;
  stst.textContent = 'Start';
});


async function main(): Promise<void> {
  const delay = speed.valueAsNumber;

  const grid: State[][] = Array.from({ length: heightSize.valueAsNumber }, () => Array(widthSize.valueAsNumber).fill(State.YOUNG));

  const width = canvas.width / grid[0].length;
  const height = canvas.height / grid.length;

  ctx.fillStyle = COLORS[State.YOUNG];
  ctx.fillRect(0, 0, canvas.width, canvas.height);

  const delayer = delay <= 10 ?
    (cb: () => any) => requestAnimationFrame(cb) :
    (cb: () => any) => setTimeout(() => requestAnimationFrame(cb), delay);

  function automaton(grid: State[][]): Promise<void> {
    return new Promise((resolve) => {
      const newGrid = grid.map((cells) => cells.slice());

      for (let i = 0; i < grid.length; i++) {
        for (let j = 0; j < grid[i].length; j++) {
          const cell = transition(grid[i][j], [
            grid[i][j - 1], // left
            grid[i][j + 1], // right
            grid[i - 1]?.[j], // top
            grid[i + 1]?.[j], // bottom
            grid[i - 1]?.[j - 1], // top-left
            grid[i - 1]?.[j + 1], // top-right
            grid[i + 1]?.[j - 1], // bottom-left
            grid[i + 1]?.[j + 1] // bottom-right
          ].filter((cell) => cell !== undefined));
        
          if (grid[i][j] !== cell) {
            const color = COLORS[newGrid[i][j] = cell];
            ctx.fillStyle = color;
            ctx.fillRect(width * j, height * i, width, height);
          }
        }
      }

      if (stopped) {
        stopped = true;
        stst.textContent = 'Start';
        return resolve();
      }

      delayer(() => resolve(automaton(newGrid)));
    });
  }

  function transition(cell: State, neighbours: State[]): State {
    let probability = 0;
    let to = cell;

    if (cell === State.YOUNG || cell === State.OLD) {
      to = State.START_OF_COMBUSTION;

      let multiplier = cell === State.OLD ? 10 : 1;
      if (neighbours.includes(State.START_OF_COMBUSTION)) {
        probability += 0.01 * multiplier;
      }
      if (neighbours.includes(State.COMBUSTION)) {
        probability += 0.02 * multiplier;
      }
      if (neighbours.includes(State.END_OF_COMBUSTION)) {
        probability += 0.01 * multiplier;
      }

      if (cell === State.YOUNG && probability === 0) {
        to = State.OLD;
        probability = 0.005;
      } else if (cell === State.OLD && neighbours.filter((cell) => cell === State.OLD).length > 4) {
        probability += 0.00005;
      }
    } else if ([State.START_OF_COMBUSTION, State.COMBUSTION, State.END_OF_COMBUSTION].includes(cell)) {
      to = cell + 1;
      probability = 0.1;
    } else if (cell === State.ASH) {
      to = State.YOUNG;
      probability = 0.001;
    }

    return probability === 0 || Math.random() > probability ? cell : to;
  }

  if (record.checked) {
    const closeRecord = canvasRecord(canvas);
    await new Promise((resolve) => delayer(() => resolve(automaton(grid))));
    const blob = await closeRecord();
    download.href = URL.createObjectURL(blob);
    download.download = `record.${blob.type.split('/')[1]}`;
    download.hidden = false;
    return;
  }

  return new Promise((resolve) => delayer(() => resolve(automaton(grid))));
}
