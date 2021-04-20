import { canvasRecord } from '../src/utils';

const canvas = document.getElementsByTagName('canvas')[0];
const stst = document.getElementById('stst') as HTMLButtonElement;
const reset = document.getElementById('reset') as HTMLButtonElement;
const record = document.getElementById('record') as HTMLInputElement;
const download = document.getElementById('download') as HTMLAnchorElement;
const length = document.getElementById('length') as HTMLInputElement;
const speed = document.getElementById('speed') as HTMLInputElement;

const ctx = canvas.getContext('2d')!;

const scale = window.devicePixelRatio;
canvas.width = canvas.clientWidth * scale;
canvas.height = canvas.clientHeight * scale;

ctx.scale(scale, scale);

const TRANSITIONS = [0, 3, 2, 0, 0, 1, 3, 2, 3, 1];
const COLORS = ['#0d2282', '#10b05d', '#91a5ff', '#e81a81'];

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

  const cells = Array(length.valueAsNumber).fill(0);
  if (cells.length % 2 === 0) {
    return alert('The length must be odd.');
  }
  
  cells[cells.length / 2 - 0.5] = 1;

  const width = canvas.width / cells.length;

  ctx.fillStyle = COLORS[0];
  ctx.fillRect(0, 0, canvas.width, canvas.height);
  ctx.fillStyle = COLORS[1];
  ctx.fillRect(width * (cells.length / 2 - 0.5), 0, width, canvas.height);

  const delayer = delay <= 10 ?
    (cb: () => any) => requestAnimationFrame(cb) :
    (cb: () => any) => setTimeout(() => requestAnimationFrame(cb), delay);

  function automaton(cells: number[]): Promise<void> {
    return new Promise((resolve) => {
      const newCells = cells.slice();

      for (let i = 0; i < cells.length; i++) {
        const cell = TRANSITIONS[(cells[i - 1] ?? 0) + cells[i] + (cells[i + 1] ?? 0)];
        
        if (cells[i] !== cell) {
          const color = COLORS[newCells[i] = cell];
          ctx.fillStyle = color;
          ctx.fillRect(width * i, 0, width, canvas.height);
        }
      }

      if (stopped || cells.every((e, i) => e === newCells[i])) {
        stopped = true;
        stst.textContent = 'Start';
        return resolve();
      }

      delayer(() => resolve(automaton(newCells)));
    });
  }

  if (record.checked) {
    const closeRecord = canvasRecord(canvas);
    await new Promise((resolve) => delayer(() => resolve(automaton(cells))));
    const blob = await closeRecord();
    download.href = URL.createObjectURL(blob);
    download.download = `record.${blob.type.split('/')[1]}`;
    download.hidden = false;
    return;
  }

  return new Promise((resolve) => delayer(() => resolve(automaton(cells))));
}
