import { Maze, CellType } from ".";

const [width, height] = process.argv.slice(2);
const myMaze = new Maze(parseInt(width, 10) || 21, parseInt(height, 10) || 11);

console.log(myMaze.toString()); // normal output

console.log('\nCSV:\n')
// csv output
console.log(myMaze.toString({
  separator: ',',
  customCharMap: {
    [CellType.EMPTY]: '" "',
  }
}));
