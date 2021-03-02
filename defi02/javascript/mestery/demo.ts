import { Maze } from ".";
const myMaze = new Maze(21, 11);

console.log(myMaze.toString()); // normal output
console.log(myMaze.toString({ separator: ',' })); // csv output
