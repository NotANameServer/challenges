import { GAME_PROPERTIES } from ".";
import { Piece } from "../core";

export const POTENTIALS_GRID: number[][] = [];
/**
 * @deprecated For some not understood reason,
 * Webpack doesn't resolve all modules before
 * executing following code.
 * The hack here is to wrap it inside a setTimeout
 * callback to execute it on the next event loop iteration.
 */
setTimeout(() => {
    for (let row = 0; row < GAME_PROPERTIES.rows; ++row) {
        POTENTIALS_GRID[row] = [];
        for (let column = 0; column < GAME_PROPERTIES.columns; ++column) {
            // powering up the values for more differentiation
            POTENTIALS_GRID[row][column] = Math.pow(new Piece(row, column).comboPotential(), 2);
        }
    }
});
