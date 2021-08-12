export const MINIMAX_PROPERTIES = {
    gameEvaluation: {
        draw: 0,
        win: 500000, // offset of any value significantly greater than the largest in POTENTIALS_GRID
        loss: -500000 // opposite of line above
    },
    depth: 7
};
