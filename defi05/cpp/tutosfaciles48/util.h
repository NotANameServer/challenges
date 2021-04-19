//
// Created by Romain Neil on 18/04/2021.
//

#ifndef SDLFOREST_UTIL_H
#define SDLFOREST_UTIL_H

#include <SDL.h>
#include <SDL_image.h>

const int SCREEN_WIDTH = 400;
const int SCREEN_HEIGHT = 400;

const int BLOCK_SIZE = 4;

const float FPS = 1.f/10.f;

const int nb_cells = (SCREEN_WIDTH / BLOCK_SIZE) + (SCREEN_HEIGHT / BLOCK_SIZE);
const int nb_rows = (SCREEN_WIDTH / BLOCK_SIZE);
const int nb_cols = (SCREEN_HEIGHT / BLOCK_SIZE);

struct App {
	SDL_Renderer *renderer;
	SDL_Window *window;
};

enum CellStatus {
	YOUNG = 0,
	MATURE,
	START_BURNING,
	BURNING,
	END_BURNING,
	ASHES,
};

#endif //SDLFOREST_UTIL_H
