//
// Created by Romain Neil on 18/04/2021.
//

#ifndef SDLFOREST_CELL_H
#define SDLFOREST_CELL_H

#include "util.h"

class Cell {

	public:

		Cell(int x, int y, CellStatus status = YOUNG);

		CellStatus getStatus() {
			return m_status;
		}

		void setStatus(CellStatus status) {
			m_status = status;
		}

		void draw(SDL_Renderer *renderer, SDL_Window *window);

	private:

		int m_x;
		int m_y;

		SDL_Rect m_rect;

		CellStatus m_status;
};


#endif //SDLFOREST_CELL_H
