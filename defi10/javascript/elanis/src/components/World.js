import React, { useState } from 'react';

import './World.css';

import { Canvas2D } from 'canvas2d-wrapper';
import render from '../business/render';

export default function World({ baseElements, windowDimensions }) {
	const [elements, setElements] = useState([...baseElements]); 
	const [drawing, setDrawing] = useState(false);

	if(!drawing) {
		setDrawing(true);

		window.requestAnimationFrame(() => render(elements, setElements, windowDimensions));
	}

	return (
		<Canvas2D 
			elements={elements}
			width={windowDimensions.width}
			height={windowDimensions.height}
			tileSize={1}
			lockXAxis={true}
			lockYAxis={true}
		/>
	);
}
