import applyPhysics from './applyPhysics';

export default function render(elements, setElements, windowDimensions) {
	const halfWidth = windowDimensions.width/2;
	const halfHeight = windowDimensions.height/2;
	
	const framerate = 60;

	setElements(elements.map((elt) => applyPhysics(elt, halfHeight, halfWidth, framerate)));

	window.requestAnimationFrame(() => render(elements, setElements, windowDimensions));
}