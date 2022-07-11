import applyCollisions from './applyCollisions';
import applyPhysics from './applyPhysics';

export default function render(elements, setElements, windowDimensions) {
	const halfWidth = windowDimensions.width/2;
	const halfHeight = windowDimensions.height/2;

	const framerate = 60;

	const newElements = elements.map((elt) => applyPhysics(elt, halfHeight, halfWidth, framerate));

	setElements(newElements.map((elt) => applyCollisions(elt, elements)));

	window.requestAnimationFrame(() => render(elements, setElements, windowDimensions));
}