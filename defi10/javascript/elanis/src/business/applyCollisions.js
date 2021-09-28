export default function applyCollisions(elt, elements) {
	for(const otherElement of elements) {
		if(otherElement.id === elt.id) {
			continue;
		}

		const distance = Math.pow(otherElement.x - elt.x, 2) + Math.pow(otherElement.y - elt.y, 2);

		if(distance <= Math.pow(elt.radius + otherElement.radius, 2)) {
			elt.velocity.x = (elt.x - otherElement.x); // * (elt.velocity.x / (elt.velocity.x + otherElement.velocity.x));
			elt.velocity.y = (elt.y - otherElement.y); // * (elt.velocity.y / (elt.velocity.y + otherElement.velocity.y));
		}
	}

	return elt;
}