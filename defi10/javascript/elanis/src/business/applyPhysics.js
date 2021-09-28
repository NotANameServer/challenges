export default function applyPhysics(elt, halfHeight, halfWidth, framerate) {
	elt.x += elt.velocity.x / framerate;
	elt.y += elt.velocity.y / framerate;

	if(elt.x < -halfWidth || elt.x > halfWidth) {
		elt.velocity.x = -elt.velocity.x;
	}

	if(elt.y < -halfHeight || elt.y > halfHeight) {
		elt.velocity.y = -elt.velocity.y;
	}

	return elt;
}