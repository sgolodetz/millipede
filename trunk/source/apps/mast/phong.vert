varying vec3 normal, L, viewDir;

void main()
{
	// Clip to any user-defined clipping planes.
	gl_ClipVertex = gl_ModelViewMatrix * gl_Vertex;

	//L = normalize(vec3(gl_LightSource[0].position));
	L = normalize(vec3(30.0, 30.0, 0.0));
	normal = normalize(gl_NormalMatrix * gl_Normal);
	viewDir = normalize(ftransform().xyz);

	gl_FrontColor = gl_Color;
	gl_Position = ftransform();
}
