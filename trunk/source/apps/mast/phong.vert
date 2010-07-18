varying vec3 lightDir, normal, viewDir;

void main()
{
	// Calculate the eye coordinate position of the vertex.
	vec4 ecPos = gl_ModelViewMatrix * gl_Vertex;

	// Clip to any user-defined clipping planes.
	gl_ClipVertex = ecPos;

	// Calculate lightDir as the eye coordinate vector from the vertex to the light.
	if(gl_LightSource[0].position.w == 0.0)
	{
		// Directional light.
		lightDir = normalize(vec3(gl_LightSource[0].position));
	}
	else
	{
		// Point light.
		lightDir = normalize(vec3(gl_LightSource[0].position - ecPos));
	}
	

	// Calculate the eye coordinate normal.
	normal = normalize(gl_NormalMatrix * gl_Normal);

	// Calculate viewDir as the eye coordinate vector from the vertex to the viewer.
	// Note that in eye coordinates, the viewer is at the origin by definition.
	viewDir = normalize(-vec3(ecPos));

	// Set the colour and transform the vertex.
	gl_FrontColor = gl_Color;
	gl_Position = ftransform();
}

