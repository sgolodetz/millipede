varying vec3 lightDir, normal, viewDir;

void main()
{
	// TODO: Replace these with uniform parameters.
	float ambient = 0.3;
	float lambertianCoefficient = 0.35;
	float phongCoefficient = 0.35;
	float phongExponent = 20.0;

	// Calculate the Lambertian lighting term.
	vec3 L = normalize(lightDir);
	vec3 N = normalize(normal);
	float NdotL = dot(N, L);
	float lambertian = clamp(NdotL, 0.0, 1.0);

	// Calculate the Phong lighting term.
	vec3 R = 2.0*NdotL*N - L;
	vec3 V = normalize(viewDir);
	float phong = pow(clamp(dot(R, V), 0.0, 1.0), phongExponent);
	phong = clamp(phong, 0.0, 1.0);

	// Calculate the desired intensity.
	float intensity = ambient + lambertianCoefficient * lambertian + phongCoefficient * phong;

	// Use the intensity to modulate the user-specified colour.
	vec4 colour = gl_Color;
	colour.xyz *= intensity;

	// Set the desired fragment colour.
	gl_FragColor = colour;
}

