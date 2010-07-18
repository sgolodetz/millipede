varying vec3 normal, L, viewDir;

void main()
{
	// TODO: Replace these with uniform parameters.
	float ambient = 0.3;
	float lambertianCoefficient = 0.35;
	float phongCoefficient = 0.35;
	float phongExponent = 20;

	vec3 N = normalize(normal);
	float NdotL = dot(N, L);
	float lambertian = clamp(NdotL, 0.0, 1.0);

	vec3 R = 2*NdotL*N - L;
	vec3 V = normalize(viewDir);
	float phong = pow(clamp(dot(R, V), 0.0, 1.0), phongExponent);
	phong = clamp(phong, 0.0, 1.0);

	float intensity = ambient + lambertianCoefficient * lambertian + phongCoefficient * phong;

	vec4 colour = gl_Color;
	colour.xyz *= intensity;

	gl_FragColor = colour;
}
