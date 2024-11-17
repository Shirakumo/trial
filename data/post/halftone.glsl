#section FRAGMENT_SHADER
uniform float dotsize = 2.5;

vec4 halftone(in vec2 fc, in mat2 m, in vec2 resolution){
	vec2 smp = m*fc - mod(m*fc, dotsize);
    smp = (smp + 0.5*dotsize) * m;
	float s = min(length(fc-smp) / (1.48*0.5*dotsize), 1.0);
    vec3 texc = texture(previous_pass, (smp/resolution+0.5)).rgb;
    float k = max(max(texc.r, texc.g), texc.b);
    vec4 c = min(vec4(texc.rgb / k, k), 1.0);
	return c+s;
}

const float SST = 0.888;
const float SSQ = 0.288;
const float R1 = radians(30.0 + 15.0);
const float R2 = radians(30.0 + 75.0);
const float R3 = radians(30.0 +  0.0);
const float R4 = radians(30.0 + 45.0);
const mat2 mc = mat2(cos(R1),-sin(R1),sin(R1),cos(R1));
const mat2 mm = mat2(cos(R2),-sin(R2),sin(R2),cos(R2));
const mat2 my = mat2(cos(R3),-sin(R3),sin(R3),cos(R3));
const mat2 mk = mat2(cos(R4),-sin(R4),sin(R4),cos(R4));

vec4 post_process(sampler2D previous_pass, vec2 uv){
  vec2 resolution = textureSize(previous_pass, 0);
  vec2 fc = (uv.xy-0.5)*resolution;
  float k = halftone(fc, mk, resolution).a;
  vec4 c = vec4(halftone(fc, mc, resolution).r,
                halftone(fc, mm, resolution).g,
                halftone(fc, my, resolution).b,
                halftone(fc, mk, resolution).a);
  c = smoothstep(SST-SSQ, SST+SSQ, c);
  return vec4(c.rgb*c.a, 1.0);
}
