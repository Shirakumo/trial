#section FRAGMENT_SHADER
#define F_PI_2 1.5707964f
uniform float intensity = 1.0;
uniform vec2 origin = vec2(0.5);

vec4 post_process(sampler2D previous_pass, vec2 uv){
  vec2 d = uv - origin;
  float r = sqrt(dot(d, d));
  float power = intensity * F_PI_2;
  vec2 cuv;
  if (power > 0.0)
    cuv = 0.5 + normalize(d) * tan(r*power) / tan(power);
  else if (power < 0.0)
    cuv = 0.5 + normalize(d) * atan(r*power*-10.0) * 0.5 / atan(power*-5.0);
  else cuv = uv;

  return texture(previous_pass, cuv);
}
