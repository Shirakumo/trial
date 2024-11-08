#section FRAGMENT_SHADER
#define F_PI_2 1.5707964f
in vec2 uv;
out vec4 color;
uniform sampler2D previous_pass;
uniform float intensity = 1.0;
uniform vec2 origin = vec2(0.5);

void main(){
  vec2 d = uv - origin;
  float r = sqrt(dot(d, d));
  float power = intensity * F_PI_2;
  vec2 cuv;
  if (power > 0.0)
    cuv = 0.5 + normalize(d) * tan(r*power) / tan(power);
  else if (power < 0.0)
    cuv = 0.5 + normalize(d) * atan(r*power*-10.0) * 0.5 / atan(power*-5.0);
  else cuv = uv;

  color = texture(previous_pass, cuv);
}
