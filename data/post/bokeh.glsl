#section FRAGMENT_SHADER
#define ITERATIONS 150
const float GOLDEN_ANGLE = (3.-sqrt(5.0))*acos(-1.0);
const mat2 rot = mat2(cos(GOLDEN_ANGLE), sin(GOLDEN_ANGLE), -sin(GOLDEN_ANGLE), cos(GOLDEN_ANGLE));
in vec2 uv;
out vec4 color;
uniform sampler2D previous_pass;
uniform float intensity = 1.0;

void main(){
  vec2 res = textureSize(previous_pass, 0).xy;
  vec2 prop = vec2(1.0, res.x/res.y);
  vec3 acc = vec3(0);
  vec3 div = vec3(0);
  float r = 1.0;
  vec2 vangle = vec2(0.0,intensity*0.01 / sqrt(float(ITERATIONS)));
    
  for(int j=0; j<ITERATIONS; j++){
    r += 1.0 / r;
    vangle = (rot * vangle);
    vec3 col = texture(previous_pass, uv + (r-1) * vangle * prop).xyz;
    vec3 bokeh = pow(col, vec3(8));
    acc += col * bokeh;
    div += bokeh;
  }
  color = vec4(acc / div, 1.0);
}
