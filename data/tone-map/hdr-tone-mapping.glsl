#section FRAGMENT_SHADER
uniform sampler2D previous_pass;
uniform float inv_gamma;
in vec2 uv;
out vec4 color;

float color_luminance(vec3 color){
  return dot(color, vec3(0.299, 0.587, 0.114));
}

vec3 tone_map(vec3 color);

void main(){
  vec3 mapped = clamp(tone_map(texture(previous_pass, uv).xyz), vec3(0), vec3(1));
  color = vec4(pow(mapped, vec3(inv_gamma)), 1.0);
}
