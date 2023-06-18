#section FRAGMENT_SHADER
uniform float inv_gamma = 1.0/2.2;

float color_luminance(vec3 color){
  return dot(color, vec3(0.299, 0.587, 0.114));
}

vec3 tone_map(vec3 color);

void main(){
  maybe_call_next_method();
  vec3 mapped = tone_map(color.xyz);
  mapped = clamp(mapped, vec3(0), vec3(1));
  color.xyz = pow(mapped.xyz, vec3(inv_gamma));
}
