#section FRAGMENT_SHADER
uniform float p, hi_val;

vec3 tone_map(vec3 color){
  float l_in = color_luminance(color);
  float l_out = (p * l_in) / (p * l_in + hi_val - l_in);
  return (color / l_in) * l_out;
}
