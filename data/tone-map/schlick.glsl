uniform float p, hi_val;

vec3 tone_map(vec3 color){
  float l_in = color_luminance(color);
  float l_out = (p * l_in) / (p * l_in - l_in + hi_val);
  return color / l_in * l_out;
}
