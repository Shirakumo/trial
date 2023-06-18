#section FRAGMENT_SHADER
uniform float ld_max;

vec3 tone_map(vec3 color){
  float l_in = color_luminance(color);
  float m = pow((1.219 + pow(ld_max / 2.0, 0.4)) / (1.219 + pow(l_in, 0.4)), 2.5);
  float l_out = m / ld_max * l_in;
  return color / l_in * l_out;
}
