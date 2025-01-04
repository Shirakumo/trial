#section FRAGMENT_SHADER
uniform float c_white;

vec3 tone_map(vec3 color){
  float l_in = color_luminance(color);
  float l_out = (l_in / (1.0 + l_in / (c_white * c_white))) / (1.0 + l_in);
  return (color / l_in) * l_out;
}
