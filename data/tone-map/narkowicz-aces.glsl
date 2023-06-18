#section FRAGMENT_SHADER
vec3 tone_map(vec3 color){
  return (color*(2.51*color+0.03)) / (color*(2.43*color+0.59)+0.14);
}
