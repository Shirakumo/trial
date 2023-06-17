uniform float a,b,c,d,e,f,w;
uniform float exposure_bias = 2.0;

vec3 uncharted_2_tonemap(vec3 x){
  return ((x*(a*x+c*b)+d*e)/(x*(a*x+b)+d*f))-e/f;
}

vec3 tone_map(vec3 color){
  vec3 curr = exposure_bias * uncharted_2_tonemap(color);
  vec3 white_scale = 1.0 / uncharted_2_tonemap(vec3(w));
  return curr * white_scale;
}
